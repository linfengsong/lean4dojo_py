
import re
import os
import json
import toml
import time
import urllib
import shutil
import tempfile
import webbrowser
from enum import Enum
from pathlib import Path
from loguru import logger
from functools import cache
from git import Repo, BadName
from github import Github, Auth
from dataclasses import dataclass, field
from github.Repository import Repository
from github.GithubException import GithubException
from typing import List, Dict, Any, Union, Optional, Tuple, Iterator

from .lean import LeanRepo
from ..constants import TMP_DIR
from .cache import cache as repo_cache
from ..utils import (
    read_url,
    url_exists
)

GITHUB_ACCESS_TOKEN = os.getenv("GITHUB_ACCESS_TOKEN", None)
"""GiHub personal access token is optional. 
If provided, it can increase the rate limit for GitHub API calls.
"""

if GITHUB_ACCESS_TOKEN:
    logger.debug("Using GitHub personal access token for authentication")
    GITHUB = Github(auth=Auth.Token(GITHUB_ACCESS_TOKEN))
    GITHUB.get_user().login
else:
    logger.debug(
        "Using GitHub without authentication. Don't be surprised if you hit the API rate limit."
    )
    GITHUB = Github()

LEAN4_REPO = None
"""The GitHub Repo for Lean 4 itself."""

LEAN4_NIGHTLY_REPO = None
"""The GitHub Repo for Lean 4 nightly releases."""

_URL_REGEX = re.compile(r"(?P<url>.*?)/*")

_SSH_TO_HTTPS_REGEX = re.compile(r"git@github\.com:(?P<user>.+)/(?P<repo>.+?)(\.git)?")

REPO_CACHE_PREFIX = "repos"


class RepoType(Enum):
    GITHUB = 0
    REMOTE = 1  # Remote but not GitHub.
    LOCAL = 2


def normalize_url(url: str, repo_type: RepoType = RepoType.GITHUB) -> str:
    if repo_type == RepoType.LOCAL:  # Convert to absolute path if local.
        return os.path.abspath(url)
    # Remove trailing `/`.
    url = _URL_REGEX.fullmatch(url)["url"]  # type: ignore
    return ssh_to_https(url)


def ssh_to_https(url: str) -> str:
    m = _SSH_TO_HTTPS_REGEX.fullmatch(url)
    return f"https://github.com/{m.group('user')}/{m.group('repo')}" if m else url


def get_repo_type(url: str) -> Optional[RepoType]:
    """Get the type of the repository.

    Args:
        url (str): The URL of the repository.
    Returns:
        Optional[str]: The type of the repository (None if the repo cannot be found).
    """
    url = ssh_to_https(url)
    parsed_url = urllib.parse.urlparse(url)  # type: ignore
    if parsed_url.scheme in ["http", "https"]:
        # Case 1 - GitHub URL.
        if "github.com" in url:
            if not url.startswith("https://"):
                logger.warning(f"{url} should start with https://")
                return None
            else:
                return RepoType.GITHUB
        # Case 2 - remote URL.
        elif url_exists(url):  # Not check whether it is a git URL
            return RepoType.REMOTE
    # Case 3 - local path
    elif is_git_repo(Path(parsed_url.path)):
        return RepoType.LOCAL
    logger.warning(f"{url} is not a valid URL")
    return None


def _split_git_url(url: str) -> Tuple[str, str]:
    """Split a Git URL into user name and repo name."""
    if url.endswith("/"):
        url = url[:-1]
        assert not url.endswith("/"), f"Unexpected URL: {url}"
    fields = url.split("/")
    user_name = fields[-2]
    repo_name = fields[-1]
    return user_name, repo_name


def _format_cache_dirname(url: str, commit: str) -> str:
    user_name, repo_name = _split_git_url(url)
    repo_type = get_repo_type(url)
    assert repo_type is not None, f"Invalid url {url}"
    if repo_type == RepoType.GITHUB:
        return f"{user_name}-{repo_name}-{commit}"
    else:  # git repo
        return f"gitpython-{repo_name}-{commit}"


@cache
def url_to_repo(
    url: str,
    num_retries: int = 2,
    repo_type: Optional[RepoType] = None,
    tmp_dir: Optional[Path] = None,
) -> Union[Repo, Repository]:
    """Convert a URL to a Repo object.

    Args:
        url (str): The URL of the repository.
        num_retries (int): Number of retries in case of failure.
        repo_type (Optional[RepoType]): The type of the repository. Defaults to None.
        tmp_dir (Optional[Path]): The temporary directory to clone the repo to. Defaults to None.

    Returns:
        Repo: A Git Repo object.
    """
    url = normalize_url(url)
    backoff = 1
    if tmp_dir is None:
        tmp_dir = (TMP_DIR or Path("/tmp")) / next(tempfile._get_candidate_names())  # type: ignore
    repo_type = repo_type or get_repo_type(url)
    assert repo_type is not None, f"Invalid url {url}"
    while True:
        try:
            if repo_type == RepoType.GITHUB:
                return GITHUB.get_repo("/".join(url.split("/")[-2:]))
            with working_directory(tmp_dir):
                repo_name = os.path.basename(url)
                if repo_type == RepoType.LOCAL:
                    assert is_git_repo(url), f"Local path {url} is not a git repo"
                    shutil.copytree(url, repo_name)
                    return Repo(repo_name)
                else:
                    return Repo.clone_from(url, repo_name)
        except Exception as ex:
            if num_retries <= 0:
                raise ex
            num_retries -= 1
            logger.debug(f'url_to_repo("{url}") failed. Retrying...')
            time.sleep(backoff)
            backoff *= 2


@cache
def get_latest_commit(url: str) -> str:
    """Get the hash of the latest commit of the Git repo at ``url``."""
    repo = url_to_repo(url)
    if isinstance(repo, Repository):
        return repo.get_branch(repo.default_branch).commit.sha
    else:
        return repo.head.commit.hexsha


def cleanse_string(s: Union[str, Path]) -> str:
    """Replace : and / with _ in a string."""
    return str(s).replace("/", "_").replace(":", "_")


@cache
def _to_commit_hash(repo: Union[Repository, Repo], label: str) -> str:
    """Convert a tag or branch to a commit hash."""
    if isinstance(repo, Repository):  # GitHub repository
        logger.debug(f"Querying the commit hash for {repo.name} {label}")
        try:
            return repo.get_commit(label).sha
        except GithubException as ex:
            raise ValueError(f"Invalid tag or branch: `{label}` for {repo.name}")
    else:  # Local or remote Git repository
        assert isinstance(repo, Repo)
        logger.debug(
            f"Querying the commit hash for {repo.working_dir} repository {label}"
        )
        try:
            # Resolve the label to a commit hash
            return repo.commit(label).hexsha
        except Exception as ex:
            raise ValueError(f"Error converting ref to commit hash: {ex}")


_COMMIT_REGEX = re.compile(r"[0-9a-z]+")
_LEAN4_VERSION_REGEX = re.compile(r"leanprover/lean4:(?P<version>.+?)")


def is_commit_hash(s: str):
    """Check if a string is a valid commit hash."""
    return len(s) == 40 and _COMMIT_REGEX.fullmatch(s)


def get_lean4_version_from_config(toolchain: str) -> str:
    """Return the required Lean version given a ``lean-toolchain`` config."""
    m = _LEAN4_VERSION_REGEX.fullmatch(toolchain.strip())
    assert m is not None, "Invalid config."
    v = m["version"]
    if not v.startswith("v") and v[0].isnumeric():
        v = "v" + v
    return v

def get_lean4_commit_from_config(config_dict: Dict[str, Any]) -> str:
    """Return the required Lean commit given a ``lean-toolchain`` config."""
    global LEAN4_REPO
    if LEAN4_REPO is None:
        LEAN4_REPO = GITHUB.get_repo("leanprover/lean4")
    assert "content" in config_dict, "config_dict must have a 'content' field"
    version = get_lean4_version_from_config(config_dict["content"].strip())
    if version.startswith("nightly-"):
        global LEAN4_NIGHTLY_REPO
        if LEAN4_NIGHTLY_REPO is None:
            LEAN4_NIGHTLY_REPO = GITHUB.get_repo("leanprover/lean4-nightly")
        return _to_commit_hash(LEAN4_NIGHTLY_REPO, version)
    else:
        return _to_commit_hash(LEAN4_REPO, version)


def is_supported_version(v) -> bool:
    """Check if ``v`` is at least `v4.3.0-rc2`."""
    if not v.startswith("v"):
        return False
    v = v[1:]
    major, minor, patch = [int(_) for _ in v.split("-")[0].split(".")]
    if major < 4 or (major == 4 and minor < 3):
        return False
    if (
        major > 4
        or (major == 4 and minor > 3)
        or (major == 4 and minor == 3 and patch > 0)
    ):
        return True
    assert major == 4 and minor == 3 and patch == 0
    if "4.3.0-rc" in v:
        rc = int(v.split("-")[1][2:])
        return rc >= 2
    else:
        return True

URL = str
TAG = str
COMMIT = str

@dataclass(frozen=True)
class RepoInfoCache:
    """To minize the number of network requests, we cache and re-use the info
    of all repos, assuming it does not change during the execution of LeanDojo."""

    tag2commit: Dict[Tuple[URL, TAG], COMMIT] = field(default_factory=dict)
    lean_version: Dict[Tuple[URL, COMMIT], str] = field(default_factory=dict)


info_cache = RepoInfoCache()

class LeanGitRepo(LeanRepo):
    """Git repo of a Lean project."""

    url: str
    """The repo's URL.

    It can be a GitHub URL that starts with https:// or git@github.com, a local path, or any other valid Git URL.
    """

    commit: str
    """The repo's commit hash.

    You can also use tags such as ``v3.5.0``. They will be converted to commit hashes.
    """

    repo: Union[Repository, Repo]
    """A :class:`github.Repository` object for GitHub repos or
    a :class:`git.Repo` object for local or remote Git repos.
    """

    repo_type: RepoType
    """Type of the repo. It can be ``GITHUB``, ``LOCAL`` or ``REMOTE``.
    """

    def __init__(self, url: str, commit: str) -> None:
        object.__setattr__(self, "url", url)
        object.__setattr__(self, "commit", commit)
        print(f"LeanGitRepo Initializing LeanRepo: url={self.url}, commit={self.commit}")
        repo_type = get_repo_type(self.url)
        if repo_type is None:
            raise ValueError(f"{self.url} is not a valid URL")
        object.__setattr__(self, "repo_type", repo_type)
        object.__setattr__(self, "url", normalize_url(self.url, repo_type=repo_type))
        # set repo and commit
        working_dir = None

        """Return the formatted cache directory name"""
        assert is_commit_hash(self.commit), f"Invalid commit hash: {self.commit}"
        cache_dirname = Path(_format_cache_dirname(self.url, self.commit))
        repo_name = os.path.basename(self.url)
        cache_repo_dir = f"{cache_dirname}/{repo_name}"

        if repo_type == RepoType.GITHUB:
            repo = url_to_repo(self.url, repo_type=self.repo_type)
            assert is_commit_hash(self.commit), f"Invalid commit hash: {self.commit}"
        else:
            # get repo from cache
            rel_cache_dir = lambda url, commit: Path(
                f"{REPO_CACHE_PREFIX}/{cache_dirname}/{self.name}"
            )
            cache_repo_dir = repo_cache.get(rel_cache_dir(self.url, self.commit))
            print(f"#### LeanRepo: cache_repo_dir={cache_repo_dir}")
            if cache_repo_dir is None:
                with working_directory() as tmp_dir:
                    repo = url_to_repo(self.url, repo_type=self.repo_type, tmp_dir=tmp_dir)
                    commit = _to_commit_hash(repo, self.commit)
                    cache_repo_dir = repo_cache.store(
                        repo.working_dir, rel_cache_dir(self.url, commit)
                    )
            repo = Repo(cache_repo_dir)
            working_dir = repo.working_dir

        # Convert tags or branches to commit hashes
        if not is_commit_hash(self.commit):
            if (self.url, self.commit) in info_cache.tag2commit:
                commit = info_cache.tag2commit[(self.url, self.commit)]
            else:
                repo = Repo(cache_repo_dir)
                commit = _to_commit_hash(repo, self.commit)
                assert is_commit_hash(commit), f"Invalid commit hash: {commit}"
                info_cache.tag2commit[(self.url, commit)] = commit
            object.__setattr__(self, "commit", commit)
        object.__setattr__(self, "repo", repo)

        # Determine the required Lean version.
        if (self.url, self.commit) in info_cache.lean_version:
            lean_version = info_cache.lean_version[(self.url, self.commit)]
        else:
            config = self.get_config("lean-toolchain")
            lean_version = get_lean4_version_from_config(config["content"])
            if not is_supported_version(lean_version):
                logger.warning(
                    f"{self} relies on an unsupported Lean version: {lean_version}"
                )
        info_cache.lean_version[(self.url, self.commit)] = lean_version

        super().__init__(working_dir, lean_version, repo_name, cache_repo_dir)

    @property
    def commit_url(self) -> str:
        return f"{self.url}/tree/{self.commit}"

    def exists(self) -> bool:
        if self.repo_type != RepoType.GITHUB:
            repo = self.repo  # git repo
            try:
                repo.commit(self.commit)
                return repo.head.commit.hexsha == self.commit
            except BadName:
                logger.warning(
                    f"Commit {self.commit} does not exist in this repository."
                )
                return False
        else:
            return url_exists(self.commit_url)
        
    def show(self) -> None:
        """Show the repo in the default browser."""
        webbrowser.open(self.commit_url)

    def clone_and_checkout(self) -> None:
        """Clone the repo to the current working directory and checkout a specific commit."""
        logger.debug(f"Cloning {self}")
        repo = Repo.clone_from(self.url, Path(self.name), no_checkout=True)
        repo.git.checkout(self.commit)
        repo.submodule_update(init=True, recursive=True)


    def _parse_deps(
        self, matches: Union[Iterator[re.Match[str]], Dict[str, str]]
    ) -> List[Tuple[str, "LeanGitRepo"]]:
        deps = []

        for m in matches:
            url = m["url"]  # type: ignore
            if url.endswith(".git"):
                url = url[:-4]
            if url.startswith("git@"):
                url = "https://" + url[4:].replace(":", "/")
            try:
                rev = m["rev"]  # type: ignore
            except KeyError:
                rev = None
            if rev is None:
                commit = get_latest_commit(url)
            elif len(rev) == 40 and _COMMIT_REGEX.fullmatch(rev):
                commit = rev
            else:
                try:
                    commit = _to_commit_hash(url_to_repo(url), rev)
                except ValueError:
                    commit = get_latest_commit(url)
                assert _COMMIT_REGEX.fullmatch(commit)

            deps.append((m["name"], LeanGitRepo(url, commit)))  # type: ignore

        return deps
    
    def get_license(self) -> Optional[str]:
        """Return the content of the ``LICENSE`` file."""
        if self.repo_type == RepoType.GITHUB:
            assert "github.com" in self.url, f"Unsupported URL: {self.url}"
            url = self.url.replace("github.com", "raw.githubusercontent.com")
            license_url = f"{url}/{self.commit}/LICENSE"
            try:
                return read_url(license_url)
            except urllib.error.HTTPError:  # type: ignore
                return None
        else:
            return super().get_license()

    def _get_config_url(self, filename: str) -> str:
        assert self.repo_type == RepoType.GITHUB
        assert "github.com" in self.url, f"Unsupported URL: {self.url}"
        url = self.url.replace("github.com", "raw.githubusercontent.com")
        return f"{url}/{self.commit}/{filename}"

    def get_config(self, filename: str, num_retries: int = 2) -> Dict[str, Any]:
        """Return the repo's files."""
        if self.repo_type == RepoType.GITHUB:
            config_url = self._get_config_url(filename)
            content = read_url(config_url, num_retries)
        else:
            return super().get_config(filename, num_retries)
        if filename.endswith(".toml"):
            return toml.loads(content)
        elif filename.endswith(".json"):
            return json.loads(content)
        else:
            return {"content": content}

    def uses_lakefile_lean(self) -> bool:
        """Check if the repo uses a ``lakefile.lean``."""
        if self.repo_type == RepoType.GITHUB:
            url = self._get_config_url("lakefile.lean")
            return url_exists(url)
        else:
            return super().uses_lakefile_lean()

    def uses_lakefile_toml(self) -> bool:
        """Check if the repo uses a ``lakefile.toml``."""
        if self.repo_type == RepoType.GITHUB:
            url = self._get_config_url("lakefile.toml")
            return url_exists(url)
        else:
            return super().uses_lakefile_toml()
