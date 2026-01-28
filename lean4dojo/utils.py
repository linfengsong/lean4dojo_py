"""Utility functions used internally by LeanDojo."""

import re
import os
import ray
import time
import urllib
import typing
import hashlib
import tempfile
import subprocess
import json
from pathlib import Path
from loguru import logger
from functools import cache
from contextlib import contextmanager
from ray.util.actor_pool import ActorPool
from typing import Tuple, Union, List, Generator, Optional

from .constants import (
    NUM_WORKERS, 
    TMP_DIR,
    LEAN4_PACKAGES_DIR,
    LEAN4_BUILD_DIR,
    LEAN4_SRC_LEAN_DIR,
    LEAN4_SRC_LEAN_LAKE_DIR,
    LEAN4_LIB_LEAN_DIR,
    LEAN4_LEAN_LAKE_NAME_PREFIX,
    LEAN4_TOOLCHAINS_DIR,
    LEAN4_TRACED_DIR,
    LEAN4_TRACED_PACKAGES_DIR,
    LEAN4_TRACED_TOOLCHAINS_DIR,
)


@contextmanager
def working_directory(
    path: Optional[Union[str, Path]] = None,
) -> Generator[Path, None, None]:
    """Context manager setting the current working directory (CWD) to ``path`` (or a temporary directory if ``path`` is None).

    The original CWD is restored after the context manager exits.

    Args:
        path (Optional[Union[str, Path]], optional): The desired CWD. Defaults to None.

    Yields:
        Generator[Path, None, None]: A ``Path`` object representing the CWD.
    """
    origin = Path.cwd()
    if path is None:
        tmp_dir = tempfile.TemporaryDirectory(dir=TMP_DIR)
        path = tmp_dir.__enter__()
        is_temporary = True
    else:
        is_temporary = False

    path = Path(path)
    if not path.exists():
        path.mkdir(parents=True)
    os.chdir(path)

    try:
        yield path
    finally:
        os.chdir(origin)
        if is_temporary:
            tmp_dir.__exit__(None, None, None)


@contextmanager
def ray_actor_pool(
    actor_cls: type, *args, **kwargs
) -> Generator[ActorPool, None, None]:
    """Create a pool of Ray Actors of class ``actor_cls``.

    Args:
        actor_cls (type): A Ray Actor class (annotated by ``@ray.remote``).
        *args: Position arguments passed to ``actor_cls``.
        **kwargs: Keyword arguments passed to ``actor_cls``.

    Yields:
        Generator[ActorPool, None, None]: A :class:`ray.util.actor_pool.ActorPool` object.
    """
    assert not ray.is_initialized()
    ray.init(address="local")
    pool = ActorPool([actor_cls.remote(*args, **kwargs) for _ in range(NUM_WORKERS)])  # type: ignore
    try:
        yield pool
    finally:
        ray.shutdown()


@contextmanager
def report_critical_failure(msg: str) -> Generator[None, None, None]:
    """Context manager logging ``msg`` in case of any exception.

    Args:
        msg (str): The message to log in case of exceptions.

    Raises:
        ex: Any exception that may be raised within the context manager.
    """
    try:
        yield
    except Exception as ex:
        logger.error(msg)
        raise ex


def execute(
    cmd: Union[str, List[str]], capture_output: bool = False
) -> Optional[Tuple[str, str]]:
    """Execute the shell command ``cmd`` and optionally return its output.

    Args:
        cmd (Union[str, List[str]]): The shell command to execute.
        capture_output (bool, optional): Whether to capture and return the output. Defaults to False.

    Returns:
        Optional[Tuple[str, str]]: The command's output, including stdout and stderr (None if ``capture_output == False``).
    """
    logger.debug(cmd)
    res = subprocess.run(cmd, shell=True, capture_output=capture_output, check=True)
    if not capture_output:
        return None
    output = res.stdout.decode()
    error = res.stderr.decode()
    return output, error


def compute_md5(path: Path) -> str:
    """Return the MD5 hash of the file ``path``."""
    # The file could be large
    # See: https://stackoverflow.com/questions/48122798/oserror-errno-22-invalid-argument-when-reading-a-huge-file
    hasher = hashlib.md5()
    with path.open("rb") as inp:
        while True:
            block = inp.read(64 * (1 << 20))
            if not block:
                break
            hasher.update(block)
    return hasher.hexdigest()


_CAMEL_CASE_REGEX = re.compile(r"(_|-)+")


def camel_case(s: str) -> str:
    """Convert the string ``s`` to camel case."""
    return _CAMEL_CASE_REGEX.sub(" ", s).title().replace(" ", "")


def is_optional_type(tp: type) -> bool:
    """Test if ``tp`` is Optional[X]."""
    if typing.get_origin(tp) != Union:
        return False
    args = typing.get_args(tp)
    return len(args) == 2 and args[1] == type(None)


def remove_optional_type(tp: type) -> type:
    """Given Optional[X], return X."""
    assert typing.get_origin(tp) == Union
    args = typing.get_args(tp)
    if len(args) == 2 and args[1] == type(None):
        return args[0]
    else:
        raise ValueError(f"{tp} is not Optional")


@cache
def read_url(url: str, num_retries: int = 2) -> str:
    """Read the contents of the URL ``url``. Retry if failed"""
    backoff = 1
    while True:
        try:
            request = urllib.request.Request(url)  # type: ignore
            gh_token = os.getenv("GITHUB_ACCESS_TOKEN")
            if gh_token is not None:
                request.add_header("Authorization", f"token {gh_token}")
            with urllib.request.urlopen(request) as f:  # type: ignore
                return f.read().decode()
        except Exception as ex:
            if num_retries <= 0:
                raise ex
            num_retries -= 1
            logger.debug(f"Request to {url} failed. Retrying...")
            time.sleep(backoff)
            backoff *= 2


@cache
def url_exists(url: str) -> bool:
    """Return True if the URL ``url`` exists, using the GITHUB_ACCESS_TOKEN for authentication if provided."""
    try:
        request = urllib.request.Request(url)  # type: ignore
        gh_token = os.getenv("GITHUB_ACCESS_TOKEN")
        if gh_token is not None:
            request.add_header("Authorization", f"token {gh_token}")
        with urllib.request.urlopen(request) as _:  # type: ignore
            return True
    except urllib.error.HTTPError:  # type: ignore
        return False


def parse_int_list(s: str) -> List[int]:
    assert s.startswith("[") and s.endswith("]")
    return [int(_) for _ in s[1:-1].split(",") if _ != ""]


def parse_str_list(s: str) -> List[str]:
    assert s.startswith("[") and s.endswith("]")
    return [_.strip()[1:-1] for _ in s[1:-1].split(",") if _ != ""]


@cache
def is_git_repo(path: Path) -> bool:
    """Check if ``path`` is a Git repo."""
    with working_directory(path):
        return (
            os.system("git rev-parse --is-inside-work-tree 1>/dev/null 2>/dev/null")
            == 0
        )

@cache
def get_toolchain_name(working_dir: str) -> str:
     with working_directory(working_dir):
        lean_prefix = execute(f"lake env lean --print-prefix", capture_output=True)[0].strip()
        path = Path(lean_prefix)
        return path.name

@cache
def get_lean_version() -> str:
    """Get the version of Lean."""
    output = execute("lean --version", capture_output=True)[0].strip()
    m = re.match(r"Lean \(version (?P<version>\S+?),", output)
    return m["version"]  # type: ignore

@cache
def is_new_version(v: str) -> bool:
    """Check if ``v`` is at least `4.3.0-rc2`."""
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

@cache
def get_packages_path() -> Path:
    if is_new_version(get_lean_version()):
        return Path(".lake/packages")    
    else:
        return Path("lake-packages")
    
@cache
def get_build_path() -> Path:
    if is_new_version(get_lean_version()):
        return Path(".lake/build")    
    else:
        return Path("build")

@cache
def get_elan_toolchain_path(working_dir: str) -> Path:
    toolchainName =get_toolchain_name(working_dir)
    return Path.home() / LEAN4_TOOLCHAINS_DIR /Path(toolchainName)

@cache
def get_package_versions(working_dir: str) -> dict:
    with open(working_dir + "/lake-manifest.json", 'r') as file:
        package_versions = {}
        jsonData = json.load(file)
        packages = jsonData["packages"]
        for package in packages:
            name = package["name"]
            rev = package["rev"]
            package_versions[name] = rev
        return package_versions
    
@cache    
def get_traced_package_parent_path() ->Path:
    return Path.home() / LEAN4_TRACED_PACKAGES_DIR

@cache    
def get_traced_package_paths(working_dir: str) ->List[Path]:
    h = str(Path.home())
    paths = []
    package_versions = get_package_versions(working_dir)
    for name, version in package_versions.items():
        p = get_traced_package_parent_path() / Path(name + "-" + version)
        paths.append(p)
    return paths

def get_traced_working_path(working_dir: str) ->str:
    return Path(working_dir) / LEAN4_TRACED_DIR

@cache    
def get_traced_package_path(working_dir: str, package_name: str) ->str:
    h = str(Path.home())
    package_versions = get_package_versions(working_dir)
    if package_name in package_versions:
        version = package_versions[package_name]
        return get_traced_package_parent_path() / Path(package_name + "-" + version)
    print(f"Package {package_name} not found in lake-manifest.json: {package_versions}")
    return None

def get_traced_toolchain_path(working_dir: str) -> Path:
    toolchainName =get_toolchain_name(working_dir)
    return Path.home() / LEAN4_TRACED_TOOLCHAINS_DIR / Path(toolchainName)


def get_olean_working_path(working_dir: Path) -> Path:
    return Path(working_dir) / LEAN4_BUILD_DIR / LEAN4_LIB_LEAN_DIR

@cache    
def get_olean_package_paths(working_dir: str) -> List[Path]:
    h = str(Path.home())
    paths = []
    packages_path = get_packages_path()
    working_dir / get_packages_path()
    package_versions = get_package_versions(working_dir)
    for package_name in package_versions.keys():
        path = Path(working_dir) / Path(packages_path) / Path(package_name) / LEAN4_BUILD_DIR / LEAN4_LIB_LEAN_DIR
        paths.append(path)
    return paths

def get_olean_toolchain_path(working_dir: str) -> Path:
    return get_elan_toolchain_path(working_dir) / Path("lib/lean")

def to_trace_file_ext_from_olean(root_dir: Path, path: Path, repo, ext: str) -> Path:
    assert path.suffix == ".olean", "path is {path}"
    ext_path = path.with_suffix(ext)
    if ext_path.is_absolute():
        assert ext_path.is_relative_to(root_dir), f"root_dir is {root_dir}, lean_path is {ext_path}"
        ext_path = ext_path.relative_to(root_dir)

    if root_dir == get_elan_toolchain_path(repo.working_dir):
        #/home/linfe/.elan/toolchains/leanprover--lean4---v4.20.0, lib/lean/Init/Data.olean
        #/home/linfe/.alean/toolchains/leanprover--lean4---v4.20.0, Init/Data.ast.json
        return  get_traced_toolchain_path(repo.working_dir), ext_path.relative_to(LEAN4_LIB_LEAN_DIR)
    else:
        assert root_dir == Path(repo.working_dir), f"root_dir {root_dir} is not working_dir {repo.working_dir}"
        if ext_path.is_relative_to(LEAN4_PACKAGES_DIR):
            #/home/linfe/math/dojo/lean4dojo_py/lean4-example3, .lake/packages/mathlib/.lake/build/lib/lean/Mathlib/Algebra/EuclideanDomain/Field.olean
            ext_path = ext_path.relative_to(LEAN4_PACKAGES_DIR)
            package_name = ext_path.parts[0]
            ext_package_path = ext_path.relative_to(Path(package_name))
            package_path = get_traced_package_path(repo.working_dir, package_name)
            #/home/linfe/.alean/packages/mathlib-c211948581bde9846a99e32d97a03f0d5307c31e, Mathlib/Data/Nat/Factorial/Cast.ast.json
            return package_path, ext_package_path.to_relative(LEAN4_BUILD_DIR / LEAN4_LIB_LEAN_DIR)
        else:
            #/home/linfe/math/dojo/lean4dojo_py/lean4-example3, .lake/build/lib/lean/Lean4Examp.olean
            #/home/linfe/math/dojo/lean4dojo_py/lean4-example3, .alean/Lean4Example.ast.json
            return root_dir, LEAN4_TRACED_DIR / ext_path.to_relative(LEAN4_BUILD_DIR / LEAN4_LIB_LEAN_DIR)  

def _from_lean_path(root_dir: Path, path: Path, repo, ext: str) -> tuple[Path, Path]:
    """ working_dir: project working_dir, path relative path to working_dir or absolute lean file path
	    return package, toolchain or project working directory and relative trace file path 
    """
	
    assert path.suffix == ".lean", "path is {path}"

    ext_path = path.with_suffix(ext)
    working_dir = Path(repo.working_dir)
    if ext_path.is_absolute():
        assert ext_path.is_relative_to(root_dir), f"root_dir is {root_dir}, lean_path is {ext_path}"
        ext_path = ext_path.relative_to(root_dir)

    if root_dir == get_elan_toolchain_path(repo.working_dir):
        #/home/linfe/.elan/toolchains/leanprover--lean4---v4.20.0, src/lean/Init/Data.lean
        assert ext_path.is_relative_to(LEAN4_SRC_LEAN_DIR), f"ext_path {ext_path} is not relative to {LEAN4_SRC_LEAN_DIR}"
        ext_path = ext_path.relative_to(LEAN4_SRC_LEAN_DIR)

        if ext_path.is_relative_to(LEAN4_SRC_LEAN_LAKE_DIR):
            #/home/linfe/.elan/toolchains/leanprover--lean4---v4.20.0, lake/Lake/Build/Package.lean
            #Lake/Build/Package.ast.json
            ext_path = ext_path.relative_to(LEAN4_SRC_LEAN_LAKE_DIR)
    
        #//home/linfe/.alean/toolchains/leanprover--lean4---v4.20.0, Init/Data.ast.json
        return  get_traced_toolchain_path(repo.working_dir), ext_path
    elif root_dir.is_relative_to(working_dir / LEAN4_PACKAGES_DIR):
        #/home/linfe/math/dojo/lean4dojo_py/lean4-example3/.lake/packages/mathlib, Mathlib/Data/Nat/Factorial/Cast.lean
        package_name = str(root_dir.relative_to(working_dir / LEAN4_PACKAGES_DIR))
        package_path = get_traced_package_path(repo.working_dir, package_name)
        #/home/linfe/.alean/packages/mathlib-c211948581bde9846a99e32d97a03f0d5307c31e, Mathlib/Data/Nat/Factorial/Cast.ast.json
        return package_path, ext_path
    else:
        assert root_dir == Path(repo.working_dir), f"root_dir {root_dir}  {type(root_dir)}is not working_dir {repo.working_dir}"
        #/home/linfe/math/dojo/lean4dojo_py/lean4-example3, Lean4Examp.lean
        #/home/linfe/math/dojo/lean4dojo_py/lean4-example3, .alean/Lean4Example.ast.json
        return root_dir, LEAN4_TRACED_DIR / ext_path  
    
def to_xml_path(root_dir: Path, path: Path, repo) -> tuple[Path, Path]:
    return _from_lean_path(root_dir, path, repo, ext=".trace.xml")


def to_dep_path(root_dir: Path, path: Path, repo) -> tuple[Path, Path]:
    return _from_lean_path(root_dir, path, repo, ext=".dep_paths")


def to_json_path(root_dir: Path, path: Path, repo) -> tuple[Path, Path]:
    return _from_lean_path(root_dir, path, repo, ext=".ast.json")


def to_lean_path(root_dir: Path, path: Path, repo) -> tuple[Path, Path]:
    """ working_dir: project working_dir, path relative path to working_dir or absolute trace file path
        return package, toolchain or project working directory and relative leantrace file path 
    """

    assert path.suffix == ".json" and Path(path.stem).suffix == ".ast" or path.suffix == ".dep_paths" or path.suffix == ".xml" and Path(path.stem).suffix == ".trace", f"path is {path}, root_dir is {root_dir}"

    lean_path = path.with_suffix("").with_suffix(".lean")
    if lean_path.is_absolute():
        assert lean_path.is_relative_to(root_dir), f"root_dir is {root_dir}, lean_path is {lean_path}"
        lean_path = lean_path.relative_to(root_dir)

    working_dir = Path(repo.working_dir)
    traced_package_parent_path = get_traced_package_parent_path()
    if root_dir == get_traced_toolchain_path(repo.working_dir):
        if str(lean_path).startswith(LEAN4_LEAN_LAKE_NAME_PREFIX):
            #/home/linfe/.elan/toolchains/leanprover--lean4---v4.20.0, Lake/Build/Package.lean
            #/home/linfe/.elan/toolchains/leanprover--lean4---v4.20.0, lake/Lake/Build/Package.lean
            lean_path = LEAN4_SRC_LEAN_LAKE_DIR / lean_path
        #/home/linfe/.alean/toolchains/leanprover--lean4---v4.20.0, Init/Data.ast.json

        #/home/linfe/.elan/toolchains/leanprover--lean4---v4.20.0, src/lean/Init/Data.lean
        #/home/linfe/.alean/toolchains/leanprover--lean4---v4.20.0, src/lean/lake/Lake/Build/Package.lean
        return get_elan_toolchain_path(repo.working_dir), LEAN4_SRC_LEAN_DIR / lean_path
    elif root_dir.is_relative_to(traced_package_parent_path):
        #/home/linfe/.alean/packages/mathlib-c211948581bde9846a99e32d97a03f0d5307c31e, Mathlib/Data/Nat/Factorial/Cast.ast.json
        package_path = root_dir.relative_to(traced_package_parent_path)
        index = str(package_path).rindex("-")
        package_name = str(package_path)[:index]
        #/home/linfe/math/dojo/lean4dojo_py/lean4-example3/.lake/packages/mathlib, Mathlib/Data/Nat/Factorial/Cast.lean
        return working_dir / LEAN4_PACKAGES_DIR / Path(package_name), lean_path 
    else:
        #/home/linfe/math/dojo/lean4dojo_py/lean4-example3, .alean/Lean4Example.ast.json
        #/home/linfe/math/dojo/lean4dojo_py/lean4-example3, Lean4Examp.lean
        assert lean_path.is_relative_to(LEAN4_TRACED_DIR), f"lean_path {lean_path} is not relative to {LEAN4_TRACED_DIR}"
        return working_dir, lean_path.relative_to(LEAN4_TRACED_DIR)
