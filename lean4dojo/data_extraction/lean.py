"""This module define classes for repos, files, and theorems in Lean.
Objects of these classes contain only surface information, without extracting any trace.
"""

import re
import os
import shutil
import json
import toml
from pathlib import Path
from loguru import logger
from dataclasses import dataclass, field
from typing import List, Dict, Any, Generator, Union, Optional, Tuple

from .trace import build_trace

def get_lean4_src_path(lean4_version: str) -> str:
    """Return the required Lean src path given a ``lean4_version``."""
    home_directory = Path.home()
    return str(home_directory / f".elan/toolchains/leanprover--lean4---{lean4_version}/src/lean")

@dataclass(eq=True, unsafe_hash=True)
class Pos:
    """Position in source files.

    We use 1-index to keep it consistent with code editors such as Visual Studio Code.
    """

    line_nb: int
    """Line number
    """

    column_nb: int
    """Column number
    """

    @classmethod
    def from_str(cls, s: str) -> "Pos":
        """Construct a :class:`Pos` object from its string representation, e.g., :code:`"(323, 1109)"`."""
        assert s.startswith("(") and s.endswith(
            ")"
        ), f"Invalid string representation of a position: {s}"
        line, column = s[1:-1].split(",")
        line_nb = int(line)
        column_nb = int(column)
        return cls(line_nb, column_nb)

    def __iter__(self) -> Generator[int, None, None]:
        yield self.line_nb
        yield self.column_nb

    def __repr__(self) -> str:
        return repr(tuple(self))

    def __lt__(self, other):
        return self.line_nb < other.line_nb or (
            self.line_nb == other.line_nb and self.column_nb < other.column_nb
        )

    def __le__(self, other):
        return self < other or self == other


@dataclass(frozen=True)
class LeanFile:
    """A Lean source file (:file:`*.lean`)."""

    root_dir: Path = field(repr=False)
    """Root directory of the traced repo this :class:`LeanFile` object belongs to.

    ``root_dir`` must be an absolute path, e.g., :file:`/home/kaiyu/traced_lean-example/lean-example`
    """

    path: Path
    """Relative path w.r.t. ``root_dir``
    
    E.g., :file:`lean-example/src/example.lean`
    """

    code: List[str] = field(init=False, repr=False)
    """Raw source code as a list of lines."""

    endwith_newline: bool = field(init=False, repr=False)
    """Whether the last line ends with a newline."""

    num_bytes: List[int] = field(init=False, repr=False)
    """The number of UTF-8 bytes of each line, including newlines.
    """

    def __post_init__(self) -> None:
        assert (
            self.root_dir.is_absolute()
        ), f"Root directory must be an absolute path: {self.root_dir}"
        assert self.path.suffix == ".lean", f"File extension must be .lean: {self.path}"
        assert not self.path.is_absolute(), f"Path must be a relative path: {self.path}"

        code = []
        endwith_newline = None
        num_bytes = []

        for line in self.abs_path.open("rb"):
            if b"\r\n" in line:
                raise RuntimeError(
                    f"{self.abs_path} contains Windows-style line endings. This is discouraged (see https://github.com/leanprover-community/mathlib4/pull/6506)."
                )
            if line.endswith(b"\n"):
                endwith_newline = True
                line = line[:-1]
            else:
                endwith_newline = False
            code.append(line.decode("utf-8"))
            num_bytes.append(len(line) + 1)

        object.__setattr__(self, "code", code)
        object.__setattr__(self, "endwith_newline", endwith_newline)
        object.__setattr__(self, "num_bytes", num_bytes)

    @property
    def abs_path(self) -> Path:
        """Absolute path of a :class:`LeanFile` object.

        E.g., :file:`/home/kaiyu/traced_lean-example/lean-example/src/example.lean`
        """
        return self.root_dir / self.path

    @property
    def num_lines(self) -> int:
        """Number of lines in a source file."""
        return len(self.code)

    def num_columns(self, line_nb: int) -> int:
        """Number of columns in a source file."""
        return len(self.get_line(line_nb))

    @property
    def start_pos(self) -> Pos:
        """Return the start position of a source file.

        Returns:
            Pos: A :class:`Pos` object representing the start of this file.
        """
        return Pos(1, 1)

    @property
    def end_pos(self) -> Pos:
        """Return the end position of a source file.

        Returns:
            Pos: A :class:`Pos` object representing the end of this file.
        """
        # Line and column numbers are 1-indexed by default.
        if self.is_empty():
            return self.start_pos
        line_nb = self.num_lines
        column_nb = 1 + len(self.code[-1])
        return Pos(line_nb, column_nb)

    def is_empty(self) -> bool:
        return len(self.code) == 0

    def convert_pos(self, byte_idx: int) -> Pos:
        """Convert a byte index (:code:`String.Pos` in Lean 4) to a :class:`Pos` object."""
        n = 0
        for i, num_bytes in enumerate(self.num_bytes, start=1):
            n += num_bytes
            if n == byte_idx and i == self.num_lines:
                byte_idx -= 1
            if n > byte_idx:
                line_byte_idx = byte_idx - (n - num_bytes)
                if line_byte_idx == 0:
                    return Pos(i, 1)

                line = self.get_line(i)
                m = 0

                for j, c in enumerate(line, start=1):
                    m += len(c.encode("utf-8"))
                    if m >= line_byte_idx:
                        return Pos(i, j + 1)

        raise ValueError(f"Invalid byte index {byte_idx} in {self.path}.")

    def offset(self, pos: Pos, delta: int) -> Pos:
        """Off set a position by a given number."""
        line_nb, column_nb = pos
        num_columns = len(self.get_line(line_nb)) - column_nb + 1
        if delta <= num_columns:
            return Pos(line_nb, column_nb + delta)
        delta_left = delta - num_columns - 1

        for i in range(line_nb, self.num_lines):
            line = self.code[i]
            l = len(line)
            if delta_left <= l:
                return Pos(i + 1, delta_left + 1)
            delta_left -= l + 1

        if delta_left == 0 and self.endwith_newline:
            return Pos(self.num_lines + 1, 1)

        raise ValueError(f"Invalid offset {delta} in {self.path}: {pos}.")

    def get_line(self, line_nb: int) -> str:
        """Return a given line of the source file.

        Args:
            line_nb (int): Line number (1-indexed).
        """
        return self.code[line_nb - 1]

    def __getitem__(self, key) -> str:
        """Return a code segment given its start/end positions.

        This enables ``lean_file[start:end]``.

        Args:
            key (slice): A slice of two :class:`Pos` objects for the start/end of the code segment.
        """
        assert isinstance(key, slice) and key.step is None
        if key.start is None:
            start_line = start_column = 1
        else:
            start_line, start_column = key.start
        if key.stop is None:
            end_line = self.num_lines
            end_column = 1 + len(self.get_line(end_line))
        else:
            end_line, end_column = key.stop
        if start_line == end_line:
            assert start_column <= end_column
            return self.get_line(start_line)[start_column - 1 : end_column - 1]
        else:
            assert start_line < end_line
            code_slice = [self.code[start_line - 1][start_column - 1 :]]
            for line_nb in range(start_line + 1, end_line):
                code_slice.append(self.get_line(line_nb))
            code_slice.append(self.get_line(end_line)[: end_column - 1])
            return "\n".join(code_slice)

_LAKEFILE_LEAN_GIT_REQUIREMENT_REGEX = re.compile(
    r"require\s+(?P<name>\S+)\s+from\s+git\s+\"(?P<url>.+?)\"(\s+@\s+\"(?P<rev>\S+)\")?"
)

_LAKEFILE_LEAN_LOCAL_REQUIREMENT_REGEX = re.compile(r"require \S+ from \"")

_LAKEFILE_TOML_REQUIREMENT_REGEX = re.compile(r"(?<=\[\[require\]\]).+(?=\n\n)")


class LeanRepo:
    """Repo of a Lean project."""
    
    working_dir: str
    """The working directory of the repo on the disk.

    It is the path to the working directory of the repo on the disk.
    """

    lean_version: str
    """Required Lean version.
    """

    name: str
    """Required repo name.
    """

    cache_dir: str = None
    """ Optional to specify the cache directory name.
    """

    def __init__(self, working_dir: str, lean_version: str, name: str, cache_dir: str = None) -> None:
        self.working_dir = working_dir
        self.lean_version = lean_version
        self.name = name
        self.cache_dir = cache_dir

    @classmethod
    def from_path(cls, path: Union[Path, str], lean_version: str, name: str, build_deps = False) -> "LeanRepo":
        """Construct a :class:`LeanRepo` object from the path to a local Git repo."""
        if build_deps and Path(path).exists():
                shutil.rmtree(path)
        if build_deps or not Path(path).exists():
            build_trace(path, build_deps)
        repo = cls(str(path), lean_version, name)
        return repo

    @property
    def is_lean4(self) -> bool:
        return self.name == "lean4"
    
    def is_available_in_cache(self) -> bool:
        return self.cache_dir is not None

    def get_cache_dir(self) -> Path:
        """Return the formatted cache directory name"""
        assert self.cache_dir is not None, "cache_dir is not set"
        return Path(self.cache_dir)

    def get_dependencies(
        self, path: Union[str, Path, None] = None
    ) -> Dict[str, "LeanRepo"]:
        """Return the dependencies required by the target repo.

        Args:
            path (Union[str, Path, None], optional): Root directory of the repo if it is on the disk.

        Returns:
            Dict[str, :class:`LeanGitRepo`]: A dictionary mapping the name of each
            dependency to its :class:`LeanGitRepo` object.
        """
        print(f"#### get_dependencies")
        logger.debug(f"Querying the dependencies of {self}")

        lean4_src_path = get_lean4_src_path(self.lean_version)
        deps = {"lean4": LeanRepo(lean4_src_path, self.lean_version, "lean4")}

        try:
            lake_manifest = (
                self.get_config("lake-manifest.json", num_retries=0)
                if path is None
                else json.load((Path(path) / "lake-manifest.json").open())
            )
            packagesDir = lake_manifest["packagesDir"]
            for pkg in lake_manifest["packages"]:
                pkg_name = pkg["name"]
                pkg_path = path / packagesDir / "packages" / name
                pkg_version = pkg["inputRev"]
                print(f"#### dependency name: {pkg_name}, path: {pkg_path}, version:{pkg_version}")
                deps[name] = LeanRepo(pkg_path, pkg_version, pkg_name)
        except Exception:
            for name, repo in self._parse_lakefile_dependencies(path):
                if name not in deps:
                    deps[name] = repo
                for dd_name, dd_repo in repo.get_dependencies().items():
                    deps[dd_name] = dd_repo

        return deps

    def _parse_lakefile_dependencies(
        self, path: Union[str, Path, None]
    ) -> List[Tuple[str, "LeanRepo"]]:
        if self.uses_lakefile_lean():
            return self._parse_lakefile_lean_dependencies(path)
        else:
            return self._parse_lakefile_toml_dependencies(path)

    def _parse_lakefile_lean_dependencies(
        self, path: Union[str, Path, None]
    ) -> List[Tuple[str, "LeanRepo"]]:
        lakefile = (
            self.get_config("lakefile.lean")["content"]
            if path is None
            else (Path(path) / "lakefile.lean").open().read()
        )

        if _LAKEFILE_LEAN_LOCAL_REQUIREMENT_REGEX.search(lakefile):
            raise ValueError("Local dependencies are not supported.")

        return self._parse_deps(_LAKEFILE_LEAN_GIT_REQUIREMENT_REGEX.finditer(lakefile))

    def _parse_lakefile_toml_dependencies(
        self, path: Union[str, Path, None]
    ) -> List[Tuple[str, "LeanRepo"]]:
        lakefile = (
            self.get_config("lakefile.toml")
            if path is None
            else (Path(path) / "lakefile.toml").open().read()
        )
        # Parsing worked
        if isinstance(lakefile, dict) and "require" in lakefile:
            matches = lakefile["require"]
        else:
            if "content" in lakefile:
                lakefile = lakefile["content"]
            matches = []
            for req in _LAKEFILE_TOML_REQUIREMENT_REGEX.finditer(lakefile):
                match = {}
                for line in req.group().strip().splitlines():
                    key, value = line.split("=")
                    match[key.strip()] = value.strip()
                matches.append(match)
        for match in matches:
            if "path" in match:
                raise ValueError("Local dependencies are not supported.")
            if "git" in match:
                match["url"] = match["git"]
                del match["git"]

        return self._parse_deps(matches)

    def get_license(self) -> Optional[str]:
        """Return the content of the ``LICENSE`` file."""
        license_path = Path(self.working_dir) / "LICENSE"
        if license_path.exists():
            return license_path.open("r").read()
        else:
            return None

    def get_config(self, filename: str, num_retries: int = 2) -> Dict[str, Any]:
        """Return the repo's files."""
        working_dir = self.working_dir
        config_path = os.path.join(working_dir, filename)
        print(f"#### get_config: {config_path}")
        with open(config_path, "r") as f:
            content = f.read()
        if filename.endswith(".toml"):
            return toml.loads(content)
        elif filename.endswith(".json"):
            return json.loads(content)
        else:
            return {"content": content}

    def uses_lakefile_lean(self) -> bool:
        """Check if the repo uses a ``lakefile.lean``."""
        lakefile_path = Path(self.working_dir) / "lakefile.lean"
        return lakefile_path.exists()

    def uses_lakefile_toml(self) -> bool:
        """Check if the repo uses a ``lakefile.toml``."""
        lakefile_path = Path(self.working_dir) / "lakefile.toml"
        return lakefile_path.exists()
        
    def save_to(self, dst_dir: Path) -> None:
        dst_dir = Path(dst_dir)
        assert (
            not dst_dir.exists()
        ), f"The destination directory {dst_dir} already exists."
        shutil.copytree(self.working_dir, dst_dir)
        print(f"Saved repo to {dst_dir}")


@dataclass(frozen=True)
class Theorem:
    """Theorem in Lean.

    Theorems are named constants of type :code:`Prop`. They are typically defined
    using the keywords :code:`theorem` or :code:`lemma`, but it's possible to use other
    keywords such as :code:`def` or :code:`instance`
    """

    repo: LeanRepo
    """Lean repo the theorem comes from.
    """

    file_path: Path
    """Lean source file the theorem comes from.
    """

    full_name: str
    """Fully qualified name of the theorem.
    """

    def __post_init__(self) -> None:
        if isinstance(self.file_path, str):
            object.__setattr__(self, "file_path", Path(self.file_path))
        assert (
            self.file_path.suffix == ".lean"
        ), f"File extension must be .lean: {self.file_path}"

