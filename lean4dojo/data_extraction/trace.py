"""This module provides the main interfaces for tracing Lean repos, i.e., extracting data from them.
To estimate the time for tracing a repo, a good rule of thumb is 1.5x the time for compiling the repo using :code:`leanpkg build`.
A repo has to be traced only once, and the traced repo will be stored in a cache for fast access in the future.
"""

import os
import re
import shutil
import itertools
from tqdm import tqdm
from pathlib import Path
from loguru import logger
from time import sleep, monotonic
from multiprocessing import Process
from contextlib import contextmanager
from subprocess import CalledProcessError
from typing import Union, Optional, List, Generator

from ..constants import NUM_PROCS
from ..utils import working_directory, execute


LEAN4_DATA_EXTRACTOR_PATH = Path(__file__).with_name("ExtractData.lean")
LEAN4_REPL_PATH = Path(__file__).parent.parent / "interaction" / "Lean4Repl.lean"
assert LEAN4_DATA_EXTRACTOR_PATH.exists() and LEAN4_REPL_PATH.exists()

_PROGRESSBAR_UPDATE_INTERNAL = 5


def _monitor(paths: List[Path], num_total: int) -> None:
    with tqdm(total=num_total) as pbar:
        while True:
            time_start = monotonic()
            try:
                num_done = len(
                    list(
                        itertools.chain.from_iterable(
                            p.glob(f"**/*.ast.json") for p in paths
                        )
                    )
                )
            except Exception:
                continue
            time_elapsed = monotonic() - time_start
            if time_elapsed < _PROGRESSBAR_UPDATE_INTERNAL:
                sleep(_PROGRESSBAR_UPDATE_INTERNAL - time_elapsed)
            pbar.update(num_done - pbar.n)
            if num_done >= num_total:
                break
    print("")


@contextmanager
def launch_progressbar(paths: List[Path]) -> Generator[None, None, None]:
    """Launch an async progressbar to monitor the progress of tracing the repo."""
    paths = [Path(p) for p in paths]
    olean_files = list(
        itertools.chain.from_iterable(p.glob("**/*.olean") for p in paths)
    )
    num_total = len(olean_files)
    p = Process(target=_monitor, args=(paths, num_total), daemon=True)
    p.start()
    yield
    p.kill()


def get_lean_version() -> str:
    """Get the version of Lean."""
    output = execute("lean --version", capture_output=True)[0].strip()
    m = re.match(r"Lean \(version (?P<version>\S+?),", output)
    return m["version"]  # type: ignore


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


def check_files(packages_path: Path, no_deps: bool) -> None:
    """Check if all :file:`*.lean` files have been processed to produce :file:`*.ast.json` and :file:`*.dep_paths` files."""
    cwd = Path.cwd()
    packages_path = cwd / packages_path
    jsons = {
        p.with_suffix("").with_suffix("")
        for p in cwd.glob("**/build/ir/**/*.ast.json")
        if not no_deps or not p.is_relative_to(packages_path)
    }
    deps = {
        p.with_suffix("")
        for p in cwd.glob("**/build/ir/**/*.dep_paths")
        if not no_deps or not p.is_relative_to(packages_path)
    }
    oleans = {
        Path(str(p.with_suffix("")).replace("/build/lib/lean/", "/build/ir/"))
        for p in cwd.glob("**/build/lib/lean/**/*.olean")
        if not no_deps or not p.is_relative_to(packages_path)
    }
    assert len(jsons) <= len(oleans) and len(deps) <= len(oleans)
    missing_jsons = {p.with_suffix(".ast.json") for p in oleans - jsons}
    missing_deps = {p.with_suffix(".dep_paths") for p in oleans - deps}
    if len(missing_jsons) > 0 or len(missing_deps) > 0:
        for p in missing_jsons.union(missing_deps):
            logger.warning(f"Missing {p}")

def build_trace(path: Optional[Union[str, Path]], build_deps: bool) -> None:
    with working_directory(path):
        # Build the repo using lake.
        if not build_deps:
            try:
                execute("lake exe cache get")
            except CalledProcessError:
                pass
        execute("lake build")

        # Copy the Lean 4 stdlib into the path of packages.
        lean_prefix = execute(f"lean --print-prefix", capture_output=True)[0].strip()
        if is_new_version(get_lean_version()):
            packages_path = Path(".lake/packages")
            build_path = Path(".lake/build")
        else:
            packages_path = Path("lake-packages")
            build_path = Path("build")
        shutil.copytree(lean_prefix, str(packages_path / "lean4"))

        # Run ExtractData.lean to extract ASTs, tactic states, and premise information.
        shutil.copyfile(LEAN4_DATA_EXTRACTOR_PATH, LEAN4_DATA_EXTRACTOR_PATH.name)
        dirs_to_monitor = [build_path]
        if build_deps:
            dirs_to_monitor.append(packages_path)
        with launch_progressbar(dirs_to_monitor):
            cmd = f"lake env lean --threads {NUM_PROCS} --run ExtractData.lean"
            if not build_deps:
                cmd += " noDeps"
            execute(cmd)

        check_files(packages_path, not build_deps)
        os.remove(LEAN4_DATA_EXTRACTOR_PATH.name)

        # Copy Lean4Repl.lean into the repo and build it.
        if os.path.exists(LEAN4_REPL_PATH.name):
            logger.warning(
                f"{path} contains {LEAN4_REPL_PATH.name}. You may run into issues when interacting with this repo."
            )
        shutil.copyfile(LEAN4_REPL_PATH, LEAN4_REPL_PATH.name)

        if os.path.exists("lakefile.lean"):
            with open("lakefile.lean", "a") as oup:
                oup.write("\nlean_lib Lean4Repl {\n\n}\n")
        else:
            assert os.path.exists("lakefile.toml")
            with open("lakefile.toml", "a") as oup:
                oup.write('\n[[lean_lib]]\nname = "Lean4Repl"\n')

        try:
            execute("lake build Lean4Repl")
        except CalledProcessError:
            logger.warning(
                f"Failed to build Lean4Repl. You may run into issues when interacting with the repo."
            )

