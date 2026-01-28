"""This module provides the main interfaces for tracing Lean repos, i.e., extracting data from them.
To estimate the time for tracing a repo, a good rule of thumb is 1.5x the time for compiling the repo using :code:`leanpkg build`.
A repo has to be traced only once, and the traced repo will be stored in a cache for fast access in the future.
"""

import os
import shutil
import itertools
from tqdm import tqdm
from pathlib import Path
from loguru import logger
from time import sleep, monotonic
from multiprocessing import Process
from contextlib import contextmanager
from subprocess import CalledProcessError
from typing import List, Generator

from ..constants import NUM_PROCS
from ..utils import (
    working_directory,
    execute,
    get_packages_path,
    get_build_path,
    get_traced_working_path,
    get_traced_toolchain_path,
    get_olean_working_path,
    get_olean_package_paths,
    get_olean_toolchain_path,
    get_traced_package_path,
    get_traced_package_paths,
    to_trace_file_ext_from_olean,
)
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

def get_traced_files(working_dir: str) -> List:
    list = [] 
    r = get_traced_working_path(working_dir)
    for n in r.glob("**/*.ast.json"):
        list.append((working_dir, n))
    paths = get_traced_package_paths(working_dir)
    for path in paths:
        for n in path.glob("**/*.ast.json"):
            list.append((path, n))
            
    toolchainPath = get_traced_toolchain_path(working_dir)
    for n in toolchainPath.glob("**/*.ast.json"):
        list.append((toolchainPath, n))
    return list

def get_build_traced_files(working_dir: str, ext: str, no_deps: bool) -> dict:
    d = {}
    r = get_traced_working_path(working_dir)
    for p in r.glob("**/*" + ext):
        p = p.with_suffix("")
        if ext == ".ast.json":
            p = p.with_suffix("")
        relative = p.relative_to(r)
        d[str(relative)] = (working_dir, relative)

    if not no_deps:
        paths = get_traced_package_paths(working_dir)
        for path in paths:
            for p in path.glob("**/*" + ext):
                p = p.with_suffix("")
                if ext == ".ast.json":
                    p = p.with_suffix("")
                relative = p.relative_to(path)
                d[str(relative)] = (path, relative)

        toolchainPath = get_traced_toolchain_path(working_dir)
        for p in toolchainPath.glob("**/*" + ext):
            p = p.with_suffix("")
            if ext == ".ast.json":
                p = p.with_suffix("")
            relative = p.relative_to(toolchainPath)
            d[str(relative)] = (toolchainPath, relative)
    return d

def get_build_olean_files(working_dir: Path, no_deps: bool) -> dict:
    d = {}
    r = get_olean_working_path(working_dir)
    for p in r.glob("**/*.olean"):
        relative = p.relative_to(r).with_suffix("")
        d[str(relative)] = (r, relative)

    if not no_deps:
        for path in get_olean_package_paths(working_dir):
            for p in path.glob("**/*.olean"):
                relative = p.relative_to(path).with_suffix("")
                d[str(relative)] = (path, relative)
        
        toolchainPath = get_olean_toolchain_path(working_dir)
        for p in toolchainPath.glob("**/*.olean"):
            relative = p.relative_to(toolchainPath).with_suffix("")
            d[str(relative)] = (toolchainPath, relative)

    return d

def get_build_trace_file_ext_by_olean(working_dir: Path, path: Path, ext: str) -> str:
    # /home/linfe/math/dojo/lean4dojo_py/lean4-example3/.lake/packages/mathlib/.lake/build/ir/Mathlib/Algebra/EuclideanDomain/Field
    r = str(working_dir)
    p = str(path.with_suffix(""))
    h = str(Path.home())
    if p.startswith(r + "/"):
        p = p[len(r) + 1:]
        #.lake/packages/mathlib/.lake/build/ir/Mathlib/Algebra/EuclideanDomain/Field
        if p.startswith(".lake/packages/"):
            p = p[15:]
            # mathlib/.lake/build/ir/Mathlib/Algebra/EuclideanDomain/Field
            index = p.index("/")
            packageName = p[:index]
            p = p[len(packageName) + 1:]
            # .lake/build/ir/Mathlib/Algebra/EuclideanDomain/Field
            if p.startswith(".lake/build/ir/"):
                p = p[15:]
                # Mathlib/Algebra/EuclideanDomain/Field
                p = get_traced_package_path(packageName)
    return p + ext

def check_files(repo, no_deps: bool) -> None:
    """Check if all :file:`*.lean` files have been processed to produce :file:`*.ast.json` and :file:`*.dep_paths` files."""
    logger.debug(f"check_files, path:{repo.working_dir}, no_deps: {no_deps}")

    with working_directory(repo.working_dir):
        jsons = get_build_traced_files(repo.working_dir, ".ast.json", no_deps)
        deps = get_build_traced_files(repo.working_dir, ".dep_paths", no_deps)
        oleans = get_build_olean_files(repo.working_dir, no_deps)
        logger.debug(f"check_files  jsons:{len(jsons)}, deps: {len(deps)}, oleans: {len(oleans)}")
        #assert len(jsons) <= len(oleans) and len(deps) <= len(oleans)
        missing_jsons = {to_trace_file_ext_from_olean(*oleans[k], repo,".ast.json") for k in oleans.keys() - jsons.keys()}
        missing_deps = {to_trace_file_ext_from_olean(*oleans[k], repo, ".dep_paths") for k in oleans.keys() - deps.keys()}
        if len(missing_jsons) > 0 or len(missing_deps) > 0:
            for p in missing_jsons.union(missing_deps):
                logger.warning(f"Missing {p}")
        
        logger.debug(f"missing_jsons:{len(missing_jsons)}, missing_deps: {len(missing_deps)}, oleans: {len(oleans)}, jsons: {len(jsons)}, dep_paths: {len(deps)}")
    
    logger.debug(f"check_files end")

def build_trace(path: Path, build_deps: bool) -> None:
    logger.debug(f"build_trace, path:{path}, build_deps: {build_deps}")
    with working_directory(path):
        lean_prefix = execute(f"lean --print-prefix", capture_output=True)[0].strip()
        logger.debug(f"lean_prefix: {lean_prefix}")

        # Copy the Lean 4 stdlib into the path of packages.
        packages_path = get_packages_path()
        build_path = get_build_path()

        if build_deps and packages_path.exists():
            logger.info(f"delete {packages_path}")
            shutil.rmtree(packages_path)

        # Build the repo using lake.
        logger.debug(f"build_trace: {path}")
        if build_deps:
            try:
                execute("lake exe cache get")
            except CalledProcessError:
                try:
                    # without lean4
                    execute("lake update")
                except CalledProcessError:
                    pass
        execute("lake build")

        dirs_to_monitor = [build_path]

        # Run ExtractData.lean to extract ASTs, tactic states, and premise information.
        shutil.copyfile(LEAN4_DATA_EXTRACTOR_PATH, LEAN4_DATA_EXTRACTOR_PATH.name)
            
        with launch_progressbar(dirs_to_monitor):
            cmd = f"lake env lean --threads {NUM_PROCS} --run ExtractData.lean"
            if not build_deps:
                cmd += " noDeps"
            execute(cmd)
    logger.debug(f"build_trace end")

def build_repl(path: Path) -> None:
    logger.debug(f"build_repl, path:{path}")
        
    with working_directory(path):
        #os.remove(LEAN4_DATA_EXTRACTOR_PATH.name)

        # Copy Lean4Repl.lean into the repo and build it.
        if os.path.exists(LEAN4_REPL_PATH.name):
            logger.warning(
                f"{path} contains {LEAN4_REPL_PATH.name}. You may run into issues when interacting with this repo."
            )
        else:
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
    logger.debug(f"build_repl end")
