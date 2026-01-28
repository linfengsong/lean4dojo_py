"""Constants controlling LeanDojo's behaviors.
Many of them are configurable via :ref:`environment-variables`.
"""

import os
import re
import sys
import multiprocessing
from pathlib import Path
from typing import Tuple
from loguru import logger
from dotenv import load_dotenv

load_dotenv()

__version__ = "4.20.0"

logger.remove()
if "VERBOSE" in os.environ or "DEBUG" in os.environ:
    logger.add(sys.stderr, level="DEBUG")
else:
    logger.add(sys.stderr, level="INFO")

CACHE_DIR = (
    Path(os.environ["CACHE_DIR"])
    if "CACHE_DIR" in os.environ
    else Path.home() / ".cache/lean_dojo"
).absolute()
"""Cache directory for storing traced repos (see :ref:`caching`).
"""

REMOTE_CACHE_URL = "https://dl.fbaipublicfiles.com/lean-dojo"
"""URL of the remote cache (see :ref:`caching`)."""

DISABLE_REMOTE_CACHE = "DISABLE_REMOTE_CACHE" in os.environ
"""Whether to disable remote caching (see :ref:`caching`) and build all repos locally.
"""

TMP_DIR = Path(os.environ["TMP_DIR"]).absolute() if "TMP_DIR" in os.environ else None
"""Temporary directory used by LeanDojo for storing intermediate files
"""

MAX_NUM_PROCS = 32

NUM_PROCS = int(os.getenv("NUM_PROCS", min(multiprocessing.cpu_count(), MAX_NUM_PROCS)))
"""Number of processes to use
"""

NUM_WORKERS = NUM_PROCS - 1

LEAN4_URL = "https://github.com/leanprover/lean4"
"""The URL of the Lean 4 repo."""

LEAN4_PACKAGES_DIR = Path(".lake/packages")
"""The directory where Lean 4 dependencies are stored (since v4.3.0-rc2)."""

LEAN4_TOOLCHAINS_DIR = Path(".elan/toolchains")
"""The directory where Lean 4 are stored"""

LOAD_USED_PACKAGES_ONLY = "LOAD_USED_PACKAGES_ONLY" in os.environ
"""Only load depdendency files that are actually used by the target repo."""

LEAN4_BUILD_DIR = Path(".lake/build")

LEAN4_SRC_LEAN_DIR = Path("src/lean")
LEAN4_LIB_LEAN_DIR = Path("lib/lean")
LEAN4_SRC_LEAN_LAKE_DIR = Path("lake")
LEAN4_LEAN_LAKE_NAME_PREFIX = "Lake"

LEAN4_TRACED_DIR = Path(".alean")
LEAN4_TRACED_PACKAGES_DIR = LEAN4_TRACED_DIR / Path("packages")
LEAN4_TRACED_TOOLCHAINS_DIR = LEAN4_TRACED_DIR / Path("toolchains")

TACTIC_CPU_LIMIT = int(os.getenv("TACTIC_CPU_LIMIT", 1))
"""Number of CPUs for executing tactics when interacting with Lean.
"""

TACTIC_MEMORY_LIMIT = os.getenv("TACTIC_MEMORY_LIMIT", "32g")
"""Maximum memory when interacting with Lean.
"""

assert re.fullmatch(r"\d+g", TACTIC_MEMORY_LIMIT)

