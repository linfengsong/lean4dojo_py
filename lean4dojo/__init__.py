import os
from loguru import logger

from .data_extraction.traced_data import (
    TracedRepo,
    TracedFile,
    TracedTheorem,
    TracedTactic,
)
#from .interaction.dojo import (
#    CommandState,
#    TacticState,
#    LeanError,
#    TacticResult,
#    DojoCrashError,
#    DojoTacticTimeoutError,
#    DojoInitError,
#    Dojo,
#    ProofFinished,
#    ProofGivenUp,
#    check_proof,
#)
#from .interaction.parse_goals import Declaration, Goal, parse_goals
from .data_extraction.lean import LeanRepo, LeanFile, Theorem, Pos
from .constants import __version__