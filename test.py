from lean4dojo import LeanGitRepo

from loguru import logger
import sys

logger.remove()
logger.add(sys.stderr, level="DEBUG")
#repo = LeanGitRepo("https://github.com/yangky11/lean4-example", "7b6ecb9ad4829e4e73600a3329baeb3b5df8d23f")
repo = LeanGitRepo("https://github.com/linfengsong/lean4-example", "main")
#trace(repo, dst_dir="traced_lean4-example")

#repo = LeanGitRepo("https://github.com/leanprover-community/mathlib4", "v4.24.0")
logger.debug(f"repo: {repo}")

toolchain = repo.get_config("lean-toolchain")
logger.debug(f"Toolchain: {toolchain}")

#traced_repo = trace(repo, dst_dir="traced_mathlib4")
traced_repo = repo.get_traced_repo()
logger.debug(f"traced_repo: {traced_repo}")

#files_graph = traced_repo.traced_files_graph()
#logger.debug(f"Files graph nodes: {files_graph.nodes}")
#logger.debug(f"Files graph edges: {files_graph.edges}")

#for file in traced_repo.get_all_files():
#    logger.debug(f"File:  {file.path}, size: {file.size} bytes")

#traced_file = traced_repo.get_traced_file("Mathlib/LinearAlgebra/Basic.lean")
traced_file = traced_repo.get_traced_file("Lean4Example.lean")
logger.debug(f"Traced file: {traced_file}")

direct_dependencies = traced_file.get_direct_dependencies(repo)
logger.debug(f"Premise dependencies of {traced_file.path}: {direct_dependencies}")

traced_theorems = traced_file.get_traced_theorems()
logger.debug(f"traced_theorems len: {len(traced_theorems)}")

#thm = traced_file.get_traced_theorem("pi_eq_sum_univ")
thm = traced_file.get_traced_theorem("hello_world")
logger.debug(f"Theorem: {thm}")

logger.debug(f"Theorem.theorem: {thm.theorem}")

#logger.debug(f"Theorem start: {thm.start_line}:{thm.start_col}, end: {thm.end_line}:{thm.end_col}")

tactic_proof = thm.get_tactic_proof()
logger.debug(f"Tactic proof: {tactic_proof}")

#num_tactics = len(tactic_proof.get_tactics())
#logger.debug(f"Number of tactics in the proof: {num_tactics}")

proof_node = thm.get_proof_node()
logger.debug(f"Proof node: {proof_node}")

proof = proof_node.lean_file[proof_node.start: proof_node.end]
logger.debug(f"Proof text: {proof}")

#traced_tactics = tactic_proof.get_tactics()
#logger.debug(f"tactics: {traced_tactics}")

#logger.debug(f"second tactic: {traced_tactics[1]}")







