from lean4dojo.data_extraction.lean_git import LeanGitRepo, trace

repo = LeanGitRepo("https://github.com/yangky11/lean4-example", "7b6ecb9ad4829e4e73600a3329baeb3b5df8d23f")
#trace(repo, dst_dir="traced_lean4-example")

#repo = LeanGitRepo("https://github.com/leanprover-community/mathlib4", "v4.24.0")
print(f"repo: {repo}")

toolchain = repo.get_config("lean-toolchain")
print(f"Toolchain: {toolchain}")

#traced_repo = trace(repo, dst_dir="traced_mathlib4")
traced_repo = trace(repo)
print(f"Traced repo at: {traced_repo}")

#files_graph = traced_repo.traced_files_graph()
#print(f"Files graph nodes: {files_graph.nodes}")
#print(f"Files graph edges: {files_graph.edges}")    

#for file in traced_repo.get_all_files():
#    print(f"File: {file.path}, size: {file.size} bytes")

#traced_file = traced_repo.get_traced_file("Mathlib/LinearAlgebra/Basic.lean")
traced_file = traced_repo.get_traced_file("Lean4Example.lean")
print(f"Traced file: {traced_file}")

direct_dependencies = traced_file.get_direct_dependencies(repo)
print(f"Premise dependencies of {traced_file.path}: {direct_dependencies}")

traced_theorems = traced_file.get_traced_theorems()
print(f"traced_theorems len: {len(traced_theorems)}")

#thm = traced_file.get_traced_theorem("pi_eq_sum_univ")
thm = traced_file.get_traced_theorem("hello_world")
print(f"Theorem: {thm}")

print(f"Theorem.theorem: {thm.theorem}")

#print(f"Theorem start: {thm.start_line}:{thm.start_col}, end: {thm.end_line}:{thm.end_col}")

tactic_proof = thm.get_tactic_proof()
print(f"Tactic proof: {tactic_proof}")

#num_tactics = len(tactic_proof.get_tactics())
#print(f"Number of tactics in the proof: {num_tactics}")

proof_node = thm.get_proof_node()
print(f"Proof node: {proof_node}") 

proof = proof_node.lean_file[proof_node.start: proof_node.end]
print(f"Proof text: {proof}")

#traced_tactics = tactic_proof.get_tactics()
#print(f"tactics: {traced_tactics}")

#print(f"second tactic: {traced_tactics[1]}")







