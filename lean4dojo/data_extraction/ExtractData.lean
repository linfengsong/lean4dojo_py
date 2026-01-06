import Lean4dojo.ExtractData

unsafe def main (args : List String) : IO Unit := do
  process (extractLeanPath := "LeanDojoExtract.lean") (args := args)
