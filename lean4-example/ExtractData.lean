import Lean
import Lake
import Lean.Data.Json.Parser
import Lean.Data.Json.Basic


open Lean Elab System

set_option maxHeartbeats 2000000  -- 10x the default maxHeartbeats.


instance : ToJson Substring where
  toJson s := toJson s.toString

instance : ToJson String.Pos where
  toJson n := toJson n.1

deriving instance ToJson for SourceInfo
deriving instance ToJson for Syntax.Preresolved
deriving instance ToJson for Syntax
deriving instance ToJson for Position


namespace LeanDojo


/--
The trace of a tactic.
-/
structure TacticTrace where
  stateBefore: String
  stateAfter: String
  pos: String.Pos      -- Start position of the tactic.
  endPos: String.Pos   -- End position of the tactic.
deriving ToJson


/--
The trace of a premise.
-/
structure PremiseTrace where
  fullName: String            -- Fully-qualified name of the premise.
  defPos: Option Position     -- Where the premise is defined.
  defEndPos: Option Position
  modName: String             -- In which module the premise is defined.
  defPath: String             -- The path of the file where the premise is defined.
  pos: Option Position        -- Where the premise is used.
  endPos: Option Position
deriving ToJson


/--
The trace of a Lean file.
-/
structure Trace where
  commandASTs : Array Syntax    -- The ASTs of the commands in the file.
  tactics: Array TacticTrace    -- All tactics in the file.
  premises: Array PremiseTrace  -- All premises in the file.
deriving ToJson


abbrev TraceM := StateT Trace MetaM


namespace Pp


private def addLine (s : String) : String :=
  if s.isEmpty then s else s ++ "\n"


-- Similar to `Meta.ppGoal` but uses String instead of Format to make sure local declarations are separated by "\n".
private def ppGoal (mvarId : MVarId) : MetaM String := do
  match (← getMCtx).findDecl? mvarId with
  | none          => return "unknown goal"
  | some mvarDecl =>
    let indent         := 2
    let lctx           := mvarDecl.lctx
    let lctx           := lctx.sanitizeNames.run' { options := (← getOptions) }
    Meta.withLCtx lctx mvarDecl.localInstances do
      -- The followint two `let rec`s are being used to control the generated code size.
      -- Then should be remove after we rewrite the compiler in Lean
      let rec pushPending (ids : List Name) (type? : Option Expr) (s : String) : MetaM String := do
        if ids.isEmpty then
          return s
        else
          let s := addLine s
          match type? with
          | none      => return s
          | some type =>
            let typeFmt ← Meta.ppExpr type
            return (s ++ (Format.joinSep ids.reverse (format " ") ++ " :" ++ Format.nest indent (Format.line ++ typeFmt)).group).pretty
      let rec ppVars (varNames : List Name) (prevType? : Option Expr) (s : String) (localDecl : LocalDecl) : MetaM (List Name × Option Expr × String) := do
        match localDecl with
        | .cdecl _ _ varName type _ _ =>
          let varName := varName.simpMacroScopes
          let type ← instantiateMVars type
          if prevType? == none || prevType? == some type then
            return (varName :: varNames, some type, s)
          else do
            let s ← pushPending varNames prevType? s
            return ([varName], some type, s)
        | .ldecl _ _ varName type val _ _ => do
          let varName := varName.simpMacroScopes
          let s ← pushPending varNames prevType? s
          let s  := addLine s
          let type ← instantiateMVars type
          let typeFmt ← Meta.ppExpr type
          let mut fmtElem  := format varName ++ " : " ++ typeFmt
          let val ← instantiateMVars val
          let valFmt ← Meta.ppExpr val
          fmtElem := fmtElem ++ " :=" ++ Format.nest indent (Format.line ++ valFmt)
          let s := s ++ fmtElem.group.pretty
          return ([], none, s)
      let (varNames, type?, s) ← lctx.foldlM (init := ([], none, "")) fun (varNames, prevType?, s) (localDecl : LocalDecl) =>
         if localDecl.isAuxDecl || localDecl.isImplementationDetail then
           -- Ignore auxiliary declarations and implementation details.
           return (varNames, prevType?, s)
         else
           ppVars varNames prevType? s localDecl
      let s ← pushPending varNames type? s
      let goalTypeFmt ← Meta.ppExpr (← instantiateMVars mvarDecl.type)
      let goalFmt := Meta.getGoalPrefix mvarDecl ++ Format.nest indent goalTypeFmt
      let s := s ++ "\n" ++ goalFmt.pretty
      match mvarDecl.userName with
      | Name.anonymous => return s
      | name           => return "case " ++ name.eraseMacroScopes.toString ++ "\n" ++ s


def ppGoals (ctx : ContextInfo) (goals : List MVarId) : IO String :=
  if goals.isEmpty then
    return "no goals"
  else
    let fmt := ctx.runMetaM {} (return Std.Format.prefixJoin "\n\n" (← goals.mapM (ppGoal ·)))
    return (← fmt).pretty.trim


end Pp

namespace Manifest

structure PackageStruct: Type where
  url: String
  type: String
  subDir: Option String
  rev: String
  name: String
  manifestFile: String
  inputRev: String
  inherited: Bool
  configFile: String
deriving ToJson, FromJson, Inhabited, Repr

structure LakeManifestStruct : Type where
  version: String
  packagesDir: String
  packages: Array PackageStruct
deriving Lean.ToJson, Lean.FromJson, Inhabited, Repr

def get_lakeManifest_from_json_string (s: String): Except String LakeManifestStruct := do
  let j : Json <- Json.parse s
  let lakeManifest : LakeManifestStruct <- fromJson? j
  return lakeManifest

def create_lakeManifest : IO LakeManifestStruct := do
    let s ← IO.FS.readFile ((← IO.currentDir) / "lake-manifest.json")
    let lakeManifest : LakeManifestStruct <- IO.ofExcept (get_lakeManifest_from_json_string s)
    return lakeManifest

def find_package (lakeManifest : LakeManifestStruct) (packageName : String): Option PackageStruct :=
  lakeManifest.packages.find? (λ s => s.name == packageName)

def lakeManifest : IO LakeManifestStruct := do
  return ← create_lakeManifest

end Manifest

namespace Path

/--
Return the path of `path` relative to `parent`.
-/
def relativeTo (path parent : FilePath) : Option FilePath :=
  let rec componentsRelativeTo (pathComps parentComps : List String) : Option FilePath :=
    match pathComps, parentComps with
    | _, [] => mkFilePath pathComps
    | [], _ => none
    | (h₁ :: t₁), (h₂ :: t₂) =>
      if h₁ == h₂ then
        componentsRelativeTo t₁ t₂
      else
        none

    componentsRelativeTo path.components parent.components


/--
Return if the path `path` is relative to `parent`.
-/
def isRelativeTo (path parent : FilePath) : Bool :=
  match relativeTo path parent with
  | some _ => true
  | none => false


/--
Convert the path `path` to an absolute path.
-/
def toAbsolute (path : FilePath) : IO FilePath := do
  if path.isAbsolute then
    pure path
  else
    let cwd ← IO.currentDir
    pure $ cwd / path


private def trim (path : FilePath) : FilePath :=
  assert! path.isRelative
  mkFilePath $ path.components.filter (· != ".")


def packagesDir : IO FilePath := do
  if Lake.defaultPackagesDir == "packages"  then
    toAbsolute (".lake" / Lake.defaultPackagesDir)
  else
    toAbsolute Lake.defaultPackagesDir


def buildPath : FilePath :=
  if Lake.defaultPackagesDir.fileName == "packages" then  -- Lean >= v4.3.0-rc2
    ".lake/build"
  else  -- Lean < v4.3.0-rc2
   "build"


def libPath : FilePath := "lib/lean"
def libLakePrefix : FilePath := "Lake"


def srcPath : FilePath := "src/lean"
def srcLakePath : FilePath := "src/lean/lake"
def srcLakePrefix : FilePath := "lake"

def outputPath : FilePath := ".alean"


def toolchainDir : IO FilePath := do
  return (←  Lean.findSysroot)


def toolchainName: IO String := do
  let path ← Lean.findSysroot
  match path.fileName  with
  | none    => throw  <| IO.userError s!"Fail to retrieve fileName: {path}"
  | some s  => return s


def homeDir : IO FilePath := do
  -- On Unix-like systems, "HOME" is standard.
  -- On Windows, "USERPROFILE" is the standard home variable.
  match ← IO.getEnv "HOME" with
  | some path => return path
  | none =>
    match ← IO.getEnv "USERPROFILE" with
    | some path => return path
    | none => throw  <| IO.userError s!"Fail to retrieve home directory"


def toPackageVersionPath (pktPath : FilePath) : IO FilePath := do
  let path: String := pktPath.toString
  let list: List String := path.split (fun c => c == '/')
  if h : list.length > 1 then
    let pkgName := list[0]
    let option_package := Manifest.find_package (← Manifest.lakeManifest) pkgName
    if option_package.isSome then
      let version := option_package.get!.rev
      let pkgNameVersion: String := s!"{pkgName}-{version}"
      let list' := list.set 0 pkgNameVersion
      return String.intercalate "/" list'
    else
      return pktPath
  else
    throw <| IO.userError s!"Fail to convert package version path {pktPath}"


/--
Convert the path of a *.lean file to its corresponding file (e.g., *.olean) in the "build" directory.
-/
def toBuildDir (rootDir : FilePath) (path : FilePath) (ext : String) : IO (Option FilePath) := do
  -- E.g., `<toolchain path>/src/lean/Init/Prelude.lean` rootDir `<toolchain path>` -> `<toolchain path>/lib/lean/Init/Prelude.olean`
  -- E.g., `<toolchain path>/src/lean/lake/LakeMain.lean` rootDir `<toolchain path>` -> `<toolchain path>/lib/lean/LakeMain.olean`
  -- E.g., `<toolchain path>/src/lean/lake/Lake/Util/Casing.lean` rootDir `<toolchain path>` -> `<toolchain path>/lib/lean/Lake/Util/Casing.olean`
  -- E.g., `<current path>/.lake/packages/mathlib/Mathlib/Util/Tactic.lean` rootDir `<current path>` -> `<current path>/.lake/packages/mathlib/.lake/build/lib/lean/Mathlib/Util/Tactic.olean`
  -- E.g., `<current path>/Lean4Example.lean` rootDir `<current path>` -> `<current path>/.lake/build/lib/lean/Lean4Example.olean`
  match relativeTo path rootDir with
  | some p =>
    -- E.g., `src/lean/Init/Prelude.lean` -> `<toolchain path>/lib/lean/Init/Prelude.olean`
    -- E.g., `src/lean/lake/LakeMain.lean` -> `<toolchain path>/lib/lean/LakeMain.olean`
    -- E.g., `src/lean/lake/Lake/Util/Casing.lean` -> `<toolchain path>/lib/lean/Lake/Util/Casing.olean`
    -- E.g., `.lake/packages/mathlib/Mathlib/Util/Tactic.lean` -> `<current path>/.lake/packages/mathlib/.lake/build/lib/lean/Mathlib/Util/Tactic.olean`
    -- E.g., `Lean4Example.lean` -> `<current path>/.lake/build/lib/lean/Lean4Example.olean`
    match relativeTo p srcPath with
    | some p' =>
      -- E.g., `Init/Prelude.lean` -> `<toolchain path>/lib/lean/Init/Prelude.olean`
      -- E.g., `lake/LakeMain.lean` -> `<toolchain path>/lib/lean/LakeMain.olean`
      -- E.g., `lake/Lake/Util/Casing.lean` -> `<toolchain path>/lib/lean/Lake/Util/Casing.olean`
      match relativeTo p' srcLakePrefix with
      | some p'' =>
        -- E.g., `LakeMain.lean` -> `<toolchain path>/lib/lean/LakeMain.olean`
        -- E.g., `Lake/Util/Casing.lean` -> `<toolchain path>/lib/lean/Lake/Util/Casing.olean`
        return rootDir / libPath / p''.withExtension ext
      | none =>
        -- E.g., `Init/Prelude.lean` -> `<toolchain path>/lib/lean/Init/Prelude.olean`
        return rootDir / libPath / p'.withExtension ext
    | none =>
      -- E.g., `.lake/packages/mathlib/Mathlib/Util/Tactic.lean` -> `<current path>/.lake/packages/mathlib/.lake/build/lib/lean/Mathlib/Util/Tactic.olean`
      -- E.g., `Lean4Example.lean` -> `<current path>/.lake/build/lib/lean/Lean4Example.olean`
      match relativeTo path (← packagesDir) with
      | some p' =>
        -- E.g., `mathlib/Mathlib/Util/Tactic.lean` -> `<current path>/.lake/packages/mathlib/.lake/build/lib/lean/Mathlib/Util/Tactic.olean`
        match p'.components with
        | [] => return none
        | packageName :: _ =>
          match relativeTo p' packageName with
          | some p'' =>
            -- E.g., `Mathlib/Util/Tactic.lean` -> `<current path>/.lake/packages/mathlib/.lake/build/lib/lean/Mathlib/Util/Tactic.olean`
            return (← packagesDir) / packageName / buildPath / libPath / p''.withExtension ext
          | none => return none
      | none =>
        -- E.g., `Lean4Example.lean` -> `<current path>/.lake/build/lib/lean/Lean4Example.olean`
        return rootDir / buildPath / libPath /  p.withExtension ext
  | none => return none


/--
The reverse of `toBuildDir`.
-/
-- proofwidgets/build/lib/ProofWidgets/Compat.lean
-- proofwidgets/.lake/build/lib
def toSrcDir (rootDir : FilePath) (path : FilePath) (ext : String) : Option FilePath :=
  -- E.g., `<toolchain path>/lib/lean/Init/Prelude.olean` rootDir `<toolchain path>` -> `<toolchain path>/src/lean/Init/Prelude.lean`
  -- E.g., `<toolchain path>/lib/lean/LakeMain.olean` rootDir `<toolchain path>` -> `<toolchain path>/src/lean/lake/LakeMain.lean`
  -- E.g., `<toolchain path>/lib/lean/Lake/Util/Casing.olean` rootDir `<toolchain path>` -> `<toolchain path>/src/lean/lake/Lake/Util/Casing.lean`
  -- E.g., `<current path>/.lake/packages/mathlib/.lake/build/lib/lean/Mathlib/Util/Tactic.olean` rootDir `<current path>/.lake/packages` -> `<current path>/.lake/packages/mathlib/Mathlib/Util/Tactic.lean`
  -- E.g., `<current path>/.lake/build/lib/lean/Lean4Example.olean` rootDir `<current path>` -> `<current path>/Lean4Example.lean`
  match relativeTo path rootDir with
  | some p =>
    -- E.g., `lib/lean/Init/Prelude.olean` -> `<toolchain path>/src/lean/Init/Prelude.lean`
    -- E.g., `lib/lean/LakeMain.olean` -> `<toolchain path>/src/lean/lake/LakeMain.lean`
    -- E.g., `lib/lean/Lake/Util/Casing.olean` -> `<toolchain path>/src/lean/lake/Lake/Util/Casing.lean`
    -- E.g., `.lake/packages/mathlib/.lake/build/lib/lean/Mathlib/Util/Tactic.olean` -> `<current path>/.lake/packages/mathlib/Mathlib/Util/Tactic.lean`
    match relativeTo p libPath with
    | some p' =>
      -- E.g., `Init/Prelude.olean` -> `<toolchain path>/src/lean/Init/Prelude.lean`
      -- E.g., `LakeMain.olean` ->`<toolchain path>/src/lean/lake/LakeMain.lean`
      -- E.g., `lake/Lake/Util/Casing.olean` -> `<toolchain path>/src/lean/lake/Lake/Util/Casing.lean`
      if p'.toString.startsWith libLakePrefix.toString then
        -- E.g., `LakeMain.olean` -> `<toolchain path>/src/lean/lake/LakeMain.lean`
        -- E.g., `Lake/Util/Casing.olean` -> `<toolchain path>/src/lean/lake/Lake/Util/Casing.lean`
        rootDir / srcPath / srcLakePrefix / p'.withExtension ext
      else
        -- E.g., `Init/Prelude.olean` -> `<toolchain path>/src/lean/Init/Prelude.lean`
        rootDir / srcPath/ p'.withExtension ext
    | none =>
      -- E.g., `mathlib/.lake/build/lib/lean/Mathlib/Util/Tactic.olean` -> `<current path>/.lake/packages/mathlib/Mathlib/Util/Tactic.lean`
      match p.components with
      | [] => none
      | packageName :: _ =>
        match relativeTo p $ packageName / buildPath / libPath with
        | some p' =>
          -- E.g., `Mathlib/Util/Tactic.olean`  -> `<current path>/.lake/packages/mathlib/Mathlib/Util/Tactic.lean`
          rootDir / packageName / p'.withExtension ext
        | none => none
  | none =>
    -- E.g., `.lake/build/lib/lean/Lean4Example.olean` -> `<current path>/Lean4Example.lean`
    match relativeTo path $ buildPath / libPath with
    | some p => rootDir / p.withExtension ext
    | none => none

def isOutputPackage (path : FilePath):  IO  Bool := do
  let path' ← toAbsolute path
  match relativeTo path' (← toolchainDir) with
  | some _ =>
    return true
  | none =>
    match relativeTo path' (← packagesDir) with
    | some _ =>
      return true
    | none =>
      return false

def toOutputDir (path : FilePath)(ext : String) :  IO (Option FilePath) := do
  let cwd ← IO.currentDir
  let homeDir: FilePath ← Path.homeDir

  -- E.g., `<toolchain path>/src/lean/Init/Prelude.lean` -> `<home_dir>/.alean/toolchains/<toolchain name>/Init/Prelude.ast.json`
  -- E.g., `<toolchain path>/src/lean/lake/LakeMain.lean` -> `<home_dir>/.alean/toolchains/<toolchain name>/LakeMain.ast.json`
  -- E.g., `<toolchain path>/src/lean/lake/Lake/Util/Casing.lean` -> `<home_dir>/.alean/toolchains/<toolchain name>/Lake/Util/Casing.ast.json`
  -- E.g., `<current path>/.lake/packages/mathlib/Mathlib/Util/Tactic.lean` -> `<home_dir>/.alean/packages/mathlib-<version>/Mathlib/Util/Tactic.ast.json`
  -- E.g., `<current path>/Lean4Example.lean` rootDir `<current path>` -> `<current path>/.alean/Lean4Example.ast.json`

  let path' := (← toAbsolute path).withExtension ext
  match relativeTo path' $ (← toolchainDir) / srcPath  with
  | some p =>
    -- E.g., `Init/Prelude.ast.lean` -> `<home_dir>/.alean/toolchains/<toolchain name>/Init/Prelude.ast.json`
    -- E.g., `lake/LakeMain.lean` -> `<home_dir>/.alean/toolchains/<toolchain name>/LakeMain.ast.json`
    -- E.g., `lake/Lake/Util/Casing.lean` -> `<home_dir>/.alean/toolchains/<toolchain name>/Lake/Util/Casing.ast.json`
    match relativeTo p srcLakePrefix with
    | some p' =>
      -- E.g., `LakeMain.lean` -> `<home_dir>/.alean/toolchains/<toolchain name>/LakeMain.ast.json`
      -- E.g., `Lake/Util/Casing.lean` -> `<home_dir>/.alean/toolchains/<toolchain name>/Lake/Util/Casing.ast.json`
      return homeDir / outputPath / "toolchains" / (← toolchainName) / p'
    | none =>
      -- E.g., `Init/Prelude.lean` -> `<home_dir>/.alean/toolchains/<toolchain name>/Init/Prelude.ast.json`
      return homeDir / outputPath / "toolchains" / (← toolchainName) / p
  | none =>
    match relativeTo path' (← packagesDir) with
    | some p =>
      -- E.g., `mathlib/Mathlib/Util/Tactic.lean` -> `<home_dir>/.alean/packages/mathlib-<version>/Mathlib/Util/Tactic.ast.json`
      let packageNameVersion <- toPackageVersionPath p
      return homeDir / outputPath / "packages" / packageNameVersion
    | none =>
       -- E.g., `<current path>/Lean4Example.lean` rootDir `<current path>` -> `<current path>/.alean/Lean4Example.ast.json`
      match relativeTo path' cwd with
      | some p =>
        return cwd / outputPath/ p
      | none =>
        return none


/--
Create all parent directories of `p` if they don't exist.
-/
def makeParentDirs (p : FilePath) : IO Unit := do
  let some parent := p.parent | throw $ IO.userError s!"Unable to get the parent of {p}"
  IO.FS.createDirAll parent

/--
Return the *.lean file corresponding to a module name.
-/
def findLean (mod : Name) : IO FilePath := do
  let modStr := mod.toString
  if modStr.startsWith "«lake-packages»." then
    return FilePath.mk (modStr.replace "«lake-packages»" "lake-packages" |>.replace "." "/") |>.withExtension "lean"
  if modStr.startsWith "«.lake»." then
    return FilePath.mk (modStr.replace "«.lake»" ".lake" |>.replace "." "/") |>.withExtension "lean"
  if modStr == "Lake" then
    return (← toolchainDir) / "src/lean/lake/Lake.lean"
  let olean ← findOLean mod
  -- Remove a "build/lib/lean/" substring from the path.
  let lean := olean.toString.replace ".lake/build/lib/lean/" ""
    |>.replace "build/lib/lean/" "" |>.replace "lib/lean/Lake/" "lib/lean/lake/Lake/"
  let mut path := FilePath.mk lean |>.withExtension "lean"
  let leanLib ← getLibDir (← getBuildDir)
  if let some p := relativeTo path leanLib then
    --path := packagesDir / "lean4/src/lean" / p
    let toolchainDir ← toolchainDir
    if p.toString.startsWith libLakePrefix.toString then
      path := toolchainDir / srcLakePath / p
    else
      path := toolchainDir / srcPath / p
  assert! ← path.pathExists
  return path

end Path


namespace Traversal


/--
Extract tactic information from `TacticInfo` in `InfoTree`.
-/
private def visitTacticInfo (ctx : ContextInfo) (ti : TacticInfo) (parent : InfoTree) : TraceM Unit := do
  match ti.stx.getKind with
  | ``Lean.Parser.Term.byTactic =>
    match ti.stx with
    | .node _ _ #[.atom _ "by", .node _ ``Lean.Parser.Tactic.tacticSeq _] => pure ()
    | _ => assert! false

  | ``Lean.Parser.Tactic.tacticSeq =>
    match ti.stx with
    | .node _ _ #[.node _ ``Lean.Parser.Tactic.tacticSeq1Indented _] => pure ()
    | .node _ _ #[.node _ ``Lean.Parser.Tactic.tacticSeqBracketed _] => pure ()
    | _ => assert! false

  | _ => pure ()

  match parent with
  | .node (Info.ofTacticInfo i) _ =>
    match i.stx.getKind with
    | ``Lean.Parser.Tactic.tacticSeq1Indented | ``Lean.Parser.Tactic.tacticSeqBracketed | ``Lean.Parser.Tactic.rewriteSeq =>
      let ctxBefore := { ctx with mctx := ti.mctxBefore }
      let ctxAfter := { ctx with mctx := ti.mctxAfter }
      let stateBefore ← Pp.ppGoals ctxBefore ti.goalsBefore
      let stateAfter ← Pp.ppGoals ctxAfter ti.goalsAfter
      if stateBefore == "no goals" || stateBefore == stateAfter then
        pure ()
      else
        let some posBefore := ti.stx.getPos? true | pure ()
        let some posAfter := ti.stx.getTailPos? true | pure ()
        match ti.stx with
        | .node _ _ _ =>
          modify fun trace => {
            trace with tactics := trace.tactics.push {
              stateBefore := stateBefore,
              stateAfter := stateAfter,
              pos := posBefore,
              endPos := posAfter,
             }
          }
        | _ => pure ()
    | _ => pure ()
  | _ => pure ()


/--
Extract premise information from `TermInfo` in `InfoTree`.
-/
private def visitTermInfo (ti : TermInfo) (env : Environment) : TraceM Unit := do
  let some fullName := ti.expr.constName? | return ()
  let fileMap ← getFileMap

  let posBefore := match ti.toElabInfo.stx.getPos? with
    | some posInfo => fileMap.toPosition posInfo
    | none => none

  let posAfter := match ti.toElabInfo.stx.getTailPos? with
    | some posInfo => fileMap.toPosition posInfo
    | none => none

  let decRanges ← withEnv env $ findDeclarationRanges? fullName
  let defPos := decRanges >>= fun (decR : DeclarationRanges) => decR.selectionRange.pos
  let defEndPos := decRanges >>= fun (decR : DeclarationRanges) => decR.selectionRange.endPos

  let modName :=
  if let some modIdx := env.const2ModIdx.get? fullName then
    env.header.moduleNames[modIdx.toNat]!
  else
    env.header.mainModule

  let mut defPath := toString $ ← Path.findLean modName
  while defPath.startsWith "./" do
    defPath := defPath.drop 2
  if defPath.startsWith "/lake/" then
    defPath := ".lake/" ++ (defPath.drop 6)

  if defPos != posBefore ∧ defEndPos != posAfter then  -- Don't include defintions as premises.
    modify fun trace => {
        trace with premises := trace.premises.push {
          fullName := toString fullName,
          defPos := defPos,
          defEndPos := defEndPos,
          defPath := defPath,
          modName := toString modName,
          pos := posBefore,
          endPos := posAfter,
        }
    }


private def visitInfo (ctx : ContextInfo) (i : Info) (parent : InfoTree) (env : Environment) : TraceM Unit := do
  match i with
  | .ofTacticInfo ti => visitTacticInfo ctx ti parent
  | .ofTermInfo ti => visitTermInfo ti env
  | _ => pure ()


private partial def traverseTree (ctx: ContextInfo) (tree : InfoTree)
(parent : InfoTree) (env : Environment) : TraceM Unit := do
  match tree with
  | .context ctx' t =>
    match ctx'.mergeIntoOuter? ctx with
    | some ctx' => traverseTree ctx' t tree env
    | none => panic! "fail to synthesis contextInfo when traversing infoTree"
  | .node i children =>
    visitInfo ctx i parent env
    for x in children do
      traverseTree ctx x tree env
  | _ => pure ()


private def traverseTopLevelTree (tree : InfoTree) (env : Environment) : TraceM Unit := do
  match tree with
  | .context ctx t =>
    match ctx.mergeIntoOuter? none with
    | some ctx => traverseTree ctx t tree env
    | none => panic! "fail to synthesis contextInfo for top-level infoTree"
  | _ => pure ()


/--
Process an array of `InfoTree` (one for each top-level command in the file).
-/
def traverseForest (trees : Array InfoTree) (env : Environment) : TraceM Trace := do
  for t in trees do
    traverseTopLevelTree t env
  get


end Traversal


open Traversal


def getImports (header: TSyntax `Lean.Parser.Module.header) : IO String := do
  -- Similar to `lean --deps` in Lean 3.
  let mut s := ""

  for dep in headerToImports header do
    -- let oleanPath ← findOLean dep.module
    let leanPath ← Path.findLean dep.module
    s := s ++ "\n" ++ leanPath.toString
    /-
    if oleanPath.isRelative then
      let leanPath := Path.toSrcDir! oleanPath "lean"
      assert! ← leanPath.pathExists
      s := s ++ "\n" ++ leanPath.toString
    else if ¬(oleanPath.toString.endsWith "/lib/lean/Init.olean") then
      let mut p := (Path.packagesDir / "lean4").toString ++ FilePath.pathSeparator.toString
      let mut found := false
      for c in (oleanPath.withExtension "lean").components do
        if c == "lib" then
          found := true
          p := p ++ "src"
          continue
        if found then
          p := p ++ FilePath.pathSeparator.toString ++ c
      p := p.replace "/lean4/src/lean/Lake" "/lean4/src/lean/lake/Lake"
      assert! ← FilePath.mk p |>.pathExists
      s := s ++ "\n" ++ p
  -/

  return s.trim


def moduleNameByPath (path : FilePath) : IO Name := do
  if path.isRelative then
    return ← moduleNameOfFileName path none
  else
    let toolchainDir ← Path.toolchainDir
    match Path.relativeTo path toolchainDir with
    | some p =>
      match Path.relativeTo p Path.srcPath with
      | some _ =>
        match Path.relativeTo p Path.srcLakePath with
        | some _ =>
          return ← moduleNameOfFileName path $ toolchainDir / Path.srcLakePath
        | none =>
          return ← moduleNameOfFileName path $ toolchainDir / Path.srcPath
      | none =>
        return ← moduleNameOfFileName path $ toolchainDir
    | none =>
      return ← moduleNameOfFileName path none


/--
Trace a *.lean file.
-/
unsafe def processFile (path : FilePath)(noDeps : Bool) : IO Unit := do
  let isPackage :=  (← Path.isOutputPackage path)
  let json_path := (← Path.toOutputDir path "ast.json").get!
  let dep_path := (← Path.toOutputDir path "dep_paths").get!

  if isPackage ∧ ¬ noDeps ∧ (← json_path.pathExists) ∧ (← dep_path.pathExists) then
    return

  let input ← IO.FS.readFile path
  enableInitializersExecution
  let inputCtx := Parser.mkInputContext input path.toString
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (env, messages) ← processHeader header {} messages inputCtx

  if messages.hasErrors then
    for msg in messages.toList do
      if msg.severity == .error then
        println! "ERROR: {← msg.toString}"
    throw $ IO.userError "Errors during import; aborting"

  let env := env.setMainModule (← moduleNameByPath path)
  let commandState := { Command.mkState env messages {} with infoState.enabled := true }
  let s ← IO.processCommands inputCtx parserState commandState
  let env' := s.commandState.env
  let commands := s.commands.pop -- Remove EOI command.
  let trees := s.commandState.infoState.trees.toArray

  let traceM := (traverseForest trees env').run' ⟨#[header] ++ commands, #[], #[]⟩
  let (trace, _) ← traceM.run'.toIO {fileName := s!"{path}", fileMap := FileMap.ofString input} {env := env}

  Path.makeParentDirs json_path
  IO.FS.writeFile json_path (toJson trace).pretty

  Path.makeParentDirs dep_path
  IO.FS.writeFile dep_path (← getImports header)

end LeanDojo


open LeanDojo

/--
Whether a *.lean file should be traced.
-/
def shouldProcess (path : FilePath) (rootDir : FilePath) (noDeps : Bool) : IO Bool := do
  if (← path.isDir) ∨ path.extension != "lean" then
    return false

  if noDeps ∧  Path.isRelativeTo path rootDir then
    return false

  let some oleanPath ←  Path.toBuildDir rootDir path "olean" |
    throw $ IO.userError s!"Invalid path: {path}, root: {rootDir}"

  let exist ← oleanPath.pathExists
  if ¬ exist then
    return false

  let isPackage :=  (← Path.isOutputPackage path)
  let json_path := (← Path.toOutputDir path "ast.json").get!
  let dep_path := (← Path.toOutputDir path "dep_paths").get!

  if isPackage ∧ ¬ noDeps ∧ (← json_path.pathExists) ∧ (← dep_path.pathExists) then
    return false
  return true


/--
Trace all *.lean files in the current directory whose corresponding *.olean file exists.
-/
def processAllFiles (noDeps : Bool) : IO Unit := do
    let cwd ← IO.currentDir
    println! "Extracting data at {cwd}"

    let mut tasks := #[]
    for path in ← System.FilePath.walkDir cwd do
      if ← shouldProcess path (← IO.currentDir) noDeps then
        if noDeps then
          let t ← IO.asTask $ IO.Process.run
            {cmd := "lake", args := #["env", "lean", "--run", "ExtractData.lean", path.toString, "noDeps"]}
          tasks := tasks.push (t, path)
        else
          let t ← IO.asTask $ IO.Process.run
            {cmd := "lake", args := #["env", "lean", "--run", "ExtractData.lean", path.toString]}
          tasks := tasks.push (t, path)

    for path in ← System.FilePath.walkDir (← Path.toolchainDir) do
      if ← shouldProcess path (← Path.toolchainDir) noDeps then
        if noDeps then
          let t ← IO.asTask $ IO.Process.run
            {cmd := "lake", args := #["env", "lean", "--run", "ExtractData.lean", path.toString, "noDeps"]}
          tasks := tasks.push (t, path)
        else
          let t ← IO.asTask $ IO.Process.run
            {cmd := "lake", args := #["env", "lean", "--run", "ExtractData.lean", path.toString]}
          tasks := tasks.push (t, path)

    for (t, path) in tasks do
      match ← IO.wait t with
      | Except.error e =>
        IO.println s!"Error: {e.toString} path: {path}"
        pure ()
        -- throw e
      | Except.ok _ => pure ()


unsafe def main (args : List String) : IO Unit := do
  try
    match args with
    | ["noDeps", path] => processFile path (noDeps := true)
    | ["noDeps"] => processAllFiles (noDeps := true)
    | [path] => processFile path (noDeps := false)
    | [] => processAllFiles (noDeps := false)
    | _ => throw $ IO.userError "Invalid arguments"
  catch e =>
    -- Explicitly handle the IO.Error
    IO.eprintln s!"An error occurred: {e}"
