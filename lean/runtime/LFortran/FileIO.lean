/-!
# Fortran external file I/O runtime (`open`, `close`, `read`, `write`, `rewind`, `backspace`, `endfile`, `flush`, `inquire`)

Kept separate from `LFortran.Arith` and `LFortran.Arr` so pure/proof templates
never depend on `IO` or mutable file state.
-/

namespace LFortran

namespace FileIO

private def strTrim (s : String) : String :=
  s.trimAscii.toString

private def normOpt (o : Option String) (default : String) : String :=
  match o with
  | some s => (strTrim s).toLower
  | none => default

private def stripCR (s : String) : String :=
  if s.endsWith "\r" then (s.dropEnd 1).toString else s

private def splitLines (content : String) : Array String :=
  if content.isEmpty then #[]
  else
    let parts := (content.splitOn "\n").toArray.map stripCR
    if parts.size > 0 && parts[parts.size - 1]! == "" then
      parts.pop
    else
      parts

private def joinLines (lines : Array String) (trailingNewline : Bool) : String :=
  if lines.isEmpty then ""
  else
    let body := String.intercalate "\n" lines.toList
    if trailingNewline then body ++ "\n" else body

structure FileUnit where
  unit : Int
  filename : Option String
  isScratch : Bool := false
  status : String := "unknown"
  access : String := "sequential"
  form : String := "formatted"
  action : String := "readwrite"
  position : String := "asis"
  recl : Nat := 0
  lines : Array String := #[]
  linePos : Nat := 0
  colPos : Nat := 0
  afterEof : Bool := false
  dirty : Bool := false
  trailingNewline : Bool := true
  deriving Inhabited

structure InquireResult where
  exist : Bool := false
  opened : Bool := false
  number : Int := -1
  named : Bool := false
  name : String := ""
  access : String := "UNDEFINED"
  sequential : String := "UNKNOWN"
  direct : String := "UNKNOWN"
  form : String := "UNDEFINED"
  formatted : String := "UNKNOWN"
  unformatted : String := "UNKNOWN"
  recl : Int := -1
  nextrec : Int := 0
  action : String := "UNDEFINED"
  read : String := "UNKNOWN"
  write : String := "UNKNOWN"
  readwrite : String := "UNKNOWN"
  position : String := "UNDEFINED"
  size : Int := -1
  pos : Int := 1
  iostat : Int := 0
  iomsg : String := ""

initialize unitTable : IO.Ref (Array FileUnit) ← IO.mkRef #[]

private def findIdx (tbl : Array FileUnit) (unit : Int) : Option Nat := Id.run do
  for i in [:tbl.size] do
    if tbl[i]!.unit == unit then
      return some i
  return none

private def findByName (tbl : Array FileUnit) (fname : String) : Option Nat := Id.run do
  let f := strTrim fname
  for i in [:tbl.size] do
    match tbl[i]!.filename with
    | some n => if n == f then return some i
    | none => pure ()
  return none

private def syncUnit (u : FileUnit) : IO (Int × String) := do
  if !u.dirty || u.isScratch then
    return (0, "")
  match u.filename with
  | none => return (0, "")
  | some path =>
    try
      IO.FS.writeFile (System.FilePath.mk path) (joinLines u.lines u.trailingNewline)
      return (0, "")
    catch e =>
      return (1, toString e)

def openUnit (unit : Int) (filename status form access action position : Option String)
    (recl : Option Int) : IO (Int × String) := do
  let st := normOpt status "unknown"
  let acc := normOpt access "sequential"
  let fm := normOpt form (if acc == "direct" then "unformatted" else "formatted")
  let act := normOpt action "readwrite"
  let pos := normOpt position "asis"
  let reclNat : Nat := match recl with | some r => if r > 0 then r.toNat else 0 | none => 0
  let isScratch := st == "scratch"
  let fname : Option String :=
    if isScratch then none
    else match filename with
      | some s =>
        let t := strTrim s
        if t.isEmpty then some s!"fort.{unit}" else some t
      | none => some s!"fort.{unit}"
  -- Close any existing connection on this unit first (syncing it)
  let tbl ← unitTable.get
  match findIdx tbl unit with
  | some idx =>
    let _ ← syncUnit tbl[idx]!
    unitTable.set (tbl.eraseIdx! idx)
  | none => pure ()
  let mut lines : Array String := #[]
  let mut trailingNl : Bool := true
  if !isScratch then
    match fname with
    | some path =>
      let fp := System.FilePath.mk path
      let existsOnDisk ← fp.pathExists
      if st == "old" && !existsOnDisk then
        return (1, s!"Cannot open file '{path}': No such file or directory")
      if st == "new" && existsOnDisk then
        return (1, s!"Cannot open file '{path}': File already exists")
      if st == "old" || (st == "unknown" && existsOnDisk) then
        try
          let content ← IO.FS.readFile fp
          lines := splitLines content
          trailingNl := content.isEmpty || content.endsWith "\n"
        catch e =>
          return (1, toString e)
      else if st == "replace" || st == "new" || (st == "unknown" && !existsOnDisk) then
        if act != "read" then
          try
            IO.FS.writeFile fp ""
          catch e =>
            return (1, toString e)
    | none => pure ()
  let initLinePos : Nat := if pos == "append" then lines.size else 0
  let fu : FileUnit := {
    unit := unit
    filename := fname
    isScratch := isScratch
    status := st
    access := acc
    form := fm
    action := act
    position := pos
    recl := reclNat
    lines := lines
    linePos := initLinePos
    colPos := 0
    afterEof := false
    dirty := false
    trailingNewline := trailingNl
  }
  unitTable.modify (·.push fu)
  return (0, "")

def closeUnit (unit : Int) (status : Option String) : IO (Int × String) := do
  let st := normOpt status "keep"
  let tbl ← unitTable.get
  match findIdx tbl unit with
  | none => return (0, "")
  | some idx =>
    let u := tbl[idx]!
    unitTable.set (tbl.eraseIdx! idx)
    if st == "delete" || u.isScratch then
      match u.filename with
      | some path =>
        try
          IO.FS.removeFile (System.FilePath.mk path)
        catch _ => pure ()
      | none => pure ()
      return (0, "")
    else
      syncUnit u

partial def writeLine (unit : Int) (text : String) (advance : Bool) : IO (Int × String) := do
  let tbl ← unitTable.get
  match findIdx tbl unit with
  | none =>
    if unit == 6 then
      if advance then IO.println text else IO.print text
      return (0, "")
    else if unit == 0 then
      let h ← IO.getStderr
      h.putStr (if advance then text ++ "\n" else text)
      return (0, "")
    else
      let (st, msg) ← openUnit unit none none none none none none none
      if st != 0 then return (st, msg)
      writeLine unit text advance
  | some idx =>
    let u := tbl[idx]!
    if u.action == "read" then
      return (1, s!"Cannot write to unit {unit} opened with READ action")
    if u.afterEof then
      return (1, s!"Cannot write to unit {unit} after ENDFILE")
    let newParts := (text.splitOn "\n").toArray
    let mut lines := u.lines.extract 0 u.linePos
    let mut linePos := u.linePos
    let mut colPos := u.colPos
    if colPos > 0 && u.linePos < u.lines.size then
      let existing := u.lines[u.linePos]!
      let first := if newParts.size > 0 then newParts[0]! else ""
      lines := lines.push (existing ++ first)
      for k in [1:newParts.size] do
        lines := lines.push newParts[k]!
    else
      for k in [:newParts.size] do
        lines := lines.push newParts[k]!
    if advance then
      linePos := lines.size
      colPos := 0
    else
      linePos := if lines.size > 0 then lines.size - 1 else 0
      colPos := if lines.size > 0 then lines[lines.size - 1]!.length else 0
    let u' := { u with lines := lines, linePos := linePos, colPos := colPos, dirty := true, trailingNewline := advance }
    let (st, msg) ← syncUnit u'
    let u'' := if st == 0 then { u' with dirty := false } else u'
    let tbl2 ← unitTable.get
    match findIdx tbl2 unit with
    | some i2 => unitTable.set (tbl2.set! i2 u'')
    | none => pure ()
    return (st, msg)

def readLine (unit : Int) : IO (Option String × Int × String) := do
  let tbl ← unitTable.get
  match findIdx tbl unit with
  | none =>
    if unit == 5 then
      let h ← IO.getStdin
      let line ← h.getLine
      if line.isEmpty then
        return (none, -1, "End of file")
      let trimmed := if line.endsWith "\n" then stripCR (line.dropEnd 1).toString else stripCR line
      return (some trimmed, 0, "")
    else
      return (none, 1, s!"Unit {unit} is not open")
  | some idx =>
    let u := tbl[idx]!
    if u.action == "write" then
      return (none, 1, s!"Cannot read from unit {unit} opened with WRITE action")
    if u.afterEof || u.linePos >= u.lines.size then
      let u' := { u with afterEof := true }
      unitTable.set (tbl.set! idx u')
      return (none, -1, "End of file")
    let line := u.lines[u.linePos]!
    let u' := { u with linePos := u.linePos + 1, colPos := 0 }
    unitTable.set (tbl.set! idx u')
    return (some line, 0, "")

def rewindUnit (unit : Int) : IO (Int × String) := do
  let tbl ← unitTable.get
  match findIdx tbl unit with
  | none => return (0, "")
  | some idx =>
    let u := tbl[idx]!
    if u.access == "direct" then
      return (1, s!"REWIND cannot be used on UNIT {unit} opened with DIRECT access.")
    let (st, msg) ← syncUnit u
    if st != 0 then return (st, msg)
    let u' := { u with linePos := 0, colPos := 0, afterEof := false, dirty := false }
    let tbl2 ← unitTable.get
    match findIdx tbl2 unit with
    | some i2 => unitTable.set (tbl2.set! i2 u')
    | none => pure ()
    return (0, "")

def backspaceUnit (unit : Int) : IO (Int × String) := do
  let tbl ← unitTable.get
  match findIdx tbl unit with
  | none => return (0, "")
  | some idx =>
    let u := tbl[idx]!
    if u.access == "direct" then
      return (1, s!"BACKSPACE cannot be used on UNIT {unit} opened with DIRECT access.")
    let (newPos, newAfterEof) :=
      if u.afterEof then (u.linePos, false)
      else if u.linePos > 0 then (u.linePos - 1, false)
      else (0, false)
    let u' := { u with linePos := newPos, colPos := 0, afterEof := newAfterEof }
    unitTable.set (tbl.set! idx u')
    return (0, "")

def endfileUnit (unit : Int) : IO (Int × String) := do
  let tbl ← unitTable.get
  match findIdx tbl unit with
  | none => return (1, s!"Unit {unit} is not open")
  | some idx =>
    let u := tbl[idx]!
    let keep := if u.colPos > 0 && u.linePos < u.lines.size then u.linePos + 1 else u.linePos
    let lines := u.lines.extract 0 keep
    let u' := { u with lines := lines, linePos := keep, colPos := 0, afterEof := true, dirty := true }
    let (st, msg) ← syncUnit u'
    let u'' := if st == 0 then { u' with dirty := false } else u'
    let tbl2 ← unitTable.get
    match findIdx tbl2 unit with
    | some i2 => unitTable.set (tbl2.set! i2 u'')
    | none => pure ()
    return (st, msg)

def flushUnit (unit : Int) : IO (Int × String) := do
  let tbl ← unitTable.get
  match findIdx tbl unit with
  | none =>
    if unit == 6 then
      (← IO.getStdout).flush
      return (0, "")
    else if unit == 0 then
      (← IO.getStderr).flush
      return (0, "")
    else
      return (0, "")
  | some idx =>
    let u := tbl[idx]!
    let (st, msg) ← syncUnit u
    if st == 0 then
      let tbl2 ← unitTable.get
      match findIdx tbl2 unit with
      | some i2 => unitTable.set (tbl2.set! i2 { u with dirty := false })
      | none => pure ()
    return (st, msg)

private def fileSizeBytes (path : String) : IO Int := do
  try
    let md ← (System.FilePath.mk path).metadata
    return Int.ofNat md.byteSize.toNat
  catch _ =>
    return -1

def inquireUnit (unit : Option Int) (file : Option String) : IO InquireResult := do
  let tbl ← unitTable.get
  match file with
  | some rawName =>
    let fname := strTrim rawName
    let existsOnDisk ← (System.FilePath.mk fname).pathExists
    let fsize ← if existsOnDisk then fileSizeBytes fname else pure (-1)
    match findByName tbl fname with
    | some idx =>
      let u := tbl[idx]!
      return {
        exist := existsOnDisk || true
        opened := true
        number := u.unit
        named := true
        name := fname
        access := u.access.toUpper
        sequential := if u.access == "sequential" then "YES" else "NO"
        direct := if u.access == "direct" then "YES" else "NO"
        form := u.form.toUpper
        formatted := if u.form == "formatted" then "YES" else "NO"
        unformatted := if u.form == "unformatted" then "YES" else "NO"
        recl := if u.recl > 0 then Int.ofNat u.recl else -1
        nextrec := Int.ofNat (u.linePos + 1)
        action := u.action.toUpper
        read := if u.action == "read" || u.action == "readwrite" then "YES" else "NO"
        write := if u.action == "write" || u.action == "readwrite" then "YES" else "NO"
        readwrite := if u.action == "readwrite" then "YES" else "NO"
        position := u.position.toUpper
        size := fsize
        pos := Int.ofNat (u.linePos + 1)
      }
    | none =>
      return {
        exist := existsOnDisk
        opened := false
        number := -1
        named := true
        name := fname
        size := fsize
      }
  | none =>
    match unit with
    | none => return { iostat := 1, iomsg := "INQUIRE requires UNIT or FILE" }
    | some un =>
      match findIdx tbl un with
      | some idx =>
        let u := tbl[idx]!
        let fsize ← match u.filename with
          | some p => fileSizeBytes p
          | none => pure (Int.ofNat (joinLines u.lines u.trailingNewline).utf8ByteSize)
        return {
          exist := true
          opened := true
          number := u.unit
          named := u.filename.isSome
          name := u.filename.getD ""
          access := u.access.toUpper
          sequential := if u.access == "sequential" then "YES" else "NO"
          direct := if u.access == "direct" then "YES" else "NO"
          form := u.form.toUpper
          formatted := if u.form == "formatted" then "YES" else "NO"
          unformatted := if u.form == "unformatted" then "YES" else "NO"
          recl := if u.recl > 0 then Int.ofNat u.recl else -1
          nextrec := Int.ofNat (u.linePos + 1)
          action := u.action.toUpper
          read := if u.action == "read" || u.action == "readwrite" then "YES" else "NO"
          write := if u.action == "write" || u.action == "readwrite" then "YES" else "NO"
          readwrite := if u.action == "readwrite" then "YES" else "NO"
          position := u.position.toUpper
          size := fsize
          pos := Int.ofNat (u.linePos + 1)
        }
      | none =>
        let isPre := un == 0 || un == 5 || un == 6
        return {
          exist := un >= 0
          opened := isPre
          number := if isPre then un else -1
          named := false
          name := ""
        }

end FileIO

end LFortran
