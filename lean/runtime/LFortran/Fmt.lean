import LFortran.Arith

/-!
# Fortran formatted output (`FFmt`) and list-directed internal input (`FRead`)

Used only by executable code (`[FKindsIO K]`); proofs over `[FKinds K]` do not
depend on this module.
-/

namespace LFortran

/-- Runtime value passed to Fortran formatted output (`FFmt.format`). -/
inductive FFmtVal where
  | int (n : Int) (bits : Nat := 32)
  | real (x : Float)
  | bool (b : Bool)
  | str (s : String)
  deriving Inhabited

private def strTake (s : String) (n : Nat) : String := (s.take n).toString
private def strDrop (s : String) (n : Nat) : String := (s.drop n).toString
private def strTrim (s : String) : String := s.trimAscii.toString

namespace FFmt

private inductive FmtItem where
  | lit (s : String)
  | slash
  | colon
  | skip (n : Nat)
  | tab (n : Nat)
  | tabL (n : Nat)
  | tabR (n : Nat)
  | scale (k : Int)
  | edit (kind : Char) (w : Nat) (d : Option Nat) (e : Option Nat)
  | group (rep : Nat) (isParen : Bool) (items : List FmtItem)
  deriving Inhabited

private def isDigit (c : Char) : Bool := c >= '0' && c <= '9'

private def upChar (c : Char) : Char :=
  if c >= 'a' && c <= 'z' then Char.ofNat (c.toNat - 32) else c

private def parseNatAt (cs : Array Char) (pos : Nat) : Nat × Nat := Id.run do
  let mut i := pos
  let mut v : Nat := 0
  while i < cs.size && isDigit cs[i]! do
    v := v * 10 + (cs[i]!.toNat - '0'.toNat)
    i := i + 1
  return (v, i)

private def skipWs (cs : Array Char) (pos : Nat) : Nat := Id.run do
  let mut i := pos
  while i < cs.size && (cs[i]! == ' ' || cs[i]! == '\t' || cs[i]! == '\r' || cs[i]! == '\n') do
    i := i + 1
  return i

private partial def parseItems (cs : Array Char) (start : Nat) : List FmtItem × Nat := Id.run do
  let mut i := start
  let mut out : Array FmtItem := #[]
  while i < cs.size do
    i := skipWs cs i
    if i >= cs.size then break
    let c := cs[i]!
    if c == ')' then
      i := i + 1
      break
    if c == ',' then
      i := i + 1
      continue
    if c == '/' then
      out := out.push FmtItem.slash
      i := i + 1
      continue
    if c == ':' then
      out := out.push FmtItem.colon
      i := i + 1
      continue
    if c == '\'' || c == '"' then
      let q := c
      i := i + 1
      let mut s := ""
      while i < cs.size do
        if cs[i]! == q then
          if i + 1 < cs.size && cs[i + 1]! == q then
            s := s.push q
            i := i + 2
          else
            i := i + 1
            break
        else
          s := s.push cs[i]!
          i := i + 1
      out := out.push (FmtItem.lit s)
      continue
    if c == '*' then
      let i1 := skipWs cs (i + 1)
      if i1 < cs.size && cs[i1]! == '(' then
        let (sub, i2) := parseItems cs (i1 + 1)
        out := out.push (FmtItem.group 0 true sub)
        i := i2
      else
        i := i + 1
      continue
    -- Optional signed integer for scale factor `kP` or unsigned repeat count
    let mut neg := false
    if c == '+' || c == '-' then
      neg := c == '-'
      i := i + 1
      i := skipWs cs i
    let hasNum := i < cs.size && isDigit cs[i]!
    let (num, i1) := if hasNum then parseNatAt cs i else (1, i)
    i := skipWs cs i1
    if i >= cs.size then break
    let ch := upChar cs[i]!
    if hasNum && ch == 'P' then
      let k : Int := if neg then -(Int.ofNat num) else Int.ofNat num
      out := out.push (FmtItem.scale k)
      i := i + 1
      continue
    if hasNum && ch == 'H' then
      i := i + 1
      let mut s := ""
      for _ in [:num] do
        if i < cs.size then
          s := s.push cs[i]!
          i := i + 1
      out := out.push (FmtItem.lit s)
      continue
    if ch == '(' then
      let (sub, i2) := parseItems cs (i + 1)
      out := out.push (FmtItem.group num true sub)
      i := i2
      continue
    if ch == '/' then
      for _ in [:num] do out := out.push FmtItem.slash
      i := i + 1
      continue
    if ch == 'X' then
      out := out.push (FmtItem.skip num)
      i := i + 1
      continue
    if ch == 'T' then
      if i + 1 < cs.size && upChar cs[i + 1]! == 'L' then
        let (n, i2) := parseNatAt cs (i + 2)
        out := out.push (FmtItem.tabL n)
        i := i2
      else if i + 1 < cs.size && upChar cs[i + 1]! == 'R' then
        let (n, i2) := parseNatAt cs (i + 2)
        out := out.push (FmtItem.tabR n)
        i := i2
      else
        let (n, i2) := parseNatAt cs (i + 1)
        out := out.push (FmtItem.tab n)
        i := i2
      continue
    if ch == 'S' && i + 1 < cs.size && (upChar cs[i + 1]! == 'P' || upChar cs[i + 1]! == 'S') then
      i := i + 2
      continue
    if ch == 'B' && i + 1 < cs.size && (upChar cs[i + 1]! == 'N' || upChar cs[i + 1]! == 'Z') then
      i := i + 2
      continue
    -- Data edit descriptors: I, A, L, F, E, ES, EN, D, G, B, O, Z
    let mut kind := ch
    let mut j := i + 1
    if ch == 'E' && j < cs.size then
      let nxt := upChar cs[j]!
      if nxt == 'S' then kind := 's'; j := j + 1
      else if nxt == 'N' then kind := 'n'; j := j + 1
    if kind == 'I' || kind == 'A' || kind == 'L' || kind == 'F' || kind == 'E'
        || kind == 's' || kind == 'n' || kind == 'D' || kind == 'G'
        || kind == 'B' || kind == 'O' || kind == 'Z' then
      let (w, j1) := parseNatAt cs j
      let mut j2 := j1
      let mut dOpt : Option Nat := none
      if j2 < cs.size && cs[j2]! == '.' then
        let (d, j3) := parseNatAt cs (j2 + 1)
        dOpt := some d
        j2 := j3
      let mut eOpt : Option Nat := none
      if j2 < cs.size && upChar cs[j2]! == 'E' then
        let (e, j3) := parseNatAt cs (j2 + 1)
        eOpt := some e
        j2 := j3
      let item := FmtItem.edit kind w dOpt eOpt
      if num == 1 then
        out := out.push item
      else
        out := out.push (FmtItem.group num false [item])
      i := j2
    else
      i := i + 1
  return (out.toList, i)

private def parseFormat (fmt : String) : List FmtItem :=
  let cs := fmt.toList.toArray
  let i := skipWs cs 0
  if i < cs.size && cs[i]! == '(' then
    (parseItems cs (i + 1)).1
  else
    (parseItems cs i).1

private def repeatChar (c : Char) (n : Nat) : String :=
  String.ofList (List.replicate n c)

private def padLeft (s : String) (w : Nat) : String :=
  if w == 0 then s
  else if s.length > w then repeatChar '*' w
  else repeatChar ' ' (w - s.length) ++ s

private def padZero (s : String) (m : Nat) : String :=
  if s.length >= m then s else repeatChar '0' (m - s.length) ++ s

private def padReal (s : String) (w : Nat) : String :=
  if w == 0 then s
  else if s.length <= w then repeatChar ' ' (w - s.length) ++ s
  else if s.length == w + 1 && s.length > 3 && s.startsWith "-0." && isDigit (s.toList.getD 3 ' ') then "-" ++ strDrop s 2
  else if s.length == w + 1 && s.length > 2 && s.startsWith "0." && isDigit (s.toList.getD 2 ' ') then strDrop s 1
  else repeatChar '*' w

private def toBase (u : Nat) (base : Nat) : String := Id.run do
  if u == 0 then return "0"
  let hex := #['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']
  let mut v := u
  let mut cs : List Char := []
  while v > 0 do
    cs := hex[v % base]! :: cs
    v := v / base
  return String.ofList cs

def pow10Float (k : Int) : Float := Id.run do
  if k == 0 then return 1.0
  let mut p : Float := 1.0
  let n := if k < 0 then (-k).toNat else k.toNat
  for _ in [:n] do p := p * 10.0
  return if k < 0 then 1.0 / p else p

/-- Extract `sig` rounded decimal digits of non-negative finite `x`, returning
`(digits, exp10)` where `x ≈ 0.<digits> * 10^exp10` (with `digits.length == sig`).
For `x == 0`, returns `(repeatChar '0' sig, 0)`. -/
private def decimalDigits (x : Float) (sig : Nat) : String × Int := Id.run do
  if x == 0.0 || sig == 0 then
    return (repeatChar '0' sig, 0)
  let mut e : Int := Int.ofNat (Float.floor (Float.log10 x)).toUInt64.toNat
  if Float.log10 x < 0.0 then
    let l := Float.floor (Float.log10 x)
    e := -(Int.ofNat (-l).toUInt64.toNat)
  let mut m :=
    if e < -300 then (x * pow10Float 300) / pow10Float (e + 300)
    else if e > 300 then (x / pow10Float 300) / pow10Float (e - 300)
    else x / pow10Float e
  while !m.isInf && !m.isNaN && m >= 10.0 do
    m := m / 10.0
    e := e + 1
  while !m.isInf && !m.isNaN && m < 1.0 && m > 0.0 do
    m := m * 10.0
    e := e - 1
  -- Now 1.0 <= m < 10.0, so x = m * 10^e = (m/10) * 10^(e+1).
  -- Extract `sig` digits with rounding on the last digit.
  let mut digs : Array Nat := Array.mkEmpty (sig + 1)
  let mut cur := m
  for _ in [:sig + 1] do
    let d := (Float.floor cur).toUInt64.toNat
    let d := if d > 9 then 9 else d
    digs := digs.push d
    cur := (cur - Float.ofNat d) * 10.0
  let mut carry : Nat := if digs[sig]! >= 5 then 1 else 0
  let mut res : Array Nat := Array.replicate sig 0
  for k in [:sig] do
    let idx := sig - 1 - k
    let sum := digs[idx]! + carry
    if sum >= 10 then
      res := res.set! idx (sum - 10)
      carry := 1
    else
      res := res.set! idx sum
      carry := 0
  let mut exp10 := e + 1
  if carry > 0 then
    -- Overflowed 9.999... -> 1.000... * 10^(exp10+1)
    res := Array.replicate sig 0
    res := res.set! 0 1
    exp10 := exp10 + 1
  let mut s := ""
  for d in res do
    s := s.push (Char.ofNat ('0'.toNat + d))
  return (s, exp10)

private def formatFixed (x : Float) (w d : Nat) (scale : Int) : String := Id.run do
  if x.isNaN then return padLeft "NaN" w
  if x.isInf then return padLeft (if x < 0.0 then "-Infinity" else "Infinity") w
  let neg := x < 0.0 || (x == 0.0 && (1.0 / x) < 0.0)
  let ax := (if x < 0.0 then -x else x) * pow10Float scale
  if ax == 0.0 then
    let body := (if neg then "-" else "") ++ "0." ++ repeatChar '0' d
    return padReal body w
  let l := Float.floor (Float.log10 ax)
  let e : Int := if l < 0.0 then -(Int.ofNat (-l).toUInt64.toNat) else Int.ofNat l.toUInt64.toNat
  let totalSig : Int := e + 1 + Int.ofNat d
  let body :=
    if totalSig <= 0 then
      let roundUp := totalSig == 0 && (ax * pow10Float (Int.ofNat d) >= 0.5)
      if roundUp then
        if d == 0 then "1."
        else "0." ++ repeatChar '0' (d - 1) ++ "1"
      else
        "0." ++ repeatChar '0' d
    else
      let sig := totalSig.toNat
      let (digs, exp10) := decimalDigits ax sig
      let intDigits := if exp10 > 0 then exp10.toNat else 0
      if intDigits == 0 then
        let leadZeros := (-exp10).toNat
        let frac := strTake (repeatChar '0' leadZeros ++ digs) d
        let frac := frac ++ repeatChar '0' (d - frac.length)
        "0." ++ frac
      else if intDigits <= digs.length then
        let ip := strTake digs intDigits
        let fp := strTake (strDrop digs intDigits) d
        let fp := fp ++ repeatChar '0' (d - fp.length)
        ip ++ "." ++ fp
      else
        let ip := digs ++ repeatChar '0' (intDigits - digs.length)
        ip ++ "." ++ repeatChar '0' d
  let s := (if neg then "-" else "") ++ body
  return padReal s w

private def formatExp (kind : Char) (x : Float) (w d : Nat) (eOpt : Option Nat) (scale : Int) : String := Id.run do
  if x.isNaN then return padLeft "NaN" w
  if x.isInf then return padLeft (if x < 0.0 then "-Infinity" else "Infinity") w
  let neg := x < 0.0 || (x == 0.0 && (1.0 / x) < 0.0)
  let ax := if x < 0.0 then -x else x
  let expChar := if kind == 'D' then "D" else "E"
  let ew := eOpt.getD 2
  let fmtExpPart (ev : Int) : String :=
    let sign := if ev < 0 then "-" else "+"
    let ae := if ev < 0 then (-ev).toNat else ev.toNat
    let es := toString ae
    if es.length > ew && eOpt.isNone then
      sign ++ es
    else
      expChar ++ sign ++ padZero es ew
  if ax == 0.0 then
    let zeroFd :=
      if (kind == 'E' || kind == 'D') && scale > 0 then
        let k := scale.toNat
        if d + 1 > k then d + 1 - k else 0
      else d
    let body := (if neg then "-" else "") ++ "0." ++ repeatChar '0' zeroFd ++ fmtExpPart 0
    return padReal body w
  let (ip, fp, ev) :=
    if kind == 's' then
      let (digs, exp10) := decimalDigits ax (d + 1)
      (strTake digs 1, strDrop digs 1, exp10 - 1)
    else if kind == 'n' then
      let (_, exp0) := decimalDigits ax 1
      let e0 := exp0 - 1
      let rem := (e0 % 3).toNat
      let nInt := rem + 1
      let (digs, exp10) := decimalDigits ax (nInt + d)
      let e1 := exp10 - 1
      let rem1 := (e1 % 3).toNat
      let nInt1 := rem1 + 1
      let (digs2, exp2) := if nInt1 == nInt then (digs, exp10) else decimalDigits ax (nInt1 + d)
      (strTake digs2 nInt1, strDrop digs2 nInt1, exp2 - Int.ofNat nInt1)
    else
      if scale <= 0 then
        let zeros := (-scale).toNat
        let sig := if d > zeros then d - zeros else 1
        let (digs, exp10) := decimalDigits ax sig
        let fp := strTake (repeatChar '0' zeros ++ digs) d
        ("0", fp ++ repeatChar '0' (d - fp.length), exp10 - scale)
      else
        let k := scale.toNat
        let fd := if d + 1 > k then d + 1 - k else 0
        let sig := d + 1
        let (digs, exp10) := decimalDigits ax sig
        let ip := if k <= digs.length then strTake digs k else digs ++ repeatChar '0' (k - digs.length)
        let fp := if k <= digs.length then strDrop digs k else ""
        let fp := strTake (fp ++ repeatChar '0' (fd - fp.length)) fd
        (ip, fp, exp10 - scale)
  let s := (if neg then "-" else "") ++ ip ++ "." ++ fp ++ fmtExpPart ev
  return padReal s w

private def formatEdit (kind : Char) (w : Nat) (dOpt eOpt : Option Nat) (scale : Int)
    (v : FFmtVal) : String :=
  match kind, v with
  | 'A', FFmtVal.str s =>
    if w == 0 then s
    else if s.length >= w then strTake s w
    else repeatChar ' ' (w - s.length) ++ s
  | 'L', FFmtVal.bool b =>
    let c := if b then "T" else "F"
    if w <= 1 then c else repeatChar ' ' (w - 1) ++ c
  | 'I', FFmtVal.int n _ =>
    let m := dOpt.getD 1
    if n == 0 && m == 0 then repeatChar ' ' w
    else
      let neg := n < 0
      let an := if neg then (-n).toNat else n.toNat
      let body := (if neg then "-" else "") ++ padZero (toString an) m
      padLeft body w
  | 'B', FFmtVal.int n bits =>
    let u := (n % Int.ofNat (1 <<< bits)).toNat
    let m := dOpt.getD 1
    let body := if u == 0 && m == 0 then "" else padZero (toBase u 2) m
    padLeft body w
  | 'O', FFmtVal.int n bits =>
    let u := (n % Int.ofNat (1 <<< bits)).toNat
    let m := dOpt.getD 1
    let body := if u == 0 && m == 0 then "" else padZero (toBase u 8) m
    padLeft body w
  | 'Z', FFmtVal.int n bits =>
    let u := (n % Int.ofNat (1 <<< bits)).toNat
    let m := dOpt.getD 1
    let body := if u == 0 && m == 0 then "" else padZero (toBase u 16) m
    padLeft body w
  | 'F', FFmtVal.real x => formatFixed x w (dOpt.getD 0) scale
  | 'E', FFmtVal.real x => formatExp 'E' x w (dOpt.getD 6) eOpt scale
  | 'D', FFmtVal.real x => formatExp 'D' x w (dOpt.getD 6) eOpt scale
  | 's', FFmtVal.real x => formatExp 's' x w (dOpt.getD 6) eOpt scale
  | 'n', FFmtVal.real x => formatExp 'n' x w (dOpt.getD 6) eOpt scale
  | 'G', FFmtVal.int n _ =>
    let body := toString n
    if w == 0 then body else padLeft body w
  | 'G', FFmtVal.bool b =>
    let c := if b then "T" else "F"
    if w <= 1 then c else repeatChar ' ' (w - 1) ++ c
  | 'G', FFmtVal.str s =>
    if w == 0 then s
    else if s.length >= w then strTake s w
    else repeatChar ' ' (w - s.length) ++ s
  | 'G', FFmtVal.real x =>
    if w == 0 then toString x
    else
      let d := dOpt.getD 6
      let ax := if x < 0.0 then -x else x
      if ax == 0.0 || (ax >= 0.1 && ax < pow10Float (Int.ofNat d)) then
        let n := if ax == 0.0 then 0 else (decimalDigits ax d).2
        let fd := if n <= Int.ofNat d then (Int.ofNat d - n).toNat else 0
        formatFixed x w fd 0
      else
        formatExp 'E' x w (if d > 0 then d - 1 else 0) eOpt scale
  | _, FFmtVal.int n _ => padLeft (toString n) w
  | _, FFmtVal.real x => padLeft (toString x) w
  | _, FFmtVal.bool b => padLeft (if b then "T" else "F") w
  | _, FFmtVal.str s => if w == 0 then s else strTake s w

private structure FmtState where
  out : String := ""
  line : Array Char := #[]
  col : Nat := 0
  idx : Nat := 0
  scale : Int := 0
  done : Bool := false

private def writeStr (st : FmtState) (s : String) : FmtState := Id.run do
  let mut buf := st.line
  let mut c := st.col
  for ch in s.toList do
    while buf.size <= c do buf := buf.push ' '
    buf := buf.set! c ch
    c := c + 1
  return { st with line := buf, col := c }

private def flushLine (st : FmtState) : String :=
  st.out ++ String.ofList st.line.toList

private def writeNewline (st : FmtState) : FmtState :=
  { st with out := (flushLine st).push '\n', line := #[], col := 0 }

private partial def runItems (items : List FmtItem) (vals : Array FFmtVal) (st : FmtState) : FmtState := Id.run do
  let mut s := st
  for it in items do
    if s.done then break
    match it with
    | FmtItem.lit txt => s := writeStr s txt
    | FmtItem.slash => s := writeNewline s
    | FmtItem.colon =>
      if s.idx >= vals.size then s := { s with done := true }
    | FmtItem.skip n =>
      s := { s with col := s.col + n }
    | FmtItem.tab n =>
      let target := if n > 0 then n - 1 else 0
      s := { s with col := target }
    | FmtItem.tabR n =>
      s := { s with col := s.col + n }
    | FmtItem.tabL n =>
      let target := if s.col > n then s.col - n else 0
      s := { s with col := target }
    | FmtItem.scale k => s := { s with scale := k }
    | FmtItem.edit kind w dOpt eOpt =>
      if s.idx >= vals.size then
        s := { s with done := true }
      else
        let v := vals[s.idx]!
        s := writeStr { s with idx := s.idx + 1 } (formatEdit kind w dOpt eOpt s.scale v)
    | FmtItem.group rep _ sub =>
      if rep == 0 then
        -- Unlimited format item `*(...)`: repeat until values are exhausted.
        let mut guard : Nat := 0
        while !s.done && s.idx < vals.size && guard < vals.size + 16 do
          let prevIdx := s.idx
          s := runItems sub vals s
          if s.idx == prevIdx then break
          guard := guard + 1
      else
        for _ in [:rep] do
          if s.done then break
          s := runItems sub vals s
  return s

private def lastGroup (items : List FmtItem) : List FmtItem := Id.run do
  let arr := items.toArray
  let mut found : Option Nat := none
  for i in [:arr.size] do
    match arr[i]! with
    | FmtItem.group _ true _ => found := some i
    | _ => pure ()
  match found with
  | some idx => return (arr.extract idx arr.size).toList
  | none => return items

/-- Format `vals` according to Fortran format string `fmt`. -/
def format (fmt : String) (vals : Array FFmtVal) : String := Id.run do
  let items := parseFormat fmt
  let mut st : FmtState := {}
  st := runItems items vals st
  let rev := lastGroup items
  let mut guard : Nat := 0
  while !st.done && st.idx < vals.size && guard < vals.size + 16 do
    let prevIdx := st.idx
    st := writeNewline st
    st := runItems rev vals st
    if st.idx == prevIdx then break
    guard := guard + 1
  return flushLine st

end FFmt

namespace FRead

private def isWs (c : Char) : Bool :=
  c == ' ' || c == '\t' || c == '\r' || c == '\n'

/-- Tokenize a list-directed Fortran input string into values (handling quotes,
complex `(a, b)`, repeat counts `n*v`, and `/` termination). -/
def tokenize (s : String) : Array String := Id.run do
  let cs := s.toList.toArray
  let mut i : Nat := 0
  let mut out : Array String := #[]
  while i < cs.size do
    while i < cs.size && isWs cs[i]! do i := i + 1
    if i >= cs.size then break
    let c := cs[i]!
    if c == '/' then break
    if c == ',' then
      i := i + 1
      continue
    if c == '\'' || c == '"' then
      let q := c
      i := i + 1
      let mut tok := ""
      while i < cs.size do
        if cs[i]! == q then
          if i + 1 < cs.size && cs[i + 1]! == q then
            tok := tok.push q
            i := i + 2
          else
            i := i + 1
            break
        else
          tok := tok.push cs[i]!
          i := i + 1
      out := out.push tok
      while i < cs.size && isWs cs[i]! do i := i + 1
      if i < cs.size && cs[i]! == ',' then i := i + 1
      continue
    if c == '(' then
      let mut tok := ""
      while i < cs.size && cs[i]! != ')' do
        tok := tok.push cs[i]!
        i := i + 1
      if i < cs.size then
        tok := tok.push ')'
        i := i + 1
      out := out.push tok
      while i < cs.size && isWs cs[i]! do i := i + 1
      if i < cs.size && cs[i]! == ',' then i := i + 1
      continue
    let mut tok := ""
    while i < cs.size && !isWs cs[i]! && cs[i]! != ',' && cs[i]! != '/' do
      tok := tok.push cs[i]!
      i := i + 1
    -- Check for repeat count `n*val`
    match tok.splitOn "*" with
    | [nStr, valStr] =>
      match nStr.toNat? with
      | some rep =>
        for _ in [:rep] do out := out.push valStr
      | none => out := out.push tok
    | _ => out := out.push tok
    while i < cs.size && isWs cs[i]! do i := i + 1
    if i < cs.size && cs[i]! == ',' then i := i + 1
  return out

def parseInt (s : String) : Option Int :=
  let t := strTrim s
  if t.startsWith "+" then
    let rest := strDrop t 1
    if rest.toList.headD ' ' >= '0' && rest.toList.headD ' ' <= '9' then rest.toInt? else none
  else t.toInt?

def parseBool (s : String) : Option Bool :=
  let t := (strTrim s).toLower
  if t == "t" || t == ".true." || t.startsWith "t" || t.startsWith ".t" then some true
  else if t == "f" || t == ".false." || t.startsWith "f" || t.startsWith ".f" then some false
  else none

def parseFloat (s : String) : Option Float := Id.run do
  let trimmed := strTrim s
  let low := trimmed.toLower
  if low == "nan" || low == "+nan" || low == "-nan" then return some (0.0 / 0.0)
  if low == "inf" || low == "+inf" || low == "infinity" || low == "+infinity" then return some (1.0 / 0.0)
  if low == "-inf" || low == "-infinity" then return some (-1.0 / 0.0)
  let cs := trimmed.toList.toArray
  if cs.size == 0 then return none
  let mut i : Nat := 0
  let mut neg := false
  if cs[i]! == '-' then neg := true; i := i + 1
  else if cs[i]! == '+' then i := i + 1
  let mut mant : Float := 0.0
  let mut digits : Nat := 0
  while i < cs.size && cs[i]! >= '0' && cs[i]! <= '9' do
    mant := mant * 10.0 + Float.ofNat (cs[i]!.toNat - '0'.toNat)
    digits := digits + 1
    i := i + 1
  let mut fracPow : Int := 0
  if i < cs.size && cs[i]! == '.' then
    i := i + 1
    while i < cs.size && cs[i]! >= '0' && cs[i]! <= '9' do
      mant := mant * 10.0 + Float.ofNat (cs[i]!.toNat - '0'.toNat)
      fracPow := fracPow - 1
      digits := digits + 1
      i := i + 1
  if digits == 0 then return none
  let mut exp : Int := 0
  if i < cs.size && (cs[i]! == 'e' || cs[i]! == 'E' || cs[i]! == 'd' || cs[i]! == 'D') then
    i := i + 1
    let mut eneg := false
    if i < cs.size && cs[i]! == '-' then eneg := true; i := i + 1
    else if i < cs.size && cs[i]! == '+' then i := i + 1
    let mut ed : Nat := 0
    let mut ev : Nat := 0
    while i < cs.size && cs[i]! >= '0' && cs[i]! <= '9' do
      ev := ev * 10 + (cs[i]!.toNat - '0'.toNat)
      ed := ed + 1
      i := i + 1
    if ed == 0 then return none
    exp := if eneg then -(Int.ofNat ev) else Int.ofNat ev
  if i != cs.size then return none
  let totalExp := exp + fracPow
  if mant == 0.0 then return some (if neg then -0.0 else 0.0)
  if totalExp > 320 then return some (if neg then -1.0 / 0.0 else 1.0 / 0.0)
  if totalExp < -350 then return some (if neg then -0.0 else 0.0)
  let mut val := mant
  if totalExp > 0 then
    let mut rem := totalExp.toNat
    while rem > 0 do
      let step := if rem > 300 then 300 else rem
      val := val * FFmt.pow10Float (Int.ofNat step)
      rem := rem - step
  else if totalExp < 0 then
    let mut rem := (-totalExp).toNat
    while rem > 0 do
      let step := if rem > 300 then 300 else rem
      val := val / FFmt.pow10Float (Int.ofNat step)
      rem := rem - step
  return some (if neg then -val else val)

def parseComplex (s : String) : Option (Float × Float) :=
  let t := strTrim s
  if t.startsWith "(" && t.endsWith ")" then
    let inner := ((t.drop 1).dropEnd 1).toString
    match inner.splitOn "," with
    | [rStr, iStr] =>
      match parseFloat rStr, parseFloat iStr with
      | some r, some im => some (r, im)
      | _, _ => none
    | _ => none
  else none

inductive FReadKind where
  | int
  | real
  | bool
  | str (len : Nat)
  deriving Repr, Inhabited

inductive FReadVal where
  | int (n : Int)
  | real (x : Float)
  | bool (b : Bool)
  | str (s : String)
  deriving Inhabited

private def parseBaseInt (s : String) (base : Nat) : Option Int := Id.run do
  let t := strTrim s
  if t.isEmpty then return some 0
  let mut acc : Nat := 0
  for c in t.toList do
    let d? : Option Nat :=
      if '0' <= c && c <= '9' then some (c.toNat - '0'.toNat)
      else if 'a' <= c && c <= 'f' then some (10 + c.toNat - 'a'.toNat)
      else if 'A' <= c && c <= 'F' then some (10 + c.toNat - 'A'.toNat)
      else none
    match d? with
    | some d =>
      if d >= base then return none
      acc := acc * base + d
    | none => return none
  return some (Int.ofNat acc)

private def parseFmtReal (field : String) (d : Nat) (scale : Int) : Option Float :=
  let t := strTrim field
  if t.isEmpty then some 0.0
  else
    let hasDot := t.any (· == '.')
    let hasExp := t.any (fun c => c == 'e' || c == 'E' || c == 'd' || c == 'D')
    match parseFloat t with
    | none => none
    | some v =>
      let v1 := if !hasDot && d > 0 then v / FFmt.pow10Float (Int.ofNat d) else v
      let v2 :=
        if !hasExp && scale != 0 then
          if scale > 0 then v1 / FFmt.pow10Float scale
          else v1 * FFmt.pow10Float (-scale)
        else v1
      some v2

private def sliceField (line : String) (col w : Nat) : String × Nat :=
  let len := line.length
  if w == 0 then
    let rest := if col < len then ((line.drop col).toString) else ""
    (rest, len)
  else
    let s := if col < len then (((line.drop col).take w).toString) else ""
    (s, col + w)

private def applyStrWidth (field : String) (w targetLen : Nat) : String :=
  if targetLen == 0 then ""
  else if w == 0 then
    if field.length >= targetLen then (field.take targetLen).toString
    else field ++ String.ofList (List.replicate (targetLen - field.length) ' ')
  else
    if field.length > targetLen then (field.drop (field.length - targetLen)).toString
    else field ++ String.ofList (List.replicate (targetLen - field.length) ' ')

private structure ReadState where
  line : String := ""
  col : Nat := 0
  scale : Int := 0
  idx : Nat := 0
  out : Array FReadVal := #[]
  status : Int := 0
  msg : String := ""
  stopped : Bool := false

private partial def execReadItems {m : Type → Type} [Monad m]
    (items : List FFmt.FmtItem) (kinds : Array FReadKind)
    (nextLine : m (Option String × Int × String)) (st : ReadState) : m ReadState := do
  let mut s := st
  for it in items do
    if s.stopped || s.status != 0 then return s
    match it with
    | .lit _ => pure ()
    | .skip n => s := { s with col := s.col + n }
    | .tab n => s := { s with col := if n > 0 then n - 1 else 0 }
    | .tabL n => s := { s with col := s.col - n }
    | .tabR n => s := { s with col := s.col + n }
    | .scale k => s := { s with scale := k }
    | .slash =>
      match ← nextLine with
      | (some nl, 0, _) => s := { s with line := nl, col := 0 }
      | (_, st', mg') =>
        if st' > 0 then
          s := { s with status := st', msg := mg', stopped := true }
        else if s.idx < kinds.size then
          s := { s with status := if st' != 0 then st' else -1, msg := if mg'.isEmpty then "End of file" else mg', stopped := true }
        else
          s := { s with stopped := true }
    | .colon =>
      if s.idx >= kinds.size then s := { s with stopped := true }
    | .group rep _ sub =>
      if rep == 0 then
        let mut guard := 0
        while s.idx < kinds.size && !s.stopped && s.status == 0 && guard < 100000 do
          let prev := s.idx
          s ← execReadItems sub kinds nextLine s
          guard := guard + 1
          if s.idx == prev then break
      else
        for _ in [:rep] do
          if s.stopped || s.status != 0 then break
          s ← execReadItems sub kinds nextLine s
    | .edit kind w dOpt _ =>
      if s.idx >= kinds.size then s := { s with stopped := true }
      else
        let d := dOpt.getD 0
        if kind == 'I' then
          let (fld, nc) := sliceField s.line s.col w
          let t := strTrim fld
          if t.isEmpty then
            s := { s with col := nc, idx := s.idx + 1, out := s.out.push (.int 0) }
          else
            match parseInt t with
            | some n => s := { s with col := nc, idx := s.idx + 1, out := s.out.push (.int n) }
            | none => s := { s with status := 1, msg := "Invalid integer", stopped := true }
        else if kind == 'B' || kind == 'O' || kind == 'Z' then
          let base := if kind == 'B' then 2 else if kind == 'O' then 8 else 16
          let (fld, nc) := sliceField s.line s.col w
          match parseBaseInt fld base with
          | some n => s := { s with col := nc, idx := s.idx + 1, out := s.out.push (.int n) }
          | none => s := { s with status := 1, msg := "Invalid integer", stopped := true }
        else if kind == 'F' || kind == 'E' || kind == 'D' || kind == 'S' || kind == 'N' then
          let (fld, nc) := sliceField s.line s.col w
          match parseFmtReal fld d s.scale with
          | some v => s := { s with col := nc, idx := s.idx + 1, out := s.out.push (.real v) }
          | none => s := { s with status := 1, msg := "Invalid real", stopped := true }
        else if kind == 'G' then
          let (fld, nc) := sliceField s.line s.col w
          match kinds[s.idx]! with
          | .int =>
            let t := strTrim fld
            if t.isEmpty then
              s := { s with col := nc, idx := s.idx + 1, out := s.out.push (.int 0) }
            else
              match parseInt t with
              | some n => s := { s with col := nc, idx := s.idx + 1, out := s.out.push (.int n) }
              | none => s := { s with status := 1, msg := "Invalid integer", stopped := true }
          | .real =>
            match parseFmtReal fld d s.scale with
            | some v => s := { s with col := nc, idx := s.idx + 1, out := s.out.push (.real v) }
            | none => s := { s with status := 1, msg := "Invalid real", stopped := true }
          | .bool =>
            match parseBool fld with
            | some b => s := { s with col := nc, idx := s.idx + 1, out := s.out.push (.bool b) }
            | none => s := { s with status := 1, msg := "Invalid logical", stopped := true }
          | .str tlen =>
            s := { s with col := nc, idx := s.idx + 1, out := s.out.push (.str (applyStrWidth fld w tlen)) }
        else if kind == 'L' then
          let effW := if w == 0 then 1 else w
          let (fld, nc) := sliceField s.line s.col effW
          match parseBool fld with
          | some b => s := { s with col := nc, idx := s.idx + 1, out := s.out.push (.bool b) }
          | none => s := { s with status := 1, msg := "Invalid logical", stopped := true }
        else if kind == 'A' then
          let targetLen := match kinds[s.idx]! with | .str n => n | _ => w
          let effW := if w == 0 then targetLen else w
          let (fld, nc) := if effW == 0 then ("", s.col) else sliceField s.line s.col effW
          let val := applyStrWidth fld w targetLen
          s := { s with col := nc, idx := s.idx + 1, out := s.out.push (.str val) }
        else
          s := { s with status := 1, msg := "Unsupported edit descriptor", stopped := true }
  return s

def readFormattedM {m : Type → Type} [Monad m]
    (fmt : String) (nextLine : m (Option String × Int × String)) (kinds : Array FReadKind)
    : m (Array FReadVal × Int × String) := do
  match ← nextLine with
  | (none, st', mg') =>
    return (#[], if st' != 0 then st' else -1, if mg'.isEmpty then "End of file" else mg')
  | (some firstLine, st', mg') =>
    if st' != 0 then return (#[], st', mg')
    let items := FFmt.parseFormat fmt
    let mut s ← execReadItems items kinds nextLine { line := firstLine }
    let rev := FFmt.lastGroup items
    let mut guard := 0
    while s.idx < kinds.size && !s.stopped && s.status == 0 && guard < 100000 do
      match ← nextLine with
      | (some nl, 0, _) =>
        let prev := s.idx
        s ← execReadItems rev kinds nextLine { s with line := nl, col := 0 }
        guard := guard + 1
        if s.idx == prev then break
      | (_, st2, mg2) =>
        s := { s with status := if st2 != 0 then st2 else -1, msg := if mg2.isEmpty then "End of file" else mg2, stopped := true }
        break
    return (s.out, s.status, s.msg)

def readFormattedPure (fmt : String) (lines : Array String) (kinds : Array FReadKind)
    : Array FReadVal × Int × String :=
  (readFormattedM (m := StateM Nat) fmt (do
    let i ← get
    if i < lines.size then
      set (i + 1)
      return (some lines[i]!, 0, "")
    else
      return (none, -1, "End of file")) kinds).run' 0

end FRead

end LFortran
