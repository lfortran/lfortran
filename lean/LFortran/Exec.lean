/-!
# Executable runtime for `lfortran --backend=lean`

The proof translation (`--show-lean`) models `real` as an exact real number
rounded by an abstract `FPModel`, which Lean cannot compute. This module is
the other translation: the one integration tests run. Integers are the
matching two's-complement width (`Int8`/`Int16`/`Int32`/`Int64`), `real(4)`
is `Float32`, `real(8)` is `Float` (IEEE binary64, the same width as
`real(8)`), `logical` is `Bool`, and `character` is a `String`. Arrays are
column-major, with the lower bound of each dimension stored beside the data.

Nothing here is an axiom and nothing is `noncomputable`, so `lean --run` can
execute a translated program. `IO.Process.exit` is how `stop` and
`error stop` leave the process.
-/

namespace LFortran.Exec

/-- Trip count of `do i = lo, hi, step`, with toward-zero division. -/
def tripCount (lo hi step : Int) : Nat :=
  if step == 0 then 0
  else
    let n := (hi - lo + step).tdiv step
    if n > 0 then n.toNat else 0

def fitString (s : String) (n : Nat) : String :=
  let cs := s.toList
  if cs.length >= n then String.ofList (cs.take n)
  else String.ofList (cs ++ List.replicate (n - cs.length) ' ')

def padRight (s : String) (n : Nat) : String :=
  if s.toList.length >= n then s else fitString s n

def strEq (a b : String) : Bool :=
  let n := max a.toList.length b.toList.length
  padRight a n == padRight b n

def strCmp (a b : String) : Ordering :=
  let n := max a.toList.length b.toList.length
  compare (padRight a n).toList (padRight b n).toList

def lenTrim (s : String) : Nat :=
  (s.toList.reverse.dropWhile (· == ' ')).length

def trimRight (s : String) : String :=
  String.ofList (s.toList.reverse.dropWhile (· == ' ') |>.reverse)

def adjustl (s : String) : String :=
  let cs := s.toList
  let body := cs.dropWhile (· == ' ')
  String.ofList (body ++ List.replicate (cs.length - body.length) ' ')

def adjustr (s : String) : String :=
  let cs := s.toList
  let rev := cs.reverse
  let body := rev.dropWhile (· == ' ')
  String.ofList (List.replicate (rev.length - body.length) ' ' ++ body.reverse)

def repeatString (s : String) (n : Nat) : String := Id.run do
  let mut out : String := ""
  for _ in [:n] do
    out := out ++ s
  return out

/-- One character, 1-based, blank past the end. -/
def charAt (s : String) (i : Int) : String :=
  let cs := s.toList
  match (cs.drop (i.toNat - 1)).head? with
  | some c => String.ofList [c]
  | none => " "

/-- Substring from `i` through `j`, both 1-based and inclusive. -/
def substr (s : String) (i j : Int) : String :=
  let cs := s.toList
  let a := i.toNat - 1
  let b := j.toNat
  if b <= a then "" else String.ofList ((cs.drop a).take (b - a))

def halt {α : Type} (code : UInt8) : IO α :=
  IO.Process.exit code

def stopCode (i : Int) : UInt8 :=
  let m := i.tmod 256
  UInt8.ofNat (if m < 0 then (m + 256).toNat else m.toNat)

def ipow32 (a b : Int32) : Int32 := Id.run do
  if b < 0 then return 0
  let mut r : Int32 := 1
  let mut i : Int32 := 0
  while i < b do
    r := r * a
    i := i + 1
  return r

def ipow64 (a b : Int64) : Int64 := Id.run do
  if b < 0 then return 0
  let mut r : Int64 := 1
  let mut i : Int64 := 0
  while i < b do
    r := r * a
    i := i + 1
  return r

def ipow16 (a b : Int16) : Int16 := Id.run do
  if b < 0 then return 0
  let mut r : Int16 := 1
  let mut i : Int16 := 0
  while i < b do
    r := r * a
    i := i + 1
  return r

def ipow8 (a b : Int8) : Int8 := Id.run do
  if b < 0 then return 0
  let mut r : Int8 := 1
  let mut i : Int8 := 0
  while i < b do
    r := r * a
    i := i + 1
  return r

/-- Fortran `mod`: remainder of toward-zero division. -/
def fmod32 (a b : Int32) : Int32 := a - (a / b) * b
def fmod64 (a b : Int64) : Int64 := a - (a / b) * b
def fmod16 (a b : Int16) : Int16 := a - (a / b) * b
def fmod8 (a b : Int8) : Int8 := a - (a / b) * b

/-- Fortran `modulo`: remainder of floor division. -/
def fmodulo32 (a b : Int32) : Int32 :=
  let ai := a.toInt
  let bi := b.toInt
  if bi == 0 then 0 else Int32.ofInt (ai - ai.fdiv bi * bi)

def fmodulo64 (a b : Int64) : Int64 :=
  let ai := a.toInt
  let bi := b.toInt
  if bi == 0 then 0 else Int64.ofInt (ai - ai.fdiv bi * bi)

def fmodulo16 (a b : Int16) : Int16 :=
  let ai := a.toInt
  let bi := b.toInt
  if bi == 0 then 0 else Int16.ofInt (ai - ai.fdiv bi * bi)

def fmodulo8 (a b : Int8) : Int8 :=
  let ai := a.toInt
  let bi := b.toInt
  if bi == 0 then 0 else Int8.ofInt (ai - ai.fdiv bi * bi)

def fsign32 (a b : Int32) : Int32 :=
  let m := if a < 0 then -a else a
  if b < 0 then -m else m

def fsign64 (a b : Int64) : Int64 :=
  let m := if a < 0 then -a else a
  if b < 0 then -m else m

def fsignF (a b : Float) : Float :=
  let m := Float.abs a
  let neg := (Float.toBits b &&& ((1 : UInt64) <<< 63)) != 0
  if neg then -m else m

def fsignF32 (a b : Float32) : Float32 :=
  let m := Float32.abs a
  let neg := (Float32.toBits b &&& ((1 : UInt32) <<< 31)) != 0
  if neg then -m else m

/-- `complex(4)`: two binary32 values. -/
structure C32 where
  re : Float32
  im : Float32
deriving Inhabited

/-- `complex(8)`: two binary64 values. -/
structure C64 where
  re : Float
  im : Float
deriving Inhabited

namespace C32
def add (a b : C32) : C32 := ⟨a.re + b.re, a.im + b.im⟩
def sub (a b : C32) : C32 := ⟨a.re - b.re, a.im - b.im⟩
def neg (a : C32) : C32 := ⟨-a.re, -a.im⟩
def mul (a b : C32) : C32 := ⟨a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re⟩
def div (a b : C32) : C32 :=
  let d := b.re * b.re + b.im * b.im
  ⟨(a.re * b.re + a.im * b.im) / d, (a.im * b.re - a.re * b.im) / d⟩
def abs (a : C32) : Float32 := Float32.sqrt (a.re * a.re + a.im * a.im)
def eq (a b : C32) : Bool := a.re == b.re && a.im == b.im
end C32

namespace C64
def add (a b : C64) : C64 := ⟨a.re + b.re, a.im + b.im⟩
def sub (a b : C64) : C64 := ⟨a.re - b.re, a.im - b.im⟩
def neg (a : C64) : C64 := ⟨-a.re, -a.im⟩
def mul (a b : C64) : C64 := ⟨a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re⟩
def div (a b : C64) : C64 :=
  let d := b.re * b.re + b.im * b.im
  ⟨(a.re * b.re + a.im * b.im) / d, (a.im * b.re - a.re * b.im) / d⟩
def abs (a : C64) : Float := Float.sqrt (a.re * a.re + a.im * a.im)
def eq (a b : C64) : Bool := a.re == b.re && a.im == b.im
end C64

/-- Column-major array with a lower bound per dimension. -/
structure Arr (α : Type) where
  lbs : Array Int
  exts : Array Nat
  data : Array α

instance {α : Type} [Inhabited α] : Inhabited (Arr α) where
  default := { lbs := #[], exts := #[], data := #[] }

def Arr.map {α β : Type} (f : α → β) (a : Arr α) : Arr β :=
  { lbs := a.lbs, exts := a.exts, data := a.data.map f }

def Arr.zipWith {α β γ : Type} (f : α → β → γ) (a : Arr α) (b : Arr β) : Arr γ :=
  { lbs := a.lbs, exts := a.exts, data := Array.zipWith f a.data b.data }

def Arr.offset (lbs : Array Int) (exts : Array Nat) (idxs : Array Int) : Nat := Id.run do
  let mut off : Nat := 0
  let mut stride : Nat := 1
  for d in [:exts.size] do
    let delta := idxs[d]! - lbs[d]!
    off := off + delta.toNat * stride
    stride := stride * exts[d]!
  return off

def Arr.empty {α : Type} : Arr α :=
  { lbs := #[], exts := #[], data := #[] }

def Arr.make {α : Type} (lbs : Array Int) (exts : Array Nat) (fill : α) : Arr α := Id.run do
  let mut n : Nat := 1
  for e in exts do
    n := n * e
  return { lbs, exts, data := Array.replicate n fill }

def Arr.get {α : Type} [Inhabited α] (a : Arr α) (idxs : Array Int) : α :=
  a.data[Arr.offset a.lbs a.exts idxs]!

def Arr.set {α : Type} (a : Arr α) (idxs : Array Int) (v : α) : Arr α :=
  { a with data := a.data.set! (Arr.offset a.lbs a.exts idxs) v }

def Arr.fill {α : Type} (a : Arr α) (v : α) : Arr α :=
  { a with data := Array.replicate a.data.size v }

def Arr.total {α : Type} (a : Arr α) : Int :=
  Int.ofNat (a.exts.foldl (fun n e => n * e) 1)

def Arr.extent {α : Type} (a : Arr α) (dim : Int) : Int :=
  Int.ofNat (a.exts[dim.toNat - 1]!)

def Arr.lbound {α : Type} (a : Arr α) (dim : Int) : Int :=
  a.lbs[dim.toNat - 1]!

def Arr.ubound {α : Type} (a : Arr α) (dim : Int) : Int :=
  Arr.lbound a dim + Arr.extent a dim - 1

/-!
Module variables and variables used by a contained procedure are not Lean
locals: a contained `def` cannot see the caller's `let mut`. They live in
process-wide tables keyed by a compiler-chosen name. Each `lean --run` is its
own process, so tests do not share the tables. Pointer association is not
modeled: an allocatable or pointer is the value itself.
-/

initialize cellI8 : IO.Ref (List (String × Int8)) ← IO.mkRef []
initialize cellI16 : IO.Ref (List (String × Int16)) ← IO.mkRef []
initialize cellI32 : IO.Ref (List (String × Int32)) ← IO.mkRef []
initialize cellI64 : IO.Ref (List (String × Int64)) ← IO.mkRef []
initialize cellF32 : IO.Ref (List (String × Float32)) ← IO.mkRef []
initialize cellF64 : IO.Ref (List (String × Float)) ← IO.mkRef []
initialize cellB : IO.Ref (List (String × Bool)) ← IO.mkRef []
initialize cellS : IO.Ref (List (String × String)) ← IO.mkRef []
initialize cellC32 : IO.Ref (List (String × C32)) ← IO.mkRef []
initialize cellC64 : IO.Ref (List (String × C64)) ← IO.mkRef []
initialize cellAI8 : IO.Ref (List (String × Arr Int8)) ← IO.mkRef []
initialize cellAI16 : IO.Ref (List (String × Arr Int16)) ← IO.mkRef []
initialize cellAI32 : IO.Ref (List (String × Arr Int32)) ← IO.mkRef []
initialize cellAI64 : IO.Ref (List (String × Arr Int64)) ← IO.mkRef []
initialize cellAF32 : IO.Ref (List (String × Arr Float32)) ← IO.mkRef []
initialize cellAF64 : IO.Ref (List (String × Arr Float)) ← IO.mkRef []
initialize cellAB : IO.Ref (List (String × Arr Bool)) ← IO.mkRef []
initialize cellAS : IO.Ref (List (String × Arr String)) ← IO.mkRef []
initialize cellAC32 : IO.Ref (List (String × Arr C32)) ← IO.mkRef []
initialize cellAC64 : IO.Ref (List (String × Arr C64)) ← IO.mkRef []

private def lookup {α : Type} (xs : List (String × α)) (k : String) : Option α :=
  (xs.find? (fun p => p.1 == k)).map (fun p => p.2)

private def put {α : Type} (xs : List (String × α)) (k : String) (v : α) :
    List (String × α) :=
  (k, v) :: xs.filter (fun p => p.1 != k)

def cellGetI8 (k : String) (d : Int8) : IO Int8 := do
  return lookup (← cellI8.get) k |>.getD d
def cellSetI8 (k : String) (v : Int8) : IO Unit := do
  cellI8.set (put (← cellI8.get) k v)
def cellGetI16 (k : String) (d : Int16) : IO Int16 := do
  return lookup (← cellI16.get) k |>.getD d
def cellSetI16 (k : String) (v : Int16) : IO Unit := do
  cellI16.set (put (← cellI16.get) k v)
def cellGetI32 (k : String) (d : Int32) : IO Int32 := do
  return lookup (← cellI32.get) k |>.getD d
def cellSetI32 (k : String) (v : Int32) : IO Unit := do
  cellI32.set (put (← cellI32.get) k v)
def cellGetI64 (k : String) (d : Int64) : IO Int64 := do
  return lookup (← cellI64.get) k |>.getD d
def cellSetI64 (k : String) (v : Int64) : IO Unit := do
  cellI64.set (put (← cellI64.get) k v)
def cellGetF32 (k : String) (d : Float32) : IO Float32 := do
  return lookup (← cellF32.get) k |>.getD d
def cellSetF32 (k : String) (v : Float32) : IO Unit := do
  cellF32.set (put (← cellF32.get) k v)
def cellGetF64 (k : String) (d : Float) : IO Float := do
  return lookup (← cellF64.get) k |>.getD d
def cellSetF64 (k : String) (v : Float) : IO Unit := do
  cellF64.set (put (← cellF64.get) k v)
def cellGetB (k : String) (d : Bool) : IO Bool := do
  return lookup (← cellB.get) k |>.getD d
def cellSetB (k : String) (v : Bool) : IO Unit := do
  cellB.set (put (← cellB.get) k v)
def cellGetS (k : String) (d : String) : IO String := do
  return lookup (← cellS.get) k |>.getD d
def cellSetS (k : String) (v : String) : IO Unit := do
  cellS.set (put (← cellS.get) k v)
def cellGetC32 (k : String) (d : C32) : IO C32 := do
  return lookup (← cellC32.get) k |>.getD d
def cellSetC32 (k : String) (v : C32) : IO Unit := do
  cellC32.set (put (← cellC32.get) k v)
def cellGetC64 (k : String) (d : C64) : IO C64 := do
  return lookup (← cellC64.get) k |>.getD d
def cellSetC64 (k : String) (v : C64) : IO Unit := do
  cellC64.set (put (← cellC64.get) k v)
def cellGetAI8 (k : String) (d : Arr Int8) : IO (Arr Int8) := do
  return lookup (← cellAI8.get) k |>.getD d
def cellSetAI8 (k : String) (v : Arr Int8) : IO Unit := do
  cellAI8.set (put (← cellAI8.get) k v)
def cellGetAI16 (k : String) (d : Arr Int16) : IO (Arr Int16) := do
  return lookup (← cellAI16.get) k |>.getD d
def cellSetAI16 (k : String) (v : Arr Int16) : IO Unit := do
  cellAI16.set (put (← cellAI16.get) k v)
def cellGetAI32 (k : String) (d : Arr Int32) : IO (Arr Int32) := do
  return lookup (← cellAI32.get) k |>.getD d
def cellSetAI32 (k : String) (v : Arr Int32) : IO Unit := do
  cellAI32.set (put (← cellAI32.get) k v)
def cellGetAI64 (k : String) (d : Arr Int64) : IO (Arr Int64) := do
  return lookup (← cellAI64.get) k |>.getD d
def cellSetAI64 (k : String) (v : Arr Int64) : IO Unit := do
  cellAI64.set (put (← cellAI64.get) k v)
def cellGetAF32 (k : String) (d : Arr Float32) : IO (Arr Float32) := do
  return lookup (← cellAF32.get) k |>.getD d
def cellSetAF32 (k : String) (v : Arr Float32) : IO Unit := do
  cellAF32.set (put (← cellAF32.get) k v)
def cellGetAF64 (k : String) (d : Arr Float) : IO (Arr Float) := do
  return lookup (← cellAF64.get) k |>.getD d
def cellSetAF64 (k : String) (v : Arr Float) : IO Unit := do
  cellAF64.set (put (← cellAF64.get) k v)
def cellGetAB (k : String) (d : Arr Bool) : IO (Arr Bool) := do
  return lookup (← cellAB.get) k |>.getD d
def cellSetAB (k : String) (v : Arr Bool) : IO Unit := do
  cellAB.set (put (← cellAB.get) k v)
def cellGetAS (k : String) (d : Arr String) : IO (Arr String) := do
  return lookup (← cellAS.get) k |>.getD d
def cellSetAS (k : String) (v : Arr String) : IO Unit := do
  cellAS.set (put (← cellAS.get) k v)
def cellGetAC32 (k : String) (d : Arr C32) : IO (Arr C32) := do
  return lookup (← cellAC32.get) k |>.getD d
def cellSetAC32 (k : String) (v : Arr C32) : IO Unit := do
  cellAC32.set (put (← cellAC32.get) k v)
def cellGetAC64 (k : String) (d : Arr C64) : IO (Arr C64) := do
  return lookup (← cellAC64.get) k |>.getD d
def cellSetAC64 (k : String) (v : Arr C64) : IO Unit := do
  cellAC64.set (put (← cellAC64.get) k v)

end LFortran.Exec
