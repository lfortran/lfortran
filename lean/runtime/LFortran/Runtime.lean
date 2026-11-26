import LFortran.Arith
import LFortran.Arr

/-!
# The executable instance

`machine` is the `Kinds` a compiled Fortran program uses: two's-complement
`Int8`…`Int64` and IEEE `Float32`/`Float`. A generated program's `main`
instantiates its template here, in one line:

    def main : IO UInt32 := LFortran.run (prog (K := LFortran.machine))

`stop` and `error stop` leave the process with `halt`.
-/

namespace LFortran

instance : FInt Int8 where
  ofInt := Int8.ofInt
  toInt := Int8.toInt
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  div := (· / ·)
  mod := (· % ·)
  modulo a b := let r := a % b; if r != 0 && (decide (r < 0) != decide (b < 0)) then r + b else r
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

instance : FInt Int16 where
  ofInt := Int16.ofInt
  toInt := Int16.toInt
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  div := (· / ·)
  mod := (· % ·)
  modulo a b := let r := a % b; if r != 0 && (decide (r < 0) != decide (b < 0)) then r + b else r
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

instance : FInt Int32 where
  ofInt := Int32.ofInt
  toInt := Int32.toInt
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  div := (· / ·)
  mod := (· % ·)
  modulo a b := let r := a % b; if r != 0 && (decide (r < 0) != decide (b < 0)) then r + b else r
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

instance : FInt Int64 where
  ofInt := Int64.ofInt
  toInt := Int64.toInt
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  div := (· / ·)
  mod := (· % ·)
  modulo a b := let r := a % b; if r != 0 && (decide (r < 0) != decide (b < 0)) then r + b else r
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

/-- The IEEE sign bit of an unpacked float (`false` for NaN). -/
def unpackedSignbit : Float.Model.UnpackedFloat → Bool
  | .finite .negative _ _ _ => true
  | .zero .negative => true
  | .infinity .negative => true
  | _ => false

/-- The sign bit of a `Float`, read through Lean's logical `Float.Model`. -/
def floatSignbit (x : Float) : Bool := unpackedSignbit x.toModel.unpack

/-- The sign bit of a `Float32`. -/
def float32Signbit (x : Float32) : Bool := unpackedSignbit x.toModel.unpack

/-- Fortran `int(x)` for a `Float`: truncation toward zero, through
`Float.toUInt64` and the sign bit, which (unlike `Float.toInt64`) have a
logical model, so proofs can reason about them. Exact for `|x| < 2^64`;
Fortran leaves larger values undefined. -/
def floatTrunc (x : Float) : Int :=
  if floatSignbit x then -((-x).toUInt64.toNat : Int) else (x.toUInt64.toNat : Int)

/-- The same for `Float32`. -/
def float32Trunc (x : Float32) : Int :=
  if float32Signbit x then -((-x).toUInt64.toNat : Int) else (x.toUInt64.toNat : Int)

/-- `2^e` as a `Float`; exact for `-1022 ≤ e ≤ 1023`, the range the
generator emits. -/
def floatPow2 (e : Int) : Float :=
  Float.ofBits ((e + 1023).toNat.toUInt64 <<< 52)

/-- `2^e` as a `Float32`; exact for `-126 ≤ e ≤ 127`. -/
def float32Pow2 (e : Int) : Float32 :=
  Float32.ofBits ((e + 127).toNat.toUInt32 <<< 23)

instance : FReal Float where
  zero := 0
  ofInt := Float.ofInt
  lit m e := Float.ofInt m * floatPow2 e
  toInt := floatTrunc
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  div := (· / ·)
  neg := (- ·)
  abs := Float.abs
  signbit := floatSignbit
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

instance : FReal Float32 where
  zero := 0
  ofInt := Float32.ofInt
  lit m e := Float32.ofInt m * float32Pow2 e
  toInt := float32Trunc
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  div := (· / ·)
  neg := (- ·)
  abs := Float32.abs
  signbit := float32Signbit
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

instance : FRealMath Float where
  sqrt := Float.sqrt
  exp := Float.exp
  log := Float.log
  log10 := Float.log10
  sin := Float.sin
  cos := Float.cos
  tan := Float.tan
  asin := Float.asin
  acos := Float.acos
  atan := Float.atan
  atan2 := Float.atan2
  sinh := Float.sinh
  cosh := Float.cosh
  tanh := Float.tanh
  pow := Float.pow

instance : FRealMath Float32 where
  sqrt := Float32.sqrt
  exp := Float32.exp
  log := Float32.log
  log10 := Float32.log10
  sin := Float32.sin
  cos := Float32.cos
  tan := Float32.tan
  asin := Float32.asin
  acos := Float32.acos
  atan := Float32.atan
  atan2 := Float32.atan2
  sinh := Float32.sinh
  cosh := Float32.cosh
  tanh := Float32.tanh
  pow := Float32.pow

/-- The kinds of a compiled program. -/
abbrev machine : Kinds where
  I1 := Int8
  I2 := Int16
  I4 := Int32
  I8 := Int64
  R4 := Float32
  R8 := Float

instance : FKindsIO machine where
  i1 := inferInstanceAs (FInt Int8)
  i2 := inferInstanceAs (FInt Int16)
  i4 := inferInstanceAs (FInt Int32)
  i8 := inferInstanceAs (FInt Int64)
  r4 := inferInstanceAs (FReal Float32)
  r8 := inferInstanceAs (FReal Float)
  r4to8 := Float32.toFloat
  r8to4 := Float.toFloat32
  m4 := inferInstanceAs (FRealMath Float32)
  m8 := inferInstanceAs (FRealMath Float)
  fmtR4 := toString
  fmtR8 := toString

/-- Leave the process with exit code `code` (`stop`, `error stop`). -/
def halt {α : Type} (code : UInt8) : IO α :=
  IO.Process.exit code

/-- The exit code of `stop i` / `error stop i`. -/
def stopCode (i : Int) : UInt8 :=
  let m := i.tmod 256
  UInt8.ofNat (if m < 0 then (m + 256).toNat else m.toNat)

/-- Run a program; normal termination is exit code 0. -/
def run (prog : IO Unit) : IO UInt32 := do
  prog
  return 0

end LFortran
