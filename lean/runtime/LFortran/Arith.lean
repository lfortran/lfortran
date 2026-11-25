/-!
# The arithmetic interface of generated code

`lfortran --show-lean` and `lfortran --backend=lean` emit every procedure once,
as a template over `K : Kinds`, one type per Fortran kind (`K.I4` for
`integer(4)`, `K.R8` for `real(8)`, ...). A procedure asks for the weakest
class it needs:

* `[FKinds K]`: integer and real arithmetic, conversions, comparisons. This
  is what a proof instantiates (`ℤ` and an `FPModel` on `ℝ`,
  `LFortran/Arith/Model.lean`).
* `[FKindsMath K]`: also `sqrt`, `exp`, `sin`, ... (`FRealMath`).
* `[FKindsIO K]`: also formatting for `print`.

The executable instance is `LFortran.machine` (`LFortran/Runtime.lean`):
`Int8`…`Int64`, `Float32`, `Float`. A program's `main` is one line:
`def main : IO UInt32 := LFortran.run (prog (K := LFortran.machine))`.

Comparisons return `Bool`, so the same code runs and can be reasoned about.
-/

namespace LFortran

/-- Fortran `integer` of one kind: exact `ℤ` in a proof, two's-complement
`Int32` (etc.) when run. -/
class FInt (ι : Type) where
  ofInt : Int → ι
  toInt : ι → Int
  add : ι → ι → ι
  sub : ι → ι → ι
  mul : ι → ι → ι
  /-- Division, truncating toward zero. -/
  div : ι → ι → ι
  /-- `mod`: the remainder with the sign of the dividend. -/
  mod : ι → ι → ι
  /-- `modulo`: the remainder with the sign of the divisor. -/
  modulo : ι → ι → ι
  beq : ι → ι → Bool
  lt : ι → ι → Bool
  le : ι → ι → Bool

/-- Fortran `real` of one kind: `FPModel` operations on `ℝ` in a proof,
`Float` (etc.) when run. -/
class FReal (α : Type) where
  /-- The zero used to initialise a local. Not a rounded literal. -/
  zero : α
  /-- Conversion `integer → real`. -/
  ofInt : Int → α
  /-- The literal `m · 2^e`: the exact value of a Fortran real constant. -/
  lit : Int → Int → α
  /-- Conversion `real → integer`, truncating toward zero. -/
  toInt : α → Int
  add : α → α → α
  sub : α → α → α
  mul : α → α → α
  div : α → α → α
  /-- Negation (exact: a sign change). -/
  neg : α → α
  /-- Absolute value (exact). -/
  abs : α → α
  /-- The sign bit: true for negative values and for `-0`, as Fortran's
  `sign` uses it on a processor with signed zeros. -/
  signbit : α → Bool
  beq : α → α → Bool
  lt : α → α → Bool
  le : α → α → Bool

/-- Real operations an `FPModel` does not model. Only executable instances
provide them; code that uses them runs but is not (yet) provable. -/
class FRealMath (α : Type) where
  sqrt : α → α
  exp : α → α
  log : α → α
  sin : α → α
  cos : α → α
  tan : α → α
  atan : α → α
  /-- `x ** y` for real `y`. -/
  pow : α → α → α

/-- One Lean type per Fortran kind. -/
structure Kinds where
  I1 : Type
  I2 : Type
  I4 : Type
  I8 : Type
  R4 : Type
  R8 : Type

/-- Arithmetic of every kind, and conversions between real kinds. -/
class FKinds (K : Kinds) where
  i1 : FInt K.I1
  i2 : FInt K.I2
  i4 : FInt K.I4
  i8 : FInt K.I8
  r4 : FReal K.R4
  r8 : FReal K.R8
  /-- `real(x, 8)` of a `real(4)`. -/
  r4to8 : K.R4 → K.R8
  /-- `real(x, 4)` of a `real(8)`: rounds. -/
  r8to4 : K.R8 → K.R4

attribute [instance_reducible, instance] FKinds.i1 FKinds.i2 FKinds.i4 FKinds.i8
  FKinds.r4 FKinds.r8

/-- `FKinds` with the math functions of `FRealMath`. -/
class FKindsMath (K : Kinds) extends FKinds K where
  m4 : FRealMath K.R4
  m8 : FRealMath K.R8

attribute [instance_reducible, instance] FKindsMath.m4 FKindsMath.m8

/-- `FKindsMath` with list-directed formatting of reals, for `print`. -/
class FKindsIO (K : Kinds) extends FKindsMath K where
  fmtR4 : K.R4 → String
  fmtR8 : K.R8 → String

instance {K : Kinds} [FKinds K] : Inhabited K.I1 := ⟨FInt.ofInt 0⟩
instance {K : Kinds} [FKinds K] : Inhabited K.I2 := ⟨FInt.ofInt 0⟩
instance {K : Kinds} [FKinds K] : Inhabited K.I4 := ⟨FInt.ofInt 0⟩
instance {K : Kinds} [FKinds K] : Inhabited K.I8 := ⟨FInt.ofInt 0⟩
instance {K : Kinds} [FKinds K] : Inhabited K.R4 := ⟨FReal.zero⟩
instance {K : Kinds} [FKinds K] : Inhabited K.R8 := ⟨FReal.zero⟩

namespace FInt

variable {ι : Type} [FInt ι]

/-- `do k = lo, hi`, as a list. Step is 1. The `i`-th index is computed as
`ofInt (lo + i)`, so it is exact whenever `lo + i` fits the kind. -/
def range (lo hi : ι) : List ι :=
  (List.range (toInt hi + 1 - toInt lo).toNat).map fun (i : Nat) =>
    ofInt (toInt lo + (i : Int))

end FInt

end LFortran
