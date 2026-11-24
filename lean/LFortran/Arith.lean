/-!
# One arithmetic interface for generated code

`lfortran --show-lean` emits each procedure once, against these classes: a
Fortran `integer` is any `ι` with `[FInt ι]`, a `real` any `α` with
`[FReal α]`. `lean --run` uses the `Int32`/`Float` instances (the generated
`_float` wrappers pick them). Proofs use `ℤ` and an `FPModel` on `ℝ`
(`Arith/Model.lean`). Comparisons return `Bool`, so the same code runs and
can be reasoned about.
-/

namespace LFortran

/-- Fortran `integer`: exact `ℤ` in a proof, two's-complement `Int32` when run. -/
class FInt (ι : Type) where
  ofInt : Int → ι
  toInt : ι → Int
  add : ι → ι → ι
  sub : ι → ι → ι
  mul : ι → ι → ι
  beq : ι → ι → Bool
  lt : ι → ι → Bool
  le : ι → ι → Bool

/-- Fortran `real`: `FPModel` operations on `ℝ` in a proof, `Float` when run. -/
class FReal (α : Type) where
  /-- The zero used to initialise a local. Not a rounded literal. -/
  zero : α
  /-- Conversion `integer → real`. -/
  ofInt : Int → α
  /-- The literal `m · 2^e`: the exact value of a Fortran real constant. -/
  lit : Int → Int → α
  add : α → α → α
  sub : α → α → α
  mul : α → α → α
  div : α → α → α
  /-- Negation (exact: a sign change). -/
  neg : α → α
  beq : α → α → Bool
  lt : α → α → Bool
  le : α → α → Bool

namespace FInt

variable {ι : Type} [FInt ι]

/-- `do k = lo, hi`, as a list. Step is 1. -/
def range (lo hi : ι) : List ι :=
  (List.range (toInt hi + 1 - toInt lo).toNat).map fun (i : Nat) =>
    add lo (ofInt (i : Int))

end FInt

instance : FInt Int8 where
  ofInt := Int8.ofInt
  toInt := Int8.toInt
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

instance : FInt Int16 where
  ofInt := Int16.ofInt
  toInt := Int16.toInt
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

instance : FInt Int32 where
  ofInt := Int32.ofInt
  toInt := Int32.toInt
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

instance : FInt Int64 where
  ofInt := Int64.ofInt
  toInt := Int64.toInt
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

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
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  div := (· / ·)
  neg := (- ·)
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

instance : FReal Float32 where
  zero := 0
  ofInt := Float32.ofInt
  lit m e := Float32.ofInt m * float32Pow2 e
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  div := (· / ·)
  neg := (- ·)
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

end LFortran
