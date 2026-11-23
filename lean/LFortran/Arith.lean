/-!
# One arithmetic interface for the Legendre prototype

`legendre_poly` is written once against these classes. `lean --run` uses the
`Int32` and `Float` instances. The proof uses `ℤ` and `ℝ` (`Arith/Model.lean`).
This is not yet what `lfortran` emits; it is the shape the backend should emit.
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

/-- Fortran `real`: `FPModel` operations on `ℝ` in a proof, `Float` when run. -/
class FReal (α : Type) where
  /-- The zero used to initialise a local. Not a rounded literal. -/
  zero : α
  ofInt : Int → α
  add : α → α → α
  sub : α → α → α
  mul : α → α → α
  div : α → α → α

namespace FInt

variable {ι : Type} [FInt ι]

/-- `do k = lo, hi`, as a list. Step is 1, as in `legendre_poly`. -/
def range (lo hi : ι) : List ι :=
  (List.range (toInt hi + 1 - toInt lo).toNat).map fun (i : Nat) =>
    add lo (ofInt (i : Int))

end FInt

instance : FInt Int32 where
  ofInt := Int32.ofInt
  toInt := Int32.toInt
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  beq := fun a b => a == b

instance : FReal Float where
  zero := 0
  ofInt := Float.ofInt
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  div := (· / ·)

end LFortran
