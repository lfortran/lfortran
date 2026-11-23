import LFortran.Arith
import LFortran.Prelude

/-!
# Proof instances

`ℤ` is exact. `FReal.ofModel fp` makes every real operation the corresponding
`FPModel` field, so the shared `legendre_poly` reduces to the function the
existing proof already checks.
-/

namespace LFortran

instance : FInt Int where
  ofInt := id
  toInt := id
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  beq := fun a b => a == b

@[instance_reducible]
noncomputable def FReal.ofModel (fp : FPModel) : FReal ℝ where
  zero := 0
  ofInt := fp.ofInt
  add := fp.add
  sub := fp.sub
  mul := fp.mul
  div := fp.div

@[simp] theorem ofModel_zero (fp : FPModel) :
    FReal.zero (self := FReal.ofModel fp) = 0 := rfl

@[simp] theorem ofModel_ofInt (fp : FPModel) (n : ℤ) :
    FReal.ofInt (self := FReal.ofModel fp) n = fp.ofInt n := rfl

@[simp] theorem ofModel_add (fp : FPModel) (a b : ℝ) :
    FReal.add (self := FReal.ofModel fp) a b = fp.add a b := rfl

@[simp] theorem ofModel_sub (fp : FPModel) (a b : ℝ) :
    FReal.sub (self := FReal.ofModel fp) a b = fp.sub a b := rfl

@[simp] theorem ofModel_mul (fp : FPModel) (a b : ℝ) :
    FReal.mul (self := FReal.ofModel fp) a b = fp.mul a b := rfl

@[simp] theorem ofModel_div (fp : FPModel) (a b : ℝ) :
    FReal.div (self := FReal.ofModel fp) a b = fp.div a b := rfl

@[simp] theorem ofInt_int (n : Int) : (FInt.ofInt n : Int) = n := rfl
@[simp] theorem toInt_int (n : Int) : FInt.toInt n = n := rfl
@[simp] theorem mul_int (a b : Int) : FInt.mul a b = a * b := rfl
@[simp] theorem sub_int (a b : Int) : FInt.sub a b = a - b := rfl

/-- `FInt.range` on `ℤ` is the `intRange` the Legendre loop proof unfolds. -/
theorem range_eq_intRange (lo hi : Int) : FInt.range lo hi = intRange lo hi := by
  simp [FInt.range, intRange, FInt.toInt, FInt.ofInt, FInt.add]

end LFortran
