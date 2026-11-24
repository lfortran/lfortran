import LFortran.Arith
import LFortran.Prelude
import LFortran.NoGuard

/-!
# Proof instances

`ℤ` is exact. `FReal.ofModel fp` makes every real operation the corresponding
`FPModel` field, so the shared `legendre_poly` reduces to the function the
existing proof already checks. `FReal.ofNoGuard m` does the same for a
`NoGuardModel`; at `fp.toNoGuard` it is `FReal.ofModel fp`.
-/

namespace LFortran

instance : FInt Int where
  ofInt := id
  toInt := id
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

open Classical in
/-- `FReal` operations shared by the two proof instances: literals are
rounded, negation and comparisons are exact. -/
@[instance_reducible]
noncomputable def FReal.mk' (round : ℝ → ℝ) (add sub mul div : ℝ → ℝ → ℝ) : FReal ℝ where
  zero := 0
  ofInt i := round (i : ℝ)
  lit m e := round ((m : ℝ) * (2 : ℝ) ^ e)
  add := add
  sub := sub
  mul := mul
  div := div
  neg a := -a
  beq a b := decide (a = b)
  lt a b := decide (a < b)
  le a b := decide (a ≤ b)

@[instance_reducible]
noncomputable def FReal.ofModel (fp : FPModel) : FReal ℝ :=
  FReal.mk' fp.round fp.add fp.sub fp.mul fp.div

@[simp] theorem ofModel_zero (fp : FPModel) :
    FReal.zero (self := FReal.ofModel fp) = 0 := rfl

@[simp] theorem ofModel_ofInt (fp : FPModel) (n : ℤ) :
    FReal.ofInt (self := FReal.ofModel fp) n = fp.ofInt n := rfl

@[simp] theorem ofModel_lit (fp : FPModel) (m e : ℤ) :
    FReal.lit (self := FReal.ofModel fp) m e = fp.round ((m : ℝ) * (2 : ℝ) ^ e) := rfl

@[simp] theorem ofModel_add (fp : FPModel) (a b : ℝ) :
    FReal.add (self := FReal.ofModel fp) a b = fp.add a b := rfl

@[simp] theorem ofModel_sub (fp : FPModel) (a b : ℝ) :
    FReal.sub (self := FReal.ofModel fp) a b = fp.sub a b := rfl

@[simp] theorem ofModel_mul (fp : FPModel) (a b : ℝ) :
    FReal.mul (self := FReal.ofModel fp) a b = fp.mul a b := rfl

@[simp] theorem ofModel_div (fp : FPModel) (a b : ℝ) :
    FReal.div (self := FReal.ofModel fp) a b = fp.div a b := rfl

/-- The same for a `NoGuardModel`; integer conversion is `round` of the integer. -/
@[instance_reducible]
noncomputable def FReal.ofNoGuard (m : NoGuardModel) : FReal ℝ :=
  FReal.mk' m.round m.add m.sub m.mul m.div

@[simp] theorem ofNoGuard_zero (m : NoGuardModel) :
    FReal.zero (self := FReal.ofNoGuard m) = 0 := rfl

@[simp] theorem ofNoGuard_ofInt (m : NoGuardModel) (n : ℤ) :
    FReal.ofInt (self := FReal.ofNoGuard m) n = m.round (n : ℝ) := rfl

@[simp] theorem ofNoGuard_lit (m : NoGuardModel) (k e : ℤ) :
    FReal.lit (self := FReal.ofNoGuard m) k e = m.round ((k : ℝ) * (2 : ℝ) ^ e) := rfl

@[simp] theorem ofNoGuard_add (m : NoGuardModel) (a b : ℝ) :
    FReal.add (self := FReal.ofNoGuard m) a b = m.add a b := rfl

@[simp] theorem ofNoGuard_sub (m : NoGuardModel) (a b : ℝ) :
    FReal.sub (self := FReal.ofNoGuard m) a b = m.sub a b := rfl

@[simp] theorem ofNoGuard_mul (m : NoGuardModel) (a b : ℝ) :
    FReal.mul (self := FReal.ofNoGuard m) a b = m.mul a b := rfl

@[simp] theorem ofNoGuard_div (m : NoGuardModel) (a b : ℝ) :
    FReal.div (self := FReal.ofNoGuard m) a b = m.div a b := rfl

/-- At `fp.toNoGuard` the two proof instances are the same. -/
theorem ofModel_eq_ofNoGuard (fp : FPModel) :
    FReal.ofModel fp = FReal.ofNoGuard fp.toNoGuard := rfl

@[simp] theorem ofInt_int (n : Int) : (FInt.ofInt n : Int) = n := rfl
@[simp] theorem toInt_int (n : Int) : FInt.toInt n = n := rfl
@[simp] theorem mul_int (a b : Int) : FInt.mul a b = a * b := rfl
@[simp] theorem sub_int (a b : Int) : FInt.sub a b = a - b := rfl
@[simp] theorem add_int (a b : Int) : FInt.add a b = a + b := rfl
@[simp] theorem beq_int (a b : Int) : FInt.beq a b = (a == b) := rfl

/-- `FInt.range` on `ℤ` is the `intRange` the Legendre loop proof unfolds. -/
theorem range_eq_intRange (lo hi : Int) : FInt.range lo hi = intRange lo hi := by
  simp [FInt.range, intRange, FInt.toInt, FInt.ofInt, FInt.add]

end LFortran
