import LFortran.Arith
import LFortran.Prelude
import LFortran.NoGuard
import Mathlib.Algebra.Order.Archimedean.Real.Basic

/-!
# Proof instances

A proof instantiates a generated template at `proofKinds`: every integer
kind is exact `ℤ`, every real kind is `ℝ`. `FKinds.ofNoGuard m4 m8` (or
`FKinds.ofModel fp4 fp8`) makes each `real(4)`/`real(8)` operation the
corresponding field of the arithmetic model for that kind, so a template
reduces to the function a proof checks. Negation, `abs`, comparisons, the
sign bit and `real → integer` truncation are exact. `ℝ` has no `-0`: the
machine's `signbit (-0.0)` is true but the model's `signbit 0` is false, so
a proof about a run that uses `sign(a, b)` must exclude `b = -0.0`.
-/

namespace LFortran

instance : FInt Int where
  ofInt := id
  toInt := id
  add := (· + ·)
  sub := (· - ·)
  mul := (· * ·)
  div := Int.tdiv
  mod := Int.tmod
  modulo := Int.fmod
  beq := fun a b => a == b
  lt := fun a b => decide (a < b)
  le := fun a b => decide (a ≤ b)

/-- Fortran `int(x)`: truncation toward zero. -/
noncomputable def realTrunc (x : ℝ) : ℤ := if 0 ≤ x then ⌊x⌋ else ⌈x⌉

@[simp] theorem realTrunc_intCast (n : ℤ) : realTrunc (n : ℝ) = n := by
  unfold realTrunc; split_ifs <;> simp

theorem realTrunc_of_nonneg {x : ℝ} (h : 0 ≤ x) : realTrunc x = ⌊x⌋ := if_pos h

theorem realTrunc_of_neg {x : ℝ} (h : x < 0) : realTrunc x = ⌈x⌉ := if_neg (not_le.2 h)

open Classical in
/-- `FReal` operations shared by the two proof instances: literals are
rounded, negation and comparisons are exact. -/
@[instance_reducible]
noncomputable def FReal.mk' (round : ℝ → ℝ) (add sub mul div : ℝ → ℝ → ℝ) : FReal ℝ where
  zero := 0
  ofInt i := round (i : ℝ)
  lit m e := round ((m : ℝ) * (2 : ℝ) ^ e)
  toInt := realTrunc
  add := add
  sub := sub
  mul := mul
  div := div
  neg a := -a
  abs a := |a|
  signbit a := decide (a < 0)
  beq a b := decide (a = b)
  lt a b := decide (a < b)
  le a b := decide (a ≤ b)

section mk'
variable (round : ℝ → ℝ) (add sub mul div : ℝ → ℝ → ℝ)
@[simp] theorem mk'_toInt (x : ℝ) :
    FReal.toInt (self := FReal.mk' round add sub mul div) x = realTrunc x := rfl
@[simp] theorem mk'_neg (x : ℝ) : FReal.neg (self := FReal.mk' round add sub mul div) x = -x := rfl
@[simp] theorem mk'_abs (x : ℝ) : FReal.abs (self := FReal.mk' round add sub mul div) x = |x| :=
  rfl
open Classical in
@[simp] theorem mk'_signbit (x : ℝ) :
    FReal.signbit (self := FReal.mk' round add sub mul div) x = decide (x < 0) := rfl
open Classical in
@[simp] theorem mk'_lt (a b : ℝ) :
    FReal.lt (self := FReal.mk' round add sub mul div) a b = decide (a < b) := rfl
open Classical in
@[simp] theorem mk'_le (a b : ℝ) :
    FReal.le (self := FReal.mk' round add sub mul div) a b = decide (a ≤ b) := rfl
open Classical in
@[simp] theorem mk'_beq (a b : ℝ) :
    FReal.beq (self := FReal.mk' round add sub mul div) a b = decide (a = b) := rfl
end mk'

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
@[simp] theorem div_int (a b : Int) : FInt.div a b = a.tdiv b := rfl
@[simp] theorem mod_int (a b : Int) : FInt.mod a b = a.tmod b := rfl
@[simp] theorem modulo_int (a b : Int) : FInt.modulo a b = a.fmod b := rfl
@[simp] theorem lt_int (a b : Int) : FInt.lt a b = decide (a < b) := rfl
@[simp] theorem le_int (a b : Int) : FInt.le a b = decide (a ≤ b) := rfl

/-- `FInt.range` on `ℤ` is the `intRange` the Legendre loop proof unfolds. -/
theorem range_eq_intRange (lo hi : Int) : FInt.range lo hi = intRange lo hi := by
  simp [FInt.range, intRange, FInt.toInt, FInt.ofInt, FInt.add]

/-- The kinds of a proof: exact integers, real numbers. -/
abbrev proofKinds : Kinds where
  I1 := ℤ
  I2 := ℤ
  I4 := ℤ
  I8 := ℤ
  R4 := ℝ
  R8 := ℝ

/-- `real(4)` arithmetic is `m4`, `real(8)` is `m8`; conversions between the
real kinds round in the target kind. -/
@[instance_reducible]
noncomputable def FKinds.ofNoGuard (m4 m8 : NoGuardModel) : FKinds proofKinds where
  i1 := inferInstanceAs (FInt ℤ)
  i2 := inferInstanceAs (FInt ℤ)
  i4 := inferInstanceAs (FInt ℤ)
  i8 := inferInstanceAs (FInt ℤ)
  r4 := FReal.ofNoGuard m4
  r8 := FReal.ofNoGuard m8
  r4to8 := m8.round
  r8to4 := m4.round

/-- The same for `FPModel`s. -/
@[instance_reducible]
noncomputable def FKinds.ofModel (fp4 fp8 : FPModel) : FKinds proofKinds where
  i1 := inferInstanceAs (FInt ℤ)
  i2 := inferInstanceAs (FInt ℤ)
  i4 := inferInstanceAs (FInt ℤ)
  i8 := inferInstanceAs (FInt ℤ)
  r4 := FReal.ofModel fp4
  r8 := FReal.ofModel fp8
  r4to8 := fp8.round
  r8to4 := fp4.round

theorem ofModel_eq_ofNoGuard_kinds (fp4 fp8 : FPModel) :
    FKinds.ofModel fp4 fp8 = FKinds.ofNoGuard fp4.toNoGuard fp8.toNoGuard := rfl

end LFortran
