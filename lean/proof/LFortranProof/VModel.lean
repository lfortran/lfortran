import LFortranProof.Arith.Model
import LFortranProof.Models.Binary64

/-!
# Arithmetic on any carrier, read through its real values

A generated template runs on `K.R8`: `ℝ` in a proof (`FKinds.ofNoGuard`),
Lean's `Float` when executed (`LFortran.machine`). `VModel α` states what a
rounding proof needs from the `FReal α` operations, on any carrier `α`: a
relation `val a x` ("`a` is a finite number with value `x`") and error bounds
for every operation in terms of the values of its operands, as in
`NoGuardModel`:

* `ofInt n` and `lit m e` are within `u |exact| + η` of `n` and `m · 2^e`;
* `a ± b` is within `u (|x| + |y|) + η` of `x ± y` (no guard digit);
* `a * b`, `a / b` are within `u |exact| + η` of `x * y`, `x / y`,

each whenever the exact result is at most `M` in magnitude. A rounding proof
written against `VModel` (with `LFortran/ErrorProp.lean`) applies to every
instance:

* `VModel.ofNoGuard m`: the proof instance `ℝ` with a `NoGuardModel`, so every
  `FPModel` (IEEE, flush-to-zero, Higham, Trefethen & Bau) and Cray-like
  arithmetic; `val a x` is `a = x ∧ x ∈ m.F`;
* `floatV` (`LFortran/FloatVModel.lean`): Lean's `Float`, the run;
  `val f x` is `floatValue f = some x`.

So the theorem about the run is the rounding theorem instantiated at `Float`;
no separate argument relates the run to a real-valued model.

Optional properties, assumed by the programs that need them:

* `VModel.Signed`: negation, `abs`, the sign bit and truncation to an integer
  act on the values;
* `VModel.Guard`: `+` and `-` have a guard digit (the `FPModel` bound);
* `VModel.Binary64`: values are binary64 numbers and operations whose exact
  result is a binary64 number are exact (correctly rounded binary64);
* `IntExact ι B`: an integer kind computes exactly on `[-B, B)`.
-/

namespace LFortran

/-- The `real(8)` literals `FReal.lit m e = m · 2^e` that `VModel` covers: a
significand below `2^53` and an exponent in `[-1022, 971]`, as the generator
emits for normal binary64 constants. -/
def LitOk (m e : ℤ) : Prop := |m| < 2 ^ 53 ∧ -1022 ≤ e ∧ e ≤ 971

/-- An arithmetic on the carrier `α`, read through `val`. See the module
docstring. -/
structure VModel (α : Type) [FReal α] where
  /-- `val a x`: `a` is a finite number with value `x`. -/
  val : α → ℝ → Prop
  /-- Unit roundoff. -/
  u : ℝ
  /-- Absolute error allowed by underflow. -/
  η : ℝ
  /-- The bounds hold for exact results of magnitude at most `M`. -/
  M : ℝ
  ofInt_err : ∀ n : ℤ, |n| < 2 ^ 53 → |(n : ℝ)| ≤ M →
    ∃ v, val (FReal.ofInt n) v ∧ |v - n| ≤ u * |(n : ℝ)| + η
  lit_err : ∀ m e : ℤ, LitOk m e → |(m : ℝ) * (2 : ℝ) ^ e| ≤ M →
    ∃ v, val (FReal.lit m e) v ∧ |v - (m : ℝ) * (2 : ℝ) ^ e| ≤ u * |(m : ℝ) * (2 : ℝ) ^ e| + η
  add_err : ∀ a x b y, val a x → val b y → |x + y| ≤ M →
    ∃ v, val (FReal.add a b) v ∧ |v - (x + y)| ≤ u * (|x| + |y|) + η
  sub_err : ∀ a x b y, val a x → val b y → |x - y| ≤ M →
    ∃ v, val (FReal.sub a b) v ∧ |v - (x - y)| ≤ u * (|x| + |y|) + η
  mul_err : ∀ a x b y, val a x → val b y → |x * y| ≤ M →
    ∃ v, val (FReal.mul a b) v ∧ |v - x * y| ≤ u * |x * y| + η
  div_err : ∀ a x b y, val a x → val b y → y ≠ 0 → |x / y| ≤ M →
    ∃ v, val (FReal.div a b) v ∧ |v - x / y| ≤ u * |x / y| + η

variable {α : Type} [FReal α]

/-- Negation, `abs`, the sign bit and truncation act on values. The sign bit
is unspecified for the value `0` (IEEE `-0.0` has it set). Truncation is
Fortran `int`, required below `2^63`. -/
structure VModel.Signed (V : VModel α) : Prop where
  neg : ∀ a x, V.val a x → V.val (FReal.neg a) (-x)
  abs : ∀ a x, V.val a x → V.val (FReal.abs a) |x|
  signbit : ∀ a x, V.val a x → x ≠ 0 → (FReal.signbit a = true ↔ x < 0)
  toInt : ∀ a x, V.val a x → |x| < 2 ^ 63 → FReal.toInt a = realTrunc x

/-- A guard digit: the error of `+` and `-` is relative to the exact result,
`u |x ± y| + η`, as in `FPModel`, instead of `u (|x| + |y|) + η`. -/
structure VModel.Guard (V : VModel α) : Prop where
  add : ∀ a x b y, V.val a x → V.val b y → |x + y| ≤ V.M →
    ∃ v, V.val (FReal.add a b) v ∧ |v - (x + y)| ≤ V.u * |x + y| + V.η
  sub : ∀ a x b y, V.val a x → V.val b y → |x - y| ≤ V.M →
    ∃ v, V.val (FReal.sub a b) v ∧ |v - (x - y)| ≤ V.u * |x - y| + V.η

/-- Correctly rounded binary64: at least binary64's accuracy and range, values
are binary64 numbers, and an operation whose exact result is a binary64 number
returns it exactly. -/
structure VModel.Binary64 (V : VModel α) : Prop where
  u : V.u ≤ 1 / 2 ^ 53
  η : V.η ≤ 1 / 2 ^ 1075
  M : (2 : ℝ) ^ 1022 ≤ V.M
  mem : ∀ a x, V.val a x → x ∈ binary64
  ofInt : ∀ n : ℤ, |n| < 2 ^ 53 → V.val (FReal.ofInt n) n
  lit : ∀ m e : ℤ, LitOk m e → |(m : ℝ) * (2 : ℝ) ^ e| ≤ 2 ^ 1022 →
    V.val (FReal.lit m e) ((m : ℝ) * (2 : ℝ) ^ e)
  sub : ∀ a x b y, V.val a x → V.val b y → x - y ∈ binary64 → |x - y| ≤ 2 ^ 1022 →
    V.val (FReal.sub a b) (x - y)
  mul : ∀ a x b y, V.val a x → V.val b y → x * y ∈ binary64 → |x * y| ≤ 2 ^ 1022 →
    V.val (FReal.mul a b) (x * y)

/-! ## The proof instance: `ℝ` with a `NoGuardModel` -/

/-- A `NoGuardModel` on `ℝ`: the value of `a` is `a`, if `a ∈ m.F`. -/
noncomputable def VModel.ofNoGuard (m : NoGuardModel) : @VModel ℝ (FReal.ofNoGuard m) :=
  letI := FReal.ofNoGuard m
  { val := fun a x => a = x ∧ x ∈ m.F
    u := m.u
    η := m.η
    M := m.M
    ofInt_err := fun n _ hM => ⟨_, ⟨rfl, m.round_mem _ hM⟩, m.round_err _ hM⟩
    lit_err := fun _ _ _ hM => ⟨_, ⟨rfl, m.round_mem _ hM⟩, m.round_err _ hM⟩
    add_err := fun _ _ _ _ ⟨ha, hx⟩ ⟨hb, hy⟩ hM => by
      subst ha hb; exact ⟨_, ⟨rfl, m.add_mem _ hx _ hy hM⟩, m.add_err _ hx _ hy hM⟩
    sub_err := fun _ _ _ _ ⟨ha, hx⟩ ⟨hb, hy⟩ hM => by
      subst ha hb; exact ⟨_, ⟨rfl, m.sub_mem _ hx _ hy hM⟩, m.sub_err _ hx _ hy hM⟩
    mul_err := fun _ _ _ _ ⟨ha, hx⟩ ⟨hb, hy⟩ hM => by
      subst ha hb; exact ⟨_, ⟨rfl, m.mul_mem _ hx _ hy hM⟩, m.mul_err _ hx _ hy hM⟩
    div_err := fun _ _ _ _ ⟨ha, hx⟩ ⟨hb, hy⟩ hy0 hM => by
      subst ha hb; exact ⟨_, ⟨rfl, m.div_mem _ hx _ hy hy0 hM⟩, m.div_err _ hx _ hy hy0 hM⟩ }

@[simp] theorem VModel.ofNoGuard_val (m : NoGuardModel) (a x : ℝ) :
    @VModel.val ℝ (FReal.ofNoGuard m) (VModel.ofNoGuard m) a x ↔ a = x ∧ x ∈ m.F := Iff.rfl

@[simp] theorem VModel.ofNoGuard_u (m : NoGuardModel) :
    @VModel.u ℝ (FReal.ofNoGuard m) (VModel.ofNoGuard m) = m.u := rfl
@[simp] theorem VModel.ofNoGuard_η (m : NoGuardModel) :
    @VModel.η ℝ (FReal.ofNoGuard m) (VModel.ofNoGuard m) = m.η := rfl
@[simp] theorem VModel.ofNoGuard_M (m : NoGuardModel) :
    @VModel.M ℝ (FReal.ofNoGuard m) (VModel.ofNoGuard m) = m.M := rfl

open Classical in
/-- With a symmetric number set, the proof instance is `Signed`. -/
theorem VModel.ofNoGuard_signed (m : NoGuardModel) (hneg : ∀ x ∈ m.F, -x ∈ m.F) :
    @VModel.Signed ℝ (FReal.ofNoGuard m) (VModel.ofNoGuard m) :=
  letI := FReal.ofNoGuard m
  { neg := fun _ _ ⟨ha, hx⟩ => ⟨by rw [← ha]; rfl, hneg _ hx⟩
    abs := fun a x ⟨ha, hx⟩ => by
      refine ⟨by rw [← ha]; rfl, ?_⟩
      rcases abs_choice x with h | h <;> rw [h]
      exacts [hx, hneg _ hx]
    signbit := fun a x ⟨ha, _⟩ _ => by
      subst ha
      show decide (a < 0) = true ↔ a < 0
      simp
    toInt := fun _ _ ⟨ha, _⟩ _ => by rw [← ha]; rfl }

/-! ## Exact integers -/

/-- `n` is in `[-B, B)`. -/
def InR (B n : ℤ) : Prop := -B ≤ n ∧ n < B

/-- An integer kind `ι` computes exactly on `[-B, B)` (`B = 2^31` for
`integer(4)`): conversions round-trip, and `+ - *`, `modulo` by a positive
number and `==` agree with `ℤ` while all operands and results are in range. -/
structure IntExact (ι : Type) [FInt ι] (B : ℤ) : Prop where
  toInt_ofInt : ∀ n, InR B n → FInt.toInt (FInt.ofInt n : ι) = n
  add : ∀ a b, InR B a → InR B b → InR B (a + b) →
    FInt.add (FInt.ofInt a : ι) (FInt.ofInt b) = FInt.ofInt (a + b)
  sub : ∀ a b, InR B a → InR B b → InR B (a - b) →
    FInt.sub (FInt.ofInt a : ι) (FInt.ofInt b) = FInt.ofInt (a - b)
  mul : ∀ a b, InR B a → InR B b → InR B (a * b) →
    FInt.mul (FInt.ofInt a : ι) (FInt.ofInt b) = FInt.ofInt (a * b)
  modulo : ∀ a b, InR B a → InR B b → 0 < b →
    FInt.modulo (FInt.ofInt a : ι) (FInt.ofInt b) = FInt.ofInt (a.fmod b)
  beq : ∀ a b, InR B a → InR B b → (FInt.beq (FInt.ofInt a : ι) (FInt.ofInt b) = true ↔ a = b)

/-- The proof instance `ℤ` is exact on every range. -/
theorem intExact_int (B : ℤ) : IntExact ℤ B where
  toInt_ofInt _ _ := rfl
  add _ _ _ _ _ := rfl
  sub _ _ _ _ _ := rfl
  mul _ _ _ _ _ := rfl
  modulo _ _ _ _ _ := rfl
  beq a b _ _ := by simp [FInt.beq, FInt.ofInt]

end LFortran
