import LFortranProof.FloatAddReal
import Init.Data.Float.Model.Unpacked.Operations.Sub
import Init.Data.Float.Model.Unpacked.Operations.Mul
import Init.Data.Float.Model.Unpacked.Operations.Div
import Mathlib.Tactic.LinearCombination

set_option exponentiation.threshold 2048

/-!
# Subtraction, multiplication and division of finite binary64 values

`LFortran.FloatAddReal` proves the `FPModel` addition contract for Lean's
`Float`. This file does the same for `-`, `*` and `/`:

* `UnpackedFloat.sub` is `add` with the second sign flipped;
* `UnpackedFloat.mul` rounds the exact product `m₁ m₂ · 2^(e₁+e₂)`;
* `UnpackedFloat.div` rounds a quotient `q · 2^t` whose discarded fraction
  `r / m₂` is recorded in `Accuracy` by `accuracyOfFraction`.

All three go through `roundWithAccuracy`, generalized here
(`roundAcc_value`) to an inexact starting accuracy and to a zero significand
(a quotient that underflows). For canonical operands the result is within
`2^-53 |exact| + 2^-1075` of the exact result, is canonical, and is a binary64
number when `|exact| ≤ 2^1022` (`float_sub_value`, `float_mul_value`,
`float_div_value`).
-/

open LFortran
open Float.Model
open Float.Model.UnpackedFloat hiding round add ofInt sub mul div

namespace LFortran

theorem roundWithAccuracy_expand_acc (s : Sign) (m : Nat) (e : Int) (acc : Accuracy) :
    roundWithAccuracy Format.binary64 s m e acc =
      (let (em, e1) := shiftToTargetExponent Format.binary64 m e acc
       let mr := em.roundedMantissa
       let (em2, e2) := shiftToTargetExponent Format.binary64 mr e1 .exact
       if h : em2.mantissa = 0 then .zero s
       else .finite s em2.mantissa e2 (Nat.pos_of_ne_zero h)) := by
  rfl

theorem ofMA_mantissa (m : Nat) (acc : Accuracy) :
    (ExtendedMantissa.ofMantissaAndAccuracy m acc).mantissa = m := by
  unfold ExtendedMantissa.ofMantissaAndAccuracy
  split <;> rfl

theorem ofMA_accuracy (m : Nat) (acc : Accuracy) :
    (ExtendedMantissa.ofMantissaAndAccuracy m acc).accuracy = acc := by
  unfold ExtendedMantissa.ofMantissaAndAccuracy
  split <;> rfl

theorem target_eq' (m : Nat) (e : Int) :
    Format.binary64.targetExponent (totalExponent m e) =
      max ((m.log2 : Int) + 1 + e - 53) (-1074) := by
  simp [totalExponent, Format.targetExponent, binary64_mantissaBits, binary64_minExponent]

/-- `roundWithAccuracy` of a significand `m` with accuracy `acc` that
represents `x` at exponent `e ≤` target is within half a unit in the last
place of the target. `m` may be `0`. -/
theorem roundAcc_value (s : Sign) (m : Nat) (e : Int) (acc : Accuracy) {x : ℝ}
    (hx : Represents (ExtendedMantissa.ofMantissaAndAccuracy m acc) e x)
    (hle : e ≤ Format.binary64.targetExponent (totalExponent m e)) :
    ∃ w : ℝ, uvalue (roundWithAccuracy Format.binary64 s m e acc) = some (sgn s * w) ∧
      |w - x| ≤ (2 : ℝ) ^ (Format.binary64.targetExponent (totalExponent m e) - 1) ∧
      (∃ n : Nat, n ≤ 2 ^ 53 ∧
        w = (n : ℝ) * (2 : ℝ) ^ Format.binary64.targetExponent (totalExponent m e)) ∧
      (Format.binary64.targetExponent (totalExponent m e) ≤ 970 →
        Canon (roundWithAccuracy Format.binary64 s m e acc)) := by
  have htgt := target_eq' m e
  set t := Format.binary64.targetExponent (totalExponent m e) with ht
  set k := (t - e).toNat with hk
  have hke : e + (k : Int) = t := by omega
  set em0 := ExtendedMantissa.ofMantissaAndAccuracy m acc with hem0
  have hs1 : shiftToTargetExponent Format.binary64 m e acc = (em0 >>> k, t) := by
    simp only [shiftToTargetExponent, shiftToExponent]
    rw [← ht, ← hk, hke]
  have hrep : Represents (em0 >>> k) t x := by
    have := repeat_represents k hx
    rw [hke] at this
    simpa [HShiftRight.hShiftRight] using this
  set em1 := em0 >>> k with hem1
  have hman : em1.mantissa = m / 2 ^ k := by
    rw [hem1, shiftRight_mantissa, hem0, ofMA_mantissa]
  have hlt : em1.mantissa < 2 ^ 53 := by
    rw [hman]
    by_cases hm0 : m = 0
    · subst hm0; simp
    · apply div_target_lt m k (Nat.pos_of_ne_zero hm0)
      omega
  have hmr : em1.roundedMantissa ≤ 2 ^ 53 := by
    rcases rounded_sub em1 with h | h <;> omega
  have herr := represents_round_err hrep
  have htlo : -1074 ≤ t := by rw [ht]; exact target_ge m e
  have hren := renormalize_exact em1.roundedMantissa t hmr htlo
  have hlow : t ≠ -1074 → 2 ^ 52 ≤ em1.roundedMantissa := by
    intro hne
    by_cases hm0 : m = 0
    · subst hm0
      simp only [Nat.log2_zero] at htgt
      omega
    · have hk52 : 52 + k ≤ m.log2 := by omega
      have := div_target_ge m k hk52 (Nat.pos_of_ne_zero hm0)
      rcases rounded_sub em1 with h | h <;> omega
  have hcases := renorm_cases em1.roundedMantissa t hmr htlo
  rw [roundWithAccuracy_expand_acc, hs1]
  simp only [shiftToTargetExponent, shiftToExponent, ExtendedMantissa.ofMantissaAndAccuracy,
    shiftRight_mantissa]
  simp only [] at hren hcases
  split
  · rename_i h0
    refine ⟨0, by simp [uvalue], ?_, ⟨em1.roundedMantissa, hmr, ?_⟩, fun _ => trivial⟩
    · rw [h0, Nat.cast_zero, zero_mul] at hren
      rw [← hren] at herr
      simpa using herr
    · rw [h0, Nat.cast_zero, zero_mul] at hren
      exact hren
  · rename_i h0
    refine ⟨_, by simp only [uvalue, bitValue_eq], ?_, ⟨em1.roundedMantissa, hmr, hren⟩, ?_⟩
    · rw [hren]
      exact herr
    · intro h970
      have hmr0 : em1.roundedMantissa ≠ 0 := by
        intro hz; apply h0; rw [hz]; simp
      rcases hcases hmr0 with ⟨hbig, hk1⟩ | ⟨hlt, hk0⟩
      · simp only [Canon]
        rw [hk1, hbig]
        refine ⟨by norm_num, by push_cast; omega, by push_cast; omega, Or.inl (by norm_num)⟩
      · simp only [Canon]
        rw [hk0]
        simp only [pow_zero, Nat.div_one, Nat.cast_zero, add_zero]
        refine ⟨hlt, htlo, by omega, ?_⟩
        by_cases hte : t = -1074
        · exact Or.inr hte
        · exact Or.inl (hlow hte)

/-- `halfUlp_le_model` for an `x` that is at least `m · 2^e` (not equal). -/
theorem halfUlp_le_model' {m : Nat} {e t : Int} (hm : 0 < m)
    (ht : t = max ((m.log2 : Int) + 1 + e - 53) (-1074))
    {v x : ℝ} (hxlo : (m : ℝ) * (2 : ℝ) ^ e ≤ x) (hv : |v - x| ≤ (2 : ℝ) ^ (t - 1)) :
    |v - x| ≤ (1 / 2 ^ 53) * |x| + 1 / 2 ^ 1075 := by
  have hpow : (2 : ℝ) ^ (m.log2 : ℤ) ≤ m := by
    exact_mod_cast Nat.log2_self_le (by omega)
  have hx0 : 0 ≤ x := le_trans (mul_nonneg (Nat.cast_nonneg m) (zpow_nonneg (by norm_num) e)) hxlo
  rw [abs_of_nonneg hx0]
  rcases max_cases ((m.log2 : Int) + 1 + e - 53) (-1074) with ⟨hmx, -⟩ | ⟨hmx, -⟩
  · rw [ht, hmx] at hv
    have hbase : (2 : ℝ) ^ ((m.log2 : ℤ) + e) ≤ x := by
      rw [zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0)]
      exact le_trans (mul_le_mul_of_nonneg_right hpow (zpow_nonneg (by norm_num) _)) hxlo
    have hhalf : (2 : ℝ) ^ (((m.log2 : ℤ) + 1 + e - 53) - 1) ≤ (1 / 2 ^ 53) * x := by
      have hrew : ((m.log2 : ℤ) + 1 + e - 53) - 1 = (m.log2 : ℤ) + e - 53 := by omega
      rw [hrew, zpow_sub₀ (by norm_num : (2 : ℝ) ≠ 0), zpow_ofNat]
      calc (2 : ℝ) ^ ((m.log2 : ℤ) + e) / 2 ^ 53
          ≤ x / 2 ^ 53 := div_le_div_of_nonneg_right hbase (by positivity)
        _ = (1 / 2 ^ 53) * x := by ring
    calc |v - x| ≤ (2 : ℝ) ^ (((m.log2 : ℤ) + 1 + e - 53) - 1) := hv
      _ ≤ (1 / 2 ^ 53) * x := hhalf
      _ ≤ (1 / 2 ^ 53) * x + 1 / 2 ^ 1075 := le_add_of_nonneg_right (by positivity)
  · rw [ht, hmx] at hv
    have hsub : (2 : ℝ) ^ (((-1074 : ℤ) - 1)) = 1 / 2 ^ 1075 := by norm_num
    calc |v - x| ≤ (2 : ℝ) ^ ((-1074 : ℤ) - 1) := hv
      _ = 1 / 2 ^ 1075 := hsub
      _ ≤ (1 / 2 ^ 53) * x + 1 / 2 ^ 1075 := le_add_of_nonneg_left (by positivity)

/-- `target_le_970` for an `x` that is at least `m · 2^e`. -/
theorem target_le_970' (m : Nat) (e : Int) (hm : 0 < m) {x : ℝ}
    (hxlo : (m : ℝ) * (2 : ℝ) ^ e ≤ x) (hx : x ≤ 2 ^ 1022) :
    Format.binary64.targetExponent (totalExponent m e) ≤ 970 :=
  target_le_970 m e hm (le_trans hxlo hx)

theorem sgn_neg (s : Sign) : sgn (-s) = -sgn s := by
  cases s
  · show sgn .positive = -sgn .negative; norm_num [sgn]
  · show sgn .negative = -sgn .positive; norm_num [sgn]

theorem sgn_mul (s₁ s₂ : Sign) : sgn (s₁ * s₂) = sgn s₁ * sgn s₂ := by
  cases s₁ <;> cases s₂
  · show sgn .positive = sgn .negative * sgn .negative; norm_num [sgn]
  · show sgn .negative = sgn .negative * sgn .positive; norm_num [sgn]
  · show sgn .negative = sgn .positive * sgn .negative; norm_num [sgn]
  · show sgn .positive = sgn .positive * sgn .positive; norm_num [sgn]

theorem sgn_div (s₁ s₂ : Sign) : sgn (s₁ / s₂) = sgn s₁ * sgn s₂ := by
  cases s₁ <;> cases s₂
  · show sgn .positive = sgn .negative * sgn .negative; norm_num [sgn]
  · show sgn .negative = sgn .negative * sgn .positive; norm_num [sgn]
  · show sgn .negative = sgn .positive * sgn .negative; norm_num [sgn]
  · show sgn .positive = sgn .positive * sgn .positive; norm_num [sgn]

theorem bitValue_neg (s : Sign) (m : Nat) (e : Int) : bitValue (-s) m e = -bitValue s m e := by
  rw [bitValue_eq, bitValue_eq, sgn_neg]; ring

/-! ## Subtraction -/

theorem sub_finite_eq (s₁ s₂ : Sign) (m₁ m₂ : Nat) (e₁ e₂ : Int) (h₁ : 0 < m₁) (h₂ : 0 < m₂) :
    UnpackedFloat.sub Format.binary64 (.finite s₁ m₁ e₁ h₁) (.finite s₂ m₂ e₂ h₂) =
      normalize Format.binary64
        (s₁.apply (decreaseExponent m₁ e₁ (min e₁ e₂)).1 -
          s₂.apply (decreaseExponent m₂ e₂ (min e₁ e₂)).1) (min e₁ e₂) .positive := by
  rfl

/-- **Subtraction**, for finite or zero unpacked operands of either sign. -/
theorem sub_value {a b : UnpackedFloat} {x y : ℝ}
    (ha : uvalue a = some x) (hb : uvalue b = some y) :
    ∃ v : ℝ, uvalue (UnpackedFloat.sub Format.binary64 a b) = some v ∧
      |v - (x - y)| ≤ (1 / 2 ^ 53) * |x - y| + 1 / 2 ^ 1075 ∧
      (x ∈ binary64 → y ∈ binary64 → |x - y| ≤ 2 ^ 1022 → v ∈ binary64) ∧
      (Canon a → Canon b → |x - y| ≤ 2 ^ 1022 → Canon (UnpackedFloat.sub Format.binary64 a b)) := by
  have hnn : ∀ z : ℝ, (0 : ℝ) ≤ (1 / 2 ^ 53) * |z| + 1 / 2 ^ 1075 := fun z => by positivity
  cases a with
  | notANumber => simp [uvalue] at ha
  | infinity _ => simp [uvalue] at ha
  | zero s₁ =>
    simp only [uvalue, Option.some.injEq] at ha
    subst ha
    cases b with
    | notANumber => simp [uvalue] at hb
    | infinity _ => simp [uvalue] at hb
    | zero s₂ =>
      simp only [uvalue, Option.some.injEq] at hb
      subst hb
      refine ⟨0, ?_, by simp, fun _ _ _ => zero_mem_binary64, ?_⟩
      · simp only [UnpackedFloat.sub]
        split <;> rfl
      · intro _ _ _
        simp only [UnpackedFloat.sub]
        split <;> trivial
    | finite s₂ m₂ e₂ h₂ =>
      simp only [uvalue, Option.some.injEq] at hb
      refine ⟨-y, ?_, by simpa using hnn (-y), fun _ hy _ => neg_mem_binary64 hy,
        fun _ hc _ => hc⟩
      simp only [UnpackedFloat.sub, uvalue, bitValue_neg, hb]
  | finite s₁ m₁ e₁ h₁ =>
    cases b with
    | notANumber => simp [uvalue] at hb
    | infinity _ => simp [uvalue] at hb
    | zero s₂ =>
      simp only [uvalue, Option.some.injEq] at hb
      subst hb
      refine ⟨x, ?_, by simpa using hnn x, fun hx _ _ => hx, fun hc _ _ => hc⟩
      simpa [UnpackedFloat.sub] using ha
    | finite s₂ m₂ e₂ h₂ =>
      simp only [uvalue, Option.some.injEq] at ha hb
      rw [sub_finite_eq]
      obtain ⟨v, hv, herr⟩ := normalize_value
        (s₁.apply (decreaseExponent m₁ e₁ (min e₁ e₂)).1 -
          s₂.apply (decreaseExponent m₂ e₂ (min e₁ e₂)).1) (min e₁ e₂) .positive
      refine ⟨v, hv, ?_⟩
      have hv₁ := decreaseExponent_bitValue s₁ m₁ e₁ (min e₁ e₂)
      have hv₂ := decreaseExponent_bitValue s₂ m₂ e₂ (min e₁ e₂)
      rw [decrease_snd m₁ e₁ _ (min_le_left _ _)] at hv₁
      rw [decrease_snd m₂ e₂ _ (min_le_right _ _)] at hv₂
      have hsum : (((s₁.apply (decreaseExponent m₁ e₁ (min e₁ e₂)).1 -
          s₂.apply (decreaseExponent m₂ e₂ (min e₁ e₂)).1 : Int)) : ℝ) * (2 : ℝ) ^ (min e₁ e₂) =
          x - y := by
        rw [← ha, ← hb, ← hv₁, ← hv₂]
        simp only [bitValue]
        push_cast
        ring
      rw [hsum] at herr
      exact ⟨herr.1, fun _ _ => herr.2.1, fun _ _ => herr.2.2⟩

/-! ## Multiplication -/

theorem mul_finite_eq (s₁ s₂ : Sign) (m₁ m₂ : Nat) (e₁ e₂ : Int) (h₁ : 0 < m₁) (h₂ : 0 < m₂) :
    UnpackedFloat.mul Format.binary64 (.finite s₁ m₁ e₁ h₁) (.finite s₂ m₂ e₂ h₂) =
      roundWithAccuracy Format.binary64 (s₁ * s₂) (m₁ * m₂) (e₁ + e₂) .exact := by
  rfl

/-- For canonical operands the exact product is at or below its target
exponent: a normal significand has `2^52 ≤ m`, two subnormals have
`e₁ + e₂ = -2148`. -/
theorem mul_le_target {s₁ s₂ : Sign} {m₁ m₂ : Nat} {e₁ e₂ : Int} {h₁ : 0 < m₁} {h₂ : 0 < m₂}
    (hc₁ : Canon (.finite s₁ m₁ e₁ h₁)) (hc₂ : Canon (.finite s₂ m₂ e₂ h₂)) :
    e₁ + e₂ ≤ Format.binary64.targetExponent (totalExponent (m₁ * m₂) (e₁ + e₂)) := by
  obtain ⟨-, hlo₁, -, hn₁⟩ := hc₁
  obtain ⟨-, hlo₂, -, hn₂⟩ := hc₂
  rw [target_eq']
  have hpos : m₁ * m₂ ≠ 0 := Nat.mul_ne_zero (by omega) (by omega)
  rcases hn₁ with hn₁ | hn₁
  · have : 2 ^ 52 ≤ m₁ * m₂ := le_trans hn₁ (Nat.le_mul_of_pos_right _ h₂)
    have := (Nat.le_log2 hpos).2 this
    omega
  · rcases hn₂ with hn₂ | hn₂
    · have : 2 ^ 52 ≤ m₁ * m₂ := le_trans hn₂ (Nat.le_mul_of_pos_left _ h₁)
      have := (Nat.le_log2 hpos).2 this
      omega
    · omega

/-- **Multiplication**, for canonical finite or zero unpacked operands. -/
theorem mul_value {a b : UnpackedFloat} {x y : ℝ}
    (ha : uvalue a = some x) (hb : uvalue b = some y) (hca : Canon a) (hcb : Canon b) :
    ∃ v : ℝ, uvalue (UnpackedFloat.mul Format.binary64 a b) = some v ∧
      |v - x * y| ≤ (1 / 2 ^ 53) * |x * y| + 1 / 2 ^ 1075 ∧
      (|x * y| ≤ 2 ^ 1022 → v ∈ binary64) ∧
      (|x * y| ≤ 2 ^ 1022 → Canon (UnpackedFloat.mul Format.binary64 a b)) := by
  have hnn : (0 : ℝ) ≤ (1 / 2 ^ 53) * 0 + 1 / 2 ^ 1075 := by positivity
  cases a with
  | notANumber => simp [uvalue] at ha
  | infinity _ => simp [uvalue] at ha
  | zero s₁ =>
    simp only [uvalue, Option.some.injEq] at ha
    subst ha
    cases b with
    | notANumber => simp [uvalue] at hb
    | infinity _ => simp [uvalue] at hb
    | zero s₂ =>
      exact ⟨0, rfl, by simp, fun _ => zero_mem_binary64, fun _ => trivial⟩
    | finite s₂ m₂ e₂ h₂ =>
      exact ⟨0, rfl, by simp, fun _ => zero_mem_binary64, fun _ => trivial⟩
  | finite s₁ m₁ e₁ h₁ =>
    cases b with
    | notANumber => simp [uvalue] at hb
    | infinity _ => simp [uvalue] at hb
    | zero s₂ =>
      simp only [uvalue, Option.some.injEq] at hb
      subst hb
      exact ⟨0, rfl, by simp, fun _ => zero_mem_binary64, fun _ => trivial⟩
    | finite s₂ m₂ e₂ h₂ =>
      simp only [uvalue, Option.some.injEq] at ha hb
      rw [mul_finite_eq]
      have hm : 0 < m₁ * m₂ := Nat.mul_pos h₁ h₂
      obtain ⟨w, hw, herr, ⟨n, hn, hwn⟩, hcan⟩ :=
        roundAcc_value (s₁ * s₂) (m₁ * m₂) (e₁ + e₂) .exact
          (represents_exact (m₁ * m₂) (e₁ + e₂)) (mul_le_target hca hcb)
      set X : ℝ := ((m₁ * m₂ : Nat) : ℝ) * (2 : ℝ) ^ (e₁ + e₂) with hX
      have hX0 : 0 ≤ X := mul_nonneg (Nat.cast_nonneg _) (zpow_nonneg (by norm_num) _)
      have hxy : x * y = sgn (s₁ * s₂) * X := by
        rw [← ha, ← hb, bitValue_eq, bitValue_eq, sgn_mul, hX,
          zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0)]
        push_cast; ring
      have habs : |x * y| = X := by
        rw [hxy, abs_mul, abs_sgn, one_mul, abs_of_nonneg hX0]
      have herr' := halfUlp_le_model hm (target_eq' _ _) rfl herr (abs_of_nonneg hX0)
      refine ⟨sgn (s₁ * s₂) * w, hw, ?_, ?_, ?_⟩
      · rw [habs, hxy, ← mul_sub, abs_mul, abs_sgn, one_mul]
        rw [← hX, abs_of_nonneg hX0] at herr'
        exact herr'
      · intro hM
        rw [habs] at hM
        rw [hwn]
        exact sgn_mul_mem _ (mem_binary64_of n _ hn (target_ge _ _) (target_le_970 _ _ hm hM))
      · intro hM
        rw [habs] at hM
        exact hcan (target_le_970 _ _ hm hM)

/-! ## Division -/

theorem div_finite_eq (s₁ s₂ : Sign) (m₁ m₂ : Nat) (e₁ e₂ : Int) (h₁ : 0 < m₁) (h₂ : 0 < m₂) :
    UnpackedFloat.div Format.binary64 (.finite s₁ m₁ e₁ h₁) (.finite s₂ m₂ e₂ h₂) =
      roundWithAccuracy Format.binary64 (s₁ / s₂) (divCore Format.binary64 m₁ e₁ m₂ e₂).1
        (divCore Format.binary64 m₁ e₁ m₂ e₂).2.1 (divCore Format.binary64 m₁ e₁ m₂ e₂).2.2 := by
  rfl

/-- `accuracyOfFraction r d` records where `r / d` sits in `[0, 1)`. -/
theorem matches_fraction (r d : Nat) (hd : 0 < d) (hr : r < d) :
    Matches (accuracyOfFraction r d) ((r : ℝ) / d) := by
  have hd' : (0 : ℝ) < d := by exact_mod_cast hd
  have hr' : (r : ℝ) < d := by exact_mod_cast hr
  have h0 : 0 ≤ (r : ℝ) / d := by positivity
  have h1 : (r : ℝ) / d < 1 := (div_lt_one hd').2 hr'
  refine ⟨h0, h1, ?_⟩
  unfold accuracyOfFraction
  split
  · rename_i hz
    left
    exact ⟨rfl, by simp [hz]⟩
  · rename_i hz
    have hpos : (0 : ℝ) < r / d := by
      have : (0 : ℝ) < r := by exact_mod_cast Nat.pos_of_ne_zero hz
      positivity
    rcases hc : compare (2 * r) d
    · have h := Nat.compare_eq_lt.1 hc
      have h' : (2 * r : ℝ) < d := by exact_mod_cast h
      right; left
      refine ⟨rfl, hpos, ?_⟩
      rw [div_lt_iff₀ hd']; linarith
    · have h := Nat.compare_eq_eq.1 hc
      have h' : (2 * r : ℝ) = d := by exact_mod_cast h
      right; right; left
      refine ⟨rfl, ?_⟩
      rw [div_eq_iff hd'.ne']; linarith
    · have h := Nat.compare_eq_gt.1 hc
      have h' : (d : ℝ) < 2 * r := by exact_mod_cast h
      right; right; right
      refine ⟨rfl, ?_⟩
      rw [lt_div_iff₀ hd']; linarith

theorem represents_lower {em : ExtendedMantissa} {e : Int} {x : ℝ} (h : Represents em e x) :
    (em.mantissa : ℝ) * (2 : ℝ) ^ e ≤ x := by
  obtain ⟨δ, ⟨hδ, -⟩, rfl⟩ := h
  exact mul_le_mul_of_nonneg_right (by linarith) (zpow_nonneg (by norm_num) _)

/-- The quotient `divCore` produces represents `m₁ 2^e₁ / (m₂ 2^e₂)`, and its
exponent is at or below its target (for a binary64 dividend significand). -/
theorem divCore_props (m₁ m₂ : Nat) (e₁ e₂ : Int) (h₁ : 0 < m₁) (h₂ : 0 < m₂)
    (h53 : m₁ < 2 ^ 53) :
    Represents (ExtendedMantissa.ofMantissaAndAccuracy (divCore Format.binary64 m₁ e₁ m₂ e₂).1
        (divCore Format.binary64 m₁ e₁ m₂ e₂).2.2) (divCore Format.binary64 m₁ e₁ m₂ e₂).2.1
        ((m₁ : ℝ) * (2 : ℝ) ^ e₁ / ((m₂ : ℝ) * (2 : ℝ) ^ e₂)) ∧
      (divCore Format.binary64 m₁ e₁ m₂ e₂).2.1 ≤ Format.binary64.targetExponent
        (totalExponent (divCore Format.binary64 m₁ e₁ m₂ e₂).1
          (divCore Format.binary64 m₁ e₁ m₂ e₂).2.1) := by
  have htt : Format.binary64.targetExponent (totalExponent m₁ e₁ - totalExponent m₂ e₂) =
      max (((m₁.log2 : Int) + 1 + e₁) - ((m₂.log2 : Int) + 1 + e₂) - 53) (-1074) := by
    simp [totalExponent, Format.targetExponent, binary64_mantissaBits, binary64_minExponent]
  simp only [divCore]
  set t' := min (e₁ - e₂)
    (Format.binary64.targetExponent (totalExponent m₁ e₁ - totalExponent m₂ e₂)) with ht'
  set sh := (e₁ - e₂ - t').toNat with hsh
  have hsh' : (sh : Int) = e₁ - e₂ - t' := by
    rw [hsh, Int.toNat_of_nonneg (by rw [ht']; omega)]
  have hM : m₁ <<< sh = m₁ * 2 ^ sh := Nat.shiftLeft_eq _ _
  set M := m₁ <<< sh with hMdef
  refine ⟨?_, ?_⟩
  · refine ⟨(M % m₂ : Nat) / (m₂ : ℝ), ?_, ?_⟩
    · rw [ofMA_accuracy]
      exact matches_fraction _ _ h₂ (Nat.mod_lt _ h₂)
    · rw [ofMA_mantissa]
      have hm2 : (0 : ℝ) < m₂ := by exact_mod_cast h₂
      have hdm : ((M / m₂ : Nat) : ℝ) + ((M % m₂ : Nat) : ℝ) / m₂ = (M : ℝ) / m₂ := by
        have := Nat.div_add_mod M m₂
        field_simp
        have h' : ((m₂ * (M / m₂) + M % m₂ : Nat) : ℝ) = M := by rw [this]
        push_cast at h'
        linarith
      rw [hdm, hM]
      push_cast
      have he : e₁ - e₂ = (sh : Int) + t' := by omega
      have hpow : (2 : ℝ) ^ e₁ / (2 : ℝ) ^ e₂ = (2 : ℝ) ^ sh * (2 : ℝ) ^ t' := by
        rw [← zpow_sub₀ (by norm_num : (2 : ℝ) ≠ 0), he, zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0),
          zpow_natCast]
      have hz : (2 : ℝ) ^ e₂ ≠ 0 := zpow_ne_zero _ (by norm_num)
      calc (m₁ : ℝ) * 2 ^ e₁ / (m₂ * 2 ^ e₂) = (m₁ : ℝ) / m₂ * (2 ^ e₁ / 2 ^ e₂) := by
            field_simp
        _ = (m₁ : ℝ) * 2 ^ sh / m₂ * 2 ^ t' := by rw [hpow]; field_simp
  · rw [target_eq']
    by_cases hlo : t' ≤ -1074
    · exact le_trans hlo (le_max_right _ _)
    · have hL1 : m₁.log2 ≤ 52 := log2_lt_53 h₁ h53
      have ht'eq : t' = ((m₁.log2 : Int) + 1 + e₁) - ((m₂.log2 : Int) + 1 + e₂) - 53 := by
        rw [ht', htt] at hlo ⊢
        omega
      have hshv : sh = m₂.log2 + 53 - m₁.log2 := by omega
      have hq : 2 ^ 52 ≤ M / m₂ := by
        apply (Nat.le_div_iff_mul_le h₂).2
        have hm1 : 2 ^ m₁.log2 ≤ m₁ := Nat.log2_self_le (by omega)
        have hm2 : m₂ < 2 ^ (m₂.log2 + 1) := Nat.lt_log2_self
        rw [hM]
        calc 2 ^ 52 * m₂ ≤ 2 ^ 52 * 2 ^ (m₂.log2 + 1) := Nat.mul_le_mul_left _ hm2.le
          _ = 2 ^ m₁.log2 * 2 ^ sh := by rw [← Nat.pow_add, ← Nat.pow_add]; congr 1; omega
          _ ≤ m₁ * 2 ^ sh := Nat.mul_le_mul_right _ hm1
      have := (Nat.le_log2 (by omega)).2 hq
      omega

/-- **Division**, for a canonical dividend and a nonzero divisor. -/
theorem div_value {a b : UnpackedFloat} {x y : ℝ}
    (ha : uvalue a = some x) (hb : uvalue b = some y) (hca : Canon a) (hy : y ≠ 0) :
    ∃ v : ℝ, uvalue (UnpackedFloat.div Format.binary64 a b) = some v ∧
      |v - x / y| ≤ (1 / 2 ^ 53) * |x / y| + 1 / 2 ^ 1075 ∧
      (|x / y| ≤ 2 ^ 1022 → v ∈ binary64) ∧
      (|x / y| ≤ 2 ^ 1022 → Canon (UnpackedFloat.div Format.binary64 a b)) := by
  cases b with
  | notANumber => simp [uvalue] at hb
  | infinity _ => simp [uvalue] at hb
  | zero _ =>
    simp only [uvalue, Option.some.injEq] at hb
    exact absurd hb.symm hy
  | finite s₂ m₂ e₂ h₂ =>
  cases a with
  | notANumber => simp [uvalue] at ha
  | infinity _ => simp [uvalue] at ha
  | zero s₁ =>
    simp only [uvalue, Option.some.injEq] at ha
    subst ha
    exact ⟨0, rfl, by simp, fun _ => zero_mem_binary64, fun _ => trivial⟩
  | finite s₁ m₁ e₁ h₁ =>
    simp only [uvalue, Option.some.injEq] at ha hb
    rw [div_finite_eq]
    obtain ⟨h53, -, -, -⟩ := hca
    obtain ⟨hrep, hle⟩ := divCore_props m₁ m₂ e₁ e₂ h₁ h₂ h53
    set d := divCore Format.binary64 m₁ e₁ m₂ e₂
    obtain ⟨w, hw, herr, ⟨n, hn, hwn⟩, hcan⟩ := roundAcc_value (s₁ / s₂) d.1 d.2.1 d.2.2 hrep hle
    have hxlo := represents_lower hrep
    rw [ofMA_mantissa] at hxlo
    set X : ℝ := (m₁ : ℝ) * (2 : ℝ) ^ e₁ / ((m₂ : ℝ) * (2 : ℝ) ^ e₂) with hX
    have hX0 : 0 ≤ X := div_nonneg (mul_nonneg (Nat.cast_nonneg _) (zpow_nonneg (by norm_num) _))
      (mul_nonneg (Nat.cast_nonneg _) (zpow_nonneg (by norm_num) _))
    have hxy : x / y = sgn (s₁ / s₂) * X := by
      rw [← ha, ← hb, bitValue_eq, bitValue_eq, sgn_div, hX, mul_div_mul_comm]
      congr 1
      cases s₁ <;> cases s₂ <;> norm_num [sgn]
    have habs : |x / y| = X := by
      rw [hxy, abs_mul, abs_sgn, one_mul, abs_of_nonneg hX0]
    have ht970 : X ≤ 2 ^ 1022 → Format.binary64.targetExponent (totalExponent d.1 d.2.1) ≤ 970 := by
      intro hM
      by_cases hq : d.1 = 0
      · have := target_eq' d.1 d.2.1
        rw [hq, Nat.log2_zero] at this
        rw [hq] at hle ⊢
        rw [this] at hle ⊢
        omega
      · exact target_le_970' _ _ (Nat.pos_of_ne_zero hq) hxlo hM
    refine ⟨sgn (s₁ / s₂) * w, hw, ?_, ?_, ?_⟩
    · rw [habs, hxy, ← mul_sub, abs_mul, abs_sgn, one_mul]
      by_cases hq : d.1 = 0
      · have ht := target_eq' d.1 d.2.1
        rw [hq, Nat.log2_zero] at ht
        have hle' := hle
        rw [hq, ht] at hle'
        have htm : Format.binary64.targetExponent (totalExponent d.1 d.2.1) = -1074 := by
          rw [hq, ht]; omega
        rw [htm] at herr
        have hsub : (2 : ℝ) ^ (((-1074 : ℤ) - 1)) = 1 / 2 ^ 1075 := by norm_num
        rw [hsub] at herr
        linarith [mul_nonneg (show (0 : ℝ) ≤ 1 / 2 ^ 53 by norm_num) hX0]
      · have := halfUlp_le_model' (Nat.pos_of_ne_zero hq) (target_eq' _ _) hxlo herr
        rwa [abs_of_nonneg hX0] at this
    · intro hM
      rw [habs] at hM
      rw [hwn]
      exact sgn_mul_mem _ (mem_binary64_of n _ hn (target_ge _ _) (ht970 hM))
    · intro hM
      rw [habs] at hM
      exact hcan (ht970 hM)

/-! ## `Float.Model` and `Float` -/

theorem model_sub_value {a b : Float.Model} {x y : ℝ}
    (ha : modelValue a = some x) (hb : modelValue b = some y) (hM : |x - y| ≤ 2 ^ 1022) :
    ∃ v : ℝ, modelValue (a - b) = some v ∧ v ∈ binary64 ∧
      |v - (x - y)| ≤ (1 / 2 ^ 53) * |x - y| + 1 / 2 ^ 1075 := by
  obtain ⟨v, hv, herr, hmem, hcan⟩ := sub_value ha hb
  refine ⟨v, ?_, hmem (model_mem ha) (model_mem hb) hM, herr⟩
  show uvalue (Float.Model.pack (UnpackedFloat.sub Format.binary64 a.unpack b.unpack)).unpack = some v
  rw [model_unpack_pack _ (hcan (canon_model ha) (canon_model hb) hM)]
  exact hv

theorem model_mul_value {a b : Float.Model} {x y : ℝ}
    (ha : modelValue a = some x) (hb : modelValue b = some y) (hM : |x * y| ≤ 2 ^ 1022) :
    ∃ v : ℝ, modelValue (a * b) = some v ∧ v ∈ binary64 ∧
      |v - x * y| ≤ (1 / 2 ^ 53) * |x * y| + 1 / 2 ^ 1075 := by
  obtain ⟨v, hv, herr, hmem, hcan⟩ := mul_value ha hb (canon_model ha) (canon_model hb)
  refine ⟨v, ?_, hmem hM, herr⟩
  show uvalue (Float.Model.pack (UnpackedFloat.mul Format.binary64 a.unpack b.unpack)).unpack = some v
  rw [model_unpack_pack _ (hcan hM)]
  exact hv

theorem model_div_value {a b : Float.Model} {x y : ℝ}
    (ha : modelValue a = some x) (hb : modelValue b = some y) (hy : y ≠ 0)
    (hM : |x / y| ≤ 2 ^ 1022) :
    ∃ v : ℝ, modelValue (a / b) = some v ∧ v ∈ binary64 ∧
      |v - x / y| ≤ (1 / 2 ^ 53) * |x / y| + 1 / 2 ^ 1075 := by
  obtain ⟨v, hv, herr, hmem, hcan⟩ := div_value ha hb (canon_model ha) hy
  refine ⟨v, ?_, hmem hM, herr⟩
  show uvalue (Float.Model.pack (UnpackedFloat.div Format.binary64 a.unpack b.unpack)).unpack = some v
  rw [model_unpack_pack _ (hcan hM)]
  exact hv

/-- Lean's `Float` subtraction meets the `FPModel` contract of `binary64Model`. -/
theorem float_sub_value {a b : Float} {x y : ℝ}
    (ha : floatValue a = some x) (hb : floatValue b = some y) (hM : |x - y| ≤ 2 ^ 1022) :
    ∃ v : ℝ, floatValue (a - b) = some v ∧ v ∈ binary64 ∧
      |v - (x - y)| ≤ (1 / 2 ^ 53) * |x - y| + 1 / 2 ^ 1075 :=
  model_sub_value ha hb hM

/-- Lean's `Float` multiplication meets the `FPModel` contract of `binary64Model`. -/
theorem float_mul_value {a b : Float} {x y : ℝ}
    (ha : floatValue a = some x) (hb : floatValue b = some y) (hM : |x * y| ≤ 2 ^ 1022) :
    ∃ v : ℝ, floatValue (a * b) = some v ∧ v ∈ binary64 ∧
      |v - x * y| ≤ (1 / 2 ^ 53) * |x * y| + 1 / 2 ^ 1075 :=
  model_mul_value ha hb hM

/-- Lean's `Float` division meets the `FPModel` contract of `binary64Model`. -/
theorem float_div_value {a b : Float} {x y : ℝ}
    (ha : floatValue a = some x) (hb : floatValue b = some y) (hy : y ≠ 0)
    (hM : |x / y| ≤ 2 ^ 1022) :
    ∃ v : ℝ, floatValue (a / b) = some v ∧ v ∈ binary64 ∧
      |v - x / y| ≤ (1 / 2 ^ 53) * |x / y| + 1 / 2 ^ 1075 :=
  model_div_value ha hb hy hM

end LFortran

#print axioms LFortran.float_div_value
#print axioms LFortran.float_mul_value
#print axioms LFortran.float_sub_value
