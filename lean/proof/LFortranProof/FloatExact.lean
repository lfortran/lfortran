import LFortranProof.FloatOps
import LFortranProof.FloatVal

/-!
# Exact `Float` operations

Lean's `Float` `-` and `*` are exact when the exact result is a binary64
number (`float_sub_exact`, `float_mul_exact`). This is what makes
Cody–Waite range reduction work: `N * p1` is exact when `p1` has few
significant bits, and `x - N * p1` is exact by Sterbenz's lemma
(`sterbenz`) when the two operands are within a factor of two.

The proof uses the half-ulp bound of the rounding step: the result is a
multiple of `2^t` within `2^(t-1)` of the exact value, and a binary64 number
of that magnitude is itself a multiple of `2^t`.
-/

open Float.Model
open Float.Model.UnpackedFloat hiding round add ofInt sub mul div

namespace LFortran

set_option exponentiation.threshold 2048

/-- A binary64 number `m · 2^e` is a multiple of `2^t` for its target
exponent `t`, the exponent of its last significand bit. -/
theorem multiple_of_target {m : Nat} {e : Int} (hm : 0 < m)
    (hX : (m : ℝ) * (2 : ℝ) ^ e ∈ binary64) :
    ∃ K : ℤ, (m : ℝ) * (2 : ℝ) ^ e =
      K * (2 : ℝ) ^ (max ((m.log2 : Int) + 1 + e - 53) (-1074)) := by
  obtain ⟨M, E, hM, hE1, -, hXE⟩ := hX
  set t := max ((m.log2 : Int) + 1 + e - 53) (-1074) with ht
  have hpow : (2 : ℝ) ^ (m.log2 : ℤ) ≤ m := by exact_mod_cast Nat.log2_self_le (by omega)
  have hpos : (0 : ℝ) < (m : ℝ) * (2 : ℝ) ^ e :=
    mul_pos (by exact_mod_cast hm) (zpow_pos (by norm_num) _)
  -- 2^(log2 m + e) ≤ X = |M| 2^E < 2^(53 + E)
  have hlow : (2 : ℝ) ^ ((m.log2 : ℤ) + e) ≤ (m : ℝ) * (2 : ℝ) ^ e := by
    rw [zpow_add₀ (by norm_num)]
    exact mul_le_mul_of_nonneg_right hpow (zpow_nonneg (by norm_num) _)
  have hhigh : (m : ℝ) * (2 : ℝ) ^ e < (2 : ℝ) ^ (53 + E) := by
    rw [hXE, zpow_add₀ (by norm_num)]
    have hM' : (M : ℝ) < 2 ^ 53 := by
      have : (M : ℝ) ≤ |(M : ℝ)| := le_abs_self _
      have h2 : |(M : ℝ)| < 2 ^ 53 := by exact_mod_cast hM
      linarith
    have := mul_lt_mul_of_pos_right hM' (zpow_pos (by norm_num : (0 : ℝ) < 2) E)
    simpa using this
  have hexp : (m.log2 : ℤ) + e < 53 + E := by
    have := lt_of_le_of_lt hlow hhigh
    exact (zpow_lt_zpow_iff_right₀ (by norm_num : (1 : ℝ) < 2)).1 this
  have hEt : t ≤ E := by rw [ht]; omega
  refine ⟨M * 2 ^ (E - t).toNat, ?_⟩
  rw [hXE]
  push_cast
  rw [mul_assoc, ← zpow_natCast, ← zpow_add₀ (by norm_num), Int.toNat_of_nonneg (by omega)]
  congr 2
  ring

/-- Two multiples of `2^t` at most `2^(t-1)` apart are equal. -/
theorem eq_of_halfUlp {w X : ℝ} {t : ℤ} {n K : ℤ} (hw : w = n * (2 : ℝ) ^ t)
    (hX : X = K * (2 : ℝ) ^ t) (h : |w - X| ≤ (2 : ℝ) ^ (t - 1)) : w = X := by
  have h2 : (0 : ℝ) < (2 : ℝ) ^ t := zpow_pos (by norm_num) t
  have h' : |((n - K : ℤ) : ℝ)| * (2 : ℝ) ^ t ≤ (2 : ℝ) ^ t / 2 := by
    rw [hw, hX, ← sub_mul, abs_mul, abs_of_pos h2] at h
    rw [zpow_sub₀ (by norm_num), zpow_one] at h
    push_cast; exact h
  have hnk : |((n - K : ℤ) : ℝ)| ≤ 1 / 2 := by
    have := (le_div_iff₀ (by norm_num : (0 : ℝ) < 2)).1 h'
    nlinarith
  have hz : n - K = 0 := by
    have h4 : ((|n - K| : ℤ) : ℝ) < 1 := by rw [Int.cast_abs]; linarith
    have h5 : |n - K| < 1 := by exact_mod_cast h4
    have := abs_lt.1 h5
    omega
  rw [hw, hX, show n = K by omega]

/-- `round` is exact on a binary64 number. -/
theorem round_value_exact (s : Sign) (m : Nat) (e : Int) (hm : 0 < m)
    (hX : (m : ℝ) * (2 : ℝ) ^ e ∈ binary64) :
    uvalue (UnpackedFloat.round Format.binary64 s m e) = some (bitValue s m e) := by
  obtain ⟨hpos, hle, htgt, hval⟩ := decrease_props m e hm
  rw [round_eq]
  generalize decreaseExponent m e (Format.binary64.targetExponent (totalExponent m e)) = d
    at hpos hle htgt hval ⊢
  obtain ⟨w, hw, herr, ⟨n, -, hwn⟩, -⟩ := roundWithAccuracy_value s d.1 d.2 hpos (htgt ▸ hle)
  rw [htgt, hval] at herr
  rw [htgt] at hwn
  obtain ⟨K, hK⟩ := multiple_of_target hm hX
  rw [← target_eq m e hm] at hK
  have := eq_of_halfUlp (n := (n : ℤ)) (by rw [hwn]; push_cast; rfl) hK herr
  rw [hw, this, bitValue_eq]

theorem bin64_neg_iff {x : ℝ} : -x ∈ binary64 ↔ x ∈ binary64 :=
  ⟨fun h => by simpa using neg_mem_binary64 h, neg_mem_binary64⟩

/-- `normalize` is exact on a binary64 number. -/
theorem normalize_value_exact (M e : Int) (zs : Sign) (hX : (M : ℝ) * (2 : ℝ) ^ e ∈ binary64) :
    uvalue (normalize Format.binary64 M e zs) = some ((M : ℝ) * (2 : ℝ) ^ e) := by
  unfold normalize
  rcases lt_trichotomy M 0 with h | h | h
  · have hc : compare M 0 = .lt := by simp [compare, compareOfLessAndEq, h]
    simp only [hc]
    have hb : bitValue .negative (-M).toNat e = (M : ℝ) * (2 : ℝ) ^ e := by
      have : (((-M).toNat : Int) : ℝ) = -(M : ℝ) := by
        rw [Int.toNat_of_nonneg (by omega)]; push_cast; ring
      simp only [bitValue, Sign.apply]
      push_cast
      rw [show ((-M).toNat : ℝ) = (((-M).toNat : Int) : ℝ) by push_cast; rfl, this]
      ring
    have hmem : (((-M).toNat : Nat) : ℝ) * (2 : ℝ) ^ e ∈ binary64 := by
      have : (((-M).toNat : Nat) : ℝ) * (2 : ℝ) ^ e = -((M : ℝ) * (2 : ℝ) ^ e) := by
        rw [show (((-M).toNat : Nat) : ℝ) = (((-M).toNat : Int) : ℝ) by push_cast; rfl,
          Int.toNat_of_nonneg (by omega)]
        push_cast; ring
      rw [this]; exact neg_mem_binary64 hX
    rw [round_value_exact .negative (-M).toNat e (by omega) hmem, hb]
  · subst h
    have hc : compare (0 : Int) 0 = .eq := by simp [compare, compareOfLessAndEq]
    simp only [hc]
    simp [uvalue]
  · have hc : compare M 0 = .gt := by
      simp [compare, compareOfLessAndEq, show ¬ M < 0 by omega, show ¬ M = 0 by omega]
    simp only [hc]
    have hb : bitValue .positive M.toNat e = (M : ℝ) * (2 : ℝ) ^ e := by
      simp only [bitValue, Sign.apply]
      rw [Int.toNat_of_nonneg (by omega)]
    have hmem : ((M.toNat : Nat) : ℝ) * (2 : ℝ) ^ e ∈ binary64 := by
      rw [show ((M.toNat : Nat) : ℝ) = ((M.toNat : Int) : ℝ) by push_cast; rfl,
        Int.toNat_of_nonneg (by omega)]
      exact hX
    rw [round_value_exact .positive M.toNat e (by omega) hmem, hb]

/-- Subtraction is exact when the exact difference is a binary64 number. -/
theorem sub_exact {a b : UnpackedFloat} {x y : ℝ} (ha : uvalue a = some x)
    (hb : uvalue b = some y) (hX : x - y ∈ binary64) :
    uvalue (UnpackedFloat.sub Format.binary64 a b) = some (x - y) := by
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
      simp only [UnpackedFloat.sub, sub_self]
      split <;> rfl
    | finite s₂ m₂ e₂ h₂ =>
      simp only [uvalue, Option.some.injEq] at hb
      simp only [UnpackedFloat.sub, uvalue, bitValue_neg, hb, zero_sub]
  | finite s₁ m₁ e₁ h₁ =>
    cases b with
    | notANumber => simp [uvalue] at hb
    | infinity _ => simp [uvalue] at hb
    | zero s₂ =>
      simp only [uvalue, Option.some.injEq] at hb
      subst hb
      simpa [UnpackedFloat.sub] using ha
    | finite s₂ m₂ e₂ h₂ =>
      simp only [uvalue, Option.some.injEq] at ha hb
      rw [sub_finite_eq]
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
      rw [normalize_value_exact _ _ _ (by rw [hsum]; exact hX), hsum]

/-- Multiplication is exact when the exact product is a binary64 number. -/
theorem mul_exact {a b : UnpackedFloat} {x y : ℝ} (ha : uvalue a = some x)
    (hb : uvalue b = some y) (hca : Canon a) (hcb : Canon b) (hX : x * y ∈ binary64) :
    uvalue (UnpackedFloat.mul Format.binary64 a b) = some (x * y) := by
  cases a with
  | notANumber => simp [uvalue] at ha
  | infinity _ => simp [uvalue] at ha
  | zero s₁ =>
    simp only [uvalue, Option.some.injEq] at ha
    subst ha
    cases b with
    | notANumber => simp [uvalue] at hb
    | infinity _ => simp [uvalue] at hb
    | zero s₂ => simp [UnpackedFloat.mul, uvalue]
    | finite s₂ m₂ e₂ h₂ => simp [UnpackedFloat.mul, uvalue]
  | finite s₁ m₁ e₁ h₁ =>
    cases b with
    | notANumber => simp [uvalue] at hb
    | infinity _ => simp [uvalue] at hb
    | zero s₂ =>
      simp only [uvalue, Option.some.injEq] at hb
      subst hb
      simp [UnpackedFloat.mul, uvalue]
    | finite s₂ m₂ e₂ h₂ =>
      simp only [uvalue, Option.some.injEq] at ha hb
      rw [mul_finite_eq]
      have hm : 0 < m₁ * m₂ := Nat.mul_pos h₁ h₂
      obtain ⟨w, hw, herr, ⟨n, -, hwn⟩, -⟩ :=
        roundAcc_value (s₁ * s₂) (m₁ * m₂) (e₁ + e₂) .exact
          (represents_exact (m₁ * m₂) (e₁ + e₂)) (mul_le_target hca hcb)
      set X : ℝ := ((m₁ * m₂ : Nat) : ℝ) * (2 : ℝ) ^ (e₁ + e₂) with hXdef
      have hX0 : 0 ≤ X := mul_nonneg (Nat.cast_nonneg _) (zpow_nonneg (by norm_num) _)
      have hxy : x * y = sgn (s₁ * s₂) * X := by
        rw [← ha, ← hb, bitValue_eq, bitValue_eq, sgn_mul, hXdef,
          zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0)]
        push_cast; ring
      have hXmem : X ∈ binary64 := by
        have h1 : X = sgn (s₁ * s₂) * (x * y) := by
          rw [hxy, ← mul_assoc]
          cases s₁ * s₂ <;> simp [sgn]
        rw [h1]
        cases s₁ * s₂
        · simpa [sgn] using neg_mem_binary64 hX
        · simpa [sgn] using hX
      obtain ⟨K, hK⟩ := multiple_of_target hm hXmem
      rw [← target_eq' (m₁ * m₂) (e₁ + e₂)] at hK
      have hwX := eq_of_halfUlp (n := (n : ℤ)) (by rw [hwn]; push_cast; rfl) hK herr
      rw [hw, hwX, hxy]

theorem model_sub_exact {a b : Float.Model} {x y : ℝ}
    (ha : modelValue a = some x) (hb : modelValue b = some y) (hX : x - y ∈ binary64)
    (hM : |x - y| ≤ 2 ^ 1022) : modelValue (a - b) = some (x - y) := by
  obtain ⟨v, hv, herr, hmem, hcan⟩ := sub_value ha hb
  show uvalue (Float.Model.pack (UnpackedFloat.sub Format.binary64 a.unpack b.unpack)).unpack = _
  rw [model_unpack_pack _ (hcan (canon_model ha) (canon_model hb) hM)]
  exact sub_exact ha hb hX

theorem model_mul_exact {a b : Float.Model} {x y : ℝ}
    (ha : modelValue a = some x) (hb : modelValue b = some y) (hX : x * y ∈ binary64)
    (hM : |x * y| ≤ 2 ^ 1022) : modelValue (a * b) = some (x * y) := by
  obtain ⟨v, hv, herr, hmem, hcan⟩ := mul_value ha hb (canon_model ha) (canon_model hb)
  show uvalue (Float.Model.pack (UnpackedFloat.mul Format.binary64 a.unpack b.unpack)).unpack = _
  rw [model_unpack_pack _ (hcan hM)]
  exact mul_exact ha hb (canon_model ha) (canon_model hb) hX

/-- `Float` subtraction is exact when the exact difference is a binary64 number. -/
theorem float_sub_exact {f g : Float} {x y : ℝ} (hf : FVal f x) (hg : FVal g y)
    (hX : x - y ∈ binary64) (hM : |x - y| ≤ 2 ^ 1022) : FVal (f - g) (x - y) :=
  model_sub_exact hf hg hX hM

/-- `Float` multiplication is exact when the exact product is a binary64 number. -/
theorem float_mul_exact {f g : Float} {x y : ℝ} (hf : FVal f x) (hg : FVal g y)
    (hX : x * y ∈ binary64) (hM : |x * y| ≤ 2 ^ 1022) : FVal (f * g) (x * y) :=
  model_mul_exact hf hg hX hM

/-! ## Representable results -/

/-- **Sterbenz's lemma**: the difference of two binary64 numbers within a factor
of two of each other is a binary64 number. -/
theorem sterbenz {x y : ℝ} (hx : x ∈ binary64) (hy : y ∈ binary64) (h1 : y ≤ 2 * x)
    (h2 : x ≤ 2 * y) : x - y ∈ binary64 := by
  obtain ⟨mx, ex, hmx, hex1, hex2, rfl⟩ := hx
  obtain ⟨my, ey, hmy, hey1, hey2, rfl⟩ := hy
  have hp : ∀ k : ℤ, (0 : ℝ) < (2 : ℝ) ^ k := fun k => zpow_pos (by norm_num) k
  -- both are nonnegative
  have hx0 : (0 : ℝ) ≤ mx * (2 : ℝ) ^ ex := by linarith
  have hy0 : (0 : ℝ) ≤ my * (2 : ℝ) ^ ey := by linarith
  rcases le_total ex ey with hle | hle
  · -- the difference is a multiple of 2^ex, at most x
    set D : ℤ := mx - my * 2 ^ (ey - ex).toNat
    have hD : (mx : ℝ) * (2 : ℝ) ^ ex - my * (2 : ℝ) ^ ey = D * (2 : ℝ) ^ ex := by
      simp only [D]; push_cast
      rw [sub_mul, mul_assoc, ← zpow_natCast, ← zpow_add₀ (by norm_num),
        Int.toNat_of_nonneg (by omega)]
      ring_nf
    refine ⟨D, ex, ?_, hex1, hex2, hD⟩
    have hxy : |(mx : ℝ) * (2 : ℝ) ^ ex - my * (2 : ℝ) ^ ey| ≤ mx * (2 : ℝ) ^ ex := by
      rw [abs_le]; constructor <;> linarith
    rw [hD, abs_mul, abs_of_pos (hp ex)] at hxy
    have h3 : (mx : ℝ) * (2 : ℝ) ^ ex ≤ |(mx : ℝ)| * (2 : ℝ) ^ ex :=
      mul_le_mul_of_nonneg_right (le_abs_self _) (hp ex).le
    have hb' : |(D : ℝ)| ≤ |(mx : ℝ)| := le_of_mul_le_mul_right (hxy.trans h3) (hp ex)
    have : |(mx : ℝ)| < 2 ^ 53 := by exact_mod_cast hmx
    exact_mod_cast lt_of_le_of_lt hb' this
  · set D : ℤ := mx * 2 ^ (ex - ey).toNat - my
    have hD : (mx : ℝ) * (2 : ℝ) ^ ex - my * (2 : ℝ) ^ ey = D * (2 : ℝ) ^ ey := by
      simp only [D]; push_cast
      rw [sub_mul, mul_assoc, ← zpow_natCast, ← zpow_add₀ (by norm_num),
        Int.toNat_of_nonneg (by omega)]
      ring_nf
    refine ⟨D, ey, ?_, hey1, hey2, hD⟩
    have hxy : |(mx : ℝ) * (2 : ℝ) ^ ex - my * (2 : ℝ) ^ ey| ≤ my * (2 : ℝ) ^ ey := by
      rw [abs_le]; constructor <;> linarith
    rw [hD, abs_mul, abs_of_pos (hp ey)] at hxy
    have h3 : (my : ℝ) * (2 : ℝ) ^ ey ≤ |(my : ℝ)| * (2 : ℝ) ^ ey :=
      mul_le_mul_of_nonneg_right (le_abs_self _) (hp ey).le
    have hb' : |(D : ℝ)| ≤ |(my : ℝ)| := le_of_mul_le_mul_right (hxy.trans h3) (hp ey)
    have : |(my : ℝ)| < 2 ^ 53 := by exact_mod_cast hmy
    exact_mod_cast lt_of_le_of_lt hb' this

/-- An integer times `m · 2^e` is a binary64 number while the product of the
integers stays below `2^53`. -/
theorem int_mul_mem {N m e : ℤ} (h : |N * m| < 2 ^ 53) (he1 : -1074 ≤ e) (he2 : e ≤ 971) :
    (N : ℝ) * ((m : ℝ) * (2 : ℝ) ^ e) ∈ binary64 :=
  ⟨N * m, e, h, he1, he2, by push_cast; ring⟩

end LFortran
