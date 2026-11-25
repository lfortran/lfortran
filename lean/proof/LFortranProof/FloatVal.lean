import LFortranProof.FloatModel
import LFortran.Runtime
import LFortranProof.Arith.Model

/-!
# `Float` operations on values

`floatValue f` is the real number a `Float` denotes (`none` for infinities
and NaN), read from Lean's logical `Float.Model`; `FVal f x` says `f` is
finite with value `x`. This file relates the `Float` operations that
generated code uses, other than `+ - * /` (`LFortran/FloatOps.lean`), to
values: negation, `abs`, the sign bit (`floatSignbit_eq`), `Float.ofInt`
(`float_ofInt_value`) and truncation, Fortran `int` (`floatTrunc_eq`).
They make `Float` a `VModel` (`LFortran/FloatVModel.lean`).
-/

open Float.Model Float.Model.UnpackedFloat

set_option exponentiation.threshold 2048

namespace LFortran

/-- `f` is a finite `Float` with real value `x`. -/
abbrev FVal (f : Float) (x : ℝ) : Prop := floatValue f = some x

/-! ## Sign operations -/

theorem canon_neg {u : UnpackedFloat} (h : Canon u) : Canon u.neg := by
  cases u <;> simp_all [Canon, UnpackedFloat.neg]

theorem canon_abs {u : UnpackedFloat} (h : Canon u) : Canon u.abs := by
  cases u <;> simp_all [Canon, UnpackedFloat.abs]

theorem float_neg_value {f : Float} {x : ℝ} (h : FVal f x) : FVal (-f) (-x) := by
  have hc := canon_model h
  show modelValue (Float.Model.pack f.toModel.unpack.neg) = some (-x)
  unfold modelValue
  rw [model_unpack_pack _ (canon_neg hc)]
  unfold FVal floatValue modelValue at h
  revert h
  cases f.toModel.unpack with
  | finite s m e hm =>
    intro h
    simp only [uvalue, UnpackedFloat.neg, Option.some.injEq] at h ⊢
    rw [bitValue_neg, h]
  | zero s => intro h; simp only [uvalue, UnpackedFloat.neg, Option.some.injEq] at h ⊢; rw [← h]; simp
  | infinity s => intro h; simp [uvalue] at h
  | notANumber => intro h; simp [uvalue] at h

theorem float_abs_value {f : Float} {x : ℝ} (h : FVal f x) : FVal (Float.abs f) |x| := by
  have hc := canon_model h
  show modelValue (Float.Model.pack f.toModel.unpack.abs) = some |x|
  unfold modelValue
  rw [model_unpack_pack _ (canon_abs hc)]
  unfold FVal floatValue modelValue at h
  revert h
  cases f.toModel.unpack with
  | finite s m e hm =>
    intro h
    simp only [uvalue, UnpackedFloat.abs, Option.some.injEq] at h ⊢
    have h0 : (0 : ℝ) ≤ m * 2 ^ e := mul_nonneg (by positivity) (zpow_nonneg (by norm_num) _)
    rw [← h, bitValue_eq, bitValue_eq, abs_mul, abs_sgn, one_mul, abs_of_nonneg h0]
    simp [sgn]
  | zero s => intro h; simp only [uvalue, UnpackedFloat.abs, Option.some.injEq] at h ⊢; rw [← h]; simp
  | infinity s => intro h; simp [uvalue] at h
  | notANumber => intro h; simp [uvalue] at h

theorem bitValue_pos (m : Nat) (e : Int) (hm : 0 < m) : 0 < bitValue .positive m e := by
  have : (0 : ℝ) < m * 2 ^ e := mul_pos (by exact_mod_cast hm) (zpow_pos (by norm_num) _)
  rw [bitValue_eq]; simp only [sgn]; linarith

theorem bitValue_neg_lt (m : Nat) (e : Int) (hm : 0 < m) : bitValue .negative m e < 0 := by
  have : (0 : ℝ) < m * 2 ^ e := mul_pos (by exact_mod_cast hm) (zpow_pos (by norm_num) _)
  rw [bitValue_eq]; simp only [sgn]; linarith

/-- For a nonzero finite `Float`, the sign bit says whether the value is negative. -/
theorem floatSignbit_eq {f : Float} {x : ℝ} (h : FVal f x) (hx : x ≠ 0) :
    floatSignbit f = decide (x < 0) := by
  unfold floatSignbit
  unfold FVal floatValue modelValue at h
  revert h
  cases f.toModel.unpack with
  | finite s m e hm =>
    intro h
    simp only [uvalue, Option.some.injEq] at h
    cases s with
    | negative =>
      have := bitValue_neg_lt m e hm
      simp [unpackedSignbit, ← h, this]
    | positive =>
      have := bitValue_pos m e hm
      simp [unpackedSignbit, ← h, not_lt.2 this.le]
  | zero s => intro h; simp [uvalue] at h; exact absurd h.symm hx
  | infinity s => intro h; simp [uvalue] at h
  | notANumber => intro h; simp [uvalue] at h

/-- A `Float` with the sign bit set has a value `≤ 0`. -/
theorem floatSignbit_nonpos {f : Float} {x : ℝ} (h : FVal f x) (hs : floatSignbit f = true) :
    x ≤ 0 := by
  by_contra hx
  have := floatSignbit_eq h (ne_of_gt (lt_of_not_ge hx))
  rw [hs] at this
  simp at this; linarith

/-! ## Truncation -/

theorem roundToInt_positive (m : Nat) (e : Int) :
    (roundToInt .positive m e : ℤ) = ⌊(m : ℝ) * (2 : ℝ) ^ e⌋ := by
  unfold roundToInt
  simp only [decreaseExponent, shiftToExponent, Sign.apply, shiftRight_mantissa, ofMA_mantissa]
  rcases le_or_gt 0 e with he | he
  · obtain ⟨k, rfl⟩ := Int.eq_ofNat_of_zero_le he
    simp only [sub_zero, Int.toNat_natCast, sub_self, Int.toNat_zero, pow_zero, Nat.div_one,
      Nat.shiftLeft_eq, zpow_natCast]
    rw [show ((m : ℝ) * 2 ^ k) = ((m * 2 ^ k : ℕ) : ℝ) by push_cast; ring, Int.floor_natCast]
  · obtain ⟨k, hk⟩ : ∃ k : ℕ, e = -((k : ℤ) + 1) := ⟨(-e - 1).toNat, by omega⟩
    subst hk
    have h1 : (-((k : ℤ) + 1) - 0).toNat = 0 := by omega
    have h2 : (0 - (-((k : ℤ) + 1) - 0)).toNat = k + 1 := by omega
    rw [h1, Nat.shiftLeft_zero, Nat.cast_zero, h2]
    rw [zpow_neg, show ((k : ℤ) + 1) = ((k + 1 : ℕ) : ℤ) by push_cast; ring, zpow_natCast,
      ← div_eq_mul_inv]
    rw [show ((2 : ℝ) ^ (k + 1)) = ((2 ^ (k + 1) : ℕ) : ℝ) by push_cast; ring]
    rw [Int.floor_div_natCast, show ((m : ℝ)) = ((m : ℤ) : ℝ) by push_cast; rfl, Int.floor_intCast]
    push_cast; rfl

theorem float_toUInt64 {f : Float} {x : ℝ} (h : FVal f x) (h0 : 0 ≤ x) (h63 : x < 2 ^ 63) :
    ((f.toUInt64.toNat : ℕ) : ℤ) = ⌊x⌋ := by
  have key : (f.toModel.unpack.toInt 0 (UInt64.size - 1) : ℤ) = ⌊x⌋ := by
    unfold FVal floatValue modelValue at h
    revert h
    cases f.toModel.unpack with
    | finite s m e hm =>
      intro h
      simp only [uvalue, Option.some.injEq] at h
      cases s with
      | negative => have := bitValue_neg_lt m e hm; linarith
      | positive =>
        simp only [UnpackedFloat.toInt]
        rw [roundToInt_positive, ← h, bitValue_eq]; simp [sgn]
    | zero s => intro h; simp [uvalue] at h; simp [UnpackedFloat.toInt, ← h]
    | infinity s => intro h; simp [uvalue] at h
    | notANumber => intro h; simp [uvalue] at h
  have hf0 : 0 ≤ ⌊x⌋ := Int.floor_nonneg.2 h0
  have hf1 : ⌊x⌋ < 2 ^ 63 := by
    have := Int.floor_le x
    have : (⌊x⌋ : ℝ) < 2 ^ 63 := by linarith
    exact_mod_cast this
  show ((UInt64.ofNatClamp (f.toModel.unpack.toInt 0 (UInt64.size - 1)).toNat).toNat : ℤ) = ⌊x⌋
  rw [key]
  have hlt : (⌊x⌋).toNat < UInt64.size := by
    have : (⌊x⌋).toNat < 2 ^ 63 := by omega
    simp [UInt64.size]; omega
  simp [UInt64.ofNatClamp, hlt]
  omega

/-- `floatTrunc` (Fortran `int`) is truncation of the real value. -/
theorem floatTrunc_eq {f : Float} {x : ℝ} (h : FVal f x) (h63 : |x| < 2 ^ 63) :
    floatTrunc f = realTrunc x := by
  have hx := abs_lt.1 h63
  unfold floatTrunc
  split_ifs with hs
  · have hx0 := floatSignbit_nonpos h hs
    rw [float_toUInt64 (float_neg_value h) (by linarith) (by linarith)]
    rcases hx0.lt_or_eq with hlt | heq
    · rw [realTrunc_of_neg hlt, Int.floor_neg, neg_neg]
    · subst heq; simp [realTrunc]
  · have hx0 : 0 ≤ x := by
      by_contra hneg
      push Not at hneg
      have := floatSignbit_eq h hneg.ne
      simp [hneg] at this
      exact hs this
    rw [float_toUInt64 h hx0 hx.2, realTrunc_of_nonneg hx0]

/-! ## Integer conversion -/

theorem float_ofInt_zero : FVal (Float.ofInt 0) 0 := by
  unfold FVal floatValue modelValue
  rw [show (Float.ofInt 0).toModel.unpack = .zero .positive from rfl]
  rfl

/-- `Float.ofInt` is exact below `2^53`. -/
theorem float_ofInt_value (n : ℤ) (hn : |n| < 2 ^ 53) : FVal (Float.ofInt n) n := by
  rcases n with k | k
  · rcases k with _ | k
    · simpa using float_ofInt_zero
    · have hk : ((k : ℤ) + 1) < 2 ^ 53 := by
        simp only [Int.ofNat_eq_natCast, abs_lt] at hn; push_cast at hn; omega
      have := floatValue_ofInt (k + 1) (by omega) (by omega)
      simpa using this
  · have hk' : ((k : ℤ) + 1) < 2 ^ 53 := by
      simp only [Int.negSucc_eq, abs_lt] at hn; omega
    have hk := floatValue_ofInt (k + 1) (by omega) (by omega)
    have := float_neg_value hk
    show FVal (-Float.ofNat (k + 1)) _
    rw [Int.cast_negSucc]
    exact this

end LFortran
