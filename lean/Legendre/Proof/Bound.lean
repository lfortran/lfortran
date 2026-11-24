import Mathlib.Tactic.Linarith
import Mathlib.Tactic.Positivity
import Mathlib.Tactic.FieldSimp
import Mathlib.Tactic.GCongr
import Mathlib.Tactic.Ring
import Mathlib.Tactic.NormNum
import Mathlib.Tactic.LinearCombination
import Legendre.Proof.Bonnet

/-!
# `|P n x| ≤ 1` for `|x| ≤ 1`

The quadratic form `T x p q = p^2 + q^2 - 2 x p q` does not increase along
the (homogeneous) Bonnet recurrence when `x^2 ≤ 1`. This is used both for the
exact polynomials and for the propagated rounding errors.
-/

open LFortran Legendre

namespace LegendreProof

/-- The quadratic form controlling the recurrence. -/
def T (x p q : ℝ) : ℝ := p ^ 2 + q ^ 2 - 2 * x * p * q

lemma T_eq (x z p : ℝ) : T x z p = (z - x * p) ^ 2 + (1 - x ^ 2) * p ^ 2 := by
  unfold T; ring

lemma T_eq' (x p q : ℝ) : T x p q = (q - x * p) ^ 2 + (1 - x ^ 2) * p ^ 2 := by
  unfold T; ring

/-- The homogeneous Bonnet recurrence with `k = m + 2`. -/
noncomputable def W (x p q : ℝ) (m : ℕ) : ℝ := ((2 * m + 3) * x * p - (m + 1) * q) / (m + 2)

lemma W_sub (x p q : ℝ) (m : ℕ) :
    W x p q m - x * p = ((m + 1) / (m + 2)) * (x * p - q) := by
  unfold W
  field_simp
  ring

lemma T_W_le (x p q : ℝ) (m : ℕ) : T x (W x p q m) p ≤ T x p q := by
  rw [T_eq, W_sub, T_eq' x p q, mul_pow]
  have hc : (((m : ℝ) + 1) / (m + 2)) ^ 2 ≤ 1 := by
    have h0 : 0 ≤ ((m : ℝ) + 1) / (m + 2) := by positivity
    have h1 : ((m : ℝ) + 1) / (m + 2) ≤ 1 := by
      rw [div_le_one (by positivity)]; linarith
    exact pow_le_one₀ h0 h1
  have := mul_le_of_le_one_left (sq_nonneg (x * p - q)) hc
  have : (x * p - q) ^ 2 = (q - x * p) ^ 2 := by ring
  linarith

lemma P_succ_succ (n : ℕ) (x : ℝ) : P (n + 2) x = W x (P (n + 1) x) (P n x) n := rfl

lemma T_P (x : ℝ) (hx : x ^ 2 ≤ 1) : ∀ n : ℕ, T x (P (n + 1) x) (P n x) ≤ 1 - x ^ 2
  | 0 => by
    show T x x 1 ≤ 1 - x ^ 2
    unfold T; nlinarith
  | n + 1 => by
    rw [P_succ_succ]
    exact (T_W_le x _ _ n).trans (T_P x hx n)

lemma P_pow (x : ℝ) (hx : x ^ 2 = 1) : ∀ n : ℕ, P n x = x ^ n
  | 0 => by simp [P]
  | 1 => by simp [P]
  | n + 2 => by
    rw [P_succ_succ, P_pow x hx (n + 1), P_pow x hx n, W, div_eq_iff (by positivity)]
    linear_combination ((n : ℝ) + 1) * x ^ n * hx

lemma P_abs_le (x : ℝ) (hx : |x| ≤ 1) (n : ℕ) : |P n x| ≤ 1 := by
  have hx2 : x ^ 2 ≤ 1 := (sq_le_one_iff_abs_le_one x).mpr hx
  cases n with
  | zero => simp [P]
  | succ k =>
    rcases hx2.lt_or_eq with hlt | heq
    · have hT := T_P x hx2 k
      rw [T_eq'] at hT
      have hpos : 0 < 1 - x ^ 2 := by linarith
      have h1 : (1 - x ^ 2) * P (k + 1) x ^ 2 ≤ (1 - x ^ 2) * 1 := by
        nlinarith [sq_nonneg (P k x - x * P (k + 1) x)]
      have h2 : P (k + 1) x ^ 2 ≤ 1 ^ 2 := by
        rw [one_pow]; exact le_of_mul_le_mul_left h1 hpos
      exact abs_le_of_sq_le_sq h2 zero_le_one
    · rw [P_pow x heq, abs_pow]
      exact pow_le_one₀ (abs_nonneg x) hx

end LegendreProof
