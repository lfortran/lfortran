import Mathlib.Analysis.SpecialFunctions.Trigonometric.Series
import Mathlib.Analysis.SpecificLimits.Normed
import LFortran.PolyBound

/-!
# Taylor polynomials of `Real.sin` with a rigorous remainder

`sin_taylor_poly`: for `|r| ≤ 2`,
`|sin r - r · T_n(r²)| ≤ |r|^(2n+1) / (2n+1)!`, where
`T_n(z) = Σ_{i<n} (-1)^i z^i / (2i+1)!` (`sinT n`, rational coefficients, so it
can be fed to the `PolyBound` checker). The proof is the alternating series
bound applied to Mathlib's power series of `sin`.
-/

namespace LFortran

open Finset Real PolyBound

/-- The coefficients of `T_n(z) = Σ_{i<n} (-1)^i z^i / (2i+1)!`. -/
def sinT (n : ℕ) : List ℚ := (List.range n).map fun i => (-1) ^ i / ((2 * i + 1).factorial : ℚ)

/-- `r ↦ r · p(r²)` as a coefficient list in `r`. -/
def oddList : List ℚ → List ℚ
  | [] => []
  | a :: p => 0 :: a :: oddList p

theorem evalR_oddList : ∀ (p : List ℚ) (r : ℝ), evalR (oddList p) r = r * evalR p (r ^ 2)
  | [], r => by simp [oddList, evalR]
  | a :: p, r => by
    simp only [oddList, evalR, evalR_oddList p r]; push_cast; ring

theorem sin_taylor_nonneg (n : ℕ) {r : ℝ} (h0 : 0 ≤ r) (h2 : r ≤ 2) :
    |sin r - ∑ i ∈ range n, (-1) ^ i * (r ^ (2 * i + 1) / (2 * i + 1).factorial)| ≤
      r ^ (2 * n + 1) / (2 * n + 1).factorial := by
  set f : ℕ → ℝ := fun i => r ^ (2 * i + 1) / (2 * i + 1).factorial with hf
  have hsum : Summable f := (Real.hasSum_sinh r).summable
  have hanti : Antitone f := by
    apply antitone_nat_of_succ_le
    intro i
    simp only [hf]
    rw [show 2 * (i + 1) + 1 = (2 * i + 1) + 1 + 1 by ring, Nat.factorial_succ,
      Nat.factorial_succ, pow_succ, pow_succ]
    have hF : (0 : ℝ) < (2 * i + 1).factorial := by positivity
    have hp : 0 ≤ r ^ (2 * i + 1) := pow_nonneg h0 _
    rw [div_le_div_iff₀ (by positivity) hF]
    push_cast
    have hr2 : r * r ≤ ((2 * i + 1 : ℝ) + 1 + 1) * ((2 * i + 1 : ℝ) + 1) := by
      have : (0 : ℝ) ≤ i := i.cast_nonneg
      nlinarith
    have := mul_le_mul_of_nonneg_left hr2 (mul_nonneg hp hF.le)
    nlinarith
  have hsin : sin r = ∑' i, (-1) ^ i * f i := by
    rw [Real.sin_eq_tsum]
    congr 1; funext i; simp only [hf]; ring
  rw [hsin]
  exact alternating_series_error_bound f hanti hsum n

/-- The Taylor bound for `|r| ≤ 2`. -/
theorem sin_taylor (n : ℕ) {r : ℝ} (hr : |r| ≤ 2) :
    |sin r - ∑ i ∈ range n, (-1) ^ i * (r ^ (2 * i + 1) / (2 * i + 1).factorial)| ≤
      |r| ^ (2 * n + 1) / (2 * n + 1).factorial := by
  rcases le_total 0 r with h | h
  · rw [abs_of_nonneg h] at hr ⊢
    exact sin_taylor_nonneg n h hr
  · have hs := sin_taylor_nonneg n (neg_nonneg.2 h) (by rw [abs_of_nonpos h] at hr; exact hr)
    rw [abs_of_nonpos h]
    have hodd : ∀ k : ℕ, (-r) ^ (2 * k + 1) = -r ^ (2 * k + 1) :=
      fun k => Odd.neg_pow ⟨k, rfl⟩ r
    simp only [hodd, sin_neg] at hs
    rw [← abs_neg]
    convert hs using 2
    · simp only [neg_div, mul_neg, sum_neg_distrib]
      ring
    · exact hodd n

/-- The Taylor polynomial in the form `r · T_n(r²)`. -/
theorem sum_sin_eq (n : ℕ) (r : ℝ) :
    ∑ i ∈ range n, (-1) ^ i * (r ^ (2 * i + 1) / (2 * i + 1).factorial) =
      r * evalR (sinT n) (r ^ 2) := by
  rw [sinT, evalR_range_map, mul_sum]
  refine sum_congr rfl fun i _ => ?_
  push_cast
  rw [← pow_mul, pow_succ]
  ring

theorem sin_taylor_poly (n : ℕ) {r : ℝ} (hr : |r| ≤ 2) :
    |sin r - r * evalR (sinT n) (r ^ 2)| ≤ |r| ^ (2 * n + 1) / (2 * n + 1).factorial := by
  rw [← sum_sin_eq]; exact sin_taylor n hr

end LFortran
