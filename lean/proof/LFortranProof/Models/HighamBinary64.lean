import LFortranProof.Models.Binary64
import LFortranProof.Models.Higham

/-!
# binary64 round-to-nearest satisfies Higham's model with underflow

N. J. Higham, *Accuracy and Stability of Numerical Algorithms*, 2nd ed.,
SIAM, 2002, Chapter 2; IEEE Std 754-2019.

We show that IEEE-754 binary64 arithmetic with rounding to nearest (any
tie-breaking rule) is an instance of Higham's standard model with underflow
(`LFortran.Higham`) with unit roundoff `u = 2^-53`, smallest positive normal
number `λ = 2^-1022` and overflow threshold `M = 2^1022`
(`binary64Higham`):

* in the normal range the rounding error is relative, `δ` with `|δ| ≤ u`;
* in the subnormal range it is absolute, `η` with `|η| ≤ λ u = 2^-1075`;
  only one of them is nonzero, so `δ η = 0`;
* sums and differences of binary64 numbers need no `η`: every binary64 number
  is an integer multiple of `2^-1074`, hence so is every sum or difference,
  and such a number in the subnormal range is itself a binary64 number, so it
  is computed exactly (gradual underflow).

Going through Higham's model gives the same `FPModel` as `binary64Model`
(`binary64Higham_toFPModel`).
-/

namespace LFortran

/-- Every binary64 number is an integer multiple of `2^-1074`. -/
theorem binary64_eq_int_div {a : ℝ} (ha : a ∈ binary64) : ∃ k : ℤ, a = k / 2^1074 := by
  obtain ⟨m, e, -, he, -, rfl⟩ := ha
  obtain ⟨n, hn⟩ := Int.eq_ofNat_of_zero_le (by omega : 0 ≤ e + 1074)
  refine ⟨m * 2^n, ?_⟩
  rw [show e = (n : ℤ) - 1074 by omega, zpow_sub₀ (by norm_num), zpow_natCast, zpow_ofNat,
    Int.cast_mul, Int.cast_pow, Int.cast_ofNat, mul_div_assoc]

theorem binary64_add_eq_int_div {a b : ℝ} (ha : a ∈ binary64) (hb : b ∈ binary64) :
    ∃ k : ℤ, a + b = k / 2^1074 := by
  obtain ⟨k, rfl⟩ := binary64_eq_int_div ha
  obtain ⟨l, rfl⟩ := binary64_eq_int_div hb
  exact ⟨k + l, by rw [Int.cast_add, add_div]⟩

theorem binary64_sub_eq_int_div {a b : ℝ} (ha : a ∈ binary64) (hb : b ∈ binary64) :
    ∃ k : ℤ, a - b = k / 2^1074 := by
  obtain ⟨k, rfl⟩ := binary64_eq_int_div ha
  obtain ⟨l, rfl⟩ := binary64_eq_int_div hb
  exact ⟨k - l, by rw [Int.cast_sub, sub_div]⟩

/-- Rounding to nearest has either a relative error `2^-53 |x|` or an absolute
error `2^-1075`, and in the latter case it is exact on integer multiples of
`2^-1074`. -/
theorem roundToNearest_binary64_cases {r : ℝ → ℝ}
    (hr : IsRoundToNearest binary64 (2^1022) r) {x : ℝ} (hx : |x| ≤ 2^1022) :
    |r x - x| ≤ 1/2^53 * |x| ∨
      (|r x - x| ≤ 1/2^1075 ∧ ((∃ k : ℤ, x = k / 2^1074) → r x = x)) := by
  obtain ⟨f, hf, h⟩ := exists_binary64_near_cases x hx
  have hn := (hr x hx).2 f hf
  rcases h with h | ⟨h, hk⟩
  · exact Or.inl (hn.trans h)
  · refine Or.inr ⟨hn.trans h, fun hx' => ?_⟩
    rw [hk hx', sub_self, abs_zero] at hn
    exact sub_eq_zero.mp (abs_nonpos_iff.mp hn)

/-- A relative error bound gives Higham's relative perturbation `δ`. -/
theorem exists_rel_of_abs_sub_le {y z u : ℝ} (hu : 0 ≤ u) (h : |z - y| ≤ u * |y|) :
    ∃ δ, |δ| ≤ u ∧ z = y * (1 + δ) := by
  rcases eq_or_ne y 0 with rfl | hy
  · exact ⟨0, by simpa using hu, by simpa using h⟩
  · refine ⟨(z - y) / y, ?_, by field_simp; ring⟩
    rw [abs_div, div_le_iff₀ (abs_pos.mpr hy)]
    exact h

theorem one_div_two_pow_1075 : (1:ℝ)/2^1075 = 1/2^1022 * (1/2^53) := by
  rw [one_div_mul_one_div, ← pow_add]

/-- Rounding to nearest in Higham's form `x (1 + δ) + η`. -/
theorem roundToNearest_binary64_higham {r : ℝ → ℝ}
    (hr : IsRoundToNearest binary64 (2^1022) r) {x : ℝ} (hx : |x| ≤ 2^1022) :
    ∃ δ η, |δ| ≤ 1/2^53 ∧ |η| ≤ 1/2^1022 * (1/2^53) ∧ δ * η = 0 ∧
      r x = x * (1 + δ) + η := by
  rcases roundToNearest_binary64_cases hr hx with h | ⟨h, -⟩
  · obtain ⟨δ, hδ, h⟩ := exists_rel_of_abs_sub_le (by positivity) h
    exact ⟨δ, 0, hδ, by simp, mul_zero δ, by rw [h, add_zero]⟩
  · refine ⟨0, r x - x, by simp, one_div_two_pow_1075 ▸ h, zero_mul _, by ring⟩

/-- Rounding to nearest is exact up to a relative error on integer multiples of
`2^-1074` (no underflow error). -/
theorem roundToNearest_binary64_rel_of_int {r : ℝ → ℝ}
    (hr : IsRoundToNearest binary64 (2^1022) r) {x : ℝ} (hx : |x| ≤ 2^1022)
    (hk : ∃ k : ℤ, x = k / 2^1074) : ∃ δ, |δ| ≤ 1/2^53 ∧ r x = x * (1 + δ) := by
  rcases roundToNearest_binary64_cases hr hx with h | ⟨-, h⟩
  · exact exists_rel_of_abs_sub_le (by positivity) h
  · exact ⟨0, by simp, by rw [h hk]; ring⟩

/-- binary64 round-to-nearest arithmetic satisfies Higham's model with
`u = 2^-53`, `λ = 2^-1022` and `M = 2^1022`. -/
noncomputable def binary64Higham (r : ℝ → ℝ) (hr : IsRoundToNearest binary64 (2^1022) r) :
    Higham where
  F := binary64
  u := 1/2^53
  lam := 1/2^1022
  M := 2^1022
  u_nonneg := by positivity
  lam_nonneg := by positivity
  fl := r
  add a b := r (a + b)
  sub a b := r (a - b)
  mul a b := r (a * b)
  div a b := r (a / b)
  fl_mem _ hx := (hr _ hx).1
  fl_ax _ hx := roundToNearest_binary64_higham hr hx
  add_mem _ _ _ _ h := (hr _ h).1
  sub_mem _ _ _ _ h := (hr _ h).1
  mul_mem _ _ _ _ h := (hr _ h).1
  div_mem _ _ _ _ _ h := (hr _ h).1
  add_ax _ ha _ hb h := roundToNearest_binary64_rel_of_int hr h (binary64_add_eq_int_div ha hb)
  sub_ax _ ha _ hb h := roundToNearest_binary64_rel_of_int hr h (binary64_sub_eq_int_div ha hb)
  mul_ax _ _ _ _ h := roundToNearest_binary64_higham hr h
  div_ax _ _ _ _ _ h := roundToNearest_binary64_higham hr h

/-- Going through Higham's model gives `binary64Model`. -/
theorem binary64Higham_toFPModel (r : ℝ → ℝ) (hr : IsRoundToNearest binary64 (2^1022) r) :
    (binary64Higham r hr).toFPModel = binary64Model r hr := by
  simp only [Higham.toFPModel, binary64Higham, binary64Model, ← one_div_two_pow_1075]

end LFortran
