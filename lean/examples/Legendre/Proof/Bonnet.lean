import Mathlib.RingTheory.Polynomial.ShiftedLegendre
import Mathlib.Tactic.Ring
import Mathlib.Tactic.Linarith
import Mathlib.Tactic.NormNum
import Mathlib.Tactic.LinearCombination
import Mathlib.Tactic.Positivity
import Mathlib.Tactic.FieldSimp
import Legendre.Spec

/-!
# Bonnet's recurrence for Mathlib's Legendre polynomials

`Legendre.legendreP` (the specification) is Mathlib's `shiftedLegendre`
evaluated at `(1 - x)/2`. The Fortran code evaluates Bonnet's three-term
recurrence instead. This file proves the coefficient identity behind the
recurrence, lifts it to `shiftedLegendre`, and concludes that the recurrence
`Legendre.P` equals `legendreP`.
-/

namespace LegendreBonnet

/-- The magnitude of the `k`-th coefficient of `shiftedLegendre m`. -/
def b (m k : ℕ) : ℕ := m.choose k * (m + k).choose m

theorem b_fact {m i : ℕ} (h : i ≤ m) : b m i * (i.factorial * i.factorial * (m - i).factorial)
    = (m + i).factorial := by
  have h1 := Nat.choose_mul_factorial_mul_factorial h
  have h2 := Nat.choose_mul_factorial_mul_factorial (Nat.le_add_right m i)
  rw [show m + i - m = i by omega] at h2
  have hm : 0 < m.factorial := Nat.factorial_pos m
  apply Nat.eq_of_mul_eq_mul_right hm
  calc b m i * (i.factorial * i.factorial * (m - i).factorial) * m.factorial
      = (m.choose i * i.factorial * (m - i).factorial)
          * ((m + i).choose m * m.factorial * i.factorial) := by unfold b; ring
    _ = m.factorial * (m + i).factorial := by rw [h1, h2]
    _ = (m + i).factorial * m.factorial := by ring

theorem b_zero_of_lt {m k : ℕ} (h : m < k) : b m k = 0 := by
  simp [b, Nat.choose_eq_zero_of_lt h]

/-- `b_fact` with every factorial expanded down to `base`, as needed below. -/
theorem b_fact' {m i : ℕ} (h : i ≤ m) : (b m i : ℤ) * ((i.factorial : ℤ) * i.factorial
    * (m - i).factorial) = (m + i).factorial := by
  exact_mod_cast b_fact h

/-- The coefficient identity behind Bonnet's recurrence. -/
theorem bonnet_coeff (n j : ℕ) :
    (n + 2) * b (n + 2) (j + 1) + (n + 1) * b n (j + 1)
      = (2 * n + 3) * (b (n + 1) (j + 1) + 2 * b (n + 1) j) := by
  rcases lt_trichotomy j n with hlt | heq | hgt
  · -- j + 1 ≤ n: write n = j + 1 + d
    obtain ⟨d, rfl⟩ : ∃ d, n = j + 1 + d := ⟨n - (j + 1), by omega⟩
    have F1 := b_fact' (m := j + 1 + d + 2) (i := j + 1) (by omega)
    have F2 := b_fact' (m := j + 1 + d) (i := j + 1) (by omega)
    have F3 := b_fact' (m := j + 1 + d + 1) (i := j + 1) (by omega)
    have F4 := b_fact' (m := j + 1 + d + 1) (i := j) (by omega)
    rw [show j + 1 + d + 2 - (j + 1) = d + 1 + 1 by omega,
      show j + 1 + d + 2 + (j + 1) = (2 * j + d + 2) + 1 + 1 by omega] at F1
    rw [show j + 1 + d - (j + 1) = d by omega,
      show j + 1 + d + (j + 1) = 2 * j + d + 2 by omega] at F2
    rw [show j + 1 + d + 1 - (j + 1) = d + 1 by omega,
      show j + 1 + d + 1 + (j + 1) = (2 * j + d + 2) + 1 by omega] at F3
    rw [show j + 1 + d + 1 - j = d + 1 + 1 by omega,
      show j + 1 + d + 1 + j = 2 * j + d + 2 by omega] at F4
    simp only [Nat.factorial_succ, Nat.cast_mul, Nat.cast_add, Nat.cast_ofNat,
      Nat.cast_one] at F1 F2 F3 F4
    set A : ℤ := ((2 * j + d + 2).factorial : ℤ)
    set J : ℤ := (j.factorial : ℤ)
    set D : ℤ := (d.factorial : ℤ)
    have hJ : J ≠ 0 := by simp [J, Nat.factorial_ne_zero]
    have hD : D ≠ 0 := by simp [D, Nat.factorial_ne_zero]
    have hW : ((j : ℤ) + 1) ^ 2 * J ^ 2 * ((d : ℤ) + 2) * ((d : ℤ) + 1) * D ≠ 0 := by
      have : (0 : ℤ) < (j : ℤ) + 1 := by positivity
      have : (0 : ℤ) < (d : ℤ) + 1 := by positivity
      have : (0 : ℤ) < (d : ℤ) + 2 := by positivity
      positivity
    have key : (((j + 1 + d : ℕ) : ℤ) + 2) * b (j + 1 + d + 2) (j + 1)
        + (((j + 1 + d : ℕ) : ℤ) + 1) * b (j + 1 + d) (j + 1)
        = (2 * ((j + 1 + d : ℕ) : ℤ) + 3) * (b (j + 1 + d + 1) (j + 1)
          + 2 * b (j + 1 + d + 1) j) := by
      apply mul_right_cancel₀ hW
      push_cast
      linear_combination (((j : ℤ) + 1 + d) + 2) * F1
        + (((j : ℤ) + 1 + d) + 1) * (((d : ℤ) + 1) * ((d : ℤ) + 2)) * F2
        - (2 * ((j : ℤ) + 1 + d) + 3) * ((d : ℤ) + 2) * F3
        - 2 * (2 * ((j : ℤ) + 1 + d) + 3) * ((j : ℤ) + 1) ^ 2 * F4
    exact_mod_cast key
  · -- j = n
    subst heq
    rw [b_zero_of_lt (show j < j + 1 by omega)]
    have F1 := b_fact' (m := j + 2) (i := j + 1) (by omega)
    have F3 := b_fact' (m := j + 1) (i := j + 1) (by omega)
    have F4 := b_fact' (m := j + 1) (i := j) (by omega)
    rw [show j + 2 - (j + 1) = 0 + 1 by omega,
      show j + 2 + (j + 1) = (2 * j + 1) + 1 + 1 by omega] at F1
    rw [show j + 1 - (j + 1) = 0 by omega,
      show j + 1 + (j + 1) = (2 * j + 1) + 1 by omega] at F3
    rw [show j + 1 - j = 0 + 1 by omega, show j + 1 + j = 2 * j + 1 by omega] at F4
    simp only [Nat.factorial_succ, Nat.factorial_zero, Nat.cast_mul, Nat.cast_add,
      Nat.cast_ofNat, Nat.cast_one] at F1 F3 F4
    set A : ℤ := ((2 * j + 1).factorial : ℤ)
    set J : ℤ := (j.factorial : ℤ)
    have hJ : J ≠ 0 := by simp [J, Nat.factorial_ne_zero]
    have hW : ((j : ℤ) + 1) ^ 2 * J ^ 2 ≠ 0 := by
      have : (0 : ℤ) < (j : ℤ) + 1 := by positivity
      positivity
    have key : ((j : ℤ) + 2) * b (j + 2) (j + 1) + ((j : ℤ) + 1) * 0
        = (2 * (j : ℤ) + 3) * (b (j + 1) (j + 1) + 2 * b (j + 1) j) := by
      apply mul_right_cancel₀ hW
      linear_combination ((j : ℤ) + 2) * F1 - (2 * (j : ℤ) + 3) * F3
        - 2 * (2 * (j : ℤ) + 3) * ((j : ℤ) + 1) ^ 2 * F4
    exact_mod_cast key
  · rcases Nat.lt_or_ge (n + 1) j with hgt2 | hle
    · rw [b_zero_of_lt (show n + 2 < j + 1 by omega), b_zero_of_lt (show n < j + 1 by omega),
        b_zero_of_lt (show n + 1 < j + 1 by omega), b_zero_of_lt hgt2]
      simp
    · -- j = n + 1
      obtain rfl : j = n + 1 := by omega
      rw [b_zero_of_lt (show n < n + 1 + 1 by omega),
        b_zero_of_lt (show n + 1 < n + 1 + 1 by omega)]
      have F1 := b_fact' (m := n + 2) (i := n + 2) (by omega)
      have F4 := b_fact' (m := n + 1) (i := n + 1) (by omega)
      rw [show n + 2 - (n + 2) = 0 by omega,
        show n + 2 + (n + 2) = (2 * n + 2) + 1 + 1 by omega,
        show n + 2 = (n + 1) + 1 by omega] at F1
      rw [show n + 1 - (n + 1) = 0 by omega, show n + 1 + (n + 1) = 2 * n + 2 by omega] at F4
      simp only [Nat.factorial_succ (n + 1), Nat.factorial_succ (2 * n + 2),
        Nat.factorial_succ (2 * n + 2 + 1), Nat.factorial_zero, Nat.cast_mul, Nat.cast_add,
        Nat.cast_ofNat, Nat.cast_one] at F1 F4
      set A : ℤ := ((2 * n + 2).factorial : ℤ)
      set N : ℤ := ((n + 1).factorial : ℤ)
      have hN : N ≠ 0 := by simp [N, Nat.factorial_ne_zero]
      have hW : ((n : ℤ) + 1 + 1) ^ 2 * N ^ 2 ≠ 0 := by
        have : (0 : ℤ) < (n : ℤ) + 1 + 1 := by positivity
        positivity
      have key : ((n : ℤ) + 2) * b (n + 1 + 1) (n + 1 + 1) + ((n : ℤ) + 1) * 0
          = (2 * (n : ℤ) + 3) * (0 + 2 * b (n + 1) (n + 1)) := by
        apply mul_right_cancel₀ hW
        linear_combination ((n : ℤ) + 2) * F1
          - 2 * (2 * (n : ℤ) + 3) * ((n : ℤ) + 1 + 1) ^ 2 * F4
      exact_mod_cast key

open Polynomial

theorem coeff_S (m k : ℕ) : (shiftedLegendre m).coeff k = (-1 : ℤ) ^ k * (b m k : ℤ) := by
  rw [coeff_shiftedLegendre]; simp [b]; ring

/-- Bonnet's recurrence for the shifted Legendre polynomials, in `t = (1 - x)/2`. -/
theorem bonnet_poly (n : ℕ) :
    C ((n : ℤ) + 2) * shiftedLegendre (n + 2)
      = C (2 * (n : ℤ) + 3) * ((1 - 2 * X) * shiftedLegendre (n + 1))
        - C ((n : ℤ) + 1) * shiftedLegendre n := by
  ext k
  have h2X : ∀ p : ℤ[X], (1 - 2 * X) * p = p - C 2 * (X * p) := by
    intro p; rw [sub_mul, one_mul, ← mul_assoc, ← C_ofNat]
  rw [h2X]
  simp only [coeff_sub, coeff_C_mul, coeff_S]
  rcases k with _ | j
  · simp [b]; ring
  · rw [coeff_X_mul, coeff_S]
    have h := bonnet_coeff n j
    have hz : ((n + 2 : ℕ) : ℤ) * b (n + 2) (j + 1) + ((n + 1 : ℕ) : ℤ) * b n (j + 1)
        = ((2 * n + 3 : ℕ) : ℤ) * (b (n + 1) (j + 1) + 2 * b (n + 1) j) := by exact_mod_cast h
    push_cast at hz
    rw [pow_succ]
    linear_combination (-1 : ℤ) ^ j * (-1) * hz

end LegendreBonnet

namespace Legendre

open Polynomial

/-- The exact Legendre polynomial `P_n(x)` over `ℝ`, defined by Bonnet's
recurrence `P_0 = 1`, `P_1 = x`,
`(n + 1) P_{n+1}(x) = (2n + 1) x P_n(x) - n P_{n-1}(x)`. This is what the
Fortran loop computes in exact arithmetic; `P_eq_legendreP` identifies it with
Mathlib's polynomial. -/
noncomputable def P : ℕ → ℝ → ℝ
  | 0, _ => 1
  | 1, x => x
  | n + 2, x => ((2 * n + 3) * x * P (n + 1) x - (n + 1) * P n x) / (n + 2)

theorem legendreP_zero (x : ℝ) : legendreP 0 x = 1 := by
  simp [legendreP, shiftedLegendre]

theorem legendreP_one (x : ℝ) : legendreP 1 x = x := by
  simp [legendreP, shiftedLegendre, Finset.sum_range_succ]; ring

/-- Bonnet's recurrence for `legendreP`. -/
theorem legendreP_succ_succ (n : ℕ) (x : ℝ) :
    legendreP (n + 2) x
      = ((2 * n + 3) * x * legendreP (n + 1) x - (n + 1) * legendreP n x) / (n + 2) := by
  have h := congrArg (aeval ((1 - x) / 2)) (LegendreBonnet.bonnet_poly n)
  simp only [map_mul, map_sub, map_add, map_natCast, map_ofNat, aeval_X, map_one] at h
  unfold legendreP
  have hn : (n : ℝ) + 2 ≠ 0 := by positivity
  field_simp
  linear_combination h

theorem P_eq_legendreP : ∀ (n : ℕ) (x : ℝ), P n x = legendreP n x
  | 0, x => by rw [P, legendreP_zero]
  | 1, x => by rw [P, legendreP_one]
  | n + 2, x => by
    rw [P, legendreP_succ_succ, P_eq_legendreP (n + 1) x, P_eq_legendreP n x]

end Legendre
