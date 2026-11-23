import Legendre.Spec
import Legendre.Generated
import Legendre.Proof.Loop
import Legendre.Proof.Rounding
import Legendre.Proof.Bound

/-!
# Correctness of the generated `legendre_poly`

The loop computes `y m` (`Legendre/Proof/Loop.lean`). Each iteration commits a
local rounding error of at most `R = 16 u` (`Legendre/Proof/Rounding.lean`).
The errors `e m = y m - P m x` satisfy the Bonnet recurrence plus these local
errors; the form `T` (`Legendre/Proof/Bound.lean`) shows that they grow at most
like `m (m - 1) / 2 · R`, which is below `10^-11` for `m ≤ 100`.
-/

open LFortran Legendre

namespace LegendreProof

/-- Bound on the local rounding error of one iteration. -/
noncomputable def R : ℝ := 16 * unitRoundoff

lemma R_nonneg : 0 ≤ R := by unfold R; linarith [u_pos]

/-- Accumulated error of the computed sequence. -/
noncomputable def e (fp : Float64Model) (x : ℝ) (m : ℕ) : ℝ := y fp x m - P m x

/-- Invariant of the error propagation after computing `y (m + 1)`. -/
def Inv (fp : Float64Model) (x : ℝ) (m : ℕ) : Prop :=
  T x (e fp x (m + 1)) (e fp x m) ≤ ((m : ℝ) * R) ^ 2 ∧
    |e fp x (m + 1)| ≤ m * (m + 1) / 2 * R ∧ |e fp x m| ≤ m * (m + 1) / 2 * R

lemma err_small {m : ℕ} (hm : m ≤ 99) : (m : ℝ) * (m + 1) / 2 * R ≤ 1 / 10 ^ 11 := by
  have hm' : (m : ℝ) ≤ 99 := by exact_mod_cast hm
  have hm0 : (0 : ℝ) ≤ m := m.cast_nonneg
  have h1 : (m : ℝ) * (m + 1) ≤ 99 * 100 := by nlinarith
  have h2 : (m : ℝ) * (m + 1) / 2 * R ≤ 99 * 100 / 2 * R :=
    mul_le_mul_of_nonneg_right (by linarith) R_nonneg
  refine h2.trans ?_
  unfold R unitRoundoff
  norm_num

lemma y_abs_le (fp : Float64Model) {x : ℝ} (hx : |x| ≤ 1) {m j : ℕ} (hm : m ≤ 99)
    (he : |e fp x j| ≤ m * (m + 1) / 2 * R) : |y fp x j| ≤ 9 / 8 := by
  have h1 := P_abs_le x hx j
  have h2 := err_small hm
  have h3 : y fp x j = e fp x j + P j x := by unfold e; ring
  rw [h3]
  have := abs_add_le (e fp x j) (P j x)
  have : (1 : ℝ) / 10 ^ 11 ≤ 1 / 8 := by norm_num
  linarith

lemma Inv_zero (fp : Float64Model) (x : ℝ) : Inv fp x 0 := by
  have h0 : e fp x 0 = 0 := by simp [e, y, P]
  have h1 : e fp x 1 = 0 := by simp [e, y, P]
  simp [Inv, h0, h1, T]

lemma Inv_succ (fp : Float64Model) {x : ℝ} (hx : |x| ≤ 1) {m : ℕ} (hm : m ≤ 98)
    (h : Inv fp x m) : Inv fp x (m + 1) := by
  obtain ⟨hT, h1, h0⟩ := h
  have hR := R_nonneg
  have hm0 : (0 : ℝ) ≤ m := m.cast_nonneg
  have hy1 := y_abs_le fp hx (by omega) h1
  have hy0 := y_abs_le fp hx (by omega) h0
  have hr : |step fp x ((m : ℤ) + 2) (y fp x m) (y fp x (m + 1)) -
      ((2 * m + 3) * x * y fp x (m + 1) - (m + 1) * y fp x m) / (m + 2)| ≤ R :=
    step_err fp x (y fp x m) (y fp x (m + 1)) m hm hx hy0 hy1
  set r := step fp x ((m : ℤ) + 2) (y fp x m) (y fp x (m + 1)) -
    ((2 * m + 3) * x * y fp x (m + 1) - (m + 1) * y fp x m) / (m + 2) with hr_def
  set w := W x (e fp x (m + 1)) (e fp x m) m with hw
  set A := w - x * e fp x (m + 1) with hA_def
  have he2 : e fp x (m + 2) = x * e fp x (m + 1) + A + r := by
    simp only [hA_def, hw, hr_def, e, W, P_succ_succ]
    show step fp x ((m : ℤ) + 2) (y fp x m) (y fp x (m + 1)) - _ = _
    ring
  have hC : 0 ≤ (1 - x ^ 2) * e fp x (m + 1) ^ 2 := by
    have : x ^ 2 ≤ 1 := (sq_le_one_iff_abs_le_one x).mpr hx
    have := sq_nonneg (e fp x (m + 1))
    nlinarith
  have hAC : A ^ 2 + (1 - x ^ 2) * e fp x (m + 1) ^ 2 ≤ ((m : ℝ) * R) ^ 2 := by
    have := (T_W_le x (e fp x (m + 1)) (e fp x m) m).trans hT
    rwa [T_eq] at this
  have hA : |A| ≤ m * R :=
    abs_le_of_sq_le_sq (by linarith) (mul_nonneg hm0 hR)
  have hAr : A * r ≤ m * R * R := by
    calc A * r ≤ |A * r| := le_abs_self _
      _ = |A| * |r| := abs_mul A r
      _ ≤ m * R * R := mul_le_mul hA hr (abs_nonneg _) (mul_nonneg hm0 hR)
  have hr2 : r ^ 2 ≤ R ^ 2 := by
    rw [← sq_abs r]
    exact pow_le_pow_left₀ (abs_nonneg r) hr 2
  have hxe : |x * e fp x (m + 1)| ≤ m * (m + 1) / 2 * R := by
    rw [abs_mul]
    calc |x| * |e fp x (m + 1)| ≤ 1 * |e fp x (m + 1)| :=
          mul_le_mul_of_nonneg_right hx (abs_nonneg _)
      _ ≤ m * (m + 1) / 2 * R := by rw [one_mul]; exact h1
  refine ⟨?_, ?_, ?_⟩
  · rw [T_eq, he2]
    push_cast
    nlinarith
  · rw [he2]
    have := abs_add_le (x * e fp x (m + 1) + A) r
    have := abs_add_le (x * e fp x (m + 1)) A
    push_cast
    nlinarith
  · push_cast
    have : 0 ≤ ((m : ℝ) + 1) * R := by positivity
    nlinarith

lemma Inv_all (fp : Float64Model) {x : ℝ} (hx : |x| ≤ 1) :
    ∀ m : ℕ, m ≤ 99 → Inv fp x m
  | 0, _ => Inv_zero fp x
  | m + 1, hm => Inv_succ fp hx (by omega) (Inv_all fp hx m (by omega))

end LegendreProof

open LegendreProof

noncomputable abbrev legendre_fortran := legendre_mod.legendre_poly

theorem legendre_correct :
    LegendreSpec legendre_fortran := by
  intro fp n x hn0 hn100 hx
  obtain ⟨k, rfl⟩ := Int.eq_ofNat_of_zero_le hn0
  cases k with
  | zero =>
    simp [legendre_fortran, legendre_zero, P]
  | succ m =>
    have hm : m ≤ 99 := by omega
    have hcast : ((m + 1 : ℕ) : ℤ) = (m : ℤ) + 1 := by push_cast; ring
    rw [hcast, legendre_fortran, legendre_succ,
      show ((m : ℤ) + 1).toNat = m + 1 by omega]
    exact (Inv_all fp hx m hm).2.1.trans (err_small hm)

#print axioms legendre_correct

/-- The rounding model is not vacuous: exact arithmetic satisfies it. -/
example : Float64Model where
  rnd := id
  rnd_err y _ := by simp [unitRoundoff, underflowError]; positivity
  rnd_int _ _ := rfl
