import Legendre.Spec
import Legendre.Generated
import Legendre.Proof.Loop
import Legendre.Proof.Rounding
import Legendre.Proof.Bound

/-!
# Correctness of `legendre_poly`

The loop computes `y m` (`Legendre/Proof/Loop.lean`), starting from
`y 0 = fp.round 1` and `y 1 = x`. In an `Admissible` arithmetic without a
guard digit (`NoGuardModel`, unit roundoff at most `U`) every computed value
stays in `fp.F`, and each iteration commits a local rounding error of at most
`R = 18 U` (`Legendre/Proof/Rounding.lean`). The errors
`e m = y m - P m x` satisfy the Bonnet recurrence plus these local errors, with
`|e 0| ≤ ε0 = U + E` (the rounded literal `1`) and `e 1 = 0`; the form `T`
(`Legendre/Proof/Bound.lean`) shows that they grow at most like
`(m + 1) ε0 + m (m + 1) / 2 · R`, which is at most `89201 U` for `m ≤ 99`.

Every `FPModel` is a `NoGuardModel` (`FPModel.toNoGuard`), and the generated
`legendre_mod.legendre_poly fp` is the shared definition at `fp.toNoGuard`
(`legendre_fortran_eq`), so `legendre_correct` (`U = 2^-53`, `10^-11`) is a
corollary.
-/

open LFortran Legendre

namespace LegendreProof

/-- Bound on the local rounding error of one iteration. -/
noncomputable def R (U : ℝ) : ℝ := 18 * U

/-- Bound on the error of the initial value `fp.round 1`. -/
noncomputable def ε0 (U : ℝ) : ℝ := U + E U

/-- Bound on `|e m|` and `|e (m + 1)|` in the invariant `Inv m`. -/
noncomputable def B (U : ℝ) (m : ℕ) : ℝ := (m + 1) * ε0 U + m * (m + 1) / 2 * R U

/-- Accumulated error of the computed sequence. -/
noncomputable def e (fp : NoGuardModel) (x : ℝ) (m : ℕ) : ℝ := y fp x m - P m x

/-- Invariant of the error propagation after computing `y (m + 1)`. -/
def Inv (fp : NoGuardModel) (U x : ℝ) (m : ℕ) : Prop :=
  y fp x m ∈ fp.F ∧ y fp x (m + 1) ∈ fp.F ∧
    T x (e fp x (m + 1)) (e fp x m) ≤ (ε0 U + m * R U) ^ 2 ∧
    |e fp x (m + 1)| ≤ B U m ∧ |e fp x m| ≤ B U m

lemma B_le {U : ℝ} (hU : 0 ≤ U) {m : ℕ} (hm : m ≤ 99) : B U m ≤ 89201 * U := by
  have hm' : (m : ℝ) ≤ 99 := by exact_mod_cast hm
  have hm0 : (0 : ℝ) ≤ m := m.cast_nonneg
  have h1 : (m : ℝ) * (m + 1) ≤ 99 * 100 := by nlinarith
  have h2 : (m : ℝ) * (m + 1) / 2 * (18 * U) ≤ 99 * 100 / 2 * (18 * U) :=
    mul_le_mul_of_nonneg_right (by linarith) (by positivity)
  have h3 : ((m : ℝ) + 1) * (U + U / 10 ^ 6) ≤ 100 * (U + U / 10 ^ 6) :=
    mul_le_mul_of_nonneg_right (by linarith) (by positivity)
  unfold B ε0 R E
  nlinarith

lemma B_tiny {fp : NoGuardModel} {U : ℝ} (hfp : Admissible fp U) {m : ℕ} (hm : m ≤ 99) :
    B U m ≤ 1 / 10 ^ 7 := by
  have := B_le hfp.1 hm
  have := hfp.2.1
  have h : (89201 : ℝ) * (1 / 2 ^ 40) ≤ 1 / 10 ^ 7 := by norm_num
  nlinarith

lemma y_abs_le (fp : NoGuardModel) {U : ℝ} (hfp : Admissible fp U) {x : ℝ} (hx : |x| ≤ 1)
    {m j : ℕ} (hm : m ≤ 99) (he : |e fp x j| ≤ B U m) : |y fp x j| ≤ 1 + 1 / 10 ^ 6 := by
  have h1 := P_abs_le x hx j
  have h2 := B_tiny hfp hm
  have h3 : y fp x j = e fp x j + P j x := by unfold e; ring
  rw [h3]
  have := abs_add_le (e fp x j) (P j x)
  have : (1 : ℝ) / 10 ^ 7 ≤ 1 / 10 ^ 6 := by norm_num
  linarith

/-- The initial value `fp.round 1` is in `fp.F` with error at most `ε0`. -/
lemma round_one (fp : NoGuardModel) {U : ℝ} (hfp : Admissible fp U) :
    fp.round 1 ∈ fp.F ∧ |fp.round 1 - 1| ≤ ε0 U := by
  have h1 : |(1 : ℝ)| ≤ 1 := by simp
  refine ⟨fp.round_mem 1 (le_M hfp h1 (by norm_num)), ?_⟩
  obtain ⟨d, hd, hdb⟩ :=
    decomp hfp (fp.round_err 1 (le_M hfp h1 (by norm_num))) (abs_nonneg _) h1
  rw [hd, add_sub_cancel_left]
  unfold ε0
  linarith

lemma Inv_zero (fp : NoGuardModel) {U : ℝ} (hfp : Admissible fp U) {x : ℝ} (hxF : x ∈ fp.F) :
    Inv fp U x 0 := by
  obtain ⟨h1F, h1⟩ := round_one fp hfp
  have h0 : e fp x 0 = fp.round 1 - 1 := by simp [e, y, P]
  have h1' : e fp x 1 = 0 := by simp [e, y, P]
  have hB : B U 0 = ε0 U := by simp [B]
  refine ⟨h1F, hxF, ?_, ?_, ?_⟩
  · rw [h0, h1']
    simp only [T, Nat.cast_zero, zero_mul, add_zero]
    rw [← sq_abs (fp.round 1 - 1)]
    have := pow_le_pow_left₀ (abs_nonneg _) h1 2
    linarith
  · rw [h1', hB, abs_zero]; unfold ε0; linarith [E_nonneg hfp.1, hfp.1]
  · rw [h0, hB]; exact h1

lemma Inv_succ (fp : NoGuardModel) {U : ℝ} (hfp : Admissible fp U) {x : ℝ} (hxF : x ∈ fp.F)
    (hx : |x| ≤ 1) {m : ℕ} (hm : m ≤ 98) (h : Inv fp U x m) : Inv fp U x (m + 1) := by
  obtain ⟨hF0, hF1, hT, h1, h0⟩ := h
  have hR : 0 ≤ R U := by unfold R; linarith [hfp.1]
  have hε : 0 ≤ ε0 U := by unfold ε0; linarith [E_nonneg hfp.1, hfp.1]
  have hm0 : (0 : ℝ) ≤ m := m.cast_nonneg
  have hy1 := y_abs_le fp hfp hx (by omega) h1
  have hy0 := y_abs_le fp hfp hx (by omega) h0
  obtain ⟨hF2, hr⟩ := step_err fp hfp x (y fp x m) (y fp x (m + 1)) m hm hxF hF0 hF1 hx hy0 hy1
  rw [show 18 * U = R U from rfl] at hr
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
  have hAC : A ^ 2 + (1 - x ^ 2) * e fp x (m + 1) ^ 2 ≤ (ε0 U + m * R U) ^ 2 := by
    have := (T_W_le x (e fp x (m + 1)) (e fp x m) m).trans hT
    rwa [T_eq] at this
  have hA : |A| ≤ ε0 U + m * R U :=
    abs_le_of_sq_le_sq (by linarith) (by positivity)
  have hAr : A * r ≤ (ε0 U + m * R U) * R U := by
    calc A * r ≤ |A * r| := le_abs_self _
      _ = |A| * |r| := abs_mul A r
      _ ≤ (ε0 U + m * R U) * R U := mul_le_mul hA hr (abs_nonneg _) (by positivity)
  have hr2 : r ^ 2 ≤ R U ^ 2 := by
    rw [← sq_abs r]
    exact pow_le_pow_left₀ (abs_nonneg r) hr 2
  have hxe : |x * e fp x (m + 1)| ≤ B U m := by
    rw [abs_mul]
    calc |x| * |e fp x (m + 1)| ≤ 1 * |e fp x (m + 1)| :=
          mul_le_mul_of_nonneg_right hx (abs_nonneg _)
      _ ≤ B U m := by rw [one_mul]; exact h1
  have hB : B U (m + 1) = B U m + ε0 U + (m + 1) * R U := by
    unfold B; push_cast; ring
  have hmR : 0 ≤ ((m : ℝ) + 1) * R U := by positivity
  refine ⟨hF1, hF2, ?_, ?_, ?_⟩
  · rw [T_eq, he2]
    push_cast
    nlinarith
  · rw [he2, hB]
    have := abs_add_le (x * e fp x (m + 1) + A) r
    have := abs_add_le (x * e fp x (m + 1)) A
    linarith
  · rw [hB]
    linarith

lemma Inv_all (fp : NoGuardModel) {U : ℝ} (hfp : Admissible fp U) {x : ℝ} (hxF : x ∈ fp.F)
    (hx : |x| ≤ 1) : ∀ m : ℕ, m ≤ 99 → Inv fp U x m
  | 0, _ => Inv_zero fp hfp hxF
  | m + 1, hm => Inv_succ fp hfp hxF hx (by omega) (Inv_all fp hfp hxF hx m (by omega))

end LegendreProof

open LegendreProof

/-- Accuracy of the shared `legendre_poly` in every admissible arithmetic
without a guard digit. -/
theorem legendre_ng_correct : LegendreSpecNoGuard legendre_ng := by
  intro fp U hU0 hU hu hη hM n x hxF hn0 hn100 hx
  have hfp : Admissible fp U := ⟨hU0, hU, hu, hη, hM⟩
  obtain ⟨k, rfl⟩ := Int.eq_ofNat_of_zero_le hn0
  cases k with
  | zero =>
    have hB0 : ε0 U ≤ 89201 * U := by simpa [B] using B_le hU0 (m := 0) (by norm_num)
    simpa [legendre_zero, P] using (round_one fp hfp).2.trans hB0
  | succ m =>
    have hm : m ≤ 99 := by omega
    have hcast : ((m + 1 : ℕ) : ℤ) = (m : ℤ) + 1 := by push_cast; ring
    rw [hcast, legendre_succ, show ((m : ℤ) + 1).toNat = m + 1 by omega]
    exact (Inv_all fp hfp hxF hx m hm).2.2.2.1.trans (B_le hU0 hm)

#print axioms legendre_ng_correct

noncomputable abbrev legendre_fortran := legendre_mod.legendre_poly

/-- The generated `legendre_poly` is the shared one at `fp.toNoGuard`. -/
theorem legendre_fortran_eq (fp : FPModel) (n : ℤ) (x : ℝ) :
    legendre_fortran fp n x = legendre_ng fp.toNoGuard n x := by
  rw [legendre_ng, ← ofModel_eq_ofNoGuard]
  simp only [legendre_fortran, legendre_shared.legendre_poly, legendre_mod.legendre_poly,
    range_eq_intRange, ofModel_zero, ofModel_ofInt, ofModel_sub, ofModel_mul,
    ofModel_div, ofInt_int, toInt_int, mul_int, sub_int, FPModel.ofInt, Int.cast_one]

theorem legendre_correct :
    LegendreSpec legendre_fortran := by
  intro fp hu hη hM n x hxF hn0 hn100 hx
  have hU : (1 : ℝ) / 2 ^ 53 ≤ 1 / 2 ^ 40 := by norm_num
  have hu' : fp.toNoGuard.u ≤ 1 / 2 ^ 53 := max_le hu (by norm_num)
  have hη' : fp.toNoGuard.η ≤ (1 / 2 ^ 53) / 10 ^ 6 := by
    have h : (1 : ℝ) / 2 ^ 1000 ≤ (1 / 2 ^ 53) / 10 ^ 6 := by
      rw [div_div, one_div_le_one_div (by positivity) (by positivity)]
      calc (2 : ℝ) ^ 53 * 10 ^ 6 ≤ 2 ^ 53 * 2 ^ 947 := by
            gcongr
            calc (10 : ℝ) ^ 6 ≤ 2 ^ 20 := by norm_num
              _ ≤ 2 ^ 947 := pow_le_pow_right₀ (by norm_num) (by norm_num)
        _ = 2 ^ 1000 := by rw [← pow_add]
    exact hη.trans h
  rw [legendre_fortran_eq]
  have := legendre_ng_correct fp.toNoGuard (1 / 2 ^ 53) (by positivity) hU hu' hη' hM
    n x hxF hn0 hn100 hx
  have h2 : (89201 : ℝ) * (1 / 2 ^ 53) ≤ 1 / 10 ^ 11 := by norm_num
  linarith

#print axioms legendre_correct

/-- The assumptions of `LegendreSpec` are satisfiable: exact arithmetic on all
of `ℝ` (`u = η = 0`, `M = 2^10`) is an admissible `FPModel`. -/
example : ∃ fp : FPModel, fp.u ≤ 1 / 2 ^ 53 ∧ fp.η ≤ 1 / 2 ^ 1000 ∧ 2 ^ 10 ≤ fp.M ∧
    (1 : ℝ) ∈ fp.F :=
  ⟨{ F := Set.univ, u := 0, η := 0, M := 2 ^ 10, round := id,
     add := (· + ·), sub := (· - ·), mul := (· * ·), div := (· / ·),
     round_mem := fun _ _ => trivial, round_err := fun _ _ => by simp,
     add_mem := fun _ _ _ _ _ => trivial, add_err := fun _ _ _ _ _ => by simp,
     sub_mem := fun _ _ _ _ _ => trivial, sub_err := fun _ _ _ _ _ => by simp,
     mul_mem := fun _ _ _ _ _ => trivial, mul_err := fun _ _ _ _ _ => by simp,
     div_mem := fun _ _ _ _ _ _ => trivial, div_err := fun _ _ _ _ _ _ => by simp },
   by norm_num, by norm_num, le_refl _, trivial⟩
