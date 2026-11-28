import Legendre.Spec
import Legendre.Generated
import Legendre.Proof.Loop
import Legendre.Proof.Rounding
import Legendre.Proof.Bound

/-!
# Correctness of `legendre_poly`

Two parts, then their combination.

**Exact arithmetic** (`legendre_exact_eq`): the generated template at
`FPModel.exact` computes Mathlib's Legendre polynomial `legendreP n x` for
every `n ≥ 0` and real `x`. The loop evaluates Bonnet's recurrence `P`
(`Legendre/Proof/Loop.lean`), and `P = legendreP`
(`Legendre/Proof/Bonnet.lean`).

**Rounding** (`legendre_rounding`), in every `Admissible` `VModel` (on any
carrier: `ℝ` with a `NoGuardModel`, or Lean's `Float`; unit roundoff at most
`U`, no guard digit needed) with exact `integer(4)`: the loop computes
`yK m`, starting from `yK 0 = real(1)` and `yK 1 = x`; each iteration commits
a local rounding error of at most `R = 18 U` (`Legendre/Proof/Rounding.lean`).
The errors `e m = y m - P m x` of the values `y m` satisfy the Bonnet
recurrence plus these local errors, with `|e 0| ≤ ε0 = U + E` (the rounded
`1`) and `e 1 = 0`; the form `T` (`Legendre/Proof/Bound.lean`) shows that they
grow at most like `(m + 1) ε0 + m (m + 1) / 2 · R`, which is at most
`89201 U` for `m ≤ 99`.

**Corollaries**: `legendre_ng_correct` (every `NoGuardModel`),
`legendre_correct` (every `FPModel`, `U = 2^-53`, `10^-11`), and the `Float`
run (`Legendre/FloatCorrect.lean`).
-/

open LFortran Legendre

namespace LegendreProof

/-- Bound on the local rounding error of one iteration. -/
noncomputable def R (U : ℝ) : ℝ := 18 * U

/-- Bound on the error of the initial value `real(1)`. -/
noncomputable def ε0 (U : ℝ) : ℝ := U + E U

/-- Bound on `|e m|` and `|e (m + 1)|` in the invariant `Inv m`. -/
noncomputable def B (U : ℝ) (m : ℕ) : ℝ := (m + 1) * ε0 U + m * (m + 1) / 2 * R U

variable {K : Kinds} [FKinds K]

/-- Invariant of the error propagation after computing `yK (m + 1)`: the two
current values have values `y0`, `y1` with these errors against `P`. -/
def Inv (V : VModel K.R8) (U : ℝ) (xv : K.R8) (x : ℝ) (m : ℕ) : Prop :=
  ∃ y0 y1, V.val (yK xv m) y0 ∧ V.val (yK xv (m + 1)) y1 ∧
    T x (y1 - P (m + 1) x) (y0 - P m x) ≤ (ε0 U + m * R U) ^ 2 ∧
    |y1 - P (m + 1) x| ≤ B U m ∧ |y0 - P m x| ≤ B U m

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

lemma B_tiny {ρ : Type} [FReal ρ] {V : VModel ρ} {U : ℝ} (hfp : Admissible V U) {m : ℕ}
    (hm : m ≤ 99) : B U m ≤ 1 / 10 ^ 7 := by
  have := B_le hfp.1 hm
  have := hfp.2.1
  have h : (89201 : ℝ) * (1 / 2 ^ 40) ≤ 1 / 10 ^ 7 := by norm_num
  nlinarith

lemma y_abs_le {ρ : Type} [FReal ρ] {V : VModel ρ} {U : ℝ} (hfp : Admissible V U) {x y : ℝ}
    (hx : |x| ≤ 1) {m j : ℕ} (hm : m ≤ 99) (he : |y - P j x| ≤ B U m) :
    |y| ≤ 1 + 1 / 10 ^ 6 := by
  have h1 := P_abs_le x hx j
  have h2 := B_tiny hfp hm
  have h3 : y = (y - P j x) + P j x := by ring
  rw [h3]
  have := abs_add_le (y - P j x) (P j x)
  have : (1 : ℝ) / 10 ^ 7 ≤ 1 / 10 ^ 6 := by norm_num
  linarith

/-- The initial value `real(1)` has a value within `ε0` of `1`. -/
lemma round_one {ρ : Type} [FReal ρ] (V : VModel ρ) {U : ℝ} (hfp : Admissible V U) :
    ∃ y0, V.val (FReal.ofInt 1) y0 ∧ |y0 - 1| ≤ ε0 U := by
  have h1 : |(1 : ℝ)| ≤ 1 := by simp
  obtain ⟨y0, hv, he⟩ := V.ofInt_err 1 (by norm_num) (by simpa using le_M hfp h1 (by norm_num))
  simp only [Int.cast_one] at he
  obtain ⟨d, hd, hdb⟩ := decomp hfp he (abs_nonneg _) h1
  refine ⟨y0, hv, ?_⟩
  rw [hd, add_sub_cancel_left]
  unfold ε0
  linarith

lemma Inv_zero (V : VModel K.R8) {U : ℝ} (hfp : Admissible V U) {xv : K.R8} {x : ℝ}
    (hxv : V.val xv x) : Inv V U xv x 0 := by
  obtain ⟨y0, h0v, h0⟩ := round_one V hfp
  have hB : B U 0 = ε0 U := by simp [B]
  have hP0 : P 0 x = 1 := rfl
  have hP1 : P 1 x = x := rfl
  refine ⟨y0, x, h0v, hxv, ?_, ?_, ?_⟩
  · rw [hP0, hP1, sub_self]
    simp only [T, Nat.cast_zero, zero_mul, add_zero]
    rw [← sq_abs (y0 - 1)]
    have := pow_le_pow_left₀ (abs_nonneg _) h0 2
    nlinarith
  · rw [hP1, sub_self, hB, abs_zero]; unfold ε0; linarith [E_nonneg hfp.1, hfp.1]
  · rw [hP0, hB]; exact h0

lemma Inv_succ (V : VModel K.R8) (I : IntExact K.I4 (2 ^ 31)) {U : ℝ} (hfp : Admissible V U)
    {xv : K.R8} {x : ℝ} (hxv : V.val xv x) (hx : |x| ≤ 1) {m : ℕ} (hm : m ≤ 98)
    (h : Inv V U xv x m) : Inv V U xv x (m + 1) := by
  obtain ⟨y0, y1, h0v, h1v, hT, h1, h0⟩ := h
  have hR : 0 ≤ R U := by unfold R; linarith [hfp.1]
  have hε : 0 ≤ ε0 U := by unfold ε0; linarith [E_nonneg hfp.1, hfp.1]
  have hm0 : (0 : ℝ) ≤ m := m.cast_nonneg
  have hy1 := y_abs_le hfp hx (by omega) h1
  have hy0 := y_abs_le hfp hx (by omega) h0
  obtain ⟨c, hcv, hr⟩ := step_err V hfp I (m + 2) (by omega) (by omega) hxv h0v h1v hx hy0 hy1
  have hcast : (((m + 2 : ℕ) : ℤ)) = (m : ℤ) + 2 := by push_cast; ring
  rw [hcast] at hcv
  have he : ((2 * ((m + 2 : ℕ) : ℝ) - 1) * x * y1 - (((m + 2 : ℕ) : ℝ) - 1) * y0) / ((m + 2 : ℕ) : ℝ)
      = ((2 * m + 3) * x * y1 - (m + 1) * y0) / (m + 2) := by push_cast; ring_nf
  rw [he, show 18 * U = R U from rfl] at hr
  set r := c - ((2 * m + 3) * x * y1 - (m + 1) * y0) / (m + 2) with hr_def
  set w := W x (y1 - P (m + 1) x) (y0 - P m x) m with hw
  set A := w - x * (y1 - P (m + 1) x) with hA_def
  have he2 : c - P (m + 2) x = x * (y1 - P (m + 1) x) + A + r := by
    simp only [hA_def, hw, hr_def, W, P_succ_succ]
    ring
  have hC : 0 ≤ (1 - x ^ 2) * (y1 - P (m + 1) x) ^ 2 := by
    have : x ^ 2 ≤ 1 := (sq_le_one_iff_abs_le_one x).mpr hx
    have := sq_nonneg (y1 - P (m + 1) x)
    nlinarith
  have hAC : A ^ 2 + (1 - x ^ 2) * (y1 - P (m + 1) x) ^ 2 ≤ (ε0 U + m * R U) ^ 2 := by
    have := (T_W_le x (y1 - P (m + 1) x) (y0 - P m x) m).trans hT
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
  have hxe : |x * (y1 - P (m + 1) x)| ≤ B U m := by
    rw [abs_mul]
    calc |x| * |y1 - P (m + 1) x| ≤ 1 * |y1 - P (m + 1) x| :=
          mul_le_mul_of_nonneg_right hx (abs_nonneg _)
      _ ≤ B U m := by rw [one_mul]; exact h1
  have hB : B U (m + 1) = B U m + ε0 U + (m + 1) * R U := by
    unfold B; push_cast; ring
  have hmR : 0 ≤ ((m : ℝ) + 1) * R U := by positivity
  refine ⟨y1, c, h1v, hcv, ?_, ?_, ?_⟩
  · rw [T_eq, he2]
    push_cast
    nlinarith
  · rw [he2, hB]
    have := abs_add_le (x * (y1 - P (m + 1) x) + A) r
    have := abs_add_le (x * (y1 - P (m + 1) x)) A
    linarith
  · rw [hB]
    linarith

lemma Inv_all (V : VModel K.R8) (I : IntExact K.I4 (2 ^ 31)) {U : ℝ} (hfp : Admissible V U)
    {xv : K.R8} {x : ℝ} (hxv : V.val xv x) (hx : |x| ≤ 1) :
    ∀ m : ℕ, m ≤ 99 → Inv V U xv x m
  | 0, _ => Inv_zero V hfp hxv
  | m + 1, hm => Inv_succ V I hfp hxv hx (by omega) (Inv_all V I hfp hxv hx m (by omega))

/-! ### Rounding error -/

/-- **Rounding.** In every admissible `VModel` (unit roundoff at most `U`, no
guard digit needed) with exact `integer(4)`, for `0 ≤ n ≤ 100` and an input
with value `|x| ≤ 1`, the generated `legendre_poly` has a value within
`89201 U` of Mathlib's `legendreP n x`. -/
theorem legendre_rounding (V : VModel K.R8) (I : IntExact K.I4 (2 ^ 31)) {U : ℝ}
    (hfp : Admissible V U) (n : ℕ) (hn : n ≤ 100) {xv : K.R8} {x : ℝ} (hxv : V.val xv x)
    (hx : |x| ≤ 1) :
    ∃ w, V.val (legendre_mod.legendre_poly (K := K) (FInt.ofInt (n : ℤ)) xv) w ∧
      |w - legendreP n x| ≤ 89201 * U := by
  rw [← P_eq_legendreP]
  cases n with
  | zero =>
    have hB0 : ε0 U ≤ 89201 * U := by simpa [B] using B_le hfp.1 (m := 0) (by norm_num)
    obtain ⟨y0, h0v, h0⟩ := round_one V hfp
    refine ⟨y0, ?_, by simpa [P] using h0.trans hB0⟩
    rw [Nat.cast_zero, legendre_zero I (by norm_num) xv]
    exact h0v
  | succ m =>
    have hm : m ≤ 99 := by omega
    obtain ⟨y0, y1, -, h1v, -, h1, -⟩ := Inv_all V I hfp hxv hx m hm
    refine ⟨y1, ?_, h1.trans (B_le hfp.1 hm)⟩
    rw [Nat.cast_succ, legendre_succ I xv m (by omega)]
    exact h1v

#print axioms legendre_rounding

end LegendreProof

open LegendreProof

/-! ### Exact arithmetic -/

/-- The generated template `legendre_poly` at `proofKinds` and an `FPModel`
(both real kinds use `fp`). -/
noncomputable def legendre_fortran (fp : FPModel) (n : ℤ) (x : ℝ) : ℝ :=
  @legendre_mod.legendre_poly proofKinds (FKinds.ofModel fp fp) n x

/-- At an `FPModel` it is the same function as at `fp.toNoGuard`. -/
theorem legendre_fortran_eq (fp : FPModel) (n : ℤ) (x : ℝ) :
    legendre_fortran fp n x = legendre_ng fp.toNoGuard n x := rfl

/-- The generated template evaluated in exact real arithmetic. -/
noncomputable def legendre_exact (n : ℤ) (x : ℝ) : ℝ := legendre_fortran (FPModel.exact) n x

lemma y_exact (x : ℝ) : ∀ m : ℕ,
    @yK proofKinds (FKinds.ofModel FPModel.exact FPModel.exact) x m = P m x
  | 0 => by show ((1 : ℤ) : ℝ) = P 0 x; simp [P]
  | 1 => rfl
  | m + 2 => by
    have h0 := y_exact x m
    have h1 := y_exact x (m + 1)
    show ((((2 * ((m : ℤ) + 2) - 1 : ℤ) : ℝ) * x *
        @yK proofKinds (FKinds.ofModel FPModel.exact FPModel.exact) x (m + 1) -
        (((m : ℤ) + 2 - 1 : ℤ) : ℝ) * @yK proofKinds (FKinds.ofModel FPModel.exact FPModel.exact) x m)
      / (((m : ℤ) + 2 : ℤ) : ℝ)) = _
    rw [h0, h1, P_succ_succ]
    simp only [W]
    push_cast
    ring_nf

/-- **Exact step.** In exact arithmetic the generated code computes Mathlib's
Legendre polynomial `P_n(x)` for every `n ≥ 0` and every real `x`. -/
theorem legendre_exact_eq (n : ℕ) (x : ℝ) : legendre_exact n x = legendreP n x := by
  rw [← P_eq_legendreP, legendre_exact, legendre_fortran]
  cases n with
  | zero =>
    exact (@legendre_zero proofKinds (FKinds.ofModel FPModel.exact FPModel.exact) 1
      (intExact_int 1) (by norm_num) x).trans (y_exact x 0)
  | succ m =>
    push_cast
    exact (@legendre_succ proofKinds (FKinds.ofModel FPModel.exact FPModel.exact) ((m : ℤ) + 3)
      (intExact_int _) x m le_rfl).trans (y_exact x (m + 1))

#print axioms legendre_exact_eq

/-! ### Combined: accuracy against Mathlib's Legendre polynomial -/

/-- Accuracy of the shared `legendre_poly` in every admissible arithmetic
without a guard digit. -/
theorem legendre_ng_correct : LegendreSpecNoGuard legendre_ng := by
  intro fp U hU0 hU hu hη hM n x hxF hn0 hn100 hx
  obtain ⟨k, rfl⟩ := Int.eq_ofNat_of_zero_le hn0
  obtain ⟨w, ⟨hw, -⟩, h⟩ := @legendre_rounding proofKinds (FKinds.ofNoGuard fp fp)
    (VModel.ofNoGuard fp) (intExact_int _) U ⟨hU0, hU, hu, hη, hM⟩ k (by omega) x x
    ⟨rfl, hxF⟩ hx
  rw [legendre_ng, show ((k : ℤ)).toNat = k by simp]
  rw [← hw] at h
  exact h

#print axioms legendre_ng_correct

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
of `ℝ` with range `M = 2^10` is an admissible `FPModel`. -/
example : ∃ fp : FPModel, fp.u ≤ 1 / 2 ^ 53 ∧ fp.η ≤ 1 / 2 ^ 1000 ∧ 2 ^ 10 ≤ fp.M ∧
    (1 : ℝ) ∈ fp.F :=
  ⟨FPModel.exact (2 ^ 10), by simp [FPModel.exact], by simp [FPModel.exact], le_refl _, trivial⟩
