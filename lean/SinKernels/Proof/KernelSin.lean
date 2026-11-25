import SinKernels.Generated
import LFortran.Arith.Model
import LFortran.ErrorProp
import LFortran.SinTaylor
import Mathlib.Analysis.SpecialFunctions.Trigonometric.Bounds
import Mathlib.Analysis.Real.Pi.Bounds

/-!
# `kernel_sin`: exact arithmetic, rounding error, and the combination

* `kernel_sin_exact_eq`: in exact arithmetic the generated template computes
  `x · S(x²)`, the polynomial with the literal coefficients of the Fortran
  source (`Sq`).
* `kernel_sin_approx`: for `|x| ≤ π/2`, `|x S(x²) - sin x| ≤ 4.6·10^-16 |x|`.
  The Taylor polynomial `T_11` of `sin x / x` has a remainder below
  `10^-18` (`sin_taylor_poly`), and `|S(z) - T_11(z)| ≤ 4.5·10^-16` on
  `0 ≤ z ≤ 1.571²` is checked by the kernel (`PolyBound.check`); the bound
  also holds slightly beyond `π/2` (`kernel_sin_approx_wide`).
* `kernel_sin_rounding`: in every `NoGuardModel` (so every `FPModel`) with
  unit roundoff `≤ U` and underflow error `≤ H` (`U, H ≤ 2^-20`), for
  floating-point `|x| ≤ 1.6`, the computed value differs from the exact one by
  at most `|x| (6.14 U + 2107 H) + H`.
* `kernel_sin_correct`: hence `|computed - sin x| ≤ |x| (4.6·10^-16 + 6.14 U +
  2107 H) + H` for `|x| ≤ π/2`.
-/

open LFortran PolyBound Real

namespace SinKernelsProof

/-- The coefficients of `S(z)`, the literals of `kernel_sin`, lowest degree first. -/
def Sq : List ℚ := [1, -3002399751580165 / 2 ^ 54, 2401919801243361 / 2 ^ 58,
  -7320136529120817 / 2 ^ 65, 50834275503869 / 2 ^ 64, -7571455092797619 / 2 ^ 78,
  3103945121149347 / 2 ^ 84, -1822480071209569 / 2 ^ 91]

/-- `S(z) - T_11(z)`. -/
def Qq : List ℚ := addL Sq (scaleL (-1) (sinT 11))

theorem Qq_bound : check Qq 0 (2468041 / 10 ^ 6) (45 / 10 ^ 17) 12 = true := by
  decide +kernel

/-- The generated `kernel_sin` in a `NoGuardModel`. -/
noncomputable def kernel_sin_ng (m : NoGuardModel) (x : ℝ) : ℝ :=
  @sin_kernels.kernel_sin proofKinds (FKinds.ofNoGuard m m) x

/-- The generated `kernel_sin` in exact arithmetic. -/
noncomputable def kernel_sin_exact (x : ℝ) : ℝ :=
  @sin_kernels.kernel_sin proofKinds (FKinds.ofModel FPModel.exact FPModel.exact) x

/-! ### Exact arithmetic -/

theorem kernel_sin_exact_eq (x : ℝ) : kernel_sin_exact x = x * evalR Sq (x ^ 2) := by
  simp [kernel_sin_exact, sin_kernels.kernel_sin, Sq, evalR]
  left
  ring

/-- **Approximation error**, on `|x| ≤ 1.571`, slightly beyond `π/2` (range
reduction in floating point can land there). -/
theorem kernel_sin_approx_wide {x : ℝ} (hx : |x| ≤ 1571 / 1000) :
    |kernel_sin_exact x - sin x| ≤ 46 / 10 ^ 17 * |x| := by
  have hx2 : |x| ≤ 2 := by linarith
  have hz0 : (0 : ℝ) ≤ x ^ 2 := sq_nonneg x
  have hz1 : x ^ 2 ≤ ((2468041 / 10 ^ 6 : ℚ) : ℝ) := by
    have : x ^ 2 ≤ (1571 / 1000) ^ 2 := by
      rw [← sq_abs]; exact pow_le_pow_left₀ (abs_nonneg x) hx 2
    push_cast; linarith
  have hQ := check_sound Qq _ 12 0 _ Qq_bound (x ^ 2) (by simpa using hz0) hz1
  rw [Qq, evalR_addL, evalR_scaleL] at hQ
  have hT := sin_taylor_poly 11 hx2
  have hrem : |x| ^ (2 * 11 + 1) / ((2 * 11 + 1).factorial : ℝ) ≤ 1 / 10 ^ 18 * |x| := by
    have h1 : |x| ^ 22 ≤ (1571 / 1000 : ℝ) ^ 22 :=
      pow_le_pow_left₀ (abs_nonneg x) hx 22
    rw [show 2 * 11 + 1 = 23 by rfl, pow_succ, div_le_iff₀ (by positivity)]
    have h2 : (1571 / 1000 : ℝ) ^ 22 ≤ 1 / 10 ^ 18 * (Nat.factorial 23 : ℝ) := by
      rw [Nat.factorial]; norm_num [Nat.factorial]
    have := mul_le_mul_of_nonneg_right (h1.trans h2) (abs_nonneg x)
    linarith
  rw [kernel_sin_exact_eq]
  have e : x * evalR Sq (x ^ 2) - sin x
      = x * (evalR Sq (x ^ 2) + -1 * evalR (sinT 11) (x ^ 2))
        - (sin x - x * evalR (sinT 11) (x ^ 2)) := by ring
  rw [e]
  calc _ ≤ |x * (evalR Sq (x ^ 2) + -1 * evalR (sinT 11) (x ^ 2))|
        + |sin x - x * evalR (sinT 11) (x ^ 2)| := abs_sub _ _
    _ ≤ |x| * (45 / 10 ^ 17) + 1 / 10 ^ 18 * |x| := by
        rw [abs_mul]
        gcongr
        · have := hQ; push_cast at this; exact this
        · exact hT.trans hrem
    _ ≤ 46 / 10 ^ 17 * |x| := by nlinarith [abs_nonneg x]

/-- **Approximation error** on `|x| ≤ π/2`. -/
theorem kernel_sin_approx {x : ℝ} (hx : |x| ≤ π / 2) :
    |kernel_sin_exact x - sin x| ≤ 46 / 10 ^ 17 * |x| :=
  kernel_sin_approx_wide (by linarith [pi_lt_d20])

/-- Relative form: `kernel_sin` is accurate to `10^-15` relative to `sin x`. -/
theorem kernel_sin_approx_rel {x : ℝ} (hx : |x| ≤ π / 2) :
    |kernel_sin_exact x - sin x| ≤ 1 / 10 ^ 15 * |sin x| := by
  have h := kernel_sin_approx hx
  have hs : 2 / π * |x| ≤ |sin x| := by
    rcases le_total 0 x with h0 | h0
    · rw [abs_of_nonneg h0, abs_of_nonneg (sin_nonneg_of_nonneg_of_le_pi h0 (by
        rw [abs_of_nonneg h0] at hx; linarith [pi_pos]))]
      exact mul_le_sin h0 (by rwa [abs_of_nonneg h0] at hx)
    · have h1 : 0 ≤ -x := by linarith
      have h2 : -x ≤ π / 2 := by rwa [abs_of_nonpos h0] at hx
      rw [abs_of_nonpos h0, ← abs_neg, ← sin_neg, abs_of_nonneg (sin_nonneg_of_nonneg_of_le_pi h1 (by
        linarith [pi_pos]))]
      exact mul_le_sin h1 h2
  have hpi : π < 315 / 100 := by linarith [pi_lt_d2]
  have hpi0 : 0 < π := pi_pos
  have : |x| ≤ π / 2 * |sin x| := by
    have := mul_le_mul_of_nonneg_left hs (by positivity : (0 : ℝ) ≤ π / 2)
    rwa [← mul_assoc, show π / 2 * (2 / π) = 1 by field_simp, one_mul] at this
  have : π / 2 * |sin x| ≤ 315 / 200 * |sin x| :=
    mul_le_mul_of_nonneg_right (by linarith) (abs_nonneg _)
  nlinarith [abs_nonneg x, abs_nonneg (sin x)]

/-! ### Rounding error -/

/-- **Rounding error.** -/
theorem kernel_sin_rounding (m : NoGuardModel) {U H : ℝ} (hS : Small m U H)
    (hM : 4 ≤ m.M) {x : ℝ} (hxF : x ∈ m.F) (hx : |x| ≤ 8 / 5) :
    |kernel_sin_ng m x - kernel_sin_exact x| ≤ |x| * (614 / 100 * U + 2107 * H) + H := by
  have hx : Close m U H x x 0 0 (8 / 5) := Close.input hxF hx
  have hz := (Close.mul hS le_rfl le_rfl le_rfl le_rfl hx hx (by norm_num [εU]; linarith)).mono hS (α' := (64 / 25 : ℝ)) (κ' := (1 : ℝ)) (T' := (64 / 25 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have h8 := (Close.round hS (c := ((-1822480071209569 : ℤ) : ℝ) * (2 : ℝ) ^ (-91 : ℤ)) (C := (368047 / 500000000000000000 : ℝ)) (by norm_num [abs_of_neg, abs_of_pos]) (by linarith)).mono hS (α' := (7361 / 10000000000000000 : ℝ)) (κ' := (1 : ℝ)) (T' := (7361 / 10000000000000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have p7 := (Close.mul hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) hz h8 (by norm_num [εU]; linarith)).mono hS (α' := (4883 / 1000000000 : ℝ)) (κ' := (3561 / 1000 : ℝ)) (T' := (377 / 200000000000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have l7 := (Close.round hS (c := ((3103945121149347 : ℤ) : ℝ) * (2 : ℝ) ^ (-84 : ℤ)) (C := (160471 / 1000000000000000 : ℝ)) (by norm_num [abs_of_neg, abs_of_pos]) (by linarith)).mono hS (α' := (321 / 2000000000000 : ℝ)) (κ' := (1 : ℝ)) (T' := (321 / 2000000000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have h7 := (Close.add hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) l7 p7 (by norm_num [εU]; linarith)).mono hS (α' := (4617 / 500000000 : ℝ)) (κ' := (5561 / 1000 : ℝ)) (T' := (203 / 1250000000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have p6 := (Close.mul hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) hz h7 (by norm_num [εU]; linarith)).mono hS (α' := (127 / 2500000 : ℝ)) (κ' := (381 / 25 : ℝ)) (T' := (2079 / 5000000000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have l6 := (Close.round hS (c := ((-7571455092797619 : ℤ) : ℝ) * (2 : ℝ) ^ (-78 : ℤ)) (C := (250519 / 10000000000000 : ℝ)) (by norm_num [abs_of_neg, abs_of_pos]) (by linarith)).mono hS (α' := (1253 / 50000000000 : ℝ)) (κ' := (1 : ℝ)) (T' := (1253 / 50000000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have h6 := (Close.add hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) l6 p6 (by norm_num [εU]; linarith)).mono hS (α' := (3317 / 50000000 : ℝ)) (κ' := (431 / 25 : ℝ)) (T' := (637 / 25000000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have p5 := (Close.mul hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) hz h6 (by norm_num [εU]; linarith)).mono hS (α' := (1271 / 5000000 : ℝ)) (κ' := (2257 / 50 : ℝ)) (T' := (6523 / 100000000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have l5 := (Close.round hS (c := ((50834275503869 : ℤ) : ℝ) * (2 : ℝ) ^ (-64 : ℤ)) (C := (137787 / 50000000000 : ℝ)) (by norm_num [abs_of_neg, abs_of_pos]) (by linarith)).mono hS (α' := (689 / 250000000 : ℝ)) (κ' := (1 : ℝ)) (T' := (689 / 250000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have h5 := (Close.add hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) l5 p5 (by norm_num [εU]; linarith)).mono hS (α' := (1519 / 5000000 : ℝ)) (κ' := (2357 / 50 : ℝ)) (T' := (1411 / 500000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have p4 := (Close.mul hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) hz h5 (by norm_num [εU]; linarith)).mono hS (α' := (1023 / 1000000 : ℝ)) (κ' := (1217 / 10 : ℝ)) (T' := (289 / 40000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have l4 := (Close.round hS (c := ((-7320136529120817 : ℤ) : ℝ) * (2 : ℝ) ^ (-65 : ℤ)) (C := (198413 / 1000000000 : ℝ)) (by norm_num [abs_of_neg, abs_of_pos]) (by linarith)).mono hS (α' := (397 / 2000000 : ℝ)) (κ' := (1 : ℝ)) (T' := (397 / 2000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have h4 := (Close.add hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) l4 p4 (by norm_num [εU]; linarith)).mono hS (α' := (309 / 200000 : ℝ)) (κ' := (1237 / 10 : ℝ)) (T' := (1029 / 5000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have p3 := (Close.mul hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) hz h4 (by norm_num [εU]; linarith)).mono hS (α' := (5613 / 1000000 : ℝ)) (κ' := (3177 / 10 : ℝ)) (T' := (5269 / 10000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have l3 := (Close.round hS (c := ((2401919801243361 : ℤ) : ℝ) * (2 : ℝ) ^ (-58 : ℤ)) (C := (416667 / 50000000 : ℝ)) (by norm_num [abs_of_neg, abs_of_pos]) (by linarith)).mono hS (α' := (4167 / 500000 : ℝ)) (κ' := (1 : ℝ)) (T' := (4167 / 500000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have h3 := (Close.add hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) l3 p3 (by norm_num [εU]; linarith)).mono hS (α' := (289 / 12500 : ℝ)) (κ' := (3197 / 10 : ℝ)) (T' := (8861 / 1000000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have p2 := (Close.mul hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) hz h3 (by norm_num [εU]; linarith)).mono hS (α' := (531 / 5000 : ℝ)) (κ' := (1639 / 2 : ℝ)) (T' := (2269 / 100000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have l2 := (Close.round hS (c := ((-3002399751580165 : ℤ) : ℝ) * (2 : ℝ) ^ (-54 : ℤ)) (C := (166667 / 1000000 : ℝ)) (by norm_num [abs_of_neg, abs_of_pos]) (by linarith)).mono hS (α' := (1667 / 10000 : ℝ)) (κ' := (1 : ℝ)) (T' := (1667 / 10000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have h2 := (Close.add hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) l2 p2 (by norm_num [εU]; linarith)).mono hS (α' := (4631 / 10000 : ℝ)) (κ' := (1643 / 2 : ℝ)) (T' := (947 / 5000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have p1 := (Close.mul hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) hz h2 (by norm_num [εU]; linarith)).mono hS (α' := (54 / 25 : ℝ)) (κ' := (2105 : ℝ)) (T' := (4849 / 10000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have l1 := (Close.round hS (c := ((1 : ℤ) : ℝ)) (C := (1 : ℝ)) (by norm_num [abs_of_neg, abs_of_pos]) (by linarith)).mono hS (α' := (1 : ℝ)) (κ' := (1 : ℝ)) (T' := (1 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have h1 := (Close.add hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) l1 p1 (by norm_num [εU]; linarith)).mono hS (α' := (4647 / 1000 : ℝ)) (κ' := (2107 : ℝ)) (T' := (297 / 200 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])

  have hfin := Close.mul_input hS (by norm_num) (by norm_num) hxF h1
    (by rw [show εU = 1 / 2 ^ 20 from rfl]; nlinarith [abs_nonneg x])
  refine hfin.2.trans ?_
  have hU := hS.U0
  have hH := hS.H0
  have : ((4647 / 1000 : ℝ) + 297 / 200 + (4647 / 1000 + 2107) * εU) ≤ 614 / 100 := by
    norm_num [εU]
  have := mul_le_mul_of_nonneg_right this hU
  have := abs_nonneg x
  nlinarith

#print axioms kernel_sin_rounding

/-! ### Combined -/

/-- **Accuracy of `kernel_sin`** against Mathlib's `Real.sin`. -/
theorem kernel_sin_correct (m : NoGuardModel) {U H : ℝ} (hS : Small m U H)
    (hM : 4 ≤ m.M) {x : ℝ} (hxF : x ∈ m.F) (hx : |x| ≤ π / 2) :
    |kernel_sin_ng m x - sin x| ≤ |x| * (46 / 10 ^ 17 + 614 / 100 * U + 2107 * H) + H := by
  have hx' : |x| ≤ 8 / 5 := by linarith [pi_lt_d2]
  have h1 := kernel_sin_rounding m hS hM hxF hx'
  have h2 := kernel_sin_approx hx
  have := abs_sub_le (kernel_sin_ng m x) (kernel_sin_exact x) (sin x)
  nlinarith [abs_nonneg x]

#print axioms kernel_sin_correct

/-- `kernel_sin_correct` on `|x| ≤ 1.571`. -/
theorem kernel_sin_correct_wide (m : NoGuardModel) {U H : ℝ} (hS : Small m U H)
    (hM : 4 ≤ m.M) {x : ℝ} (hxF : x ∈ m.F) (hx : |x| ≤ 1571 / 1000) :
    |kernel_sin_ng m x - sin x| ≤ |x| * (46 / 10 ^ 17 + 614 / 100 * U + 2107 * H) + H := by
  have h1 := kernel_sin_rounding m hS hM hxF (by linarith)
  have h2 := kernel_sin_approx_wide hx
  have := abs_sub_le (kernel_sin_ng m x) (kernel_sin_exact x) (sin x)
  nlinarith [abs_nonneg x]

end SinKernelsProof
