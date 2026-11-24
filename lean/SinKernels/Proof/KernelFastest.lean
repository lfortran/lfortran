import SinKernels.Generated
import LFortran.Arith.Model
import LFortran.ErrorProp
import LFortran.SinTaylor
import Mathlib.Analysis.SpecialFunctions.Trigonometric.Bounds
import Mathlib.Analysis.Real.Pi.Bounds

/-!
# `kernel_fastest`: approximation, range reduction, rounding

`kernel_fastest(x0)` picks `N = int(x0/π_d + sign(x0)/2)` (`π_d` the binary64
literal for `π`), reduces `r = x0 - N π_d`, negates `r` for odd `N`, and returns
`y (S1 + y² S2)` with the two single-precision literals `S1`, `S2`.

* `poly_approx`: `|c(y) - sin y| ≤ 5.75·10^-3` for `|y| ≤ 1.6`, where
  `c(y) = y (S1 + y² S2)`: the degree-19 Taylor polynomial `T` of `sin`
  (remainder `|y|^21/21!`, bounded by `10^-7`) and the kernel-checked bound
  `|c - T| ≤ 5.7498·10^-3` on `[-1.6, 1.6]` (`PolyBound.check`).
* `reduce_core`: for every integer `N` and every `|r| ≤ 1.6`,
  `|(-1)^N c(r) - sin x0| ≤ 5.75·10^-3 + |r - (x0 - N π)|`, because
  `sin x0 = (-1)^N sin(x0 - N π)` and `sin` is 1-Lipschitz.
* `kernel_fastest_correct`: in every `NoGuardModel` (so every `FPModel`) with
  unit roundoff `≤ U` and underflow error `≤ H`, in which integers up to `X`
  are exact and negation is exact, for floating-point `|x0| ≤ X`:
  `|computed - sin x0| ≤ 5.75·10^-3 + X (19 U + 14 H + 1.3·10^-16)`.
  The last term is `|N| |π_d - π|`, the error of the literal `π_d`.
* `kernel_fastest_exact_correct`: in exact arithmetic (`U = H = 0`),
  `|kernel_fastest x0 - sin x0| ≤ 5.75·10^-3 + 1.3·10^-16 X`.

The computed and the exact-arithmetic run can choose different `N` when
`x0/π_d` is within rounding error of a half-integer; both results are then
close to `sin x0`, which is why the rounding bound is stated against `sin`
directly rather than against the exact-arithmetic value.
-/

open LFortran PolyBound Real

namespace SinKernelsProof

/-- The literal `π_d` (`3.141592653589793_dp`). -/
noncomputable def πd : ℝ := 884279719003555 * (2 ^ 48)⁻¹
/-- The literal `S1` (`0.982396485658623`, single precision). -/
noncomputable def S1 : ℝ := 8240939 * (2 ^ 23)⁻¹
/-- The literal `S2` (`-0.14013802346642243`, single precision). -/
noncomputable def S2 : ℝ := -(1175563 * (2 ^ 23)⁻¹)

/-- The polynomial of `kernel_fastest`. -/
noncomputable def cpoly (y : ℝ) : ℝ := y * (S1 + y * y * S2)

/-! ### Approximation: `c(y)` against `sin y` -/

/-- `c(y) - T_19(y)` in `y`. -/
def Rq : List ℚ :=
  addL (oddList [8240939 / 2 ^ 23, -1175563 / 2 ^ 23]) (scaleL (-1) (oddList (sinT 10)))

theorem Rq_bound : check Rq (-8 / 5) (8 / 5) (57498 / 10 ^ 7) 14 = true := by
  decide +kernel

theorem poly_approx {y : ℝ} (hy : |y| ≤ 8 / 5) : |cpoly y - sin y| ≤ 575 / 10 ^ 5 := by
  have hy2 : |y| ≤ 2 := by linarith
  have h1 := abs_le.1 hy
  have hR := check_sound Rq _ 14 _ _ Rq_bound y (by push_cast; linarith) (by push_cast; linarith)
  rw [Rq, evalR_addL, evalR_scaleL, evalR_oddList, evalR_oddList] at hR
  have hT := sin_taylor_poly 10 hy2
  have hrem : |y| ^ (2 * 10 + 1) / ((2 * 10 + 1).factorial : ℝ) ≤ 1 / 10 ^ 7 := by
    have h1 : |y| ^ 21 ≤ (8 / 5 : ℝ) ^ 21 := pow_le_pow_left₀ (abs_nonneg y) hy 21
    rw [show 2 * 10 + 1 = 21 by rfl, div_le_iff₀ (by positivity)]
    have h2 : (8 / 5 : ℝ) ^ 21 ≤ 1 / 10 ^ 7 * (Nat.factorial 21 : ℝ) := by
      norm_num [Nat.factorial]
    linarith
  have e : cpoly y - sin y = (y * evalR [8240939 / 2 ^ 23, -1175563 / 2 ^ 23] (y ^ 2)
      + -1 * (y * evalR (sinT 10) (y ^ 2))) - (sin y - y * evalR (sinT 10) (y ^ 2)) := by
    simp only [cpoly, evalR, S1, S2]; push_cast; ring
  rw [e]
  have hR' : |y * evalR [8240939 / 2 ^ 23, -1175563 / 2 ^ 23] (y ^ 2)
      + -1 * (y * evalR (sinT 10) (y ^ 2))| ≤ 57498 / 10 ^ 7 := by
    have := hR; push_cast at this ⊢; exact this
  have := abs_sub (y * evalR [8240939 / 2 ^ 23, -1175563 / 2 ^ 23] (y ^ 2)
      + -1 * (y * evalR (sinT 10) (y ^ 2))) (sin y - y * evalR (sinT 10) (y ^ 2))
  linarith [hT.trans hrem]

/-! ### Range reduction -/

theorem reduce_core (N : ℤ) (x0 r : ℝ) (hr : |r| ≤ 8 / 5) :
    |(-1) ^ N * cpoly r - sin x0| ≤ 575 / 10 ^ 5 + |r - (x0 - N * π)| := by
  have hs : sin x0 = (-1) ^ N * sin (x0 - N * π) := by
    rw [sin_sub_int_mul_pi, ← mul_assoc, ← mul_zpow, show (-1 : ℝ) * -1 = 1 by norm_num,
      one_zpow, one_mul]
  have habs : |((-1 : ℝ) ^ N)| = 1 := by simp [abs_zpow]
  rw [hs, ← mul_sub, abs_mul, habs, one_mul]
  have h1 := poly_approx hr
  have h2 := abs_sin_sub_sin_le r (x0 - N * π)
  have h3 := abs_sub_le (cpoly r) (sin r) (sin (x0 - N * π))
  linarith

/-- Negating `r` for odd `N` multiplies `c(r)` by `(-1)^N`. -/
theorem parity (N : ℤ) (r : ℝ) :
    cpoly (if N.fmod 2 = 1 then -r else r) = (-1) ^ N * cpoly r := by
  rw [Int.fmod_eq_emod_of_nonneg _ (by norm_num)]
  rcases Int.emod_two_eq_zero_or_one N with h | h
  · rw [if_neg (by omega), Even.neg_one_zpow (Int.even_iff.2 h), one_mul]
  · rw [if_pos h, Odd.neg_one_zpow (Int.odd_iff.2 h)]
    simp only [cpoly]; ring

/-! ### The generated code -/

/-- The generated `kernel_fastest` in a `NoGuardModel`. -/
noncomputable def kernel_fastest_ng (m : NoGuardModel) (x0 : ℝ) : ℝ :=
  @sin_kernels.kernel_fastest proofKinds (FKinds.ofNoGuard m m) x0

/-- The generated `kernel_fastest` in exact arithmetic. -/
noncomputable def kernel_fastest_exact (x0 : ℝ) : ℝ :=
  @sin_kernels.kernel_fastest proofKinds (FKinds.ofModel FPModel.exact FPModel.exact) x0

/-- What the generated code computes, step by step. -/
theorem kernel_fastest_ng_eq (m : NoGuardModel) (x0 : ℝ) :
    kernel_fastest_ng m x0 =
      let PI := m.round πd
      let N := realTrunc (m.add (m.div x0 PI)
        (m.mul (m.round 2⁻¹) (if x0 < 0 then -|m.round 1| else |m.round 1|)))
      let r := m.sub x0 (m.mul (m.round N) PI)
      let y := if (realTrunc (m.round N)).fmod 2 = 1 then -r else r
      m.mul y (m.add (m.round S1) (m.mul (m.mul y y) (m.round S2))) := by
  simp only [kernel_fastest_ng, sin_kernels.kernel_fastest]
  simp [πd, S1, S2]
  split_ifs <;> rfl

/-! ### Rounding: the computed value against `sin x0` -/

/-- The assumptions on the arithmetic: unit roundoff `≤ U`, underflow error
`≤ H`, both small relative to the input range `X ≥ 1`; range `M ≥ 5X`;
integers up to `X` are floating-point numbers (`round n = n`), and so are
the negatives of floating-point numbers. Every binary floating-point format
satisfies the last two for `X ≤ 2^p`. -/
structure FastestArith (m : NoGuardModel) (U H X : ℝ) : Prop where
  small : Small m U H
  X1 : 1 ≤ X
  XU : X * U ≤ εU
  XH : X * H ≤ εU
  M : 5 * X ≤ m.M
  int_exact : ∀ n : ℤ, |(n : ℝ)| ≤ X → m.round n = n
  neg_mem : ∀ v ∈ m.F, -v ∈ m.F

set_option maxHeartbeats 2000000 in
/-- **Range reduction in floating point.** The computed `N` is an integer
that the arithmetic represents exactly, and the computed reduced argument `r`
is within `X (11 U + 3 H + 1.3·10^-16)` of `x0 - N π`, with `|r| ≤ 1.6`. -/
theorem reduction (m : NoGuardModel) {U H X : ℝ} (A : FastestArith m U H X) {x0 : ℝ}
    (hxF : x0 ∈ m.F) (hx : |x0| ≤ X) :
    let PI := m.round πd
    let N := realTrunc (m.add (m.div x0 PI)
      (m.mul (m.round 2⁻¹) (if x0 < 0 then -|m.round 1| else |m.round 1|)))
    let r := m.sub x0 (m.mul (m.round N) PI)
    m.round N = N ∧ r ∈ m.F ∧ |r| ≤ 8 / 5 ∧
      |r - (x0 - N * π)| ≤ X * (11 * U + 3 * H + 13 / 10 ^ 17) := by
  intro PI N r
  obtain ⟨hS, hX1, hXU, hXH, hM, hint, hneg⟩ := A
  have hU0 := hS.U0
  have hH0 := hS.H0
  have hη := hS.η
  have hε : εU = 1 / 2 ^ 20 := rfl
  have hU : U ≤ X * U := le_mul_of_one_le_left hU0 hX1
  have hH : H ≤ X * H := le_mul_of_one_le_left hH0 hX1
  have hX0 : 0 ≤ X := by linarith
  have hx0 : 0 ≤ |x0| := abs_nonneg x0
  -- the literal π_d
  have hπd1 : (31415 / 10000 : ℝ) ≤ πd := by norm_num [πd]
  have hπd2 : πd ≤ 31416 / 10000 := by norm_num [πd]
  have hπ : |πd - π| ≤ 13 / 10 ^ 17 := by
    have h1 := pi_gt_d20
    have h2 := pi_lt_d20
    have e : πd = 884279719003555 / 2 ^ 48 := by norm_num [πd]
    rw [e, abs_le]
    norm_num at h1 h2 ⊢
    constructor <;> linarith
  have hPIm : |πd| ≤ m.M := by rw [abs_of_pos (by linarith)]; linarith
  have hPIF : PI ∈ m.F := m.round_mem _ hPIm
  have hPIe : |PI - πd| ≤ 31416 / 10000 * U + H := by
    have h1 := m.round_err _ hPIm
    have h2 := Close.u_mul_le hS (y := πd) (Y := 31416 / 10000)
      (by rw [abs_of_pos (by linarith)]; exact hπd2)
    linarith
  have hPI1 : (3141 / 1000 : ℝ) ≤ PI := by
    have := (abs_le.1 hPIe).1; linarith
  have hPI2 : PI ≤ 3142 / 1000 := by
    have := (abs_le.1 hPIe).2; linarith
  -- the literal 1 and the sign
  have h1m : |(1 : ℝ)| ≤ m.M := by rw [abs_one]; linarith
  have hONEF : m.round 1 ∈ m.F := m.round_mem 1 h1m
  have hONEe : |m.round 1 - 1| ≤ U + H := by
    have h1 := m.round_err 1 h1m
    have h2 := Close.u_mul_le hS (y := 1) (Y := 1) (by rw [abs_one])
    rw [abs_one] at h1 h2
    linarith
  have hAF : |m.round 1| ∈ m.F := by
    rcases abs_choice (m.round 1) with h | h <;> rw [h]
    exacts [hONEF, hneg _ hONEF]
  have hAe : |(|m.round 1| - 1)| ≤ U + H := by
    have := abs_abs_sub_abs_le_abs_sub (m.round 1) 1
    rw [abs_one] at this; linarith
  set s := if x0 < 0 then -|m.round 1| else |m.round 1| with hs_def
  set σ : ℝ := if x0 < 0 then -1 else 1 with hσ_def
  have hsF : s ∈ m.F := by
    rw [hs_def]; split_ifs
    exacts [hneg _ hAF, hAF]
  have hse : |s - σ| ≤ U + H := by
    rw [hs_def, hσ_def]; split_ifs
    · rw [show -|m.round 1| - -1 = -(|m.round 1| - 1) by ring, abs_neg]; exact hAe
    · exact hAe
  have hσ : |σ| = 1 := by rw [hσ_def]; split_ifs <;> simp
  have hs1 : |s| ≤ 101 / 100 := by
    have := abs_sub_abs_le_abs_sub s σ; linarith
  -- the literal 1/2
  have h2m : |(2⁻¹ : ℝ)| ≤ m.M := by norm_num; linarith
  have hHF : m.round 2⁻¹ ∈ m.F := m.round_mem _ h2m
  have hHe : |m.round 2⁻¹ - 2⁻¹| ≤ U + H := by
    have h1 := m.round_err _ h2m
    have h2 := Close.u_mul_le hS (y := 2⁻¹) (Y := 1) (by norm_num)
    linarith
  have hH1 : |m.round 2⁻¹| ≤ 51 / 100 := by
    have := abs_sub_abs_le_abs_sub (m.round 2⁻¹) 2⁻¹
    rw [abs_of_pos (by norm_num : (0 : ℝ) < 2⁻¹)] at this; norm_num at this ⊢; linarith
  -- hs = 1/2 * s
  set hs := m.mul (m.round 2⁻¹) s with hhs
  have hHs : |m.round 2⁻¹ * s| ≤ 1 := by
    rw [abs_mul]; nlinarith [abs_nonneg (m.round 2⁻¹), abs_nonneg s]
  have hhsF : hs ∈ m.F := m.mul_mem _ hHF _ hsF (hHs.trans (by linarith))
  have hhse : |hs - σ / 2| ≤ 3 * U + 3 * H := by
    have h1 := m.mul_err _ hHF _ hsF (hHs.trans (by linarith))
    rw [← hhs] at h1
    have h2 := Close.u_mul_le hS hHs
    have e : m.round 2⁻¹ * s - σ / 2 = m.round 2⁻¹ * (s - σ) + (m.round 2⁻¹ - 2⁻¹) * σ := by ring
    have h3 : |m.round 2⁻¹ * s - σ / 2| ≤ 51 / 100 * (U + H) + (U + H) := by
      rw [e]
      calc _ ≤ |m.round 2⁻¹ * (s - σ)| + |(m.round 2⁻¹ - 2⁻¹) * σ| := abs_add_le _ _
        _ ≤ 51 / 100 * (U + H) + (U + H) := by
          rw [abs_mul, abs_mul, hσ, mul_one]
          gcongr
    have := abs_sub_le hs (m.round 2⁻¹ * s) (σ / 2)
    linarith
  have hhs1 : |hs| ≤ 1 := by
    have := abs_sub_abs_le_abs_sub hs (σ / 2)
    rw [abs_div, hσ] at this
    norm_num at this
    have : 3 * U + 3 * H ≤ 6 * εU := by linarith
    norm_num [hε] at this
    linarith
  -- q = x0 / PI
  have hPI0 : 0 < PI := by linarith
  have hπd0 : 0 < πd := by linarith
  have hq1 : |x0 / PI| ≤ X / 3 := by
    rw [abs_div, abs_of_pos hPI0, div_le_div_iff₀ hPI0 (by norm_num)]
    have := mul_le_mul_of_nonneg_left hPI1 hX0
    linarith
  have hqM : |x0 / PI| ≤ m.M := hq1.trans (by linarith)
  set q := m.div x0 PI with hq
  have hqF : q ∈ m.F := m.div_mem _ hxF _ hPIF hPI0.ne' hqM
  have hqe : |q - x0 / πd| ≤ 2 * X * U + 2 * X * H := by
    have h1 := m.div_err _ hxF _ hPIF hPI0.ne' hqM
    rw [← hq] at h1
    have h2 := Close.u_mul_le hS hq1
    have e : x0 / PI - x0 / πd = x0 / (PI * πd) * (πd - PI) := by field_simp
    have hP9 : 9 ≤ PI * πd := by
      have := mul_le_mul hPI1 hπd1 (by norm_num) (by linarith)
      linarith
    have h3 : |x0 / PI - x0 / πd| ≤ X / 9 * (31416 / 10000 * U + H) := by
      rw [e, abs_mul, abs_sub_comm]
      apply mul_le_mul _ hPIe (abs_nonneg _) (by linarith)
      rw [abs_div, abs_of_pos (mul_pos hPI0 hπd0), div_le_div_iff₀ (mul_pos hPI0 hπd0)
        (by norm_num)]
      have := mul_le_mul_of_nonneg_left hP9 hX0
      linarith
    have := abs_sub_le q (x0 / PI) (x0 / πd)
    linarith
  have hq2 : |q| ≤ X / 3 + 4 * εU := by
    have := abs_sub_abs_le_abs_sub q (x0 / PI)
    have h1 := m.div_err _ hxF _ hPIF hPI0.ne' hqM
    rw [← hq] at h1
    have h2 := Close.u_mul_le hS hq1
    have : X * U ≤ εU := hXU
    linarith
  -- t = q + hs
  have htM : |q| + |hs| ≤ 2 * X := by norm_num [hε] at hq2; linarith
  have htF : m.add q hs ∈ m.F :=
    m.add_mem _ hqF _ hhsF (((abs_add_le _ _).trans htM).trans (by linarith))
  have hte : |m.add q hs - (x0 / πd + σ / 2)| ≤ 7 * X * U + 7 * X * H := by
    have h1 := m.add_err _ hqF _ hhsF (((abs_add_le _ _).trans htM).trans (by linarith))
    have h2 := Close.u_mul_le hS (y := |q| + |hs|) (Y := 2 * X)
      (by rw [abs_of_nonneg (by positivity)]; exact htM)
    rw [abs_of_nonneg (by positivity : (0 : ℝ) ≤ |q| + |hs|)] at h2
    have e : m.add q hs - (x0 / πd + σ / 2) = (m.add q hs - (q + hs)) + (q - x0 / πd)
      + (hs - σ / 2) := by ring
    rw [e]
    have := abs_add_le ((m.add q hs - (q + hs)) + (q - x0 / πd)) (hs - σ / 2)
    have := abs_add_le (m.add q hs - (q + hs)) (q - x0 / πd)
    linarith
  -- N
  set t := m.add q hs with ht
  have hEt : 7 * X * U + 7 * X * H ≤ 14 * εU := by linarith
  have hEt' : 7 * X * U + 7 * X * H ≤ 2 / 10 ^ 5 := by rw [hε] at hEt; linarith
  have hN : |x0 / πd - N| ≤ 1 / 2 + (7 * X * U + 7 * X * H) := by
    have hte' := abs_le.1 hte
    by_cases h0 : x0 < 0
    · have hσ' : σ = -1 := by rw [hσ_def, if_pos h0]
      rw [hσ'] at hte'
      have hx0' : x0 / πd < 0 := div_neg_of_neg_of_pos h0 hπd0
      have htneg : t < 0 := by linarith
      have hNt : (N : ℝ) = ⌈t⌉ := by
        show ((realTrunc t : ℤ) : ℝ) = _; rw [realTrunc_of_neg htneg]
      have c1 := Int.le_ceil t
      have c2 := Int.ceil_lt_add_one t
      rw [hNt, abs_le]; constructor <;> linarith
    · have hσ' : σ = 1 := by rw [hσ_def, if_neg h0]
      rw [hσ'] at hte'
      have hx0' : 0 ≤ x0 / πd := div_nonneg (not_lt.1 h0) hπd0.le
      have htpos : 0 ≤ t := by linarith
      have hNt : (N : ℝ) = ⌊t⌋ := by
        show ((realTrunc t : ℤ) : ℝ) = _; rw [realTrunc_of_nonneg htpos]
      have c1 := Int.floor_le t
      have c2 := Int.lt_floor_add_one t
      rw [hNt, abs_le]; constructor <;> linarith
  have hx0πd : |x0 / πd| ≤ X / 3 := by
    rw [abs_div, abs_of_pos hπd0, div_le_div_iff₀ hπd0 (by norm_num)]
    have := mul_le_mul_of_nonneg_left hπd1 hX0
    linarith
  have hNX : |(N : ℝ)| ≤ X := by
    have := abs_sub_abs_le_abs_sub (N : ℝ) (x0 / πd)
    rw [abs_sub_comm (N : ℝ)] at this
    linarith
  have hnd : m.round N = N := hint N hNX
  refine ⟨hnd, ?_⟩
  -- p = N * PI
  have hNF : (N : ℝ) ∈ m.F := hnd ▸ m.round_mem _ (hNX.trans (by linarith))
  have hpM : |(N : ℝ) * PI| ≤ 3142 / 1000 * X := by
    rw [abs_mul, abs_of_pos hPI0]
    have := mul_le_mul hNX hPI2 hPI0.le hX0
    linarith
  have hpF : m.mul N PI ∈ m.F := m.mul_mem _ hNF _ hPIF (hpM.trans (by linarith))
  have hpe : |m.mul N PI - N * PI| ≤ 3142 / 1000 * X * U + H := by
    have h1 := m.mul_err _ hNF _ hPIF (hpM.trans (by linarith))
    have h2 := Close.u_mul_le hS hpM
    linarith
  have hp1 : |m.mul (N : ℝ) PI| ≤ 3143 / 1000 * X := by
    have := abs_sub_abs_le_abs_sub (m.mul (N : ℝ) PI) (N * PI)
    linarith
  -- r = x0 - p
  have hrM : |x0| + |m.mul (N : ℝ) PI| ≤ 5 * X := by linarith
  have hrF : m.sub x0 (m.mul N PI) ∈ m.F :=
    m.sub_mem _ hxF _ hpF (((abs_sub _ _).trans hrM).trans hM)
  have hre : |m.sub x0 (m.mul N PI) - (x0 - m.mul N PI)| ≤ 4143 / 1000 * X * U + H := by
    have h1 := m.sub_err _ hxF _ hpF (((abs_sub _ _).trans hrM).trans hM)
    have h2 := Close.u_mul_le hS (y := |x0| + |m.mul (N : ℝ) PI|) (Y := 4143 / 1000 * X)
      (by rw [abs_of_nonneg (by positivity)]; linarith)
    rw [abs_of_nonneg (by positivity : (0 : ℝ) ≤ |x0| + |m.mul (N : ℝ) PI|)] at h2
    linarith
  have hr0 : |m.sub x0 (m.mul N PI) - (x0 - N * πd)| ≤ 11 * X * U + 3 * X * H := by
    have e : m.sub x0 (m.mul N PI) - (x0 - N * πd) = (m.sub x0 (m.mul N PI) - (x0 - m.mul N PI))
        - (m.mul N PI - N * PI) - N * (PI - πd) := by ring
    rw [e]
    have := abs_sub (m.sub x0 (m.mul N PI) - (x0 - m.mul N PI) - (m.mul N PI - N * PI))
      (N * (PI - πd))
    have := abs_sub (m.sub x0 (m.mul N PI) - (x0 - m.mul N PI)) (m.mul N PI - N * PI)
    have h4 : |(N : ℝ) * (PI - πd)| ≤ X * (31416 / 10000 * U + H) := by
      rw [abs_mul]; exact mul_le_mul hNX hPIe (abs_nonneg _) hX0
    linarith
  have hr_def : r = m.sub x0 (m.mul N PI) := by
    show m.sub x0 (m.mul (m.round N) PI) = _; rw [hnd]
  rw [hr_def]
  refine ⟨hrF, ?_, ?_⟩
  · have hxN : |x0 - N * πd| ≤ 31416 / 10000 * (1 / 2 + 2 / 10 ^ 5) := by
      have e : x0 - N * πd = πd * (x0 / πd - N) := by field_simp
      rw [e, abs_mul, abs_of_pos hπd0]
      exact mul_le_mul hπd2 (hN.trans (by linarith)) (abs_nonneg _) (by norm_num)
    have := abs_sub_abs_le_abs_sub (m.sub x0 (m.mul N PI)) (x0 - N * πd)
    have : 11 * X * U + 3 * X * H ≤ 14 * εU := by linarith
    norm_num [hε] at this
    linarith
  · have e : m.sub x0 (m.mul N PI) - (x0 - N * π) = (m.sub x0 (m.mul N PI) - (x0 - N * πd))
        + N * (π - πd) := by ring
    rw [e]
    have := abs_add_le (m.sub x0 (m.mul N PI) - (x0 - N * πd)) (N * (π - πd))
    have h4 : |(N : ℝ) * (π - πd)| ≤ X * (13 / 10 ^ 17) := by
      rw [abs_mul, abs_sub_comm]; exact mul_le_mul hNX hπ (abs_nonneg _) hX0
    exact this.trans (le_of_le_of_eq (add_le_add hr0 h4) (by ring))

#print axioms reduction

/-- **Accuracy of `kernel_fastest`** in floating point, against `Real.sin`. -/
theorem kernel_fastest_correct (m : NoGuardModel) {U H X : ℝ} (A : FastestArith m U H X)
    {x0 : ℝ} (hxF : x0 ∈ m.F) (hx : |x0| ≤ X) :
    |kernel_fastest_ng m x0 - sin x0| ≤ 575 / 10 ^ 5 + X * (19 * U + 14 * H + 13 / 10 ^ 17) := by
  have hred := reduction m A hxF hx
  simp only at hred
  obtain ⟨hnd, hrF, hr, her⟩ := hred
  have hS := A.small
  have hneg := A.neg_mem
  have hU0 := hS.U0
  have hH0 := hS.H0
  have hU : U ≤ X * U := le_mul_of_one_le_left hU0 A.X1
  have hH : H ≤ X * H := le_mul_of_one_le_left hH0 A.X1
  have hM : 4 ≤ m.M := by linarith [A.M, A.X1]
  rw [kernel_fastest_ng_eq]
  simp only
  set N := realTrunc (m.add (m.div x0 (m.round πd))
    (m.mul (m.round 2⁻¹) (if x0 < 0 then -|m.round 1| else |m.round 1|)))
  set r := m.sub x0 (m.mul (m.round N) (m.round πd))
  rw [hnd, realTrunc_intCast]
  set y := if N.fmod 2 = 1 then -r else r with hy_def
  have hyF : y ∈ m.F := by rw [hy_def]; split_ifs; exacts [hneg _ hrF, hrF]
  have hyr : |y| = |r| := by rw [hy_def]; split_ifs <;> simp
  have hy : Close m U H y y 0 0 (8 / 5) := Close.input hyF (hyr ▸ hr)
  have kz := (Close.mul hS le_rfl le_rfl le_rfl le_rfl hy hy (by norm_num [εU]; linarith)).mono hS (α' := (64 / 25 : ℝ)) (κ' := (1 : ℝ)) (T' := (64 / 25 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have k2 := (Close.round hS (c := S2) (C := (140139 / 1000000 : ℝ)) (by norm_num [S2, abs_of_neg]) (by linarith)).mono hS (α' := (701 / 5000 : ℝ)) (κ' := (1 : ℝ)) (T' := (701 / 5000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have kp := (Close.mul hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) kz k2 (by norm_num [εU]; linarith)).mono hS (α' := (1077 / 1000 : ℝ)) (κ' := (3701 / 1000 : ℝ)) (T' := (359 / 1000 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have k1 := (Close.round hS (c := S1) (C := (982397 / 1000000 : ℝ)) (by norm_num [S1, abs_of_pos]) (by linarith)).mono hS (α' := (614 / 625 : ℝ)) (κ' := (1 : ℝ)) (T' := (614 / 625 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])
  have ka := (Close.add hS (by norm_num) (by norm_num) (by norm_num) (by norm_num) k1 kp (by norm_num [εU]; linarith)).mono hS (α' := (3401 / 1000 : ℝ)) (κ' := (5701 / 1000 : ℝ)) (T' := (671 / 500 : ℝ))
    (by norm_num [εU]) (by norm_num [εU]) (by norm_num [εU])

  have hfin := Close.mul_input hS (by norm_num) (by norm_num) hyF ka
    (by rw [show εU = 1 / 2 ^ 20 from rfl]; nlinarith [abs_nonneg y])
  have hc : y * (S1 + y * y * S2) = (-1) ^ N * cpoly r := by rw [← parity]; rfl
  have hcore := reduce_core N x0 r hr
  rw [← hc] at hcore
  have h1 := hfin.2
  have hy1 : |y| ≤ 8 / 5 := hyr ▸ hr
  have := abs_sub_le (m.mul y (m.add (m.round S1) (m.mul (m.mul y y) (m.round S2))))
    (y * (S1 + y * y * S2)) (sin x0)
  have hg : ((3401 / 1000 : ℝ) + 671 / 500 + (3401 / 1000 + 5701 / 1000) * εU) ≤ 4744 / 1000 := by
    norm_num [εU]
  have h5 : |y| * ((3401 / 1000 + 671 / 500 + (3401 / 1000 + 5701 / 1000) * εU) * U
      + 5701 / 1000 * H) ≤ 8 / 5 * (4744 / 1000 * U + 5701 / 1000 * H) := by
    apply mul_le_mul hy1 _ (add_nonneg (mul_nonneg (by have := εU_pos; positivity) hU0)
      (mul_nonneg (by norm_num) hH0)) (by norm_num)
    have := mul_le_mul_of_nonneg_right hg hU0
    linarith
  linarith

#print axioms kernel_fastest_correct

/-- **Exact arithmetic.** With infinitely precise operations the generated
code is within `5.75·10^-3 + 1.3·10^-16 X` of `sin x0` for `|x0| ≤ X`; the
second term is the error of the literal `π_d`. -/
theorem kernel_fastest_exact_correct {X x0 : ℝ} (hX : 1 ≤ X) (hx : |x0| ≤ X) :
    |kernel_fastest_exact x0 - sin x0| ≤ 575 / 10 ^ 5 + X * (13 / 10 ^ 17) := by
  have A : FastestArith (FPModel.exact (5 * X)).toNoGuard 0 0 X :=
    { small := ⟨le_rfl, le_rfl, by norm_num [εU], by norm_num [εU],
        by simp [FPModel.toNoGuard, FPModel.exact], by simp [FPModel.toNoGuard, FPModel.exact]⟩
      X1 := hX
      XU := by norm_num [εU]
      XH := by norm_num [εU]
      M := le_rfl
      int_exact := fun n _ => rfl
      neg_mem := fun _ _ => trivial }
  have e : kernel_fastest_exact x0 = kernel_fastest_ng (FPModel.exact (5 * X)).toNoGuard x0 :=
    rfl
  rw [e]
  have h := kernel_fastest_correct _ A (x0 := x0) trivial hx
  simpa using h

#print axioms kernel_fastest_exact_correct

end SinKernelsProof
