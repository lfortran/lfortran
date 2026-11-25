import LFortran.Arith.Model
import LFortran.ErrorProp
import Mathlib.Analysis.SpecialFunctions.Trigonometric.Bounds
import Mathlib.Analysis.Real.Pi.Bounds

/-!
# Choosing `N` in range reduction

`kernel_fast(x0)` picks `N = int(x0/π_d + sign(x0)/2)` (`π_d` the binary64
literal for `π`). `chooseN` shows, in every `NoGuardModel` (so every
`FPModel`) with unit roundoff `≤ U` and underflow error `≤ H`, in which
integers up to `X` are exact and negation is exact (`ReduceArith`), that the
computed `N` is within `1/2 + 7X(U + H)` of `x0/π_d`, at most `X` in
magnitude, and represented exactly, for every floating-point `|x0| ≤ X`.

The sign of `x0` is a parameter `b` (`SignOk`): the `Float` run reads the
IEEE sign bit, which is set for `-0.0`, while `ℝ` has no `-0`.
-/

open LFortran Real

namespace SinKernelsProof

/-- The literal `π_d` (`3.141592653589793_dp`). -/
noncomputable def πd : ℝ := 884279719003555 * (2 ^ 48)⁻¹
/-! ### The choice of `N` -/

/-- The assumptions on the arithmetic: unit roundoff `≤ U`, underflow error
`≤ H`, both small relative to the input range `X ≥ 1`; range `M ≥ 5X`;
integers up to `X` are floating-point numbers (`round n = n`), and so are
the negatives of floating-point numbers. Every binary floating-point format
satisfies the last two for `X ≤ 2^p`. -/
structure ReduceArith (m : NoGuardModel) (U H X : ℝ) : Prop where
  small : Small m U H
  X1 : 1 ≤ X
  XU : X * U ≤ εU
  XH : X * H ≤ εU
  M : 5 * X ≤ m.M
  int_exact : ∀ n : ℤ, |(n : ℝ)| ≤ X → m.round n = n
  neg_mem : ∀ v ∈ m.F, -v ∈ m.F

/-- The sign choice `b` agrees with the sign of `x0` unless `x0 = 0`. -/
def SignOk (b : Bool) (x0 : ℝ) : Prop := (b = true → x0 ≤ 0) ∧ (b = false → 0 ≤ x0)

theorem signOk_decide (x0 : ℝ) : SignOk (decide (x0 < 0)) x0 := by
  constructor <;> intro h <;> simp at h <;> linarith

set_option maxHeartbeats 2000000 in
/-- **Choice of `N` in floating point.** The computed `N` is within
`1/2 + 7X(U + H)` of `x0/π_d`, at most `X` in magnitude, and represented
exactly. -/
theorem chooseN (m : NoGuardModel) {U H X : ℝ} (A : ReduceArith m U H X) {x0 : ℝ}
    (hxF : x0 ∈ m.F) (hx : |x0| ≤ X) {b : Bool} (hb : SignOk b x0) :
    let PI := m.round πd
    let N := realTrunc (m.add (m.div x0 PI)
      (m.mul (m.round 2⁻¹) (if b then -|m.round 1| else |m.round 1|)))
    |x0 / πd - N| ≤ 1 / 2 + (7 * X * U + 7 * X * H) ∧ |(N : ℝ)| ≤ X ∧ m.round N = N := by
  intro PI N
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
  set s := if b then -|m.round 1| else |m.round 1| with hs_def
  set σ : ℝ := if b then -1 else 1 with hσ_def
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
    cases b with
    | true =>
      have hσ' : σ = -1 := by rw [hσ_def]; rfl
      rw [hσ'] at hte'
      have hx0' : x0 / πd ≤ 0 := div_nonpos_of_nonpos_of_nonneg (hb.1 rfl) hπd0.le
      have htneg : t < 0 := by linarith
      have hNt : (N : ℝ) = ⌈t⌉ := by
        show ((realTrunc t : ℤ) : ℝ) = _; rw [realTrunc_of_neg htneg]
      have c1 := Int.le_ceil t
      have c2 := Int.ceil_lt_add_one t
      rw [hNt, abs_le]; constructor <;> linarith
    | false =>
      have hσ' : σ = 1 := by rw [hσ_def]; rfl
      rw [hσ'] at hte'
      have hx0' : 0 ≤ x0 / πd := div_nonneg (hb.2 rfl) hπd0.le
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
  exact ⟨hN, hNX, hint N hNX⟩

#print axioms chooseN

end SinKernelsProof
