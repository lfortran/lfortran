import LFortranProof.Arith.Model
import LFortranProof.ErrorProp
import Mathlib.Analysis.SpecialFunctions.Trigonometric.Bounds
import Mathlib.Analysis.Real.Pi.Bounds

/-!
# Choosing `N` in range reduction

`kernel_fast(x0)` picks `N = int(x0/π_d + sign(1, x0)/2)` (`π_d` the binary64
literal for `π`); `kfN xv` is that integer, computed by the generated code on
any carrier. `chooseN` shows, in every `VModel` with unit roundoff `≤ U` and
underflow error `≤ H` that is `Signed` and in which integers up to `X` are
exact (`ReduceArith`), that `N` is within `1/2 + 7X(U + H)` of `x0/π_d`, at
most `X` in magnitude, and converted back to a real exactly, for every input
with value `|x0| ≤ X`.

The sign of `x0` is read from the sign bit (`SignOk`): IEEE `-0.0` has it set,
while `ℝ` has no `-0`.
-/

open LFortran Real

namespace SinKernelsProof

/-- The literal `π_d` (`3.141592653589793_dp`). -/
noncomputable def πd : ℝ := 884279719003555 * (2 ^ 48)⁻¹

theorem πd_eq : ((884279719003555 : ℤ) : ℝ) * (2 : ℝ) ^ (-48 : ℤ) = πd := by norm_num [πd]
theorem half_eq : ((1 : ℤ) : ℝ) * (2 : ℝ) ^ (-1 : ℤ) = 2⁻¹ := by norm_num

/-! ### The choice of `N` -/

variable {ρ : Type} [FReal ρ]

/-- The integer `N = int(x0/π_d + 0.5_dp*sign(1._dp, x0))` that the generated
`kernel_fast` computes (`SinKernels/Generated.lean`). -/
def kfN (xv : ρ) : ℤ :=
  FReal.toInt (FReal.add (FReal.div xv (FReal.lit 884279719003555 (-48)))
    (FReal.mul (FReal.lit 1 (-1)) (if FReal.signbit xv then FReal.neg (FReal.abs (FReal.ofInt 1))
      else FReal.abs (FReal.ofInt 1))))

/-- The assumptions on the arithmetic: unit roundoff `≤ U`, underflow error
`≤ H`, both small relative to the input range `1 ≤ X ≤ 2^50`; range
`M ≥ 5X`; negation, `abs`, the sign bit and truncation act on values
(`VModel.Signed`); integers up to `X` convert exactly. Every binary
floating-point format with at least `log2 X` significand bits satisfies the
last one. -/
structure ReduceArith (V : VModel ρ) (U H X : ℝ) : Prop where
  small : Small V U H
  signed : V.Signed
  X1 : 1 ≤ X
  X50 : X ≤ 2 ^ 50
  XU : X * U ≤ εU
  XH : X * H ≤ εU
  M : 5 * X ≤ V.M
  int_exact : ∀ n : ℤ, |(n : ℝ)| ≤ X → V.val (FReal.ofInt n) n

/-- The sign choice `b` agrees with the sign of `x0` unless `x0 = 0`. -/
def SignOk (b : Bool) (x0 : ℝ) : Prop := (b = true → x0 ≤ 0) ∧ (b = false → 0 ≤ x0)

/-- The sign bit of an input is a valid sign choice for its value. -/
theorem signOk_signbit {V : VModel ρ} (hS : V.Signed) {xv : ρ} {x0 : ℝ} (hx : V.val xv x0) :
    SignOk (FReal.signbit xv) x0 := by
  by_cases h0 : x0 = 0
  · subst h0; exact ⟨fun _ => le_rfl, fun _ => le_rfl⟩
  · have := hS.signbit xv x0 hx h0
    constructor
    · intro h; exact (this.1 h).le
    · intro h; rw [h] at this; by_contra hn; exact absurd (this.2 (lt_of_not_ge hn)) (by simp)

set_option maxHeartbeats 2000000 in
/-- **Choice of `N` in floating point.** The computed `N` is within
`1/2 + 7X(U + H)` of `x0/π_d`, at most `X` in magnitude, and converted back
to a real exactly. -/
theorem chooseN (V : VModel ρ) {U H X : ℝ} (A : ReduceArith V U H X) {xv : ρ} {x0 : ℝ}
    (hx : V.val xv x0) (hx1 : |x0| ≤ X) :
    |x0 / πd - kfN xv| ≤ 1 / 2 + (7 * X * U + 7 * X * H) ∧ |(kfN xv : ℝ)| ≤ X ∧
      V.val (FReal.ofInt (kfN xv)) (kfN xv) := by
  have hb := signOk_signbit A.signed hx
  obtain ⟨hS, hsg, hX1, hX50, hXU, hXH, hM, hint⟩ := A
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
  have hPIm : |πd| ≤ V.M := by rw [abs_of_pos (by linarith)]; linarith
  obtain ⟨PI, hPIv, hPIe0⟩ := V.lit_err 884279719003555 (-48) (by norm_num [LitOk])
    (by rw [πd_eq]; exact hPIm)
  rw [πd_eq] at hPIe0
  have hPIe : |PI - πd| ≤ 31416 / 10000 * U + H := by
    have h2 := Close.u_mul_le hS (y := πd) (Y := 31416 / 10000)
      (by rw [abs_of_pos (by linarith)]; exact hπd2)
    linarith
  have hPI1 : (3141 / 1000 : ℝ) ≤ PI := by
    have := (abs_le.1 hPIe).1; linarith
  have hPI2 : PI ≤ 3142 / 1000 := by
    have := (abs_le.1 hPIe).2; linarith
  -- the literal 1 and the sign
  obtain ⟨o, hov, hoe⟩ := V.ofInt_err 1 (by norm_num) (by simp; linarith)
  have hONEe : |o - 1| ≤ U + H := by
    have h2 := Close.u_mul_le hS (y := 1) (Y := 1) (by rw [abs_one])
    simp only [Int.cast_one, abs_one] at hoe h2
    linarith
  have hAv := hsg.abs _ _ hov
  have hAe : |(|o| - 1)| ≤ U + H := by
    have := abs_abs_sub_abs_le_abs_sub o 1
    rw [abs_one] at this; linarith
  set σ : ℝ := if FReal.signbit xv then -1 else 1 with hσ_def
  obtain ⟨s, hsv, hse⟩ : ∃ s, V.val (if FReal.signbit xv then FReal.neg (FReal.abs (FReal.ofInt 1))
      else FReal.abs (FReal.ofInt 1)) s ∧ |s - σ| ≤ U + H := by
    rw [hσ_def]; split_ifs
    · refine ⟨-|o|, hsg.neg _ _ hAv, ?_⟩
      rw [show -|o| - -1 = -(|o| - 1) by ring, abs_neg]; exact hAe
    · exact ⟨|o|, hAv, hAe⟩
  have hσ : |σ| = 1 := by rw [hσ_def]; split_ifs <;> simp
  have hs1 : |s| ≤ 101 / 100 := by
    have := abs_sub_abs_le_abs_sub s σ; linarith
  -- the literal 1/2
  have h2m : |((1 : ℤ) : ℝ) * (2 : ℝ) ^ (-1 : ℤ)| ≤ V.M := by rw [half_eq]; norm_num; linarith
  obtain ⟨hf, hHv, hHe0⟩ := V.lit_err 1 (-1) (by norm_num [LitOk]) h2m
  rw [half_eq] at hHe0
  have hHe : |hf - 2⁻¹| ≤ U + H := by
    have h2 := Close.u_mul_le hS (y := 2⁻¹) (Y := 1) (by norm_num)
    linarith
  have hH1 : |hf| ≤ 51 / 100 := by
    have := abs_sub_abs_le_abs_sub hf 2⁻¹
    rw [abs_of_pos (by norm_num : (0 : ℝ) < 2⁻¹)] at this; norm_num at this ⊢; linarith
  -- hs = 1/2 * s
  have hHs : |hf * s| ≤ 1 := by
    rw [abs_mul]; nlinarith [abs_nonneg hf, abs_nonneg s]
  obtain ⟨hs, hhsv, h1⟩ := V.mul_err _ _ _ _ hHv hsv (hHs.trans (by linarith))
  have hhse : |hs - σ / 2| ≤ 3 * U + 3 * H := by
    have h2 := Close.u_mul_le hS hHs
    have e : hf * s - σ / 2 = hf * (s - σ) + (hf - 2⁻¹) * σ := by ring
    have h3 : |hf * s - σ / 2| ≤ 51 / 100 * (U + H) + (U + H) := by
      rw [e]
      calc _ ≤ |hf * (s - σ)| + |(hf - 2⁻¹) * σ| := abs_add_le _ _
        _ ≤ 51 / 100 * (U + H) + (U + H) := by
          rw [abs_mul, abs_mul, hσ, mul_one]
          gcongr
    have := abs_sub_le hs (hf * s) (σ / 2)
    linarith
  have hhs1 : |hs| ≤ 1 := by
    have := abs_sub_abs_le_abs_sub hs (σ / 2)
    rw [abs_div, hσ] at this
    norm_num at this
    have : 3 * U + 3 * H ≤ 6 * εU := by linarith [hS.Uε, hS.Hε]
    norm_num [hε] at this
    linarith
  -- q = x0 / PI
  have hPI0 : 0 < PI := by linarith
  have hπd0 : 0 < πd := by linarith
  have hq1 : |x0 / PI| ≤ X / 3 := by
    rw [abs_div, abs_of_pos hPI0, div_le_div_iff₀ hPI0 (by norm_num)]
    have := mul_le_mul_of_nonneg_left hPI1 hX0
    linarith
  have hqM : |x0 / PI| ≤ V.M := hq1.trans (by linarith)
  obtain ⟨q, hqv, hq0⟩ := V.div_err _ _ _ _ hx hPIv hPI0.ne' hqM
  have hqe : |q - x0 / πd| ≤ 2 * X * U + 2 * X * H := by
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
    have h2 := Close.u_mul_le hS hq1
    have : X * U ≤ εU := hXU
    linarith
  -- t = q + hs
  have htM : |q| + |hs| ≤ 2 * X := by norm_num [hε] at hq2; linarith
  obtain ⟨t, htv, h1⟩ := V.add_err _ _ _ _ hqv hhsv (((abs_add_le _ _).trans htM).trans
    (by linarith))
  have hte : |t - (x0 / πd + σ / 2)| ≤ 7 * X * U + 7 * X * H := by
    have h2 := Close.u_mul_le hS (y := |q| + |hs|) (Y := 2 * X)
      (by rw [abs_of_nonneg (by positivity)]; exact htM)
    rw [abs_of_nonneg (by positivity : (0 : ℝ) ≤ |q| + |hs|)] at h2
    have e : t - (x0 / πd + σ / 2) = (t - (q + hs)) + (q - x0 / πd) + (hs - σ / 2) := by ring
    rw [e]
    have := abs_add_le ((t - (q + hs)) + (q - x0 / πd)) (hs - σ / 2)
    have := abs_add_le (t - (q + hs)) (q - x0 / πd)
    linarith
  -- N
  have hEt : 7 * X * U + 7 * X * H ≤ 14 * εU := by linarith
  have hEt' : 7 * X * U + 7 * X * H ≤ 2 / 10 ^ 5 := by rw [hε] at hEt; linarith
  have hx0πd : |x0 / πd| ≤ X / 3 := by
    rw [abs_div, abs_of_pos hπd0, div_le_div_iff₀ hπd0 (by norm_num)]
    have := mul_le_mul_of_nonneg_left hπd1 hX0
    linarith
  have ht63 : |t| < 2 ^ 63 := by
    have := abs_sub_abs_le_abs_sub t (x0 / πd + σ / 2)
    have := abs_add_le (x0 / πd) (σ / 2)
    have : |σ / 2| = 1 / 2 := by rw [abs_div, hσ]; norm_num
    have : X / 3 + 1 / 2 + 2 / 10 ^ 5 < 2 ^ 63 := by
      have : (2 : ℝ) ^ 50 < 2 ^ 63 := by norm_num
      linarith
    linarith
  have hNt : kfN xv = realTrunc t := hsg.toInt _ _ htv ht63
  rw [hNt]
  have hN : |x0 / πd - realTrunc t| ≤ 1 / 2 + (7 * X * U + 7 * X * H) := by
    have hte' := abs_le.1 hte
    by_cases hsb : FReal.signbit xv = true
    · have hσ' : σ = -1 := by rw [hσ_def, if_pos hsb]
      rw [hσ'] at hte'
      have hx0' : x0 / πd ≤ 0 := div_nonpos_of_nonpos_of_nonneg (hb.1 hsb) hπd0.le
      have htneg : t < 0 := by linarith
      rw [realTrunc_of_neg htneg]
      have c1 := Int.le_ceil t
      have c2 := Int.ceil_lt_add_one t
      rw [abs_le]; constructor <;> linarith
    · have hsb' : FReal.signbit xv = false := by simpa using hsb
      have hσ' : σ = 1 := by rw [hσ_def, if_neg hsb]
      rw [hσ'] at hte'
      have hx0' : 0 ≤ x0 / πd := div_nonneg (hb.2 hsb') hπd0.le
      have htpos : 0 ≤ t := by linarith
      rw [realTrunc_of_nonneg htpos]
      have c1 := Int.floor_le t
      have c2 := Int.lt_floor_add_one t
      rw [abs_le]; constructor <;> linarith
  have hNX : |((realTrunc t : ℤ) : ℝ)| ≤ X := by
    have := abs_sub_abs_le_abs_sub ((realTrunc t : ℤ) : ℝ) (x0 / πd)
    rw [abs_sub_comm ((realTrunc t : ℤ) : ℝ)] at this
    linarith
  exact ⟨hN, hNX, hint _ hNX⟩

#print axioms chooseN

end SinKernelsProof
