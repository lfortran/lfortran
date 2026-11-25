import SinKernels.Proof.Reduction
import SinKernels.Proof.KernelSin
import LFortran.PiBounds
import LFortran.FloatExact
import SinKernels.Binary64

set_option exponentiation.threshold 2048

/-!
# `kernel_fast`: Cody–Waite reduction, then `kernel_sin`

`kernel_fast(x0)` picks `N = int(x0/π_d + sign(x0)/2)` (`chooseN`),
reduces `r = ((x0 - N p1) - N p2) - N p3` with the three-part representation
`p1 + p2 + p3` of `π` (`p1`, `p2` have 21-bit significands, and
`p1 + p2 + p3 - π ≈ -1.7·10^-31`), negates `r` for odd `N`, and returns
`kernel_sin(r)`.

* `kernel_fast_exact_correct`: in exact arithmetic the result is within
  `7.3·10^-16` of `sin x0` for `|x0| ≤ 10^12`.
* `kfastGen_binary64` (binary64, `|x0| ≤ 6·10^9`): the products `N p1`,
  `N p2` are exact (`fm_mul_exact`), `x0 - N p1` is exact by Sterbenz's
  lemma (`sterbenz`), and the two remaining subtractions and `N p3` add at
  most `3.6·10^-16` (`π` to 32 decimals, `LFortran/PiBounds.lean`, bounds
  `N (p1 + p2 + p3 - π)`); with `kernel_sin`'s `1.8·10^-15` on `|r| ≤ 1.571`, the
  result is within `2.2·10^-15` of `sin x0`.

The `Float` run is in `SinKernels/FloatRun.lean`.
-/

open LFortran Real

namespace SinKernelsProof

/-- The literal `p1` (`3.14159202575683594e+00_dp`). -/
noncomputable def P1 : ℝ := 1647099 * (2 ^ 19)⁻¹
/-- The literal `p2` (`6.27832832833519205e-07_dp`). -/
noncomputable def P2 : ℝ := 1380619 * (2 ^ 41)⁻¹
/-- The literal `p3` (`1.24467443437932268e-13_dp`). -/
noncomputable def P3 : ℝ := 4930663418217751 * (2 ^ 95)⁻¹

/-- The generated `kernel_fast` in a `NoGuardModel`. -/
noncomputable def kernel_fast_ng (m : NoGuardModel) (x0 : ℝ) : ℝ :=
  @sin_kernels.kernel_fast proofKinds (FKinds.ofNoGuard m m) x0

/-- The generated `kernel_fast` in exact arithmetic. -/
noncomputable def kernel_fast_exact (x0 : ℝ) : ℝ :=
  @sin_kernels.kernel_fast proofKinds (FKinds.ofModel FPModel.exact FPModel.exact) x0

/-- What the generated code computes, step by step, with the sign of `x0`
taken from `b` (`SignOk`). -/
noncomputable def kfastGen (m : NoGuardModel) (b : Bool) (x0 : ℝ) : ℝ :=
  let PI := m.round πd
  let N := realTrunc (m.add (m.div x0 PI)
    (m.mul (m.round 2⁻¹) (if b then -|m.round 1| else |m.round 1|)))
  let nd := m.round N
  let r := m.sub (m.sub (m.sub x0 (m.mul nd (m.round P1))) (m.mul nd (m.round P2)))
    (m.mul nd (m.round P3))
  kernel_sin_ng m (if (realTrunc nd).fmod 2 = 1 then -r else r)

theorem kernel_fast_ng_eq (m : NoGuardModel) (x0 : ℝ) :
    kernel_fast_ng m x0 = kfastGen m (decide (x0 < 0)) x0 := by
  simp only [kernel_fast_ng, sin_kernels.kernel_fast, kfastGen]
  simp [πd, P1, P2, P3]
  split_ifs <;> rfl

/-! ### The constants -/

/-- `p1 + p2 + p3` is `π` to `2·10^-31`. -/
theorem P_pi : |P1 + P2 + P3 - π| ≤ 2 / 10 ^ 31 := by
  have h1 := pi_gt_d32
  have h2 := pi_lt_d32
  rw [abs_le]
  norm_num [P1, P2, P3] at h1 h2 ⊢
  constructor <;> linarith

/-- `p1 + p2 + p3` is `π_d` to `1.3·10^-16`. -/
theorem P_pid : |πd - (P1 + P2 + P3)| ≤ 13 / 10 ^ 17 := by
  rw [abs_le]; norm_num [πd, P1, P2, P3]

/-- Negating `r` for odd `N` multiplies `sin r` by `(-1)^N`. -/
theorem parity_sin (N : ℤ) (r : ℝ) :
    sin (if N.fmod 2 = 1 then -r else r) = (-1) ^ N * sin r := by
  rw [Int.fmod_eq_emod_of_nonneg _ (by norm_num)]
  rcases Int.emod_two_eq_zero_or_one N with h | h
  · rw [if_neg (by omega), Even.neg_one_zpow (Int.even_iff.2 h), one_mul]
  · rw [if_pos h, Odd.neg_one_zpow (Int.odd_iff.2 h), sin_neg, neg_one_mul]

/-- The reduction argument: a value within `A` of `sin y`, where `y = ±r`
by the parity of `N`, is within `A + |r - (x0 - N π)|` of `sin x0`. -/
theorem fast_core (N : ℤ) (x0 r v A : ℝ)
    (hv : |v - sin (if N.fmod 2 = 1 then -r else r)| ≤ A) :
    |v - sin x0| ≤ A + |r - (x0 - N * π)| := by
  have hs : sin x0 = (-1) ^ N * sin (x0 - N * π) := by
    rw [sin_sub_int_mul_pi, ← mul_assoc, ← mul_zpow, show (-1 : ℝ) * -1 = 1 by norm_num,
      one_zpow, one_mul]
  have habs : |((-1 : ℝ) ^ N)| = 1 := by simp [abs_zpow]
  rw [parity_sin] at hv
  have h2 : |(-1) ^ N * sin r - sin x0| ≤ |r - (x0 - N * π)| := by
    rw [hs, ← mul_sub, abs_mul, habs, one_mul]
    exact abs_sin_sub_sin_le _ _
  have := abs_sub_le v ((-1) ^ N * sin r) (sin x0)
  linarith

/-! ### Exact arithmetic -/

theorem kernel_sin_exact_ng (y : ℝ) :
    kernel_sin_ng (FPModel.exact (5 * 1)).toNoGuard y = kernel_sin_exact y := rfl

/-- **Exact arithmetic.** With infinitely precise operations the generated
`kernel_fast` is within `7.3·10^-16` of `sin x0` for `|x0| ≤ 10^12`. -/
theorem kernel_fast_exact_correct {X x0 : ℝ} (hX : 1 ≤ X) (hX12 : X ≤ 10 ^ 12)
    (hx : |x0| ≤ X) : |kernel_fast_exact x0 - sin x0| ≤ 73 / 10 ^ 17 := by
  have A : ReduceArith (FPModel.exact (5 * X)).toNoGuard 0 0 X :=
    { small := ⟨le_rfl, le_rfl, by norm_num [εU], by norm_num [εU],
        by simp [FPModel.toNoGuard, FPModel.exact], by simp [FPModel.toNoGuard, FPModel.exact]⟩
      X1 := hX
      XU := by norm_num [εU]
      XH := by norm_num [εU]
      M := le_rfl
      int_exact := fun n _ => rfl
      neg_mem := fun _ _ => trivial }
  have e : kernel_fast_exact x0 = kfastGen (FPModel.exact (5 * X)).toNoGuard (decide (x0 < 0)) x0 := by
    rw [← kernel_fast_ng_eq]; rfl
  have hc := chooseN _ A (x0 := x0) trivial hx (signOk_decide x0)
  simp only [mul_zero, add_zero] at hc
  rw [e]
  simp only [kfastGen]
  generalize realTrunc _ = N at hc ⊢
  obtain ⟨hN, hNX, hnd⟩ := hc
  simp only [show ∀ y, (FPModel.exact (5 * X)).toNoGuard.round y = y from fun _ => rfl,
    show ∀ a b, (FPModel.exact (5 * X)).toNoGuard.sub a b = a - b from fun _ _ => rfl,
    show ∀ a b, (FPModel.exact (5 * X)).toNoGuard.mul a b = a * b from fun _ _ => rfl,
    realTrunc_intCast]
  set r := x0 - N * P1 - N * P2 - N * P3 with hr
  have hks : kernel_sin_ng (FPModel.exact (5 * X)).toNoGuard (if N.fmod 2 = 1 then -r else r)
      = kernel_sin_exact (if N.fmod 2 = 1 then -r else r) := rfl
  rw [hks]
  have hπd1 : (31415 / 10000 : ℝ) ≤ πd := by norm_num [πd]
  have hπd2 : πd ≤ 31416 / 10000 := by norm_num [πd]
  have hxN : |x0 - N * πd| ≤ 31416 / 10000 / 2 := by
    have e2 : x0 - N * πd = πd * (x0 / πd - N) := by field_simp
    rw [e2, abs_mul, abs_of_pos (by linarith)]
    nlinarith [abs_nonneg (x0 / πd - N)]
  have hPd := P_pid
  have hPp := P_pi
  have hr1 : |r| ≤ 1571 / 1000 := by
    have e3 : r = (x0 - N * πd) + N * (πd - (P1 + P2 + P3)) := by rw [hr]; ring
    have h4 : |(N : ℝ) * (πd - (P1 + P2 + P3))| ≤ 13 / 10 ^ 5 := by
      rw [abs_mul]
      exact (mul_le_mul (hNX.trans hX12) hPd (abs_nonneg _) (by norm_num)).trans (by norm_num)
    rw [e3]
    have := abs_add_le (x0 - N * πd) (N * (πd - (P1 + P2 + P3)))
    linarith
  have hy1 : |(if N.fmod 2 = 1 then -r else r)| ≤ 1571 / 1000 := by
    split_ifs <;> simpa using hr1
  have happ := kernel_sin_approx_wide hy1
  have hyr : |(if N.fmod 2 = 1 then -r else r)| = |r| := by split_ifs <;> simp
  rw [hyr] at happ
  have hcore := fast_core N x0 r _ _ happ
  have hrN : |r - (x0 - N * π)| ≤ 10 ^ 12 * (2 / 10 ^ 31) := by
    have e4 : r - (x0 - N * π) = -(N * (P1 + P2 + P3 - π)) := by rw [hr]; ring
    rw [e4, abs_neg, abs_mul]; exact mul_le_mul (hNX.trans hX12) hPp (abs_nonneg _) (by norm_num)
  have : 46 / 10 ^ 17 * |r| ≤ 46 / 10 ^ 17 * (1571 / 1000) :=
    mul_le_mul_of_nonneg_left hr1 (by norm_num)
  norm_num at hrN this ⊢
  linarith

#print axioms kernel_fast_exact_correct

/-! ### binary64 -/

theorem tng_round : floatModel.toNoGuard.round = floatModel.round := rfl
theorem tng_add : floatModel.toNoGuard.add = floatModel.add := rfl
theorem tng_sub : floatModel.toNoGuard.sub = floatModel.sub := rfl
theorem tng_mul : floatModel.toNoGuard.mul = floatModel.mul := rfl
theorem tng_div : floatModel.toNoGuard.div = floatModel.div := rfl

theorem P1_mem : P1 ∈ binary64 :=
  ⟨1647099, -19, by norm_num, by norm_num, by norm_num, by norm_num [P1]⟩
theorem P2_mem : P2 ∈ binary64 :=
  ⟨1380619, -41, by norm_num, by norm_num, by norm_num, by norm_num [P2]⟩
theorem P3_mem : P3 ∈ binary64 :=
  ⟨4930663418217751, -95, by norm_num, by norm_num, by norm_num, by norm_num [P3]⟩

/-- `floatModel` rounds a binary64 number to itself. -/
theorem fm_round_bin {c : ℝ} (hc : c ∈ binary64) (hM : |c| ≤ 2 ^ 1022) : floatModel.round c = c :=
  round_fixes floatRound_spec hM hc

/-- **Sterbenz for the first Cody–Waite step**: `x0 - N p1` is a binary64
number when `N ≠ 0` is the nearest integer to `x0/π_d` up to a small `E`. -/
theorem cw_sterbenz {x0 E : ℝ} {N : ℤ} (hx : x0 ∈ binary64) (hNP : (N : ℝ) * P1 ∈ binary64)
    (hN0 : N ≠ 0) (h : |x0 - N * πd| ≤ πd * (1 / 2 + E))
    (hE : E ≤ 6 / 10 ^ 15 * |(N : ℝ)|) : x0 - N * P1 ∈ binary64 := by
  have hpd : πd = 884279719003555 / 2 ^ 48 := by norm_num [πd]
  have hp1 : P1 = 1647099 / 2 ^ 19 := by norm_num [P1]
  rw [hpd] at h
  have h' := abs_le.1 h
  rcases lt_or_gt_of_ne hN0 with hneg | hpos
  · have hN1 : (N : ℝ) ≤ -1 := by exact_mod_cast (show N ≤ -1 by omega)
    rw [abs_of_neg (by linarith)] at hE
    have hs := sterbenz (neg_mem_binary64 hx) (neg_mem_binary64 hNP)
      (by rw [hp1]; norm_num at h' ⊢; nlinarith) (by rw [hp1]; norm_num at h' ⊢; nlinarith)
    have := neg_mem_binary64 hs
    rwa [show -(-x0 - -(N * P1)) = x0 - N * P1 by ring] at this
  · have hN1 : (1 : ℝ) ≤ N := by exact_mod_cast (show 1 ≤ N by omega)
    rw [abs_of_pos (by linarith)] at hE
    exact sterbenz hx hNP (by rw [hp1]; norm_num at h' ⊢; nlinarith)
      (by rw [hp1]; norm_num at h' ⊢; nlinarith)

set_option maxHeartbeats 4000000 in
/-- **`kernel_fast` in binary64.** For binary64 `|x0| ≤ 6·10^9` the computed
`N` is exact with `|N| ≤ 2·10^9`, the reduced argument `r` is a binary64
number with `|r| ≤ 1.571`, and the result is within `2.2·10^-15` of
`sin x0`. -/
theorem kfastGen_binary64 {X x0 : ℝ} (hX1 : 1 ≤ X) (hX : X ≤ 6 * 10 ^ 9) (hxF : x0 ∈ binary64)
    (hx : |x0| ≤ X) {b : Bool} (hb : SignOk b x0) :
    let N := realTrunc (floatModel.add (floatModel.div x0 (floatModel.round πd))
      (floatModel.mul (floatModel.round 2⁻¹)
        (if b then -|floatModel.round 1| else |floatModel.round 1|)))
    let r := floatModel.sub (floatModel.sub (floatModel.sub x0
      (floatModel.mul (floatModel.round N) (floatModel.round P1)))
      (floatModel.mul (floatModel.round N) (floatModel.round P2)))
      (floatModel.mul (floatModel.round N) (floatModel.round P3))
    |(N : ℝ)| ≤ 2 * 10 ^ 9 ∧ floatModel.round N = N ∧ r ∈ binary64 ∧ |r| ≤ 1571 / 1000 ∧
      |kfastGen floatModel.toNoGuard b x0 - sin x0| ≤ 22 / 10 ^ 16 := by
  intro N r
  have hu : floatModel.u = 1 / 2 ^ 53 := rfl
  have hη : floatModel.η = 1 / 2 ^ 1075 := rfl
  have hMM : floatModel.M = 2 ^ 1022 := rfl
  have hFF : floatModel.F = binary64 := rfl
  -- the choice of `N`, at the scale of `x0`
  set Xl := max 1 |x0| with hXl
  have hXl1 : 1 ≤ Xl := le_max_left _ _
  have hxl : |x0| ≤ Xl := le_max_right _ _
  have hXlX : Xl ≤ X := max_le hX1 hx
  have A := SinKernelsBinary64.reduceArith_binary64 hXl1 (hXlX.trans (hX.trans (by norm_num)))
  have hc : |x0 / πd - N| ≤ 1 / 2 + (7 * Xl * (1 / 2 ^ 53) + 7 * Xl * (1 / 2 ^ 1075)) ∧
      |(N : ℝ)| ≤ Xl ∧ floatModel.round N = N :=
    chooseN _ A hxF hxl hb
  obtain ⟨hN, hNXl, hnd⟩ := hc
  set E := 7 * Xl * (1 / 2 ^ 53) + 7 * Xl * (1 / 2 ^ 1075) with hEdef
  have hE0 : 0 ≤ E := by positivity
  have hEXl : E ≤ 8 / 10 ^ 16 * Xl := by
    have : (7 * (1 / 2 ^ 53) + 7 * (1 / 2 ^ 1075) : ℝ) ≤ 8 / 10 ^ 16 := by norm_num
    nlinarith
  have hE6 : E ≤ 5 / 10 ^ 6 := by nlinarith
  have hπd1 : (314159 / 100000 : ℝ) ≤ πd := by norm_num [πd]
  have hπd2 : πd ≤ 31416 / 10000 := by norm_num [πd]
  have hxN : |x0 - N * πd| ≤ πd * (1 / 2 + E) := by
    have e2 : x0 - N * πd = πd * (x0 / πd - N) := by field_simp
    rw [e2, abs_mul, abs_of_pos (by linarith)]
    exact mul_le_mul_of_nonneg_left hN (by linarith)
  have hxN' : |x0 - N * πd| ≤ 157082 / 100000 := by
    have := mul_le_mul hπd2 (show 1 / 2 + E ≤ 1 / 2 + 5 / 10 ^ 6 by linarith) (by positivity)
      (by norm_num)
    linarith
  have hN9 : |(N : ℝ)| ≤ 2 * 10 ^ 9 := by
    have hq : |x0 / πd| ≤ 6 * 10 ^ 9 / (314159 / 100000) := by
      rw [abs_div, abs_of_pos (show (0 : ℝ) < πd by linarith)]
      exact div_le_div₀ (by norm_num) (hx.trans hX) (by norm_num) hπd1
    have := abs_sub_abs_le_abs_sub (N : ℝ) (x0 / πd)
    rw [abs_sub_comm (N : ℝ)] at this
    norm_num at hq ⊢
    linarith
  have hNint : -2000000000 ≤ N ∧ N ≤ 2000000000 := by
    have := abs_le.1 hN9
    constructor
    · have : (-2000000000 : ℝ) ≤ N := by linarith
      exact_mod_cast this
    · have : (N : ℝ) ≤ 2000000000 := by linarith
      exact_mod_cast this
  refine ⟨hN9, hnd, ?_⟩
  -- the constants and `N` are binary64 numbers, rounded to themselves
  have hNmem : (N : ℝ) ∈ binary64 := by
    rw [← hnd, ← hFF]
    exact floatModel.round_mem _ (by rw [hMM]; linarith)
  have hP1r : floatModel.round P1 = P1 := fm_round_bin P1_mem (by norm_num [P1, abs_of_pos])
  have hP2r : floatModel.round P2 = P2 := fm_round_bin P2_mem (by norm_num [P2, abs_of_pos])
  have hP3r : floatModel.round P3 = P3 := fm_round_bin P3_mem (by norm_num [P3, abs_of_pos])
  -- `N p1` and `N p2` are exact
  have hNP1 : (N : ℝ) * P1 ∈ binary64 := by
    rw [show P1 = ((1647099 : ℤ) : ℝ) * (2 : ℝ) ^ (-19 : ℤ) by norm_num [P1]]
    exact int_mul_mem (by rw [abs_lt]; constructor <;> omega) (by norm_num) (by norm_num)
  have hNP2 : (N : ℝ) * P2 ∈ binary64 := by
    rw [show P2 = ((1380619 : ℤ) : ℝ) * (2 : ℝ) ^ (-41 : ℤ) by norm_num [P2]]
    exact int_mul_mem (by rw [abs_lt]; constructor <;> omega) (by norm_num) (by norm_num)
  have bNP1 : |(N : ℝ) * P1| ≤ 2 * 10 ^ 9 * 4 := by
    rw [abs_mul]; exact mul_le_mul hN9 (by norm_num [P1, abs_of_pos]) (abs_nonneg _) (by norm_num)
  have bNP2 : |(N : ℝ) * P2| ≤ 2 * 10 ^ 9 * 1 := by
    rw [abs_mul]; exact mul_le_mul hN9 (by norm_num [P2, abs_of_pos]) (abs_nonneg _) (by norm_num)
  have bNP3 : |(N : ℝ) * P3| ≤ 25 / 10 ^ 5 := by
    rw [abs_mul]
    exact (mul_le_mul hN9 (by norm_num [P3, abs_of_pos] : |P3| ≤ 125 / 10 ^ 15) (abs_nonneg _)
      (by norm_num)).trans (by norm_num)
  have ha1 : floatModel.mul N P1 = N * P1 :=
    fm_mul_exact hNmem P1_mem hNP1 (bNP1.trans (by norm_num))
  have ha2 : floatModel.mul N P2 = N * P2 :=
    fm_mul_exact hNmem P2_mem hNP2 (bNP2.trans (by norm_num))
  -- `x0 - N p1` is exact (Sterbenz)
  have hs1mem : x0 - N * P1 ∈ binary64 := by
    by_cases hN0 : N = 0
    · rw [hN0]; simpa using hxF
    · have hN1 : (1 : ℝ) ≤ |(N : ℝ)| := by
        rw [← Int.cast_abs]; exact_mod_cast Int.one_le_abs hN0
      have hXlN : Xl ≤ 6 * |(N : ℝ)| := by
        apply max_le (by linarith)
        have := abs_sub_abs_le_abs_sub x0 (N * πd)
        rw [abs_mul (N : ℝ), abs_of_pos (show (0 : ℝ) < πd by linarith)] at this
        nlinarith
      exact cw_sterbenz hxF hNP1 hN0 hxN (by nlinarith)
  have bs1 : |x0 - N * P1| ≤ 2 ^ 1022 := by
    have := abs_sub x0 (N * P1)
    have : (2 * 10 ^ 9 * 4 + 6 * 10 ^ 9 : ℝ) ≤ 2 ^ 1022 := by norm_num
    linarith [hx.trans hX]
  have hs1 : floatModel.sub x0 (N * P1) = x0 - N * P1 := fm_sub_exact hxF hNP1 hs1mem bs1
  -- `D2 = x0 - N (p1 + p2)`, then two rounded subtractions and `N p3`
  have bD2 : |x0 - N * P1 - N * P2| ≤ 15711 / 10000 := by
    have e : x0 - N * P1 - N * P2 = (x0 - N * πd) + N * (πd - P1 - P2) := by ring
    have h1 : |(N : ℝ) * (πd - P1 - P2)| ≤ 25 / 10 ^ 5 := by
      rw [abs_mul]
      exact (mul_le_mul hN9 (by rw [abs_le]; norm_num [πd, P1, P2] :
        |πd - P1 - P2| ≤ 125 / 10 ^ 15) (abs_nonneg _) (by norm_num)).trans (by norm_num)
    rw [e]
    have := abs_add_le (x0 - N * πd) (N * (πd - P1 - P2))
    linarith
  have hs2F := floatModel.sub_mem _ (hFF ▸ hs1mem) _ (hFF ▸ hNP2) (bD2.trans (by rw [hMM]; norm_num))
  have hs2e := floatModel.sub_err _ (hFF ▸ hs1mem) _ (hFF ▸ hNP2) (bD2.trans (by rw [hMM]; norm_num))
  rw [hu, hη] at hs2e
  set s2 := floatModel.sub (x0 - N * P1) (N * P2) with hs2def
  have hN3F : (N : ℝ) ∈ floatModel.F := hFF ▸ hNmem
  have hP3F : P3 ∈ floatModel.F := hFF ▸ P3_mem
  have ha3F := floatModel.mul_mem _ hN3F _ hP3F (bNP3.trans (by rw [hMM]; norm_num))
  have ha3e := floatModel.mul_err _ hN3F _ hP3F (bNP3.trans (by rw [hMM]; norm_num))
  rw [hu, hη] at ha3e
  set a3 := floatModel.mul (N : ℝ) P3 with ha3def
  have bs2 : |s2| ≤ 15712 / 10000 := by
    have := abs_sub_abs_le_abs_sub s2 (x0 - N * P1 - N * P2)
    have : (1 / 2 ^ 53 : ℝ) * |x0 - N * P1 - N * P2| ≤ 1 / 10 ^ 10 := by
      have := mul_le_mul_of_nonneg_left bD2 (by norm_num : (0 : ℝ) ≤ 1 / 2 ^ 53)
      exact this.trans (by norm_num)
    have : (1 / 2 ^ 1075 : ℝ) ≤ 1 / 10 ^ 10 := by norm_num
    linarith
  have ba3 : |a3| ≤ 26 / 10 ^ 5 := by
    have := abs_sub_abs_le_abs_sub a3 (N * P3)
    have : (1 / 2 ^ 53 : ℝ) * |(N : ℝ) * P3| ≤ 1 / 10 ^ 10 := by
      have := mul_le_mul_of_nonneg_left bNP3 (by norm_num : (0 : ℝ) ≤ 1 / 2 ^ 53)
      exact this.trans (by norm_num)
    have : (1 / 2 ^ 1075 : ℝ) ≤ 1 / 10 ^ 10 := by norm_num
    linarith
  have bra : |s2 - a3| ≤ 15715 / 10000 := by
    have := abs_sub s2 a3; linarith
  have hrF := floatModel.sub_mem _ hs2F _ ha3F (bra.trans (by rw [hMM]; norm_num))
  have hre := floatModel.sub_err _ hs2F _ ha3F (bra.trans (by rw [hMM]; norm_num))
  rw [hu, hη] at hre
  have hr_eq : r = floatModel.sub s2 a3 := by
    show floatModel.sub (floatModel.sub (floatModel.sub x0
      (floatModel.mul (floatModel.round N) (floatModel.round P1)))
      (floatModel.mul (floatModel.round N) (floatModel.round P2)))
      (floatModel.mul (floatModel.round N) (floatModel.round P3)) = _
    rw [hnd, hP1r, hP2r, hP3r, ha1, ha2, hs1]
  -- the reduction error
  have hE_r : |floatModel.sub s2 a3 - (x0 - N * (P1 + P2 + P3))| ≤ 35 / 10 ^ 17 := by
    have e : floatModel.sub s2 a3 - (x0 - N * (P1 + P2 + P3)) =
        (floatModel.sub s2 a3 - (s2 - a3)) + (s2 - (x0 - N * P1 - N * P2)) - (a3 - N * P3) := by
      ring
    rw [e]
    have := abs_sub ((floatModel.sub s2 a3 - (s2 - a3)) + (s2 - (x0 - N * P1 - N * P2)))
      (a3 - N * P3)
    have := abs_add_le (floatModel.sub s2 a3 - (s2 - a3)) (s2 - (x0 - N * P1 - N * P2))
    have h1 : (1 / 2 ^ 53 : ℝ) * |s2 - a3| ≤ 1 / 2 ^ 53 * (15715 / 10000) :=
      mul_le_mul_of_nonneg_left bra (by norm_num)
    have h2 : (1 / 2 ^ 53 : ℝ) * |x0 - N * P1 - N * P2| ≤ 1 / 2 ^ 53 * (15711 / 10000) :=
      mul_le_mul_of_nonneg_left bD2 (by norm_num)
    have h3 : (1 / 2 ^ 53 : ℝ) * |(N : ℝ) * P3| ≤ 1 / 2 ^ 53 * (25 / 10 ^ 5) :=
      mul_le_mul_of_nonneg_left bNP3 (by norm_num)
    have : (1 / 2 ^ 53 * (15715 / 10000) + 1 / 2 ^ 1075 + (1 / 2 ^ 53 * (15711 / 10000) +
      1 / 2 ^ 1075) + (1 / 2 ^ 53 * (25 / 10 ^ 5) + 1 / 2 ^ 1075) : ℝ) ≤ 35 / 10 ^ 17 := by
      norm_num
    linarith
  have hxP : |x0 - N * (P1 + P2 + P3)| ≤ 15709 / 10000 := by
    have e : x0 - N * (P1 + P2 + P3) = (x0 - N * πd) + N * (πd - (P1 + P2 + P3)) := by ring
    have h1 : |(N : ℝ) * (πd - (P1 + P2 + P3))| ≤ 26 / 10 ^ 8 := by
      rw [abs_mul]
      exact (mul_le_mul hN9 P_pid (abs_nonneg _) (by norm_num)).trans (by norm_num)
    rw [e]
    have := abs_add_le (x0 - N * πd) (N * (πd - (P1 + P2 + P3)))
    linarith
  have hr1 : |r| ≤ 1571 / 1000 := by
    rw [hr_eq]
    have := abs_sub_abs_le_abs_sub (floatModel.sub s2 a3) (x0 - N * (P1 + P2 + P3))
    linarith
  have hrN : |r - (x0 - N * π)| ≤ 36 / 10 ^ 17 := by
    rw [hr_eq]
    have e : floatModel.sub s2 a3 - (x0 - N * π) =
        (floatModel.sub s2 a3 - (x0 - N * (P1 + P2 + P3))) + N * (π - (P1 + P2 + P3)) := by ring
    have h1 : |(N : ℝ) * (π - (P1 + P2 + P3))| ≤ 1 / 10 ^ 17 := by
      rw [abs_mul, abs_sub_comm]
      exact (mul_le_mul hN9 P_pi (abs_nonneg _) (by norm_num)).trans (by norm_num)
    rw [e]
    have := abs_add_le (floatModel.sub s2 a3 - (x0 - N * (P1 + P2 + P3))) (N * (π - (P1 + P2 + P3)))
    linarith
  have hrmem : r ∈ binary64 := by rw [hr_eq, ← hFF]; exact hrF
  refine ⟨hrmem, hr1, ?_⟩
  -- `kernel_sin` on `y = ±r`
  show |kernel_sin_ng floatModel.toNoGuard
    (if (realTrunc (floatModel.round N)).fmod 2 = 1 then -r else r) - sin x0| ≤ _
  rw [hnd, realTrunc_intCast]
  have hyF : (if N.fmod 2 = 1 then -r else r) ∈ floatModel.toNoGuard.F := by
    show _ ∈ binary64
    split_ifs
    exacts [neg_mem_binary64 hrmem, hrmem]
  have hy1 : |(if N.fmod 2 = 1 then -r else r)| ≤ 1571 / 1000 := by
    split_ifs <;> simpa using hr1
  have hks := kernel_sin_correct_wide floatModel.toNoGuard SinKernelsBinary64.small
    (by show (4 : ℝ) ≤ 2 ^ 1022; norm_num) hyF hy1
  have hcore := fast_core N x0 r _ _ hks
  have hyr : |(if N.fmod 2 = 1 then -r else r)| = |r| := by split_ifs <;> simp
  rw [hyr] at hcore
  have : |r| * (46 / 10 ^ 17 + 614 / 100 * (1 / 2 ^ 53) + 2107 * (1 / 2 ^ 1075)) + 1 / 2 ^ 1075
      ≤ 1571 / 1000 * (46 / 10 ^ 17 + 614 / 100 * (1 / 2 ^ 53) + 2107 * (1 / 2 ^ 1075)) +
        1 / 2 ^ 1075 := by
    have := mul_le_mul_of_nonneg_right hr1
      (by positivity : (0 : ℝ) ≤ 46 / 10 ^ 17 + 614 / 100 * (1 / 2 ^ 53) + 2107 * (1 / 2 ^ 1075))
    linarith
  have : (1571 / 1000 * (46 / 10 ^ 17 + 614 / 100 * (1 / 2 ^ 53) + 2107 * (1 / 2 ^ 1075)) +
      1 / 2 ^ 1075 + 36 / 10 ^ 17 : ℝ) ≤ 22 / 10 ^ 16 := by norm_num
  linarith

#print axioms kfastGen_binary64

end SinKernelsProof
