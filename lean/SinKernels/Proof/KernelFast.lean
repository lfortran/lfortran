import SinKernels.Proof.Reduction
import SinKernels.Proof.KernelSin
import LFortran.PiBounds
import LFortran.FloatExact

set_option exponentiation.threshold 2048

/-!
# `kernel_fast`: Cody–Waite reduction, then `kernel_sin`

`kernel_fast(x0)` picks `N = int(x0/π_d + sign(x0)/2)` (`chooseN`),
reduces `r = ((x0 - N p1) - N p2) - N p3` with the three-part representation
`p1 + p2 + p3` of `π` (`p1`, `p2` have 21-bit significands, and
`p1 + p2 + p3 - π ≈ -1.7·10^-31`), negates `r` for odd `N`, and returns
`kernel_sin(r)`. On any carrier, `kernel_fast_split` and `kfTail_eq` put the
generated code in this form once `N` is known and integers are exact.

* `kernel_fast_exact_correct`: in exact arithmetic the result is within
  `7.3·10^-16` of `sin x0` for `|x0| ≤ 10^12`.
* `kernel_fast_binary64`: in every correctly rounded binary64 `VModel` with a
  guard digit (Lean's `Float`, `SinKernels/FloatRun.lean`), for an input with
  value `|x0| ≤ 6·10^9`, the products `N p1`, `N p2` are exact, `x0 - N p1` is
  exact by Sterbenz's lemma (`sterbenz`), and the two remaining subtractions
  and `N p3` add at most `3.6·10^-16` (`π` to 32 decimals,
  `LFortran/PiBounds.lean`, bounds `N (p1 + p2 + p3 - π)`); with
  `kernel_sin`'s `1.8·10^-15` on `|r| ≤ 1.571`, the result is within
  `2.2·10^-15` of `sin x0`.
-/

open LFortran Real

namespace SinKernelsProof

/-- The literal `p1` (`3.14159202575683594e+00_dp`). -/
noncomputable def P1 : ℝ := 1647099 * (2 ^ 19)⁻¹
/-- The literal `p2` (`6.27832832833519205e-07_dp`). -/
noncomputable def P2 : ℝ := 1380619 * (2 ^ 41)⁻¹
/-- The literal `p3` (`1.24467443437932268e-13_dp`). -/
noncomputable def P3 : ℝ := 4930663418217751 * (2 ^ 95)⁻¹

theorem P1_eq : ((1647099 : ℤ) : ℝ) * (2 : ℝ) ^ (-19 : ℤ) = P1 := by norm_num [P1]
theorem P2_eq : ((1380619 : ℤ) : ℝ) * (2 : ℝ) ^ (-41 : ℤ) = P2 := by norm_num [P2]
theorem P3_eq : ((4930663418217751 : ℤ) : ℝ) * (2 : ℝ) ^ (-95 : ℤ) = P3 := by norm_num [P3]

/-- The generated `kernel_fast` in exact arithmetic. -/
noncomputable def kernel_fast_exact (x0 : ℝ) : ℝ :=
  @sin_kernels.kernel_fast proofKinds (FKinds.ofModel FPModel.exact FPModel.exact) x0

/-! ### The generated code, once `N` is computed -/

/-- The reduced argument `((x0 - nd p1) - nd p2) - nd p3`. -/
def kfR {ρ : Type} [FReal ρ] (x0 nd : ρ) : ρ :=
  FReal.sub (FReal.sub (FReal.sub x0 (FReal.mul nd (FReal.lit 1647099 (-19))))
    (FReal.mul nd (FReal.lit 1380619 (-41)))) (FReal.mul nd (FReal.lit 4930663418217751 (-95)))

/-- The generated `kernel_fast` after `N = int(...)` is computed. -/
def kfTail {K : Kinds} [FKinds K] (x0 : K.R8) (N : ℤ) : K.R8 :=
  let nd : K.R8 := FReal.ofInt (FInt.toInt (FInt.ofInt N : K.I4))
  if FInt.beq (FInt.modulo (FInt.ofInt (FReal.toInt nd) : K.I4) (FInt.ofInt 2)) (FInt.ofInt 1)
  then sin_kernels.kernel_sin (K := K) (FReal.neg (kfR x0 nd))
  else sin_kernels.kernel_sin (K := K) (kfR x0 nd)

theorem kernel_fast_split {K : Kinds} [FKinds K] (x0 : K.R8) :
    sin_kernels.kernel_fast (K := K) x0 = kfTail x0 (kfN x0) := rfl

/-- With exact `integer(4)` and an exactly converted `N`, the generated code
returns `kernel_sin(±r)`, negated for odd `N`. -/
theorem kfTail_eq {K : Kinds} [FKinds K] (V : VModel K.R8) (hsg : V.Signed) {B : ℤ}
    (I : IntExact K.I4 B) (hB : 2 < B) (hB63 : B < 2 ^ 63) {N : ℤ} (hN : InR B N)
    (hnd : V.val (FReal.ofInt N) N) (x0 : K.R8) :
    kfTail x0 N = sin_kernels.kernel_sin (K := K)
      (if N.fmod 2 = 1 then FReal.neg (kfR x0 (FReal.ofInt N)) else kfR x0 (FReal.ofInt N)) := by
  have hT : FReal.toInt (FReal.ofInt N : K.R8) = N := by
    have h63 : |N| < 2 ^ 63 := abs_lt.2 ⟨by have := hN.1; omega, by have := hN.2; omega⟩
    rw [hsg.toInt _ _ hnd (by rw [← Int.cast_abs]; exact_mod_cast h63), realTrunc_intCast]
  have h2 : InR B 2 := ⟨by omega, hB⟩
  have hf : InR B (N.fmod 2) := by
    have := Int.fmod_eq_emod_of_nonneg N (by norm_num : (0 : ℤ) ≤ 2)
    constructor <;> omega
  have hb : FInt.beq (FInt.modulo (FInt.ofInt N : K.I4) (FInt.ofInt 2)) (FInt.ofInt 1) = true ↔
      N.fmod 2 = 1 := by
    rw [I.modulo N 2 hN h2 (by norm_num)]
    exact I.beq _ _ hf ⟨by omega, by omega⟩
  unfold kfTail
  simp only [I.toInt_ofInt N hN, hT]
  by_cases h : N.fmod 2 = 1
  · rw [if_pos (hb.2 h), if_pos h]
  · rw [if_neg (fun h' => h (hb.1 h')), if_neg h]

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

/-- **Exact arithmetic.** With infinitely precise operations the generated
`kernel_fast` is within `7.3·10^-16` of `sin x0` for `|x0| ≤ 10^12`. -/
theorem kernel_fast_exact_correct {X x0 : ℝ} (hX : 1 ≤ X) (hX12 : X ≤ 10 ^ 12)
    (hx : |x0| ≤ X) : |kernel_fast_exact x0 - sin x0| ≤ 73 / 10 ^ 17 := by
  let m := (FPModel.exact (5 * X)).toNoGuard
  have hsg := VModel.ofNoGuard_signed m (fun _ _ => trivial)
  have A : @ReduceArith ℝ (FReal.ofNoGuard m) (VModel.ofNoGuard m) 0 0 X :=
    @ReduceArith.mk ℝ (FReal.ofNoGuard m) (VModel.ofNoGuard m) 0 0 X
      (@LFortran.Small.mk ℝ (FReal.ofNoGuard m) _ 0 0 le_rfl le_rfl (by norm_num [εU])
        (by norm_num [εU]) (by simp [m, FPModel.toNoGuard, FPModel.exact])
        (by simp [m, FPModel.toNoGuard, FPModel.exact]))
      hsg hX (by linarith [(by norm_num : (10 : ℝ) ^ 12 ≤ 2 ^ 50)]) (by norm_num [εU])
      (by norm_num [εU]) le_rfl (fun _ _ => ⟨rfl, trivial⟩)
  obtain ⟨hN, hNX, -⟩ := @chooseN ℝ (FReal.ofNoGuard m) (VModel.ofNoGuard m) 0 0 X A x0 x0
    ⟨rfl, trivial⟩ hx
  simp only [mul_zero, add_zero] at hN
  set N := @kfN ℝ (FReal.ofNoGuard m) x0 with hNdef
  have hN62 : InR (2 ^ 62) N := by
    have := abs_le.1 (hNX.trans hX12)
    constructor
    · have : (-(2 : ℝ) ^ 62) ≤ N := by linarith
      exact_mod_cast this
    · have : (N : ℝ) < 2 ^ 62 := by linarith
      exact_mod_cast this
  have e : kernel_fast_exact x0 = @sin_kernels.kernel_sin proofKinds
      (FKinds.ofModel FPModel.exact FPModel.exact)
      (if N.fmod 2 = 1 then -(x0 - N * P1 - N * P2 - N * P3) else x0 - N * P1 - N * P2 - N * P3) := by
    rw [kernel_fast_exact, @kernel_fast_split proofKinds (FKinds.ofModel FPModel.exact FPModel.exact),
      show @kfN ℝ (FReal.ofModel FPModel.exact) x0 = N from rfl,
      @kfTail_eq proofKinds (FKinds.ofModel FPModel.exact FPModel.exact)
      (VModel.ofNoGuard FPModel.exact.toNoGuard)
      (VModel.ofNoGuard_signed _ (fun _ _ => trivial)) _ (intExact_int _) (by norm_num)
      (by norm_num) N hN62 ⟨rfl, trivial⟩ x0]
    rw [← P1_eq, ← P2_eq, ← P3_eq]
    rfl
  rw [e]
  set r := x0 - N * P1 - N * P2 - N * P3 with hr
  have hks : @sin_kernels.kernel_sin proofKinds (FKinds.ofModel FPModel.exact FPModel.exact)
      (if N.fmod 2 = 1 then -r else r) = kernel_sin_exact (if N.fmod 2 = 1 then -r else r) := rfl
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

theorem P1_mem : P1 ∈ binary64 :=
  ⟨1647099, -19, by norm_num, by norm_num, by norm_num, by norm_num [P1]⟩
theorem P2_mem : P2 ∈ binary64 :=
  ⟨1380619, -41, by norm_num, by norm_num, by norm_num, by norm_num [P2]⟩
theorem P3_mem : P3 ∈ binary64 :=
  ⟨4930663418217751, -95, by norm_num, by norm_num, by norm_num, by norm_num [P3]⟩


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

/-- A `VModel` error bound in binary64 terms. -/
theorem b64_err {ρ : Type} [FReal ρ] {V : VModel ρ} (hB : V.Binary64) {v t : ℝ}
    (h : |v - t| ≤ V.u * |t| + V.η) : |v - t| ≤ 1 / 2 ^ 53 * |t| + 1 / 2 ^ 1075 := by
  have := mul_le_mul_of_nonneg_right hB.u (abs_nonneg t)
  linarith [hB.η]

set_option maxHeartbeats 4000000 in
/-- **`kernel_fast` in binary64.** In every correctly rounded binary64
`VModel` with a guard digit (`Float`: `SinKernels/FloatRun.lean`), with
exact `integer(4)` arithmetic, for an input with value `|x0| ≤ 6·10^9` the
result has a value within `2.2·10^-15` of `sin x0`. -/
theorem kernel_fast_binary64 {K : Kinds} [FKinds K] (V : VModel K.R8) (hB : V.Binary64)
    (hG : V.Guard) (hsg : V.Signed) (I : IntExact K.I4 (2 ^ 31)) {xv : K.R8} {x0 : ℝ}
    (hx : V.val xv x0) (hx1 : |x0| ≤ 6 * 10 ^ 9) :
    ∃ w, V.val (sin_kernels.kernel_fast (K := K) xv) w ∧ |w - sin x0| ≤ 22 / 10 ^ 16 := by
  have hxF := hB.mem _ _ hx
  have hMM := hB.M
  have hS : Small V (1 / 2 ^ 53) (1 / 2 ^ 1075) :=
    ⟨by positivity, by positivity, by norm_num [εU], by norm_num [εU], hB.u, hB.η⟩
  have hM' : ∀ c : ℝ, c ≤ 2 ^ 1022 → c ≤ V.M := fun c hc => hc.trans hMM
  -- the choice of `N`, at the scale of `x0`
  set Xl := max 1 |x0| with hXl
  have hXl1 : 1 ≤ Xl := le_max_left _ _
  have hxl : |x0| ≤ Xl := le_max_right _ _
  have hXlX : Xl ≤ 6 * 10 ^ 9 := max_le (by norm_num) hx1
  have A : ReduceArith V (1 / 2 ^ 53) (1 / 2 ^ 1075) Xl :=
    { small := hS
      signed := hsg
      X1 := hXl1
      X50 := hXlX.trans (by norm_num)
      XU := by
        have := mul_le_mul_of_nonneg_right hXlX (by positivity : (0 : ℝ) ≤ 1 / 2 ^ 53)
        exact this.trans (by norm_num [εU])
      XH := by
        have := mul_le_mul_of_nonneg_right hXlX (by positivity : (0 : ℝ) ≤ 1 / 2 ^ 1075)
        exact this.trans (by norm_num [εU])
      M := hM' _ (by linarith [(by norm_num : (5 : ℝ) * (6 * 10 ^ 9) ≤ 2 ^ 1022)])
      int_exact := fun n hn => hB.ofInt n (by
        have : |(n : ℝ)| < 2 ^ 53 := by linarith [(by norm_num : (6 * 10 ^ 9 : ℝ) < 2 ^ 53)]
        exact_mod_cast this) }
  obtain ⟨hN, hNXl, hnd⟩ := chooseN V A hx hxl
  set N := kfN xv with hNdef
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
  have hN9 : |(N : ℝ)| ≤ 2 * 10 ^ 9 := by
    have hq : |x0 / πd| ≤ 6 * 10 ^ 9 / (314159 / 100000) := by
      rw [abs_div, abs_of_pos (show (0 : ℝ) < πd by linarith)]
      exact div_le_div₀ (by norm_num) hx1 (by norm_num) hπd1
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
  have hN31 : InR (2 ^ 31) N := ⟨by omega, by omega⟩
  rw [kernel_fast_split, kfTail_eq V hsg I (by norm_num) (by norm_num) hN31 hnd xv]
  -- the literals are exact
  have hl1 : V.val (FReal.lit 1647099 (-19)) P1 := by
    rw [← P1_eq]; exact hB.lit _ _ (by norm_num [LitOk]) (by norm_num [abs_of_pos])
  have hl2 : V.val (FReal.lit 1380619 (-41)) P2 := by
    rw [← P2_eq]; exact hB.lit _ _ (by norm_num [LitOk]) (by norm_num [abs_of_pos])
  have hl3 : V.val (FReal.lit 4930663418217751 (-95)) P3 := by
    rw [← P3_eq]; exact hB.lit _ _ (by norm_num [LitOk]) (by norm_num [abs_of_pos])
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
  have ha1 := hB.mul _ _ _ _ hnd hl1 hNP1 (bNP1.trans (by norm_num))
  have ha2 := hB.mul _ _ _ _ hnd hl2 hNP2 (bNP2.trans (by norm_num))
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
    linarith
  have hs1 := hB.sub _ _ _ _ hx ha1 hs1mem bs1
  -- `D2 = x0 - N (p1 + p2)`, then two rounded subtractions and `N p3`
  have bD2 : |x0 - N * P1 - N * P2| ≤ 15711 / 10000 := by
    have e : x0 - N * P1 - N * P2 = (x0 - N * πd) + N * (πd - P1 - P2) := by ring
    have h1 : |(N : ℝ) * (πd - P1 - P2)| ≤ 25 / 10 ^ 5 := by
      rw [abs_mul]
      exact (mul_le_mul hN9 (by rw [abs_le]; norm_num [πd, P1, P2] :
        |πd - P1 - P2| ≤ 125 / 10 ^ 15) (abs_nonneg _) (by norm_num)).trans (by norm_num)
    have hxN' : |x0 - N * πd| ≤ 157082 / 100000 := by
      have := mul_le_mul hπd2 (show 1 / 2 + E ≤ 1 / 2 + 5 / 10 ^ 6 by linarith) (by positivity)
        (by norm_num)
      linarith
    rw [e]
    have := abs_add_le (x0 - N * πd) (N * (πd - P1 - P2))
    linarith
  obtain ⟨s2, hs2v, hs2e⟩ := hG.sub _ _ _ _ hs1 ha2 (hM' _ (bD2.trans (by norm_num)))
  have hs2e := b64_err hB hs2e
  obtain ⟨a3, ha3v, ha3e⟩ := V.mul_err _ _ _ _ hnd hl3 (hM' _ (bNP3.trans (by norm_num)))
  have ha3e := b64_err hB ha3e
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
  obtain ⟨r, hrv, hre⟩ := hG.sub _ _ _ _ hs2v ha3v (hM' _ (bra.trans (by norm_num)))
  have hre := b64_err hB hre
  -- the reduction error
  have hE_r : |r - (x0 - N * (P1 + P2 + P3))| ≤ 35 / 10 ^ 17 := by
    have e : r - (x0 - N * (P1 + P2 + P3)) =
        (r - (s2 - a3)) + (s2 - (x0 - N * P1 - N * P2)) - (a3 - N * P3) := by ring
    rw [e]
    have := abs_sub ((r - (s2 - a3)) + (s2 - (x0 - N * P1 - N * P2))) (a3 - N * P3)
    have := abs_add_le (r - (s2 - a3)) (s2 - (x0 - N * P1 - N * P2))
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
    have hxN' : |x0 - N * πd| ≤ 157082 / 100000 := by
      have := mul_le_mul hπd2 (show 1 / 2 + E ≤ 1 / 2 + 5 / 10 ^ 6 by linarith) (by positivity)
        (by norm_num)
      linarith
    rw [e]
    have := abs_add_le (x0 - N * πd) (N * (πd - (P1 + P2 + P3)))
    linarith
  have hr1 : |r| ≤ 1571 / 1000 := by
    have := abs_sub_abs_le_abs_sub r (x0 - N * (P1 + P2 + P3))
    linarith
  have hrN : |r - (x0 - N * π)| ≤ 36 / 10 ^ 17 := by
    have e : r - (x0 - N * π) = (r - (x0 - N * (P1 + P2 + P3))) + N * (π - (P1 + P2 + P3)) := by
      ring
    have h1 : |(N : ℝ) * (π - (P1 + P2 + P3))| ≤ 1 / 10 ^ 17 := by
      rw [abs_mul, abs_sub_comm]
      exact (mul_le_mul hN9 P_pi (abs_nonneg _) (by norm_num)).trans (by norm_num)
    rw [e]
    have := abs_add_le (r - (x0 - N * (P1 + P2 + P3))) (N * (π - (P1 + P2 + P3)))
    linarith
  -- `kernel_sin` on `y = ±r`
  have hyv : V.val (if N.fmod 2 = 1 then FReal.neg (kfR xv (FReal.ofInt N))
      else kfR xv (FReal.ofInt N)) (if N.fmod 2 = 1 then -r else r) := by
    split_ifs
    exacts [hsg.neg _ _ hrv, hrv]
  have hy1 : |(if N.fmod 2 = 1 then -r else r)| ≤ 1571 / 1000 := by
    split_ifs <;> simpa using hr1
  obtain ⟨w, hw, hks⟩ := kernel_sin_correct_wide V hS (hM' _ (by norm_num)) hyv hy1
  refine ⟨w, hw, ?_⟩
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

#print axioms kernel_fast_binary64

end SinKernelsProof
