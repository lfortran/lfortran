import SinKernels.Binary64
import SinKernels.Proof.KernelFast
import SinKernels.Spec
import LFortran.FloatSim

set_option exponentiation.threshold 2048

/-!
# The `Float` runs of the sin kernels

The theorems here are about the functions `lfortran --backend=lean` runs:
the generated templates at `LFortran.machine`, applied to a `Float`.

* `kernel_sin_float`: for a finite `Float` `xf` with value `x`, `|x| ≤ π/2`,
  `kernel_sin xf` is finite and within `1.15·10^-15 |x| + 2^-1075` of
  `Real.sin x`.
* `kernel_fastest_float`: for a finite `Float` `xf` with value `x0`,
  `|x0| ≤ X`, `1 ≤ X ≤ 2^30`, `kernel_fastest xf` is finite and within
  `5.75·10^-3 + 2.25·10^-15 X` of `Real.sin x0`.
* `kernel_fast_float`: for a finite `Float` `xf` with value `x0`,
  `|x0| ≤ 6·10^9`, `kernel_fast xf` is finite and within `2.2·10^-15` of
  `Real.sin x0`.

Each is proved by showing that the `Float` run has exactly the value of the
same template in `floatModel` (`*_float_sim`, using `LFortran/FloatSim.lean`
and the exact values of the literals, evaluated by the kernel), and applying
the binary64 theorems of `SinKernels/Binary64.lean`.
-/

open LFortran Real SinKernelsProof SinKernelsBinary64

namespace SinKernelsFloat

/-! ## The literals -/

theorem lit2 : Sim (FReal.lit (-3002399751580165) (-54) : Float) (((-3002399751580165 : ℤ) : ℝ) * (2 : ℝ) ^ (-54 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (-3002399751580165) (-54) : Float).toModel.unpack =
    .finite .negative 6004799503160330 (-55) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem lit3 : Sim (FReal.lit (2401919801243361) (-58) : Float) (((2401919801243361 : ℤ) : ℝ) * (2 : ℝ) ^ (-58 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (2401919801243361) (-58) : Float).toModel.unpack =
    .finite .positive 4803839602486722 (-59) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem lit4 : Sim (FReal.lit (-7320136529120817) (-65) : Float) (((-7320136529120817 : ℤ) : ℝ) * (2 : ℝ) ^ (-65 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (-7320136529120817) (-65) : Float).toModel.unpack =
    .finite .negative 7320136529120817 (-65) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem lit5 : Sim (FReal.lit (50834275503869) (-64) : Float) (((50834275503869 : ℤ) : ℝ) * (2 : ℝ) ^ (-64 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (50834275503869) (-64) : Float).toModel.unpack =
    .finite .positive 6506787264495232 (-71) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem lit6 : Sim (FReal.lit (-7571455092797619) (-78) : Float) (((-7571455092797619 : ℤ) : ℝ) * (2 : ℝ) ^ (-78 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (-7571455092797619) (-78) : Float).toModel.unpack =
    .finite .negative 7571455092797619 (-78) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem lit7 : Sim (FReal.lit (3103945121149347) (-84) : Float) (((3103945121149347 : ℤ) : ℝ) * (2 : ℝ) ^ (-84 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (3103945121149347) (-84) : Float).toModel.unpack =
    .finite .positive 6207890242298694 (-85) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem lit8 : Sim (FReal.lit (-1822480071209569) (-91) : Float) (((-1822480071209569 : ℤ) : ℝ) * (2 : ℝ) ^ (-91 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (-1822480071209569) (-91) : Float).toModel.unpack =
    .finite .negative 7289920284838276 (-93) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem litPi : Sim (FReal.lit (884279719003555) (-48) : Float) (((884279719003555 : ℤ) : ℝ) * (2 : ℝ) ^ (-48 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (884279719003555) (-48) : Float).toModel.unpack =
    .finite .positive 7074237752028440 (-51) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem litHalf : Sim (FReal.lit (1) (-1) : Float) (((1 : ℤ) : ℝ) * (2 : ℝ) ^ (-1 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (1) (-1) : Float).toModel.unpack =
    .finite .positive 4503599627370496 (-53) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem litS1 : Sim (FReal.lit (8240939) (-23) : Float) (((8240939 : ℤ) : ℝ) * (2 : ℝ) ^ (-23 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (8240939) (-23) : Float).toModel.unpack =
    .finite .positive 8848640873332736 (-53) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem litS2 : Sim (FReal.lit (-1175563) (-23) : Float) (((-1175563 : ℤ) : ℝ) * (2 : ℝ) ^ (-23 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (-1175563) (-23) : Float).toModel.unpack =
    .finite .negative 5049004639387648 (-55) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem litP1 : Sim (FReal.lit (1647099) (-19) : Float) (((1647099 : ℤ) : ℝ) * (2 : ℝ) ^ (-19 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (1647099) (-19) : Float).toModel.unpack =
    .finite .positive 7074236338274304 (-51) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem litP2 : Sim (FReal.lit (1380619) (-41) : Float) (((1380619 : ℤ) : ℝ) * (2 : ℝ) ^ (-41 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (1380619) (-41) : Float).toModel.unpack =
    .finite .positive 5929713453236224 (-73) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem litP3 : Sim (FReal.lit (4930663418217751) (-95) : Float) (((4930663418217751 : ℤ) : ℝ) * (2 : ℝ) ^ (-95 : ℤ)) := by
  unfold Sim floatValue modelValue
  rw [show (FReal.lit (4930663418217751) (-95) : Float).toModel.unpack =
    .finite .positive 4930663418217751 (-95) (by decide) from rfl]
  simp only [uvalue, bitValue_eq, sgn]
  norm_num

theorem lit_one : Sim (FReal.ofInt 1 : Float) ((1 : ℤ) : ℝ) :=
  float_ofInt_value 1 (by norm_num)

/-- A literal, as `floatModel` rounds it. -/
theorem sim_round {f : Float} {c : ℝ} (h : Sim f c) (hc : |c| ≤ 2 ^ 1022) :
    Sim f (floatModel.round c) := by
  rw [fm_round_of_sim h hc]; exact h

/-! ## Bounded steps -/

theorem smul {f g : Float} {x y A B : ℝ} (hf : Sim f x) (hg : Sim g y) (hx : |x| ≤ A)
    (hy : |y| ≤ B) (hAB : A * B ≤ 2 ^ 1022) :
    Sim (f * g) (floatModel.mul x y) ∧ |floatModel.mul x y| ≤ 2 * (A * B) + 1 := by
  have hxy : |x * y| ≤ A * B := by
    rw [abs_mul]; exact mul_le_mul hx hy (abs_nonneg _) ((abs_nonneg _).trans hx)
  obtain ⟨h1, h2⟩ := sim_mul hf hg (hxy.trans hAB)
  exact ⟨h1, by linarith⟩

theorem sadd {f g : Float} {x y A B : ℝ} (hf : Sim f x) (hg : Sim g y) (hx : |x| ≤ A)
    (hy : |y| ≤ B) (hAB : A + B ≤ 2 ^ 1022) :
    Sim (f + g) (floatModel.add x y) ∧ |floatModel.add x y| ≤ 2 * (A + B) + 1 := by
  have hxy : |x + y| ≤ A + B := (abs_add_le _ _).trans (add_le_add hx hy)
  obtain ⟨h1, h2⟩ := sim_add hf hg (hxy.trans hAB)
  exact ⟨h1, by linarith⟩

theorem ssub {f g : Float} {x y A B : ℝ} (hf : Sim f x) (hg : Sim g y) (hx : |x| ≤ A)
    (hy : |y| ≤ B) (hAB : A + B ≤ 2 ^ 1022) :
    Sim (f - g) (floatModel.sub x y) ∧ |floatModel.sub x y| ≤ 2 * (A + B) + 1 := by
  have hxy : |x - y| ≤ A + B := (abs_sub _ _).trans (add_le_add hx hy)
  obtain ⟨h1, h2⟩ := sim_sub hf hg (hxy.trans hAB)
  exact ⟨h1, by linarith⟩

/-! ## `kernel_sin` -/

theorem kernel_sin_float_sim {xf : Float} {x : ℝ} (hx : Sim xf x) (hx1 : |x| ≤ 8 / 5) :
    Sim (sin_kernels.kernel_sin (K := LFortran.machine) xf)
      (kernel_sin_ng floatModel.toNoGuard x) := by
  obtain ⟨hz, bz⟩ := smul hx hx hx1 hx1 (by norm_num)
  have h8 := sim_round lit8 (by norm_num)
  have b8 : |floatModel.round (((-1822480071209569 : ℤ) : ℝ) * (2 : ℝ) ^ (-91 : ℤ))| ≤ 1 := by
    rw [fm_round_of_sim lit8 (by norm_num)]; norm_num
  obtain ⟨p7, bp7⟩ := smul hz h8 bz b8 (by norm_num)
  have l7 := sim_round lit7 (by norm_num)
  have bl7 : |floatModel.round (((3103945121149347 : ℤ) : ℝ) * (2 : ℝ) ^ (-84 : ℤ))| ≤ 1 := by rw [fm_round_of_sim lit7 (by norm_num)]; norm_num
  obtain ⟨h7, b7⟩ := sadd l7 p7 bl7 bp7 (by norm_num)
  obtain ⟨p6, bp6⟩ := smul hz h7 bz b7 (by norm_num)
  have l6 := sim_round lit6 (by norm_num)
  have bl6 : |floatModel.round (((-7571455092797619 : ℤ) : ℝ) * (2 : ℝ) ^ (-78 : ℤ))| ≤ 1 := by rw [fm_round_of_sim lit6 (by norm_num)]; norm_num
  obtain ⟨h6, b6⟩ := sadd l6 p6 bl6 bp6 (by norm_num)
  obtain ⟨p5, bp5⟩ := smul hz h6 bz b6 (by norm_num)
  have l5 := sim_round lit5 (by norm_num)
  have bl5 : |floatModel.round (((50834275503869 : ℤ) : ℝ) * (2 : ℝ) ^ (-64 : ℤ))| ≤ 1 := by rw [fm_round_of_sim lit5 (by norm_num)]; norm_num
  obtain ⟨h5, b5⟩ := sadd l5 p5 bl5 bp5 (by norm_num)
  obtain ⟨p4, bp4⟩ := smul hz h5 bz b5 (by norm_num)
  have l4 := sim_round lit4 (by norm_num)
  have bl4 : |floatModel.round (((-7320136529120817 : ℤ) : ℝ) * (2 : ℝ) ^ (-65 : ℤ))| ≤ 1 := by rw [fm_round_of_sim lit4 (by norm_num)]; norm_num
  obtain ⟨h4, b4⟩ := sadd l4 p4 bl4 bp4 (by norm_num)
  obtain ⟨p3, bp3⟩ := smul hz h4 bz b4 (by norm_num)
  have l3 := sim_round lit3 (by norm_num)
  have bl3 : |floatModel.round (((2401919801243361 : ℤ) : ℝ) * (2 : ℝ) ^ (-58 : ℤ))| ≤ 1 := by rw [fm_round_of_sim lit3 (by norm_num)]; norm_num
  obtain ⟨h3, b3⟩ := sadd l3 p3 bl3 bp3 (by norm_num)
  obtain ⟨p2, bp2⟩ := smul hz h3 bz b3 (by norm_num)
  have l2 := sim_round lit2 (by norm_num)
  have bl2 : |floatModel.round (((-3002399751580165 : ℤ) : ℝ) * (2 : ℝ) ^ (-54 : ℤ))| ≤ 1 := by rw [fm_round_of_sim lit2 (by norm_num)]; norm_num
  obtain ⟨h2, b2⟩ := sadd l2 p2 bl2 bp2 (by norm_num)
  obtain ⟨p1, bp1⟩ := smul hz h2 bz b2 (by norm_num)
  have l1 := sim_round lit_one (by norm_num)
  have bl1 : |floatModel.round ((1 : ℤ) : ℝ)| ≤ 1 := by
    rw [fm_round_of_sim lit_one (by norm_num)]; norm_num
  obtain ⟨h1, b1⟩ := sadd l1 p1 bl1 bp1 (by norm_num)
  obtain ⟨hr, -⟩ := smul hx h1 hx1 b1 (by norm_num)
  exact hr

/-- **The `Float` run of `kernel_sin`** is within `1.15·10^-15 |x| + 2^-1075` of
`Real.sin x` for every finite `Float` with `|x| ≤ π/2`. -/
theorem kernel_sin_float {xf : Float} {x : ℝ} (hx : Sim xf x) (hx1 : |x| ≤ π / 2) :
    ∃ v, Sim (sin_kernels.kernel_sin (K := LFortran.machine) xf) v ∧
      |v - Real.sin x| ≤ 115 / 10 ^ 17 * |x| + 1 / 2 ^ 1075 :=
  ⟨_, kernel_sin_float_sim hx (by linarith [pi_lt_d2]),
    kernel_sin_binary64 (float_mem hx) hx1⟩

#print axioms kernel_sin_float

/-! ## `kernel_fastest` -/

/-- The polynomial of `kernel_fastest` in `Float`. -/
def polyF (y : Float) : Float :=
  y * (FReal.lit 8240939 (-23) + y * y * FReal.lit (-1175563) (-23))

/-- The generated `kernel_fastest` at `machine`, step by step. -/
theorem kernel_fastest_machine_eq (xf : Float) :
    sin_kernels.kernel_fastest (K := LFortran.machine) xf =
      let PI : Float := FReal.lit 884279719003555 (-48)
      let s : Float :=
        if floatSignbit xf then -(Float.abs (Float.ofInt 1)) else Float.abs (Float.ofInt 1)
      let N : Int := floatTrunc (xf / PI + FReal.lit 1 (-1) * s)
      let nd : Float := Float.ofInt (Int32.ofInt N).toInt
      let r : Float := xf - nd * PI
      if FInt.beq (FInt.modulo (Int32.ofInt (floatTrunc nd)) (FInt.ofInt 2)) (FInt.ofInt 1)
      then polyF (-r) else polyF r :=
  rfl

theorem πd_eq : ((884279719003555 : ℤ) : ℝ) * (2 : ℝ) ^ (-48 : ℤ) = πd := by norm_num [πd]
theorem S1_eq : ((8240939 : ℤ) : ℝ) * (2 : ℝ) ^ (-23 : ℤ) = S1 := by norm_num [S1]
theorem S2_eq : ((-1175563 : ℤ) : ℝ) * (2 : ℝ) ^ (-23 : ℤ) = S2 := by norm_num [S2]
theorem half_eq : ((1 : ℤ) : ℝ) * (2 : ℝ) ^ (-1 : ℤ) = 2⁻¹ := by norm_num

theorem polyF_sim {yf : Float} {y : ℝ} (hy : Sim yf y) (hy1 : |y| ≤ 8 / 5) :
    Sim (polyF yf) (floatModel.toNoGuard.mul y (floatModel.toNoGuard.add
      (floatModel.toNoGuard.round S1)
      (floatModel.toNoGuard.mul (floatModel.toNoGuard.mul y y) (floatModel.toNoGuard.round S2)))) := by
  have l1 : Sim (FReal.lit 8240939 (-23) : Float) S1 := by rw [← S1_eq]; exact litS1
  have l2 : Sim (FReal.lit (-1175563) (-23) : Float) S2 := by rw [← S2_eq]; exact litS2
  have c1 : |S1| ≤ 2 ^ 1022 := by norm_num [S1, abs_of_pos]
  have c2 : |S2| ≤ 2 ^ 1022 := by norm_num [S2, abs_of_neg]
  have r1 := sim_round l1 c1
  have r2 := sim_round l2 c2
  have b1 : |floatModel.round S1| ≤ 1 := by
    rw [fm_round_of_sim l1 c1]; norm_num [S1, abs_of_pos]
  have b2 : |floatModel.round S2| ≤ 1 := by
    rw [fm_round_of_sim l2 c2]; norm_num [S2, abs_of_neg]
  obtain ⟨hz, bz⟩ := smul hy hy hy1 hy1 (by norm_num)
  obtain ⟨hp, bp⟩ := smul hz r2 bz b2 (by norm_num)
  obtain ⟨ha, ba⟩ := sadd r1 hp b1 bp (by norm_num)
  obtain ⟨hr, -⟩ := smul hy ha hy1 ba (by norm_num)
  exact hr

/-- The sign bit of a `Float` is a valid sign choice for its value. -/
theorem signOk_float {xf : Float} {x0 : ℝ} (hx : Sim xf x0) : SignOk (floatSignbit xf) x0 := by
  by_cases h0 : x0 = 0
  · subst h0; exact ⟨fun _ => le_rfl, fun _ => le_rfl⟩
  · rw [floatSignbit_eq hx h0]; exact signOk_decide x0

/-- The `Float` run after `N` is computed. -/
def kfTailF (xf : Float) (N : Int) : Float :=
  let PI : Float := FReal.lit 884279719003555 (-48)
  let nd : Float := Float.ofInt (Int32.ofInt N).toInt
  let r : Float := xf - nd * PI
  if FInt.beq (FInt.modulo (Int32.ofInt (floatTrunc nd)) (FInt.ofInt 2)) (FInt.ofInt 1)
  then polyF (-r) else polyF r

/-- The `floatModel` run after `N` is computed. -/
noncomputable def kfTailR (x0 : ℝ) (N : ℤ) : ℝ :=
  let r := floatModel.sub x0 (floatModel.mul (floatModel.round N) (floatModel.round πd))
  let y := if (realTrunc (floatModel.round N)).fmod 2 = 1 then -r else r
  floatModel.mul y (floatModel.add (floatModel.round S1)
    (floatModel.mul (floatModel.mul y y) (floatModel.round S2)))

theorem kf_machine_tail (xf : Float) :
    sin_kernels.kernel_fastest (K := LFortran.machine) xf =
      kfTailF xf (floatTrunc (xf / (FReal.lit 884279719003555 (-48) : Float)
        + (FReal.lit 1 (-1) : Float) * (if floatSignbit xf then -(Float.abs (Float.ofInt 1))
          else Float.abs (Float.ofInt 1)))) :=
  rfl

theorem kfGen_tail (b : Bool) (x0 : ℝ) :
    kfGen floatModel.toNoGuard b x0 =
      kfTailR x0 (realTrunc (floatModel.add (floatModel.div x0 (floatModel.round πd))
        (floatModel.mul (floatModel.round 2⁻¹)
          (if b then -|floatModel.round 1| else |floatModel.round 1|)))) :=
  rfl

/-- The tails agree for every `N` that the arithmetic represents exactly and
that reduces `x0` to `|r| ≤ 1.6`. -/
theorem tail_sim {xf : Float} {x0 X : ℝ} (hx : Sim xf x0) (hX : X ≤ 2 ^ 30) (hx1 : |x0| ≤ X)
    (N : ℤ) (hNX : |(N : ℝ)| ≤ X) (hnd : floatModel.round N = N)
    (hr : |floatModel.sub x0 (floatModel.mul (floatModel.round N) (floatModel.round πd))| ≤ 8 / 5) :
    Sim (kfTailF xf N) (kfTailR x0 N) := by
  have lPI : Sim (FReal.lit 884279719003555 (-48) : Float) πd := by rw [← πd_eq]; exact litPi
  have cPI : |πd| ≤ 2 ^ 1022 := by norm_num [πd, abs_of_pos]
  have ePI : floatModel.round πd = πd := fm_round_of_sim lPI cPI
  have hPI := sim_round lPI cPI
  have πd2 : πd ≤ 4 := by norm_num [πd]
  have πd1 : (3 : ℝ) ≤ πd := by norm_num [πd]
  have hN31 : -2 ^ 31 ≤ N ∧ N < 2 ^ 31 := by
    have := abs_le.1 hNX
    constructor
    · have : (-(2 : ℝ) ^ 31) ≤ N := by linarith
      exact_mod_cast this
    · have : (N : ℝ) < 2 ^ 31 := by linarith
      exact_mod_cast this
  have hnd' : Sim (Float.ofInt N) (floatModel.round N) := by
    rw [hnd]
    exact float_ofInt_value N (by
      have : |(N : ℝ)| < 2 ^ 53 := by linarith
      exact_mod_cast this)
  have hndT := floatTrunc_eq hnd' (by rw [hnd]; linarith)
  have hRT : realTrunc (floatModel.round N) = N := by rw [hnd, realTrunc_intCast]
  have hpar := int32_parity (realTrunc (floatModel.round N)) (by rw [hRT]; exact hN31.1)
    (by rw [hRT]; exact hN31.2)
  have bN : |floatModel.round N| ≤ X := by rw [hnd]; exact hNX
  have bPI : |floatModel.round πd| ≤ 4 := by rw [ePI, abs_of_pos (by linarith)]; exact πd2
  obtain ⟨hp, bp⟩ := smul hnd' hPI bN bPI (by linarith)
  obtain ⟨hrr, -⟩ := ssub hx hp hx1 bp (by linarith)
  unfold kfTailF kfTailR
  simp only [Int32.toInt_ofInt_of_le hN31.1 hN31.2, hndT, hpar]
  by_cases hP : (realTrunc (floatModel.round N)).fmod 2 = 1
  · simp only [hP, decide_true, if_true]
    exact polyF_sim (float_neg_value hrr) (by rw [abs_neg]; exact hr)
  · simp only [hP, decide_false, if_false, Bool.false_eq_true]
    exact polyF_sim hrr hr

set_option maxHeartbeats 1000000 in
/-- **The `Float` run of `kernel_fastest` is the `floatModel` run** (with the sign
of `x0` read from the sign bit, as the `Float` run does). -/
theorem kernel_fastest_float_sim {xf : Float} {x0 X : ℝ} (hx : Sim xf x0) (hX1 : 1 ≤ X)
    (hX : X ≤ 2 ^ 30) (hx1 : |x0| ≤ X) :
    Sim (sin_kernels.kernel_fastest (K := LFortran.machine) xf)
      (kfGen floatModel.toNoGuard (floatSignbit xf) x0) := by
  have hb := signOk_float hx
  have hred := reduction _ (fastestArith_binary64 hX1 (hX.trans (by norm_num))) (float_mem hx) hx1 hb
  simp only [tng_round, tng_add, tng_sub, tng_mul, tng_div] at hred
  obtain ⟨hNX, hnd, -, hr, -⟩ := hred
  have h1022 : X ≤ 2 ^ 1022 := hX.trans (by norm_num)
  have lPI : Sim (FReal.lit 884279719003555 (-48) : Float) πd := by rw [← πd_eq]; exact litPi
  have cPI : |πd| ≤ 2 ^ 1022 := by norm_num [πd, abs_of_pos]
  have ePI : floatModel.round πd = πd := fm_round_of_sim lPI cPI
  have hPI := sim_round lPI cPI
  have πd1 : (3 : ℝ) ≤ πd := by norm_num [πd]
  have lH : Sim (FReal.lit 1 (-1) : Float) 2⁻¹ := by rw [← half_eq]; exact litHalf
  have hH := sim_round lH (by norm_num)
  have bH : |floatModel.round 2⁻¹| ≤ 1 := by rw [fm_round_of_sim lH (by norm_num)]; norm_num
  have l1 : Sim (Float.ofInt 1) 1 := by simpa using float_ofInt_value 1 (by norm_num)
  have e1 : floatModel.round 1 = 1 := fm_round_of_sim l1 (by norm_num)
  have hA := float_abs_value (sim_round l1 (by norm_num))
  have hs : Sim (if floatSignbit xf then -(Float.abs (Float.ofInt 1)) else Float.abs (Float.ofInt 1))
      (if floatSignbit xf then -|floatModel.round 1| else |floatModel.round 1|) := by
    split_ifs
    · exact float_neg_value hA
    · exact hA
  have bs : |(if floatSignbit xf then -|floatModel.round 1|
      else |floatModel.round 1|)| ≤ 1 := by
    split_ifs <;> simp [e1]
  have hq0 : |x0 / floatModel.round πd| ≤ X := by
    rw [ePI, abs_div, abs_of_pos (show (0 : ℝ) < πd by linarith),
      div_le_iff₀ (show (0 : ℝ) < πd by linarith)]
    nlinarith [abs_nonneg x0]
  obtain ⟨hq, bq⟩ := sim_div hx hPI (by rw [ePI]; linarith) (hq0.trans h1022)
  obtain ⟨hhs, bhs⟩ := smul hH hs bH bs (by norm_num)
  obtain ⟨ht, bt⟩ := sadd hq hhs
    (bq.trans (by linarith : 2 * |x0 / floatModel.round πd| + 1 ≤ 2 * X + 1)) bhs (by linarith)
  have hN := floatTrunc_eq ht (by rw [abs_lt]; constructor <;> nlinarith [abs_le.1 bt])
  rw [kf_machine_tail, kfGen_tail, hN]
  exact tail_sim hx hX hx1 _ hNX hnd hr
#print axioms kernel_fastest_float_sim

/-- **The `Float` run of `kernel_fastest`** is within `5.75·10^-3 + 2.25·10^-15 X`
of `Real.sin x0` for every finite `Float` `x0` with `|x0| ≤ X`, `1 ≤ X ≤ 2^30`. -/
theorem kernel_fastest_float {xf : Float} {x0 X : ℝ} (hx : Sim xf x0) (hX1 : 1 ≤ X)
    (hX : X ≤ 2 ^ 30) (hx1 : |x0| ≤ X) :
    ∃ v, Sim (sin_kernels.kernel_fastest (K := LFortran.machine) xf) v ∧
      |v - Real.sin x0| ≤ 575 / 10 ^ 5 + X * (225 / 10 ^ 17) :=
  ⟨_, kernel_fastest_float_sim hx hX1 hX hx1,
    kfGen_binary64 hX1 hX (float_mem hx) hx1 (signOk_float hx)⟩

#print axioms kernel_fastest_float

/-- The generated `kernel_sin`, run at `Float`, meets `KernelSinFloatSpec`. -/
theorem kernel_sin_spec :
    SinKernels.KernelSinFloatSpec (sin_kernels.kernel_sin (K := LFortran.machine)) :=
  fun _ _ hx hx1 => kernel_sin_float hx hx1

/-- The generated `kernel_fastest`, run at `Float`, meets `KernelFastestFloatSpec`. -/
theorem kernel_fastest_spec :
    SinKernels.KernelFastestFloatSpec (sin_kernels.kernel_fastest (K := LFortran.machine)) :=
  fun _ _ _ hx hX1 hX hx1 => kernel_fastest_float hx hX1 hX hx1

#print axioms kernel_sin_spec
#print axioms kernel_fastest_spec

/-! ## `kernel_fast` -/

/-- The `Float` run of `kernel_fast` after `N` is computed. -/
def kfastTailF (xf : Float) (N : Int) : Float :=
  let nd : Float := Float.ofInt (Int32.ofInt N).toInt
  let r : Float := ((xf - nd * FReal.lit 1647099 (-19)) - nd * FReal.lit 1380619 (-41))
    - nd * FReal.lit 4930663418217751 (-95)
  if FInt.beq (FInt.modulo (Int32.ofInt (floatTrunc nd)) (FInt.ofInt 2)) (FInt.ofInt 1)
  then sin_kernels.kernel_sin (K := LFortran.machine) (-r)
  else sin_kernels.kernel_sin (K := LFortran.machine) r

/-- The `floatModel` run of `kernel_fast` after `N` is computed. -/
noncomputable def kfastTailR (x0 : ℝ) (N : ℤ) : ℝ :=
  let nd := floatModel.round N
  let r := floatModel.sub (floatModel.sub (floatModel.sub x0
    (floatModel.mul nd (floatModel.round P1))) (floatModel.mul nd (floatModel.round P2)))
    (floatModel.mul nd (floatModel.round P3))
  kernel_sin_ng floatModel.toNoGuard (if (realTrunc nd).fmod 2 = 1 then -r else r)

theorem kfast_machine_tail (xf : Float) :
    sin_kernels.kernel_fast (K := LFortran.machine) xf =
      kfastTailF xf (floatTrunc (xf / (FReal.lit 884279719003555 (-48) : Float)
        + (FReal.lit 1 (-1) : Float) * (if floatSignbit xf then -(Float.abs (Float.ofInt 1))
          else Float.abs (Float.ofInt 1)))) :=
  rfl

theorem kfastGen_tail (b : Bool) (x0 : ℝ) :
    kfastGen floatModel.toNoGuard b x0 =
      kfastTailR x0 (realTrunc (floatModel.add (floatModel.div x0 (floatModel.round πd))
        (floatModel.mul (floatModel.round 2⁻¹)
          (if b then -|floatModel.round 1| else |floatModel.round 1|)))) :=
  rfl

theorem P1_eq : ((1647099 : ℤ) : ℝ) * (2 : ℝ) ^ (-19 : ℤ) = P1 := by norm_num [P1]
theorem P2_eq : ((1380619 : ℤ) : ℝ) * (2 : ℝ) ^ (-41 : ℤ) = P2 := by norm_num [P2]
theorem P3_eq : ((4930663418217751 : ℤ) : ℝ) * (2 : ℝ) ^ (-95 : ℤ) = P3 := by norm_num [P3]

/-- The tails agree for an exact `N`, `|N| ≤ 2·10^9`, that reduces `x0` to
`|r| ≤ 1.571`. -/
theorem fast_tail_sim {xf : Float} {x0 : ℝ} (hx : Sim xf x0) (hx1 : |x0| ≤ 6 * 10 ^ 9)
    (N : ℤ) (hN9 : |(N : ℝ)| ≤ 2 * 10 ^ 9) (hnd : floatModel.round N = N)
    (hr : |floatModel.sub (floatModel.sub (floatModel.sub x0
      (floatModel.mul (floatModel.round N) (floatModel.round P1)))
      (floatModel.mul (floatModel.round N) (floatModel.round P2)))
      (floatModel.mul (floatModel.round N) (floatModel.round P3))| ≤ 1571 / 1000) :
    Sim (kfastTailF xf N) (kfastTailR x0 N) := by
  have hN31 : -2 ^ 31 ≤ N ∧ N < 2 ^ 31 := by
    have := abs_le.1 hN9
    constructor
    · have : (-(2 : ℝ) ^ 31) ≤ N := by linarith
      exact_mod_cast this
    · have : (N : ℝ) < 2 ^ 31 := by linarith
      exact_mod_cast this
  have hnd' : Sim (Float.ofInt N) (floatModel.round N) := by
    rw [hnd]
    exact float_ofInt_value N (by
      have : |(N : ℝ)| < 2 ^ 53 := by linarith
      exact_mod_cast this)
  have hndT := floatTrunc_eq hnd' (by rw [hnd]; linarith)
  have hRT : realTrunc (floatModel.round N) = N := by rw [hnd, realTrunc_intCast]
  have hpar := int32_parity (realTrunc (floatModel.round N)) (by rw [hRT]; exact hN31.1)
    (by rw [hRT]; exact hN31.2)
  have bN : |floatModel.round N| ≤ 2 * 10 ^ 9 := by rw [hnd]; exact hN9
  have l1 : Sim (FReal.lit 1647099 (-19) : Float) P1 := by rw [← P1_eq]; exact litP1
  have l2 : Sim (FReal.lit 1380619 (-41) : Float) P2 := by rw [← P2_eq]; exact litP2
  have l3 : Sim (FReal.lit 4930663418217751 (-95) : Float) P3 := by rw [← P3_eq]; exact litP3
  have c1 : |P1| ≤ 2 ^ 1022 := by norm_num [P1, abs_of_pos]
  have c2 : |P2| ≤ 2 ^ 1022 := by norm_num [P2, abs_of_pos]
  have c3 : |P3| ≤ 2 ^ 1022 := by norm_num [P3, abs_of_pos]
  have b1 : |floatModel.round P1| ≤ 4 := by rw [fm_round_of_sim l1 c1]; norm_num [P1, abs_of_pos]
  have b2 : |floatModel.round P2| ≤ 1 := by rw [fm_round_of_sim l2 c2]; norm_num [P2, abs_of_pos]
  have b3 : |floatModel.round P3| ≤ 1 := by rw [fm_round_of_sim l3 c3]; norm_num [P3, abs_of_pos]
  obtain ⟨hp1, bp1⟩ := smul hnd' (sim_round l1 c1) bN b1 (by norm_num)
  obtain ⟨hp2, bp2⟩ := smul hnd' (sim_round l2 c2) bN b2 (by norm_num)
  obtain ⟨hp3, bp3⟩ := smul hnd' (sim_round l3 c3) bN b3 (by norm_num)
  obtain ⟨hs1, bs1⟩ := ssub hx hp1 hx1 bp1 (by norm_num)
  obtain ⟨hs2, bs2⟩ := ssub hs1 hp2 bs1 bp2 (by norm_num)
  obtain ⟨hrr, -⟩ := ssub hs2 hp3 bs2 bp3 (by norm_num)
  unfold kfastTailF kfastTailR
  simp only [Int32.toInt_ofInt_of_le hN31.1 hN31.2, hndT, hpar]
  by_cases hP : (realTrunc (floatModel.round N)).fmod 2 = 1
  · simp only [hP, decide_true, if_true]
    exact kernel_sin_float_sim (float_neg_value hrr) (by rw [abs_neg]; linarith)
  · simp only [hP, decide_false, if_false, Bool.false_eq_true]
    exact kernel_sin_float_sim hrr (by linarith)

set_option maxHeartbeats 1000000 in
/-- **The `Float` run of `kernel_fast` is the `floatModel` run.** -/
theorem kernel_fast_float_sim {xf : Float} {x0 : ℝ} (hx : Sim xf x0) (hx1 : |x0| ≤ 6 * 10 ^ 9) :
    Sim (sin_kernels.kernel_fast (K := LFortran.machine) xf)
      (kfastGen floatModel.toNoGuard (floatSignbit xf) x0) := by
  have hb := signOk_float hx
  set X := max 1 |x0| with hXdef
  have hX1 : 1 ≤ X := le_max_left _ _
  have hxX : |x0| ≤ X := le_max_right _ _
  have hX : X ≤ 6 * 10 ^ 9 := max_le (by norm_num) hx1
  have hfast := kfastGen_binary64 hX1 hX (float_mem hx) hxX hb
  simp only at hfast
  obtain ⟨hN9, hnd, -, hr, -⟩ := hfast
  have h1022 : X ≤ 2 ^ 1022 := hX.trans (by norm_num)
  have lPI : Sim (FReal.lit 884279719003555 (-48) : Float) πd := by rw [← πd_eq]; exact litPi
  have cPI : |πd| ≤ 2 ^ 1022 := by norm_num [πd, abs_of_pos]
  have ePI : floatModel.round πd = πd := fm_round_of_sim lPI cPI
  have hPI := sim_round lPI cPI
  have πd1 : (3 : ℝ) ≤ πd := by norm_num [πd]
  have lH : Sim (FReal.lit 1 (-1) : Float) 2⁻¹ := by rw [← half_eq]; exact litHalf
  have hH := sim_round lH (by norm_num)
  have bH : |floatModel.round 2⁻¹| ≤ 1 := by rw [fm_round_of_sim lH (by norm_num)]; norm_num
  have l1 : Sim (Float.ofInt 1) 1 := by simpa using float_ofInt_value 1 (by norm_num)
  have e1 : floatModel.round 1 = 1 := fm_round_of_sim l1 (by norm_num)
  have hA := float_abs_value (sim_round l1 (by norm_num))
  have hs : Sim (if floatSignbit xf then -(Float.abs (Float.ofInt 1)) else Float.abs (Float.ofInt 1))
      (if floatSignbit xf then -|floatModel.round 1| else |floatModel.round 1|) := by
    split_ifs
    · exact float_neg_value hA
    · exact hA
  have bs : |(if floatSignbit xf then -|floatModel.round 1|
      else |floatModel.round 1|)| ≤ 1 := by
    split_ifs <;> simp [e1]
  have hq0 : |x0 / floatModel.round πd| ≤ X := by
    rw [ePI, abs_div, abs_of_pos (show (0 : ℝ) < πd by linarith),
      div_le_iff₀ (show (0 : ℝ) < πd by linarith)]
    nlinarith [abs_nonneg x0]
  obtain ⟨hq, bq⟩ := sim_div hx hPI (by rw [ePI]; linarith) (hq0.trans h1022)
  obtain ⟨hhs, bhs⟩ := smul hH hs bH bs (by norm_num)
  obtain ⟨ht, bt⟩ := sadd hq hhs
    (bq.trans (by linarith : 2 * |x0 / floatModel.round πd| + 1 ≤ 2 * X + 1)) bhs (by linarith)
  have hN := floatTrunc_eq ht (by rw [abs_lt]; constructor <;> nlinarith [abs_le.1 bt])
  rw [kfast_machine_tail, kfastGen_tail, hN]
  exact fast_tail_sim hx hx1 _ hN9 hnd hr

#print axioms kernel_fast_float_sim

/-- **The `Float` run of `kernel_fast`** is within `2.2·10^-15` of `Real.sin x0`
for every finite `Float` `x0` with `|x0| ≤ 6·10^9`. -/
theorem kernel_fast_float {xf : Float} {x0 : ℝ} (hx : Sim xf x0) (hx1 : |x0| ≤ 6 * 10 ^ 9) :
    ∃ v, Sim (sin_kernels.kernel_fast (K := LFortran.machine) xf) v ∧
      |v - Real.sin x0| ≤ 22 / 10 ^ 16 := by
  refine ⟨_, kernel_fast_float_sim hx hx1, ?_⟩
  have h := kfastGen_binary64 (X := max 1 |x0|) (le_max_left _ _) (max_le (by norm_num) hx1)
    (float_mem hx) (le_max_right _ _) (signOk_float hx)
  simp only at h
  exact h.2.2.2.2

/-- The generated `kernel_fast`, run at `Float`, meets `KernelFastFloatSpec`. -/
theorem kernel_fast_spec :
    SinKernels.KernelFastFloatSpec (sin_kernels.kernel_fast (K := LFortran.machine)) :=
  fun _ _ hx hx1 => kernel_fast_float hx hx1

#print axioms kernel_fast_spec

end SinKernelsFloat
