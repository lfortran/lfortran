import SinKernels.Proof.KernelSin
import SinKernels.Proof.KernelFastest
import LFortran.FloatModel

set_option exponentiation.threshold 2048

/-!
# The sin kernels in IEEE binary64

`floatModel` (`LFortran/FloatModel.lean`) is binary64 round-to-nearest with
`u = 2^-53`, `η = 2^-1075`, `M = 2^1022`, whose operations are Lean's `Float`
operations. Instantiating the general theorems:

* `kernel_sin_binary64`: `|kernel_sin x - sin x| ≤ 1.15·10^-15 |x| + 2^-1075`
  for binary64 `|x| ≤ π/2`; for normal `x` (where `2^-1075` is negligible
  against `|x|`) this is a relative error below `1.9·10^-15`;
* `kernel_fastest_binary64`: `|kernel_fastest x0 - sin x0| ≤ 5.75·10^-3 +
  2.25·10^-15 X` for binary64 `|x0| ≤ X`, `1 ≤ X ≤ 2^30`.
-/

open LFortran Real SinKernelsProof

namespace SinKernelsBinary64

lemma small : Small floatModel.toNoGuard (1 / 2 ^ 53) (1 / 2 ^ 1075) :=
  ⟨by positivity, by positivity, by norm_num [εU], by norm_num [εU],
    max_le le_rfl (by positivity), le_rfl⟩

theorem kernel_sin_binary64 {x : ℝ} (hxF : x ∈ binary64) (hx : |x| ≤ π / 2) :
    |kernel_sin_ng floatModel.toNoGuard x - sin x| ≤ 115 / 10 ^ 17 * |x| + 1 / 2 ^ 1075 := by
  have h := kernel_sin_correct floatModel.toNoGuard small
    (by show (4 : ℝ) ≤ 2 ^ 1022; exact le_trans (by norm_num) (pow_le_pow_right₀
      (by norm_num : (1 : ℝ) ≤ 2) (by norm_num : 2 ≤ 1022))) hxF hx
  have hc : (46 / 10 ^ 17 + 614 / 100 * (1 / 2 ^ 53) + 2107 * (1 / 2 ^ 1075) : ℝ)
      ≤ 115 / 10 ^ 17 := by norm_num
  have := mul_le_mul_of_nonneg_left hc (abs_nonneg x)
  linarith

#print axioms kernel_sin_binary64

/-- binary64 satisfies the assumptions of `kernel_fastest` for `1 ≤ X ≤ 2^30`. -/
theorem fastestArith_binary64 {X : ℝ} (hX1 : 1 ≤ X) (hX : X ≤ 2 ^ 30) :
    FastestArith floatModel.toNoGuard (1 / 2 ^ 53) (1 / 2 ^ 1075) X :=
  { small := small
    X1 := hX1
    XU := by
      have := mul_le_mul_of_nonneg_right hX (by positivity : (0 : ℝ) ≤ 1 / 2 ^ 53)
      exact this.trans (by norm_num [εU])
    XH := by
      have := mul_le_mul_of_nonneg_right hX (by positivity : (0 : ℝ) ≤ 1 / 2 ^ 1075)
      exact this.trans (by norm_num [εU])
    M := by
      show 5 * X ≤ 2 ^ 1022
      have : (5 : ℝ) * 2 ^ 30 ≤ 2 ^ 1022 := by norm_num
      linarith
    int_exact := fun n hn => by
      have h30 : (2 : ℝ) ^ 30 < 2 ^ 53 := by norm_num
      have hn' : |(n : ℝ)| < 2 ^ 53 := by linarith
      have hmem : (n : ℝ) ∈ binary64 := int_mem_binary64 (by exact_mod_cast hn')
      have h53 : (2 : ℝ) ^ 53 ≤ 2 ^ 1022 := pow_le_pow_right₀ (by norm_num) (by norm_num)
      exact round_fixes floatRound_spec (by linarith) hmem
    neg_mem := fun _ hv => neg_mem_binary64 hv }

/-- The binary64 bound for any sign choice that agrees with the sign of `x0`. -/
theorem kfGen_binary64 {X x0 : ℝ} (hX1 : 1 ≤ X) (hX : X ≤ 2 ^ 30)
    (hxF : x0 ∈ binary64) (hx : |x0| ≤ X) {b : Bool} (hb : SignOk b x0) :
    |kfGen floatModel.toNoGuard b x0 - sin x0| ≤ 575 / 10 ^ 5 + X * (225 / 10 ^ 17) := by
  have h := kfGen_correct _ (fastestArith_binary64 hX1 hX) hxF hx hb
  have hc : (19 * (1 / 2 ^ 53) + 14 * (1 / 2 ^ 1075) + 13 / 10 ^ 17 : ℝ) ≤ 225 / 10 ^ 17 := by
    norm_num
  have := mul_le_mul_of_nonneg_left hc (by linarith : (0 : ℝ) ≤ X)
  linarith

theorem kernel_fastest_binary64 {X x0 : ℝ} (hX1 : 1 ≤ X) (hX : X ≤ 2 ^ 30)
    (hxF : x0 ∈ binary64) (hx : |x0| ≤ X) :
    |kernel_fastest_ng floatModel.toNoGuard x0 - sin x0| ≤ 575 / 10 ^ 5 + X * (225 / 10 ^ 17) := by
  rw [kernel_fastest_ng_eq]
  exact kfGen_binary64 hX1 hX hxF hx (signOk_decide x0)

#print axioms kernel_fastest_binary64

end SinKernelsBinary64
