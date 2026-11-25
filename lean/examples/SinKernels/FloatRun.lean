import SinKernels.Proof.KernelFast
import SinKernels.Spec
import LFortranProof.FloatVModel

set_option exponentiation.threshold 2048

/-!
# The `Float` runs of the sin kernels

The theorems here are about the functions `lfortran --backend=lean` runs:
the generated templates at `LFortran.machine`, applied to a `Float`. They
are the theorems proved for every `VModel`, instantiated at `floatV` (Lean's
`Float`, read through `floatValue`); nothing else relates the run to the
proofs.

* `kernel_sin_spec`: for a finite `Float` `xf` with value `|x| ≤ π/2`,
  `kernel_sin xf` is finite and within `1.15·10^-15 |x| + 2^-1075` of
  `Real.sin x` (`kernel_sin_correct`, `u = 2^-53`, `η = 2^-1075`).
* `kernel_fast_spec`: for a finite `Float` `xf` with value `|x0| ≤ 6·10^9`,
  `kernel_fast xf` is finite and within `2.2·10^-15` of `Real.sin x0`
  (`kernel_fast_binary64`: `Float` is correctly rounded binary64 with a guard
  digit, and `Int32` is exact on `[-2^31, 2^31)`).
-/

open LFortran Real SinKernelsProof

namespace SinKernelsFloat

theorem small : Small floatV (1 / 2 ^ 53) (1 / 2 ^ 1075) :=
  ⟨by positivity, by positivity, by norm_num [εU], by norm_num [εU], le_rfl, le_rfl⟩

/-- The `Float` run of `kernel_sin` meets `KernelSinFloatSpec`. -/
theorem kernel_sin_spec :
    SinKernels.KernelSinFloatSpec (sin_kernels.kernel_sin (K := LFortran.machine)) := by
  intro xf x hx hx1
  obtain ⟨w, hw, h⟩ := kernel_sin_correct (K := LFortran.machine) floatV small
    (by show (4 : ℝ) ≤ 2 ^ 1022; exact le_trans (by norm_num) (pow_le_pow_right₀
      (by norm_num : (1 : ℝ) ≤ 2) (by norm_num : 2 ≤ 1022))) hx hx1
  refine ⟨w, hw, h.trans ?_⟩
  have hc : (46 / 10 ^ 17 + 614 / 100 * (1 / 2 ^ 53) + 2107 * (1 / 2 ^ 1075) : ℝ)
      ≤ 115 / 10 ^ 17 := by norm_num
  have := mul_le_mul_of_nonneg_left hc (abs_nonneg x)
  linarith

#print axioms kernel_sin_spec

/-- The `Float` run of `kernel_fast` meets `KernelFastFloatSpec`. -/
theorem kernel_fast_spec :
    SinKernels.KernelFastFloatSpec (sin_kernels.kernel_fast (K := LFortran.machine)) :=
  fun _ _ hx hx1 => kernel_fast_binary64 (K := LFortran.machine) floatV floatV_binary64
    floatV_guard floatV_signed intExact_int32 hx hx1

#print axioms kernel_fast_spec

end SinKernelsFloat
