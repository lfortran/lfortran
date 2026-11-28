import Legendre.Proof
import LFortranProof.FloatVModel

set_option exponentiation.threshold 2048

/-!
# The `Float` run of the same `legendre_poly`

`Legendre/Run.lean` executes the generated template
`legendre_mod.legendre_poly` at `LFortran.machine` (`Int32` and Lean's `Float`),
exactly as the `main` emitted by `lfortran --backend=lean` does.
`legendre_float_spec` is `legendre_rounding`, proved for every `VModel`,
instantiated at `floatV` (Lean's `Float`, read through `floatValue`: `u = 2^-53`,
`η = 2^-1075`, `M = 2^1022`) and `Int32` (`intExact_int32`): for `n ≤ 100`
and a finite `Float` `x` with `|x| ≤ 1` the run returns a finite `Float`
within `89201 · 2^-53 < 10^-11` of Mathlib's `legendreP n x`.

The trusted step is Lean's: the compiled `Float` operations implement the
logical model `Float.Model`.
-/

open LFortran Legendre LegendreProof

/-- `Float` is admissible with `U = 2^-53`. -/
theorem floatV_admissible : Admissible floatV (1 / 2 ^ 53) := by
  refine ⟨by positivity, by norm_num, le_rfl, ?_, ?_⟩
  · show (1 : ℝ) / 2 ^ 1075 ≤ 1 / 2 ^ 53 / 10 ^ 6
    rw [div_div, one_div_le_one_div (by positivity) (by positivity)]
    calc (2 : ℝ) ^ 53 * 10 ^ 6 ≤ 2 ^ 53 * 2 ^ 20 := by gcongr; norm_num
      _ ≤ 2 ^ 1075 := by rw [← pow_add]; exact pow_le_pow_right₀ (by norm_num) (by norm_num)
  · show (2 : ℝ) ^ 10 ≤ 2 ^ 1022
    exact pow_le_pow_right₀ (by norm_num) (by norm_num)

/-- The generated `legendre_poly`, run at `Int32` and `Float`, meets
`LegendreFloatSpec`. -/
theorem legendre_float_spec :
    LegendreFloatSpec (legendre_mod.legendre_poly (K := machine)) := by
  intro n xf x hn hxv hx1
  obtain ⟨w, hw, h⟩ := legendre_rounding (K := machine) floatV intExact_int32
    floatV_admissible n hn hxv hx1
  exact ⟨w, hw, h.trans (by norm_num)⟩

#print axioms legendre_float_spec
