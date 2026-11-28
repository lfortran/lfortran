import Horner.Proof
import LFortranProof.FloatVModel

set_option exponentiation.threshold 2048

/-!
# The `Float` run of `horner`

`horner_spec` is `horner_rounding`, proved for every `VModel`, instantiated at
`floatV` (Lean's `Float`: `u = 2^-53`, `η = 2^-1075`, `M = 2^1022`) and
`Int32` (`intExact_int32`): the function `lfortran --backend=lean` runs.
-/

open LFortran Horner HornerProof

/-- The generated `horner`, run at `Int32` and `Float`, meets
`HornerFloatSpec`. -/
theorem horner_spec : HornerFloatSpec (horner_mod.horner (K := LFortran.machine)) := by
  intro n cf xf c x C hn1 hn hc hC hx hM
  have hn' : (n : ℝ) < 2 ^ 31 := by exact_mod_cast hn
  exact horner_rounding (K := LFortran.machine) floatV intExact_int32 floatV_small n hn1 hn
    (by nlinarith) (by nlinarith) cf c hc hC hx hM

#print axioms horner_spec
