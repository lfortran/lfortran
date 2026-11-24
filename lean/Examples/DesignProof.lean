import LFortran.Arith.Model
import Examples.Design

/-!
# Design check: the same templates in a proof

`Examples/Design.lean` runs these templates at `LFortran.machine`. Here they
are instantiated at `proofKinds` (`ℤ`, `ℝ`) with an `FPModel` per real kind,
and reduce to the model's operations, so a proof about them is a proof
about the code that runs.
-/

open LFortran

/-- `mix` in a proof: the `real(4)` argument is rounded into `real(8)`, the
integer is converted by rounding, and each operation is `fp8`'s. -/
example (fp4 fp8 : FPModel) (a b : ℝ) (i : ℤ) :
    @design_mod.mix proofKinds (FKinds.ofModel fp4 fp8) a b i =
      fp8.add (fp8.round a) (fp8.mul b (fp8.round (i : ℝ))) := by
  rfl

/-- Integer `/` and `mod` are exact `ℤ` division and remainder (toward zero). -/
example (fp : FPModel) :
    @design_mod.divmod proofKinds (FKinds.ofModel fp fp) (-7) 2 = (-3, -1) := by
  rfl

/-- `swap` exchanges its arguments in every arithmetic. -/
example (fp : FPModel) (a b : ℝ) :
    @design_mod.swap proofKinds (FKinds.ofModel fp fp) a b = (b, a) := by
  rfl
