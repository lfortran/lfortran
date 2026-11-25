import LFortran.FloatAddReal
import Mathlib.Analysis.SpecialFunctions.Trigonometric.Basic

/-!
# Specification of the sin kernels (hand-written, human-reviewed)

Each statement is about a function `Float → Float`: the theorems apply them
to the generated templates at `LFortran.machine`, the functions that
`lfortran --backend=lean` executes (`SinKernels/FloatRun.lean`).
`floatValue` is the real number a `Float` denotes (`none` for infinities and
NaN), read from Lean's `Float.Model`; `Real.sin` is Mathlib's.
-/

open Real LFortran

namespace SinKernels

/-- `kernel_sin`: for every finite `Float` `x` with `|x| ≤ π/2`, the result is
a finite `Float` within `1.15·10^-15 |x| + 2^-1075` of `sin x`. -/
def KernelSinFloatSpec (f : Float → Float) : Prop :=
  ∀ (xf : Float) (x : ℝ), floatValue xf = some x → |x| ≤ π / 2 →
    ∃ v, floatValue (f xf) = some v ∧ |v - sin x| ≤ 115 / 10 ^ 17 * |x| + 1 / 2 ^ 1075

/-- `kernel_fastest`: for every finite `Float` `x0` with `|x0| ≤ X`,
`1 ≤ X ≤ 2^30`, the result is a finite `Float` within
`5.75·10^-3 + 2.25·10^-15 X` of `sin x0`. -/
def KernelFastestFloatSpec (f : Float → Float) : Prop :=
  ∀ (xf : Float) (x0 X : ℝ), floatValue xf = some x0 → 1 ≤ X → X ≤ 2 ^ 30 → |x0| ≤ X →
    ∃ v, floatValue (f xf) = some v ∧ |v - sin x0| ≤ 575 / 10 ^ 5 + X * (225 / 10 ^ 17)

/-- `kernel_fast`: for every finite `Float` `x0` with `|x0| ≤ 6·10^9`, the
result is a finite `Float` within `2.2·10^-15` of `sin x0`. -/
def KernelFastFloatSpec (f : Float → Float) : Prop :=
  ∀ (xf : Float) (x0 : ℝ), floatValue xf = some x0 → |x0| ≤ 6 * 10 ^ 9 →
    ∃ v, floatValue (f xf) = some v ∧ |v - sin x0| ≤ 22 / 10 ^ 16

end SinKernels
