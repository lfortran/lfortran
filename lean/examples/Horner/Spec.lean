import LFortranProof.FloatAddReal
import LFortran.Arr
import Mathlib.Algebra.BigOperators.Group.Finset.Basic

/-!
# Specification of `horner` (hand-written, human-reviewed)

`horner(n, c, x, p)` (`horner.f90`) evaluates `p = c(1) + c(2) x + … +
c(n) x^(n-1)`. The statement is about the function `lfortran --backend=lean`
executes: the generated template at `LFortran.machine` (`Int32`, `Float`),
which takes the array `c` as an `Arr Float` and returns the `intent(out)`
argument `p`. `floatValue` is the real number a `Float` denotes (`none` for
infinities and NaN), read from Lean's `Float.Model`.
-/

open LFortran

namespace Horner

/-- `Σ_{k<n} c_k x^k`. -/
noncomputable def poly (c : ℕ → ℝ) (x : ℝ) (n : ℕ) : ℝ :=
  ∑ k ∈ Finset.range n, c k * x ^ k

/-- **The run.** For `1 ≤ n < 2^31`, finite `Float` coefficients `c(1..n)`
with values `c_0, …, c_{n-1}` bounded by `C`, and a finite `Float` `x`, the
result is a finite `Float` within
`3(n-1) · 2^-53 · Σ |c_k| |x|^k + 3(n-1) · 2^-1075 · Σ_{k<n-1} |x|^k`
of `Σ c_k x^k`, provided no intermediate value can overflow:
`4 n (C + 1) max(1, |x|)^n ≤ 2^1022`. -/
def HornerFloatSpec (f : Int32 → Arr Float → Float → Float) : Prop :=
  ∀ (n : ℕ) (cf : Arr Float) (xf : Float) (c : ℕ → ℝ) (x C : ℝ),
    1 ≤ n → (n : ℤ) < 2 ^ 31 →
    (∀ k < n, ∃ h : k < cf.data.size, floatValue cf.data[k] = some (c k)) →
    (∀ k < n, |c k| ≤ C) → floatValue xf = some x →
    4 * n * (C + 1) * max 1 |x| ^ n ≤ 2 ^ 1022 →
    ∃ v, floatValue (f (Int32.ofInt n) cf xf) = some v ∧
      |v - poly c x n| ≤ 3 * (n - 1) * (1 / 2 ^ 53) * poly (fun k => |c k|) |x| n +
        3 * (n - 1) * (1 / 2 ^ 1075) * ∑ k ∈ Finset.range (n - 1), |x| ^ k

end Horner
