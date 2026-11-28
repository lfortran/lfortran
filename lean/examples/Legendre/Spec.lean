import LFortranProof.Prelude
import LFortranProof.NoGuard
import Mathlib.RingTheory.Polynomial.ShiftedLegendre
import LFortranProof.FloatAddReal

/-!
# Specification of `legendre_poly` (hand-written, human-reviewed)

This file is the part a human reviews: the mathematical definition, the
supported inputs, and the required accuracy.
-/

open LFortran

namespace Legendre

/-- The Legendre polynomial `P_n(x)`, taken from Mathlib: the shifted Legendre
polynomial `shiftedLegendre n` (defined by its explicit coefficients
`(-1)^k C(n,k) C(n+k,n)`) evaluated at `(1 - x)/2`. -/
noncomputable def legendreP (n : ℕ) (x : ℝ) : ℝ :=
  Polynomial.aeval ((1 - x) / 2) (Polynomial.shiftedLegendre n)

/-- An implementation `f fp n x` of `P_n(x)` has absolute error at most
`10^-11` for `0 ≤ n ≤ 100` and floating-point `x` with `|x| ≤ 1`, in every
arithmetic `fp` at least as accurate as binary64 (`u ≤ 2^-53`) with underflow
error `η ≤ 2^-1000` and range `M ≥ 2^10`. -/
def LegendreSpec (f : FPModel → ℤ → ℝ → ℝ) : Prop :=
  ∀ (fp : FPModel), fp.u ≤ 1 / 2 ^ 53 → fp.η ≤ 1 / 2 ^ 1000 → 2 ^ 10 ≤ fp.M →
  ∀ (n : ℤ) (x : ℝ), x ∈ fp.F → 0 ≤ n → n ≤ 100 → |x| ≤ 1 →
    |f fp n x - legendreP n.toNat x| ≤ 1 / 10 ^ 11

/-- The same accuracy statement for every arithmetic without a guard digit
(`NoGuardModel`, which includes every `FPModel`), with the unit roundoff
ceiling `U` as a parameter: the absolute error is at most `89201 U` for
`0 ≤ n ≤ 100` and floating-point `|x| ≤ 1`, whenever `fp.u ≤ U ≤ 2^-40`,
`fp.η ≤ U / 10^6` and `fp.M ≥ 2^10`. For `U = 2^-53` this is `LegendreSpec`'s
`10^-11`; for a Cray-like `U = 2^-46` it is about `1.3 · 10^-9`. -/
def LegendreSpecNoGuard (f : NoGuardModel → ℤ → ℝ → ℝ) : Prop :=
  ∀ (fp : NoGuardModel) (U : ℝ), 0 ≤ U → U ≤ 1 / 2 ^ 40 → fp.u ≤ U → fp.η ≤ U / 10 ^ 6 →
  2 ^ 10 ≤ fp.M → ∀ (n : ℤ) (x : ℝ), x ∈ fp.F → 0 ≤ n → n ≤ 100 → |x| ≤ 1 →
    |f fp n x - legendreP n.toNat x| ≤ 89201 * U

/-- **The run.** The function `lfortran --backend=lean` executes, at
`Int32` and Lean's `Float`, returns for `0 ≤ n ≤ 100` and every finite
`Float` `x` with `|x| ≤ 1` a finite `Float` within `10^-11` of `P_n(x)`.
`floatValue` is the real number a `Float` denotes (`none` for infinities
and NaN), read from Lean's `Float.Model`. -/
def LegendreFloatSpec (f : Int32 → Float → Float) : Prop :=
  ∀ (n : ℕ) (xf : Float) (x : ℝ), n ≤ 100 → floatValue xf = some x → |x| ≤ 1 →
    ∃ v, floatValue (f (Int32.ofInt n) xf) = some v ∧ |v - legendreP n x| ≤ 1 / 10 ^ 11

end Legendre
