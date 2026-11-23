import LFortran.Prelude

/-!
# Specification of `legendre_poly` (hand-written, human-reviewed)

This file is the part a human reviews: the mathematical definition, the
supported inputs, and the required accuracy.
-/

open LFortran

namespace Legendre

/-- The exact Legendre polynomial `P_n(x)` over `ℝ`, defined by Bonnet's
recurrence `P_0 = 1`, `P_1 = x`,
`(n + 1) P_{n+1}(x) = (2n + 1) x P_n(x) - n P_{n-1}(x)`. -/
noncomputable def P : ℕ → ℝ → ℝ
  | 0, _ => 1
  | 1, x => x
  | n + 2, x => ((2 * n + 3) * x * P (n + 1) x - (n + 1) * P n x) / (n + 2)

/-- An implementation `f fp n x` of `P_n(x)` has absolute error at most
`10^-11` for `0 ≤ n ≤ 100` and floating-point `x` with `|x| ≤ 1`, in every
arithmetic `fp` at least as accurate as binary64 (`u ≤ 2^-53`) with underflow
error `η ≤ 2^-1000` and range `M ≥ 2^10`. -/
def LegendreSpec (f : FPModel → ℤ → ℝ → ℝ) : Prop :=
  ∀ (fp : FPModel), fp.u ≤ 1 / 2 ^ 53 → fp.η ≤ 1 / 2 ^ 1000 → 2 ^ 10 ≤ fp.M →
  ∀ (n : ℤ) (x : ℝ), x ∈ fp.F → 0 ≤ n → n ≤ 100 → |x| ≤ 1 →
    |f fp n x - P n.toNat x| ≤ 1 / 10 ^ 11

end Legendre
