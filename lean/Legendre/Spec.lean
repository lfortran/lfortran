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

/-- An implementation `f fp n x` of `P_n(x)`, evaluated with any binary64
rounding `fp`, has absolute error at most `10^-11` for `0 ≤ n ≤ 100` and
`|x| ≤ 1`. -/
def LegendreSpec (f : Float64Model → ℤ → ℝ → ℝ) : Prop :=
  ∀ (fp : Float64Model) (n : ℤ) (x : ℝ),
    0 ≤ n → n ≤ 100 → |x| ≤ 1 →
    |f fp n x - P n.toNat x| ≤ 1 / 10 ^ 11

end Legendre
