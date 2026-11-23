import Mathlib.Data.Real.Basic

/-!
# LFortran Lean backend prelude (prototype)

Runtime model used by the code that `lfortran --show-lean` generates.

`real(8)` values are modelled as real numbers. Every floating-point operation is
the exact real operation followed by a rounding function `fp.rnd`. The rounding
function is a parameter, constrained only by two facts that hold for IEEE-754
binary64 round-to-nearest:

* `rnd_err`: the standard error model with an absolute underflow term,
  valid away from overflow;
* `rnd_int`: integers of magnitude at most `2^53` are representable and hence
  rounded exactly.

Theorems proved against this model therefore hold for any rounding satisfying
these facts, in particular IEEE-754 binary64 round-to-nearest. The model has no
infinities or NaNs, and Lean's `x / 0 = 0` differs from IEEE; a proof must keep
intermediate values bounded and divisors nonzero.
-/

namespace LFortran

/-- Unit roundoff of binary64: `2^-53`. -/
noncomputable def unitRoundoff : ℝ := 1 / 2 ^ 53

/-- Absolute rounding error bound in the subnormal range: `2^-1075`. -/
noncomputable def underflowError : ℝ := 1 / 2 ^ 1075

/-- A model of IEEE-754 binary64 round-to-nearest arithmetic. -/
structure Float64Model where
  rnd : ℝ → ℝ
  rnd_err : ∀ y : ℝ, |y| ≤ 2 ^ 1000 →
    |rnd y - y| ≤ unitRoundoff * |y| + underflowError
  rnd_int : ∀ i : ℤ, |i| ≤ 2 ^ 53 → rnd (i : ℝ) = (i : ℝ)

namespace Float64Model

variable (fp : Float64Model)

noncomputable def add (a b : ℝ) : ℝ := fp.rnd (a + b)
noncomputable def sub (a b : ℝ) : ℝ := fp.rnd (a - b)
noncomputable def mul (a b : ℝ) : ℝ := fp.rnd (a * b)
noncomputable def div (a b : ℝ) : ℝ := fp.rnd (a / b)
/-- Conversion `integer -> real(8)`. -/
noncomputable def ofInt (i : ℤ) : ℝ := fp.rnd (i : ℝ)

end Float64Model

/-- The iteration space of the Fortran loop `do k = lo, hi`. -/
def intRange (lo hi : ℤ) : List ℤ :=
  (List.range (hi + 1 - lo).toNat).map (fun i : ℕ => lo + (i : ℤ))

end LFortran
