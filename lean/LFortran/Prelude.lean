import Mathlib.Data.Real.Basic

/-!
# LFortran proof runtime

The Lean backend is one translator. `--show-lean` emits each procedure as a
template over `[FInt ι] [FReal α]` (`LFortran/Arith.lean`). A proof
instantiates it at `ℤ` and an abstract `FPModel` defined here
(`FReal.ofModel`, `LFortran/Arith/Model.lean`), so it can bound rounding
error; `lean --run` instantiates it at `Int32` and `Float`. `--backend=lean`
aims the same translator at `LFortran.Exec`. This file is not a second
language.

Floating-point values are modelled as real numbers. The arithmetic is a
parameter `fp : FPModel`: a set `F` of floating-point numbers, operations
`add sub mul div`, and a rounding `round` (used for type conversions and
literals). The only assumptions are error bounds of the form

  `|computed - exact| ≤ u * |exact| + η`  when `|exact| ≤ M`,

for operands in `F`, and that results stay in `F`. Here `u` is the unit
roundoff, `η` an absolute error allowed by underflow, and `M` the range in
which the bounds hold. Nothing is assumed beyond `M` (overflow), for division
by zero, or for operands outside `F`, so proofs must keep values in range.

The model covers many arithmetics; `LFortran/Models/` proves that IEEE-754
binary64 round-to-nearest, binary64 with flush-to-zero, and the systems of
Trefethen & Bau and of Higham are all instances. See `lean/README.md`.
-/

namespace LFortran

/-- An abstract floating-point arithmetic. -/
structure FPModel where
  /-- The floating-point numbers. -/
  F : Set ℝ
  /-- Unit roundoff. -/
  u : ℝ
  /-- Absolute error allowed by underflow. -/
  η : ℝ
  /-- The bounds hold for exact results of magnitude at most `M`. -/
  M : ℝ
  round : ℝ → ℝ
  add : ℝ → ℝ → ℝ
  sub : ℝ → ℝ → ℝ
  mul : ℝ → ℝ → ℝ
  div : ℝ → ℝ → ℝ
  round_mem : ∀ y, |y| ≤ M → round y ∈ F
  round_err : ∀ y, |y| ≤ M → |round y - y| ≤ u * |y| + η
  add_mem : ∀ a ∈ F, ∀ b ∈ F, |a + b| ≤ M → add a b ∈ F
  add_err : ∀ a ∈ F, ∀ b ∈ F, |a + b| ≤ M →
    |add a b - (a + b)| ≤ u * |a + b| + η
  sub_mem : ∀ a ∈ F, ∀ b ∈ F, |a - b| ≤ M → sub a b ∈ F
  sub_err : ∀ a ∈ F, ∀ b ∈ F, |a - b| ≤ M →
    |sub a b - (a - b)| ≤ u * |a - b| + η
  mul_mem : ∀ a ∈ F, ∀ b ∈ F, |a * b| ≤ M → mul a b ∈ F
  mul_err : ∀ a ∈ F, ∀ b ∈ F, |a * b| ≤ M →
    |mul a b - a * b| ≤ u * |a * b| + η
  div_mem : ∀ a ∈ F, ∀ b ∈ F, b ≠ 0 → |a / b| ≤ M → div a b ∈ F
  div_err : ∀ a ∈ F, ∀ b ∈ F, b ≠ 0 → |a / b| ≤ M →
    |div a b - a / b| ≤ u * |a / b| + η

/-- Conversion `integer -> real`. -/
noncomputable def FPModel.ofInt (fp : FPModel) (i : ℤ) : ℝ := fp.round (i : ℝ)

/-- The iteration space of the Fortran loop `do k = lo, hi`. -/
def intRange (lo hi : ℤ) : List ℤ :=
  (List.range (hi + 1 - lo).toNat).map (fun i : ℕ => lo + (i : ℤ))

end LFortran
