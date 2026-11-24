import LFortran.Prelude
import Mathlib.Algebra.Order.Field.Basic
import Mathlib.Tactic.Ring

/-!
# The floating-point axioms of Trefethen & Bau

L. N. Trefethen and D. Bau III, *Numerical Linear Algebra*, SIAM, 1997,
Lecture 13 ("Floating Point Arithmetic").

Trefethen & Bau work with an idealized floating-point system `F` with an
unbounded exponent range (no overflow and no underflow) and machine epsilon
`ε_machine`. They postulate two axioms:

* (13.5) for every `x ∈ ℝ` there is `ε` with `|ε| ≤ ε_machine` and
  `fl(x) = x (1 + ε)`, where `fl : ℝ → F` is the rounding;
* (13.7), the *fundamental axiom of floating-point arithmetic*: for all
  `x, y ∈ F` and each operation `* ∈ {+, -, ×, ÷}` (with `y ≠ 0` for `÷`)
  there is `ε` with `|ε| ≤ ε_machine` and `x ⊛ y = (x * y)(1 + ε)`.

We formalize these axioms as the structure `TrefethenBau` and show that every
such system is an instance of `LFortran.FPModel` with unit roundoff
`u = ε_machine`, no underflow term (`η = 0`) and an arbitrary range `M`
(`TrefethenBau.toFPModel`).
-/

namespace LFortran

/-- The axioms (13.5) and (13.7) of Trefethen & Bau, Lecture 13. -/
structure TrefethenBau where
  /-- The floating-point numbers. -/
  F : Set ℝ
  /-- Machine epsilon `ε_machine`. -/
  εmach : ℝ
  /-- Rounding to the nearest floating-point number. -/
  fl : ℝ → ℝ
  fl_mem : ∀ x, fl x ∈ F
  /-- Axiom (13.5). -/
  fl_err : ∀ x, ∃ ε, |ε| ≤ εmach ∧ fl x = x * (1 + ε)
  /-- `⊕ : F × F → F`. -/
  add : ℝ → ℝ → ℝ
  /-- `⊖ : F × F → F`. -/
  sub : ℝ → ℝ → ℝ
  /-- `⊗ : F × F → F`. -/
  mul : ℝ → ℝ → ℝ
  /-- `⊘ : F × F → F`. -/
  div : ℝ → ℝ → ℝ
  add_mem : ∀ a ∈ F, ∀ b ∈ F, add a b ∈ F
  sub_mem : ∀ a ∈ F, ∀ b ∈ F, sub a b ∈ F
  mul_mem : ∀ a ∈ F, ∀ b ∈ F, mul a b ∈ F
  div_mem : ∀ a ∈ F, ∀ b ∈ F, b ≠ 0 → div a b ∈ F
  /-- The fundamental axiom (13.7) for `⊕`. -/
  add_ax : ∀ a ∈ F, ∀ b ∈ F, ∃ ε, |ε| ≤ εmach ∧ add a b = (a + b) * (1 + ε)
  /-- The fundamental axiom (13.7) for `⊖`. -/
  sub_ax : ∀ a ∈ F, ∀ b ∈ F, ∃ ε, |ε| ≤ εmach ∧ sub a b = (a - b) * (1 + ε)
  /-- The fundamental axiom (13.7) for `⊗`. -/
  mul_ax : ∀ a ∈ F, ∀ b ∈ F, ∃ ε, |ε| ≤ εmach ∧ mul a b = (a * b) * (1 + ε)
  /-- The fundamental axiom (13.7) for `⊘`. -/
  div_ax : ∀ a ∈ F, ∀ b ∈ F, b ≠ 0 →
    ∃ ε, |ε| ≤ εmach ∧ div a b = (a / b) * (1 + ε)

/-- A relative perturbation `y (1 + ε)` with `|ε| ≤ u` has error at most
`u |y|`. -/
theorem abs_mul_one_add_sub_le {y z ε u : ℝ} (hε : |ε| ≤ u)
    (hz : z = y * (1 + ε)) : |z - y| ≤ u * |y| + 0 := by
  rw [hz, add_zero, show y * (1 + ε) - y = ε * y by ring, abs_mul]
  exact mul_le_mul_of_nonneg_right hε (abs_nonneg y)

namespace TrefethenBau

/-- Every Trefethen & Bau system is an `FPModel` with `u = ε_machine`,
`η = 0` and any range `M`. -/
def toFPModel (A : TrefethenBau) (M : ℝ) : FPModel where
  F := A.F
  u := A.εmach
  η := 0
  M := M
  round := A.fl
  add := A.add
  sub := A.sub
  mul := A.mul
  div := A.div
  round_mem y _ := A.fl_mem y
  round_err y _ := by
    obtain ⟨ε, hε, h⟩ := A.fl_err y
    exact abs_mul_one_add_sub_le hε h
  add_mem a ha b hb _ := A.add_mem a ha b hb
  add_err a ha b hb _ := by
    obtain ⟨ε, hε, h⟩ := A.add_ax a ha b hb
    exact abs_mul_one_add_sub_le hε h
  sub_mem a ha b hb _ := A.sub_mem a ha b hb
  sub_err a ha b hb _ := by
    obtain ⟨ε, hε, h⟩ := A.sub_ax a ha b hb
    exact abs_mul_one_add_sub_le hε h
  mul_mem a ha b hb _ := A.mul_mem a ha b hb
  mul_err a ha b hb _ := by
    obtain ⟨ε, hε, h⟩ := A.mul_ax a ha b hb
    exact abs_mul_one_add_sub_le hε h
  div_mem a ha b hb hb0 _ := A.div_mem a ha b hb hb0
  div_err a ha b hb hb0 _ := by
    obtain ⟨ε, hε, h⟩ := A.div_ax a ha b hb hb0
    exact abs_mul_one_add_sub_le hε h

end TrefethenBau

end LFortran
