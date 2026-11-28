import LFortranProof.Models.TrefethenBau

/-!
# Higham's standard model of floating-point arithmetic with underflow

N. J. Higham, *Accuracy and Stability of Numerical Algorithms*, 2nd ed.,
SIAM, 2002, Chapter 2.

Higham's standard model states that for `x, y ∈ F` and `op ∈ {+, -, ×, ÷}`,
`fl(x op y) = (x op y)(1 + δ)` with `|δ| ≤ u`, provided no overflow or
underflow occurs. To account for underflow, Higham (following
J. W. Demmel, *Underflow and the reliability of numerical software*, SIAM J.
Sci. Stat. Comput. 5 (1984)) extends it to

  `fl(x op y) = (x op y)(1 + δ) + η`, `|δ| ≤ u`, `|η| ≤ λ u`, `δ η = 0`,

with `η = 0` for `+` and `-` (with gradual underflow, sums and differences are
exact when they underflow). Here `λ` is the smallest positive normalized
floating-point number. The model holds whenever the exact result does not
overflow, which we express as `|x op y| ≤ M`. The rounding `fl : ℝ → F`
(used for conversions and literals) satisfies the same model.

We formalize the model as the structure `Higham`, show that it is an instance
of `LFortran.FPModel` with unit roundoff `u` and underflow term `η = λ u`
(`Higham.toFPModel`), and that the Trefethen & Bau axioms are the special case
`λ = 0` (`TrefethenBau.toHigham`).
-/

namespace LFortran

/-- Higham's standard model with underflow (Higham, Chapter 2, after Demmel). -/
structure Higham where
  /-- The floating-point numbers. -/
  F : Set ℝ
  /-- Unit roundoff. -/
  u : ℝ
  /-- The smallest positive normalized floating-point number `λ`. -/
  lam : ℝ
  /-- Overflow threshold: the model holds for exact results in `[-M, M]`. -/
  M : ℝ
  u_nonneg : 0 ≤ u
  lam_nonneg : 0 ≤ lam
  /-- Rounding. -/
  fl : ℝ → ℝ
  add : ℝ → ℝ → ℝ
  sub : ℝ → ℝ → ℝ
  mul : ℝ → ℝ → ℝ
  div : ℝ → ℝ → ℝ
  fl_mem : ∀ x, |x| ≤ M → fl x ∈ F
  fl_ax : ∀ x, |x| ≤ M → ∃ δ η, |δ| ≤ u ∧ |η| ≤ lam * u ∧ δ * η = 0 ∧
    fl x = x * (1 + δ) + η
  add_mem : ∀ a ∈ F, ∀ b ∈ F, |a + b| ≤ M → add a b ∈ F
  sub_mem : ∀ a ∈ F, ∀ b ∈ F, |a - b| ≤ M → sub a b ∈ F
  mul_mem : ∀ a ∈ F, ∀ b ∈ F, |a * b| ≤ M → mul a b ∈ F
  div_mem : ∀ a ∈ F, ∀ b ∈ F, b ≠ 0 → |a / b| ≤ M → div a b ∈ F
  /-- Addition: `η = 0`. -/
  add_ax : ∀ a ∈ F, ∀ b ∈ F, |a + b| ≤ M →
    ∃ δ, |δ| ≤ u ∧ add a b = (a + b) * (1 + δ)
  /-- Subtraction: `η = 0`. -/
  sub_ax : ∀ a ∈ F, ∀ b ∈ F, |a - b| ≤ M →
    ∃ δ, |δ| ≤ u ∧ sub a b = (a - b) * (1 + δ)
  mul_ax : ∀ a ∈ F, ∀ b ∈ F, |a * b| ≤ M →
    ∃ δ η, |δ| ≤ u ∧ |η| ≤ lam * u ∧ δ * η = 0 ∧
      mul a b = (a * b) * (1 + δ) + η
  div_ax : ∀ a ∈ F, ∀ b ∈ F, b ≠ 0 → |a / b| ≤ M →
    ∃ δ η, |δ| ≤ u ∧ |η| ≤ lam * u ∧ δ * η = 0 ∧
      div a b = (a / b) * (1 + δ) + η

/-- The error of `y (1 + δ) + η` is at most `u |y| + e` when `|δ| ≤ u` and
`|η| ≤ e`. -/
theorem abs_mul_one_add_add_sub_le {y z δ η u e : ℝ} (hδ : |δ| ≤ u)
    (hη : |η| ≤ e) (hz : z = y * (1 + δ) + η) : |z - y| ≤ u * |y| + e := by
  rw [hz, show y * (1 + δ) + η - y = δ * y + η by ring]
  calc |δ * y + η| ≤ |δ * y| + |η| := abs_add_le _ _
    _ ≤ u * |y| + e := by
      rw [abs_mul]
      exact add_le_add (mul_le_mul_of_nonneg_right hδ (abs_nonneg y)) hη

namespace Higham

/-- Higham's model is an `FPModel` with unit roundoff `u` and underflow term
`η = λ u`. -/
def toFPModel (H : Higham) : FPModel where
  F := H.F
  u := H.u
  η := H.lam * H.u
  M := H.M
  round := H.fl
  add := H.add
  sub := H.sub
  mul := H.mul
  div := H.div
  round_mem := H.fl_mem
  round_err y hy := by
    obtain ⟨δ, η, hδ, hη, -, h⟩ := H.fl_ax y hy
    exact abs_mul_one_add_add_sub_le hδ hη h
  add_mem := H.add_mem
  add_err a ha b hb h := by
    obtain ⟨δ, hδ, h⟩ := H.add_ax a ha b hb h
    exact abs_mul_one_add_add_sub_le (η := 0) hδ
      (by rw [abs_zero]; exact mul_nonneg H.lam_nonneg H.u_nonneg)
      (by rw [h, add_zero])
  sub_mem := H.sub_mem
  sub_err a ha b hb h := by
    obtain ⟨δ, hδ, h⟩ := H.sub_ax a ha b hb h
    exact abs_mul_one_add_add_sub_le (η := 0) hδ
      (by rw [abs_zero]; exact mul_nonneg H.lam_nonneg H.u_nonneg)
      (by rw [h, add_zero])
  mul_mem := H.mul_mem
  mul_err a ha b hb h := by
    obtain ⟨δ, η, hδ, hη, -, h⟩ := H.mul_ax a ha b hb h
    exact abs_mul_one_add_add_sub_le hδ hη h
  div_mem := H.div_mem
  div_err a ha b hb hb0 h := by
    obtain ⟨δ, η, hδ, hη, -, h⟩ := H.div_ax a ha b hb hb0 h
    exact abs_mul_one_add_add_sub_le hδ hη h

end Higham

namespace TrefethenBau

theorem εmach_nonneg (A : TrefethenBau) : 0 ≤ A.εmach := by
  obtain ⟨ε, hε, -⟩ := A.fl_err 0
  exact (abs_nonneg ε).trans hε

/-- The Trefethen & Bau axioms are Higham's model with `λ = 0` (no underflow)
and an arbitrary overflow threshold `M`. -/
def toHigham (A : TrefethenBau) (M : ℝ) : Higham where
  F := A.F
  u := A.εmach
  lam := 0
  M := M
  u_nonneg := A.εmach_nonneg
  lam_nonneg := le_refl 0
  fl := A.fl
  add := A.add
  sub := A.sub
  mul := A.mul
  div := A.div
  fl_mem x _ := A.fl_mem x
  fl_ax x _ := by
    obtain ⟨ε, hε, h⟩ := A.fl_err x
    exact ⟨ε, 0, hε, by simp, mul_zero ε, by rw [h, add_zero]⟩
  add_mem a ha b hb _ := A.add_mem a ha b hb
  sub_mem a ha b hb _ := A.sub_mem a ha b hb
  mul_mem a ha b hb _ := A.mul_mem a ha b hb
  div_mem a ha b hb hb0 _ := A.div_mem a ha b hb hb0
  add_ax a ha b hb _ := A.add_ax a ha b hb
  sub_ax a ha b hb _ := A.sub_ax a ha b hb
  mul_ax a ha b hb _ := by
    obtain ⟨ε, hε, h⟩ := A.mul_ax a ha b hb
    exact ⟨ε, 0, hε, by simp, mul_zero ε, by rw [h, add_zero]⟩
  div_ax a ha b hb hb0 _ := by
    obtain ⟨ε, hε, h⟩ := A.div_ax a ha b hb hb0
    exact ⟨ε, 0, hε, by simp, mul_zero ε, by rw [h, add_zero]⟩

/-- Going through Higham's model gives the same `FPModel` as
`TrefethenBau.toFPModel`. -/
theorem toHigham_toFPModel (A : TrefethenBau) (M : ℝ) :
    (A.toHigham M).toFPModel = A.toFPModel M := by
  simp only [Higham.toFPModel, toHigham, toFPModel, zero_mul]

end TrefethenBau

end LFortran
