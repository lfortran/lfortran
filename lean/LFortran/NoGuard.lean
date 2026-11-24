import LFortran.Prelude
import Mathlib.Algebra.Order.Field.Basic
import Mathlib.Tactic.Ring
import Mathlib.Tactic.Linarith
import Mathlib.Tactic.NormNum

/-!
# Arithmetic without a guard digit

N. J. Higham, *Accuracy and Stability of Numerical Algorithms*, 2nd ed.,
SIAM, 2002, §2.4, model (2.6):

  `fl(x ± y) = x (1 + α) ± y (1 + β)`, `|α|, |β| ≤ u`,

and `fl(x op y) = (x op y)(1 + δ)` for `×`, `÷`. It describes machines that
align without a guard digit (Cray 1, 2, X-MP, Y-MP, C90; see
`LFortran/Models/Cray.lean`).

`NoGuardModel` is this model with the underflow term `η` of `FPModel`: the
error of `a ± b` is at most `u (|a| + |b|) + η` instead of `u |a ± b| + η`
(`noGuard_iff` shows this is exactly (2.6a)). It is weaker than `FPModel`:
`FPModel.toNoGuard` turns every `FPModel` into a `NoGuardModel` with the same
operations. A theorem proved for every `NoGuardModel` therefore holds for
IEEE, flush-to-zero, Higham, Trefethen & Bau, and Cray-like arithmetic. For
additions without cancellation (`a b ≥ 0`) the two bounds coincide
(`NoGuardModel.add_err_same_sign`).
-/

namespace LFortran

/-- Higham's no-guard-digit model (2.6) with an underflow term `η`. The
fields are those of `FPModel`, except that the error of `+` and `-` is
relative to `|a| + |b|`, not to the result. -/
structure NoGuardModel where
  F : Set ℝ
  u : ℝ
  η : ℝ
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
    |add a b - (a + b)| ≤ u * (|a| + |b|) + η
  sub_mem : ∀ a ∈ F, ∀ b ∈ F, |a - b| ≤ M → sub a b ∈ F
  sub_err : ∀ a ∈ F, ∀ b ∈ F, |a - b| ≤ M →
    |sub a b - (a - b)| ≤ u * (|a| + |b|) + η
  mul_mem : ∀ a ∈ F, ∀ b ∈ F, |a * b| ≤ M → mul a b ∈ F
  mul_err : ∀ a ∈ F, ∀ b ∈ F, |a * b| ≤ M →
    |mul a b - a * b| ≤ u * |a * b| + η
  div_mem : ∀ a ∈ F, ∀ b ∈ F, b ≠ 0 → |a / b| ≤ M → div a b ∈ F
  div_err : ∀ a ∈ F, ∀ b ∈ F, b ≠ 0 → |a / b| ≤ M →
    |div a b - a / b| ≤ u * |a / b| + η

/-- `±1`, the sign of a real (with `sgnR 0 = 1`). -/
noncomputable def sgnR (x : ℝ) : ℝ := if 0 ≤ x then 1 else -1

/-- The bound `u (|a| + |b|)` is exactly Higham's (2.6a): the error is
`a α + b β` with `|α|, |β| ≤ u`. -/
theorem noGuard_iff {u a b d : ℝ} (hu : 0 ≤ u) :
    |d| ≤ u * (|a| + |b|) ↔ ∃ α β : ℝ, |α| ≤ u ∧ |β| ≤ u ∧ d = a * α + b * β := by
  constructor
  · intro h
    by_cases hs : |a| + |b| = 0
    · have ha : a = 0 := abs_eq_zero.1 (by linarith [abs_nonneg a, abs_nonneg b])
      have hb : b = 0 := abs_eq_zero.1 (by linarith [abs_nonneg a, abs_nonneg b])
      have hd : d = 0 := abs_eq_zero.1 (le_antisymm (by simpa [hs] using h) (abs_nonneg d))
      exact ⟨0, 0, by simpa using hu, by simpa using hu, by simp [ha, hb, hd]⟩
    · have hpos : 0 < |a| + |b| := lt_of_le_of_ne (by positivity) (Ne.symm hs)
      set c := d / (|a| + |b|)
      have hc : |c| ≤ u := by
        rw [abs_div, abs_of_pos hpos, div_le_iff₀ hpos]
        linarith
      have hsg : ∀ x : ℝ, |sgnR x| ≤ 1 := fun x => by
        unfold sgnR; split <;> simp
      have hmul : ∀ x : ℝ, x * sgnR x = |x| := fun x => by
        unfold sgnR; split
        · rename_i h; rw [abs_of_nonneg h, mul_one]
        · rename_i h; rw [abs_of_neg (lt_of_not_ge h)]; ring
      refine ⟨c * sgnR a, c * sgnR b, ?_, ?_, ?_⟩
      · rw [abs_mul]
        nlinarith [hsg a, abs_nonneg c, abs_nonneg (sgnR a)]
      · rw [abs_mul]
        nlinarith [hsg b, abs_nonneg c, abs_nonneg (sgnR b)]
      · calc d = c * (|a| + |b|) := by rw [div_mul_cancel₀ d hpos.ne']
          _ = a * (c * sgnR a) + b * (c * sgnR b) := by
            rw [← hmul a, ← hmul b]; ring
  · rintro ⟨α, β, hα, hβ, rfl⟩
    calc |a * α + b * β| ≤ |a * α| + |b * β| := abs_add_le _ _
      _ = |a| * |α| + |b| * |β| := by rw [abs_mul, abs_mul]
      _ ≤ |a| * u + |b| * u := by
        gcongr
      _ = u * (|a| + |b|) := by ring

/-- `u t ≤ max u 0 · t` for `t ≥ 0`. -/
theorem max_mul_nonneg_le (u t : ℝ) (ht : 0 ≤ t) : u * t ≤ max u 0 * t :=
  mul_le_mul_of_nonneg_right (le_max_left u 0) ht

/-- Every `FPModel` satisfies the no-guard-digit model with the same `η` and
`u' = max u 0`, because `|a ± b| ≤ |a| + |b|`. (`FPModel` does not require
`u ≥ 0`.) -/
def FPModel.toNoGuard (fp : FPModel) : NoGuardModel where
  F := fp.F
  u := max fp.u 0
  η := fp.η
  M := fp.M
  round := fp.round
  add := fp.add
  sub := fp.sub
  mul := fp.mul
  div := fp.div
  round_mem := fp.round_mem
  round_err y hy := by
    have := fp.round_err y hy
    have := max_mul_nonneg_le fp.u |y| (abs_nonneg y)
    linarith
  add_mem := fp.add_mem
  add_err a ha b hb h := by
    have := fp.add_err a ha b hb h
    have := max_mul_nonneg_le fp.u |a + b| (abs_nonneg _)
    have := mul_le_mul_of_nonneg_left (abs_add_le a b) (le_max_right fp.u 0)
    linarith
  sub_mem := fp.sub_mem
  sub_err a ha b hb h := by
    have := fp.sub_err a ha b hb h
    have := max_mul_nonneg_le fp.u |a - b| (abs_nonneg _)
    have := mul_le_mul_of_nonneg_left (abs_sub a b) (le_max_right fp.u 0)
    linarith
  mul_mem := fp.mul_mem
  mul_err a ha b hb h := by
    have := fp.mul_err a ha b hb h
    have := max_mul_nonneg_le fp.u |a * b| (abs_nonneg _)
    linarith
  div_mem := fp.div_mem
  div_err a ha b hb hb0 h := by
    have := fp.div_err a ha b hb hb0 h
    have := max_mul_nonneg_le fp.u |a / b| (abs_nonneg _)
    linarith

/-- Without cancellation the no-guard bound is the `FPModel` bound. -/
theorem NoGuardModel.add_err_same_sign (m : NoGuardModel) {a b : ℝ} (ha : a ∈ m.F)
    (hb : b ∈ m.F) (hab : 0 ≤ a * b) (h : |a + b| ≤ m.M) :
    |m.add a b - (a + b)| ≤ m.u * |a + b| + m.η := by
  have hsum : |a + b| = |a| + |b| := by
    rcases le_total 0 a with h1 | h1 <;> rcases le_total 0 b with h2 | h2
    · rw [abs_of_nonneg h1, abs_of_nonneg h2, abs_of_nonneg (by linarith)]
    · have : a * b ≤ 0 := mul_nonpos_of_nonneg_of_nonpos h1 h2
      have hab0 : a * b = 0 := le_antisymm this hab
      rcases mul_eq_zero.1 hab0 with h | h <;> simp [h]
    · have : a * b ≤ 0 := mul_nonpos_of_nonpos_of_nonneg h1 h2
      have hab0 : a * b = 0 := le_antisymm this hab
      rcases mul_eq_zero.1 hab0 with h | h <;> simp [h]
    · rw [abs_of_nonpos h1, abs_of_nonpos h2, abs_of_nonpos (by linarith)]
      ring
  rw [hsum]
  exact m.add_err a ha b hb h

end LFortran
