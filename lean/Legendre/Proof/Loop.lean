import Mathlib.Tactic.Linarith
import Mathlib.Tactic.Positivity
import Mathlib.Tactic.FieldSimp
import Mathlib.Tactic.GCongr
import Mathlib.Tactic.Ring
import Mathlib.Tactic.NormNum
import Mathlib.Tactic.LinearCombination
import Legendre.Spec
import Legendre.Generated

/-!
# The loop of `legendre_poly` as a recursion

`y fp x m` is the value of `b` after the iterations `k = 2, ..., m` of the
Fortran loop (with `y fp x 0 = fp.round 1`, the initial value of `a`), so that
`legendre_poly fp 0 x = fp.round 1` and
`legendre_poly fp (m + 1) x = y fp x (m + 1)`.
-/

open LFortran Legendre

namespace LegendreProof

/-- One evaluation of the loop body: the new `c` computed from `a`, `b`. -/
noncomputable def step (fp : FPModel) (x : ℝ) (k : ℤ) (a b : ℝ) : ℝ :=
  fp.div (fp.sub (fp.mul (fp.mul (fp.ofInt (2 * k - 1)) x) b) (fp.mul (fp.ofInt (k - 1)) a))
    (fp.ofInt k)

/-- The sequence of values computed by the loop (`y m` approximates `P m x`). -/
noncomputable def y (fp : FPModel) (x : ℝ) : ℕ → ℝ
  | 0 => fp.round 1
  | 1 => x
  | m + 2 => step fp x ((m : ℤ) + 2) (y fp x m) (y fp x (m + 1))

lemma intRange_succ (lo hi : ℤ) (h : lo ≤ hi + 1) :
    intRange lo (hi + 1) = intRange lo hi ++ [hi + 1] := by
  unfold intRange
  rw [show (hi + 1 + 1 - lo).toNat = (hi + 1 - lo).toNat + 1 by omega]
  simp [List.range_succ]
  omega

lemma fold_eq (fp : FPModel) (x : ℝ) (m : ℕ) :
    ∃ c, List.foldl (fun (s : ℝ × ℝ × ℝ) (k : ℤ) =>
        (s.2.1, step fp x k s.1 s.2.1, step fp x k s.1 s.2.1)) (fp.round 1, x, 0)
        (intRange 2 ((m : ℤ) + 1)) = (y fp x m, y fp x (m + 1), c) := by
  induction m with
  | zero => exact ⟨0, rfl⟩
  | succ m ih =>
    obtain ⟨c, hc⟩ := ih
    refine ⟨y fp x (m + 2), ?_⟩
    rw [show ((m + 1 : ℕ) : ℤ) + 1 = ((m : ℤ) + 1) + 1 by push_cast; ring,
      intRange_succ _ _ (by omega), List.foldl_append, hc]
    simp only [List.foldl_cons, List.foldl_nil]
    have : y fp x (m + 2) = step fp x ((m : ℤ) + 2) (y fp x m) (y fp x (m + 1)) := rfl
    rw [this, add_assoc, one_add_one_eq_two]

lemma legendre_zero (fp : FPModel) (x : ℝ) : legendre_mod.legendre_poly fp 0 x = fp.round 1 := by
  simp [legendre_mod.legendre_poly]

lemma legendre_succ (fp : FPModel) (x : ℝ) (m : ℕ) :
    legendre_mod.legendre_poly fp ((m : ℤ) + 1) x = y fp x (m + 1) := by
  have hne : ((m : ℤ) + 1) ≠ 0 := by omega
  obtain ⟨c, hc⟩ := fold_eq fp x m
  simp only [step] at hc
  simp [legendre_mod.legendre_poly, hne, hc]

end LegendreProof
