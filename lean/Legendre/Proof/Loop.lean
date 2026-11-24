import Mathlib.Tactic.Linarith
import Mathlib.Tactic.Positivity
import Mathlib.Tactic.FieldSimp
import Mathlib.Tactic.GCongr
import Mathlib.Tactic.Ring
import Mathlib.Tactic.NormNum
import Mathlib.Tactic.LinearCombination
import Legendre.Spec
import Legendre.Shared
import LFortran.Arith.Model

/-!
# The loop of `legendre_poly` as a recursion

`legendre_ng m n x` is the shared `legendre_poly` (`Legendre/Shared.lean`)
in a `NoGuardModel` `m`. `y m x k` is the value of `b` after the iterations
`k = 2, ..., k` of the Fortran loop (with `y m x 0 = m.round 1`, the initial
value of `a`), so that `legendre_ng m 0 x = m.round 1` and
`legendre_ng m (k + 1) x = y m x (k + 1)`.
-/

open LFortran Legendre

namespace LegendreProof

/-- One evaluation of the loop body: the new `c` computed from `a`, `b`. -/
noncomputable def step (fp : NoGuardModel) (x : ℝ) (k : ℤ) (a b : ℝ) : ℝ :=
  fp.div (fp.sub (fp.mul (fp.mul (fp.round (2 * (k : ℝ) - 1)) x) b)
    (fp.mul (fp.round ((k : ℝ) - 1)) a)) (fp.round (k : ℝ))

/-- The sequence of values computed by the loop (`y m` approximates `P m x`). -/
noncomputable def y (fp : NoGuardModel) (x : ℝ) : ℕ → ℝ
  | 0 => fp.round 1
  | 1 => x
  | m + 2 => step fp x ((m : ℤ) + 2) (y fp x m) (y fp x (m + 1))

lemma intRange_succ (lo hi : ℤ) (h : lo ≤ hi + 1) :
    intRange lo (hi + 1) = intRange lo hi ++ [hi + 1] := by
  unfold intRange
  rw [show (hi + 1 + 1 - lo).toNat = (hi + 1 - lo).toNat + 1 by omega]
  simp [List.range_succ]
  omega

lemma fold_eq (fp : NoGuardModel) (x : ℝ) (m : ℕ) :
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

/-- The shared `legendre_poly` in a `NoGuardModel`. -/
noncomputable def legendre_ng (fp : NoGuardModel) (n : ℤ) (x : ℝ) : ℝ :=
  @legendre_shared.legendre_poly ℤ ℝ _ _ (FReal.ofNoGuard fp) n x

lemma legendre_zero (fp : NoGuardModel) (x : ℝ) : legendre_ng fp 0 x = fp.round 1 := by
  simp [legendre_ng, legendre_shared.legendre_poly]

lemma legendre_succ (fp : NoGuardModel) (x : ℝ) (m : ℕ) :
    legendre_ng fp ((m : ℤ) + 1) x = y fp x (m + 1) := by
  have hne : ((m : ℤ) + 1) ≠ 0 := by omega
  obtain ⟨c, hc⟩ := fold_eq fp x m
  simp only [step] at hc
  simp [legendre_ng, legendre_shared.legendre_poly, range_eq_intRange, hne, hc]

end LegendreProof
