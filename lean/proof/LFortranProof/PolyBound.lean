import Mathlib.Data.Real.Basic
import Mathlib.Data.Rat.Cast.Order
import Mathlib.Algebra.BigOperators.Group.Finset.Basic
import Mathlib.Algebra.BigOperators.Intervals
import Mathlib.Tactic.Linarith
import Mathlib.Tactic.Positivity
import Mathlib.Tactic.Ring
import Mathlib.Tactic.FieldSimp

/-!
# Certified bounds on polynomials

`check p lo hi ε d = true` certifies `|p(x)| ≤ ε` for every real `x ∈ [lo, hi]`
(`check_sound`). The polynomial has rational coefficients (lowest degree
first) and the check is computable over `ℚ`, so a proof closes it with
`decide +kernel`: the kernel evaluates it with exact rational arithmetic, and
no floating point or `native_decide` is trusted.

The check bisects `[lo, hi]` at most `d` times. On each piece, with centre `c`
and radius `r`, it computes the coefficients `q_j` of `p(c + h)` and accepts
if `Σ |q_j| r^j ≤ ε`.
-/

namespace LFortran.PolyBound

open Finset

/-- Horner evaluation in `ℝ` of a polynomial with rational coefficients,
lowest degree first. -/
noncomputable def evalR : List ℚ → ℝ → ℝ
  | [], _ => 0
  | a :: p, x => a + x * evalR p x

/-- Coefficient-wise sum. -/
def addL : List ℚ → List ℚ → List ℚ
  | [], q => q
  | p, [] => p
  | a :: p, b :: q => (a + b) :: addL p q

/-- `c · p`. -/
def scaleL (c : ℚ) (p : List ℚ) : List ℚ := p.map (c * ·)

/-- The coefficients of `h ↦ p(c + h)`: `a + (c + h) q(h)` with `q` the
shift of the tail. -/
def shift (c : ℚ) : List ℚ → List ℚ
  | [] => []
  | a :: p => addL [a] (addL (scaleL c (shift c p)) (0 :: shift c p))

/-- `Σ |q_j| r^j`. -/
def absBound (r : ℚ) : List ℚ → ℚ
  | [] => 0
  | a :: p => |a| + r * absBound r p

/-- The bound on one interval `[lo, hi]`. -/
def pieceBound (p : List ℚ) (lo hi : ℚ) : ℚ :=
  absBound ((hi - lo) / 2) (shift ((lo + hi) / 2) p)

/-- Accept `[lo, hi]` if its bound is at most `ε`, else bisect (at most `d`
more times). -/
def check (p : List ℚ) (lo hi ε : ℚ) : ℕ → Bool
  | 0 => decide (pieceBound p lo hi ≤ ε)
  | d + 1 => decide (pieceBound p lo hi ≤ ε) ||
      (check p lo ((lo + hi) / 2) ε d && check p ((lo + hi) / 2) hi ε d)

theorem evalR_addL : ∀ (p q : List ℚ) (x : ℝ), evalR (addL p q) x = evalR p x + evalR q x
  | [], q, x => by simp [addL, evalR]
  | a :: p, [], x => by simp [addL, evalR]
  | a :: p, b :: q, x => by
    simp only [addL, evalR, evalR_addL p q x]; push_cast; ring

theorem evalR_scaleL (c : ℚ) : ∀ (p : List ℚ) (x : ℝ), evalR (scaleL c p) x = c * evalR p x
  | [], x => by simp [scaleL, evalR]
  | a :: p, x => by
    have := evalR_scaleL c p x
    simp only [scaleL, List.map_cons, evalR] at this ⊢
    rw [this]; push_cast; ring

theorem evalR_shift (c : ℚ) : ∀ (p : List ℚ) (h : ℝ), evalR (shift c p) h = evalR p (c + h)
  | [], h => by simp [shift, evalR]
  | a :: p, h => by
    simp only [shift, evalR_addL, evalR_scaleL, evalR, evalR_shift c p h]
    push_cast; ring

theorem abs_evalR_le {r : ℚ} {h : ℝ} (hr : |h| ≤ r) :
    ∀ p : List ℚ, |evalR p h| ≤ (absBound r p : ℝ)
  | [] => by simp [evalR, absBound]
  | a :: p => by
    have ih := abs_evalR_le hr p
    have h0 : (0 : ℝ) ≤ |evalR p h| := abs_nonneg _
    simp only [evalR, absBound]
    push_cast
    calc |(a : ℝ) + h * evalR p h| ≤ |(a : ℝ)| + |h| * |evalR p h| := by
          rw [← abs_mul]; exact abs_add_le _ _
      _ ≤ |(a : ℝ)| + r * absBound r p := by
          have hr0 : (0 : ℝ) ≤ r := (abs_nonneg h).trans hr
          have := mul_le_mul hr ih h0 hr0
          linarith

theorem pieceBound_sound {p : List ℚ} {lo hi : ℚ} {x : ℝ} (h1 : (lo : ℝ) ≤ x)
    (h2 : x ≤ hi) : |evalR p x| ≤ (pieceBound p lo hi : ℝ) := by
  have hx : x = (((lo + hi) / 2 : ℚ) : ℝ) + (x - (((lo + hi) / 2 : ℚ) : ℝ)) := by ring
  rw [hx, ← evalR_shift]
  apply abs_evalR_le
  push_cast
  rw [abs_le]
  constructor <;> linarith

/-- **Soundness of the checker.** -/
theorem check_sound (p : List ℚ) (ε : ℚ) :
    ∀ (d : ℕ) (lo hi : ℚ), check p lo hi ε d = true →
      ∀ x : ℝ, (lo : ℝ) ≤ x → x ≤ hi → |evalR p x| ≤ ε
  | 0, lo, hi, h, x, h1, h2 => by
    simp only [check, decide_eq_true_eq] at h
    exact (pieceBound_sound h1 h2).trans (by exact_mod_cast h)
  | d + 1, lo, hi, h, x, h1, h2 => by
    simp only [check, Bool.or_eq_true, decide_eq_true_eq, Bool.and_eq_true] at h
    rcases h with h | ⟨hl, hr⟩
    · exact (pieceBound_sound h1 h2).trans (by exact_mod_cast h)
    · rcases le_total x (((lo + hi) / 2 : ℚ) : ℝ) with hm | hm
      · exact check_sound p ε d lo _ hl x h1 hm
      · exact check_sound p ε d _ hi hr x hm h2

/-! ## Polynomials given by their coefficients -/

theorem evalR_append (p q : List ℚ) (x : ℝ) :
    evalR (p ++ q) x = evalR p x + x ^ p.length * evalR q x := by
  induction p with
  | nil => simp [evalR]
  | cons a p ih => simp only [List.cons_append, evalR, ih, List.length_cons]; ring

/-- `Σ_{i<n} f_i x^i` as a coefficient list. -/
theorem evalR_range_map (f : ℕ → ℚ) (n : ℕ) (x : ℝ) :
    evalR ((List.range n).map f) x = ∑ i ∈ range n, (f i : ℝ) * x ^ i := by
  induction n with
  | zero => simp [evalR]
  | succ n ih =>
    rw [List.range_succ, List.map_append, evalR_append, ih, sum_range_succ]
    simp [evalR]
    ring

end LFortran.PolyBound
