import Mathlib.Tactic.Linarith
import Mathlib.Tactic.Positivity
import Mathlib.Tactic.FieldSimp
import Mathlib.Tactic.GCongr
import Mathlib.Tactic.Ring
import Mathlib.Tactic.NormNum
import Mathlib.Tactic.LinearCombination
import Legendre.Proof.Bonnet
import Legendre.Generated
import LFortranProof.Arith.Model
import LFortranProof.VModel

/-!
# The loop of `legendre_poly` as a recursion

On any carrier (`K.I4`, `K.R8`) whose `integer(4)` is exact (`IntExact`),
the generated template `legendre_poly`
(`Legendre/Generated.lean`) computes `yK x n`: `yK x k` is the value of `b`
after the iterations `k = 2, ..., k` of the Fortran loop, with
`yK x 0 = real(1)` (the initial value of `a`) and `yK x 1 = x`
(`legendre_zero`, `legendre_succ`, while `n + 2 < B` for an integer kind exact
on `[-B, B)`). `legendre_ng m n x` is the
template on `ℝ` with a `NoGuardModel` `m`.
-/

open LFortran Legendre

namespace LegendreProof

variable {K : Kinds} [FKinds K]

/-- One evaluation of the loop body, the new `c` computed from `a`, `b`, as
the generated code writes it. -/
def stepK (x : K.R8) (k : K.I4) (a b : K.R8) : K.R8 :=
  FReal.div
    (FReal.sub
      (FReal.mul
        (FReal.mul (FReal.ofInt (FInt.toInt (FInt.sub (FInt.mul (FInt.ofInt 2) k) (FInt.ofInt 1))))
          x) b)
      (FReal.mul (FReal.ofInt (FInt.toInt (FInt.sub k (FInt.ofInt 1)))) a))
    (FReal.ofInt (FInt.toInt k))

/-- The sequence of values computed by the loop. -/
def yK (x : K.R8) : ℕ → K.R8
  | 0 => FReal.ofInt 1
  | 1 => x
  | m + 2 => stepK x (FInt.ofInt ((m : ℤ) + 2)) (yK x m) (yK x (m + 1))

lemma intRange_succ (lo hi : ℤ) (h : lo ≤ hi + 1) :
    intRange lo (hi + 1) = intRange lo hi ++ [hi + 1] := by
  unfold intRange
  rw [show (hi + 1 + 1 - lo).toNat = (hi + 1 - lo).toNat + 1 by omega]
  simp [List.range_succ]
  omega

/-- The loop `do k = 2, n` runs over `2, …, n` without overflow while
`n + 2 ≤ B`. -/
lemma range_ofInt {B : ℤ} (I : IntExact K.I4 B) (m : ℕ) (hm : (m : ℤ) + 3 ≤ B) :
    FInt.range (FInt.ofInt 2 : K.I4) (FInt.ofInt ((m : ℤ) + 1)) =
      (intRange 2 ((m : ℤ) + 1)).map FInt.ofInt := by
  have h2 : FInt.toInt (FInt.ofInt 2 : K.I4) = 2 := I.toInt_ofInt 2 ⟨by omega, by omega⟩
  have hn : FInt.toInt (FInt.ofInt ((m : ℤ) + 1) : K.I4) = (m : ℤ) + 1 :=
    I.toInt_ofInt _ ⟨by omega, by omega⟩
  simp only [FInt.range, intRange, h2, hn, List.map_map]
  apply List.map_congr_left
  intro i hi
  simp only [List.mem_range] at hi
  show FInt.add (FInt.ofInt 2 : K.I4) (FInt.ofInt (i : ℤ)) = FInt.ofInt (2 + (i : ℤ))
  exact I.add 2 i ⟨by omega, by omega⟩ ⟨by omega, by omega⟩ ⟨by omega, by omega⟩

lemma fold_eq (x : K.R8) (m : ℕ) :
    ∃ c, List.foldl (fun (s : K.R8 × K.R8 × K.R8) (k : K.I4) =>
        (s.2.1, stepK x k s.1 s.2.1, stepK x k s.1 s.2.1)) (FReal.ofInt 1, x, FReal.zero)
        ((intRange 2 ((m : ℤ) + 1)).map FInt.ofInt) = (yK x m, yK x (m + 1), c) := by
  induction m with
  | zero => exact ⟨FReal.zero, rfl⟩
  | succ m ih =>
    obtain ⟨c, hc⟩ := ih
    refine ⟨yK x (m + 2), ?_⟩
    rw [show ((m + 1 : ℕ) : ℤ) + 1 = ((m : ℤ) + 1) + 1 by push_cast; ring,
      intRange_succ _ _ (by omega), List.map_append, List.foldl_append, hc]
    simp only [List.map_cons, List.map_nil, List.foldl_cons, List.foldl_nil]
    have : yK x (m + 2) = stepK x (FInt.ofInt ((m : ℤ) + 2)) (yK x m) (yK x (m + 1)) := rfl
    rw [this, add_assoc, one_add_one_eq_two]

lemma legendre_zero {B : ℤ} (I : IntExact K.I4 B) (hB : 0 < B) (x : K.R8) :
    legendre_mod.legendre_poly (K := K) (FInt.ofInt 0) x = FReal.ofInt 1 := by
  have h : FInt.beq (FInt.ofInt 0 : K.I4) (FInt.ofInt 0) = true :=
    (I.beq 0 0 ⟨by omega, hB⟩ ⟨by omega, hB⟩).2 rfl
  simp [legendre_mod.legendre_poly, h]

lemma legendre_succ {B : ℤ} (I : IntExact K.I4 B) (x : K.R8) (m : ℕ) (hm : (m : ℤ) + 3 ≤ B) :
    legendre_mod.legendre_poly (K := K) (FInt.ofInt ((m : ℤ) + 1)) x = yK x (m + 1) := by
  have hne : FInt.beq (FInt.ofInt ((m : ℤ) + 1) : K.I4) (FInt.ofInt 0) = false := by
    rw [Bool.eq_false_iff]
    intro h
    have := (I.beq _ _ ⟨by omega, by omega⟩ ⟨by omega, by omega⟩).1 h
    omega
  obtain ⟨c, hc⟩ := fold_eq x m
  simp only [stepK] at hc
  simp [legendre_mod.legendre_poly, hne, range_ofInt I m hm, hc]

/-- The generated `legendre_poly` in a `NoGuardModel`. -/
noncomputable def legendre_ng (fp : NoGuardModel) (n : ℤ) (x : ℝ) : ℝ :=
  @legendre_mod.legendre_poly proofKinds (FKinds.ofNoGuard fp fp) n x

end LegendreProof
