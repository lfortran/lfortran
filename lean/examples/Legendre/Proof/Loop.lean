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
import LFortranProof.Loop

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
  simp only [legendre_mod.legendre_poly, hne]
  simp
  -- invariant: after `i` iterations, `(a, b) = (y i, y (i + 1))`
  rw [show m + 1 = ((m : ℤ) + 1 + 1 - 2).toNat + 1 by omega]
  refine (range_foldl_inv I (lo := 2) (hi := (m : ℤ) + 1) ⟨by omega, by omega⟩
    ⟨by omega, by omega⟩ _ _ (σ := K.R8 × K.R8 × K.R8)
    (P := fun i s => s.1 = yK x i ∧ s.2.1 = yK x (i + 1)) ?_ ?_).2
  · exact ⟨rfl, rfl⟩
  · intro i s _ hs
    refine ⟨hs.2, ?_⟩
    show stepK x (FInt.ofInt (2 + (i : ℤ))) s.1 s.2.1 = yK x (i + 2)
    rw [hs.1, hs.2, add_comm (2 : ℤ)]
    rfl

/-- The generated `legendre_poly` in a `NoGuardModel`. -/
noncomputable def legendre_ng (fp : NoGuardModel) (n : ℤ) (x : ℝ) : ℝ :=
  @legendre_mod.legendre_poly proofKinds (FKinds.ofNoGuard fp fp) n x

end LegendreProof
