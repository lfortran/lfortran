import Horner.Generated
import LFortranProof.Loop
import LFortranProof.Arr

/-!
# The loop of `horner` as a fold over the data of `c`

On any carrier whose `integer(4)` is exact on `[-B, B)`, for `1 ≤ n < B` the
generated
`horner (n, c, x)` starts from `c(n)` and runs `n - 1` iterations
`p ← p x + c(n - 1 - t)` (`horner_inv`, a loop invariant `P t p` after `t`
iterations). The `t`-th iteration reads the element `c.data[n - 2 - t]`.
-/

open LFortran

namespace HornerProof

variable {K : Kinds} [FKinds K]

/-- **The loop of `horner`.** A property of the state after `t` iterations
that holds for `c(n)` and is preserved by `p ↦ p x + c.data[n - 2 - t]`
holds for the result. -/
theorem horner_inv {B : ℤ} (I : IntExact K.I4 B) (n : ℕ) (hn1 : 1 ≤ n) (hn : (n : ℤ) < B)
    (cv : Arr K.R8) (xv : K.R8) (P : ℕ → K.R8 → Prop) (h0 : P 0 cv.data[n - 1]!)
    (hstep : ∀ t p, t < n - 1 → P t p → P (t + 1) (FReal.add (FReal.mul p xv) cv.data[n - 2 - t]!)) :
    P (n - 1) (horner_mod.horner (K := K) (FInt.ofInt n) cv xv) := by
  have rn : InR B (n : ℤ) := ⟨by omega, hn⟩
  have r1 : InR B 1 := ⟨by omega, by omega⟩
  have hsub : FInt.sub (FInt.ofInt (n : ℤ) : K.I4) (FInt.ofInt 1) = FInt.ofInt ((n : ℤ) - 1) :=
    I.sub _ _ rn r1 ⟨by omega, by omega⟩
  have hT : FInt.toInt (FInt.ofInt (n : ℤ) : K.I4) = n := I.toInt_ofInt _ rn
  simp only [horner_mod.horner]
  simp only [hsub, hT, Id.run, Arr.get_reshape_one]
  simp
  have hcount : (((n : ℤ) - 1) + 1 - 1).toNat = n - 1 := by omega
  rw [← hcount]
  refine range_foldl_inv I r1 ⟨by omega, by omega⟩ _ _ P ?_ ?_
  · convert h0 using 3
  · intro t p ht hp
    have rt : InR B (1 + (t : ℤ)) := ⟨by omega, by omega⟩
    rw [I.sub _ _ rn rt ⟨by omega, by omega⟩, I.toInt_ofInt _ ⟨by omega, by omega⟩]
    convert hstep t p (by omega) hp using 3
    omega

end HornerProof
