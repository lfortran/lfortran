import LFortranProof.VModel

/-!
# Loops of generated code

A Fortran `do k = lo, hi` loop is emitted as `for k in FInt.range lo hi do
body`; after `simp` unfolds the do-notation of a template, it is a
`List.foldl f init (FInt.range lo hi)` over the tuple of the loop's mutable
variables. `range_foldl_inv` proves a property of the result by a loop
invariant `P i s` ("after `i` iterations the state is `s`"), indexed by the
iteration count: it holds initially, and the `i`-th iteration, with index
`k = lo + i`, preserves it. On an integer kind that is exact on `[-B, B)`
(`IntExact`), only the bounds `lo` and `hi` have to be in range; every index
`lo + i` lies between them.
-/

namespace LFortran

/-- An invariant of `List.foldl` over `0, …, n - 1`. -/
theorem foldl_range_inv {σ : Type} (g : σ → ℕ → σ) (init : σ) (P : ℕ → σ → Prop) (n : ℕ)
    (h0 : P 0 init) (hstep : ∀ i s, i < n → P i s → P (i + 1) (g s i)) :
    P n ((List.range n).foldl g init) := by
  induction n with
  | zero => simpa using h0
  | succ n ih =>
    rw [List.range_succ, List.foldl_append, List.foldl_cons, List.foldl_nil]
    exact hstep n _ (Nat.lt_succ_self n) (ih fun i s hi => hstep i s (Nat.lt_succ_of_lt hi))

/-- `do k = lo, hi` over an exact integer kind runs `n = hi + 1 - lo` (or zero)
iterations with the indices `lo + i`. -/
theorem range_ofInt {ι : Type} [FInt ι] {B : ℤ} (I : IntExact ι B) {lo hi : ℤ} (hlo : InR B lo)
    (hhi : InR B hi) :
    FInt.range (FInt.ofInt lo : ι) (FInt.ofInt hi) =
      (List.range (hi + 1 - lo).toNat).map fun (i : ℕ) => FInt.ofInt (lo + (i : ℤ)) := by
  simp only [FInt.range, I.toInt_ofInt lo hlo, I.toInt_ofInt hi hhi]

/-- **Loop invariant.** A property `P n` of the state after the loop
`do k = lo, hi` (`n = hi + 1 - lo` iterations, or none if `hi < lo`) follows
from `P 0 init` and from each iteration `i < n`, with index `lo + i`,
preserving the invariant. -/
theorem range_foldl_inv {ι σ : Type} [FInt ι] {B : ℤ} (I : IntExact ι B) {lo hi : ℤ}
    (hlo : InR B lo) (hhi : InR B hi) (f : σ → ι → σ) (init : σ) (P : ℕ → σ → Prop)
    (h0 : P 0 init)
    (hstep : ∀ (i : ℕ) s, (i : ℤ) < hi + 1 - lo → P i s → P (i + 1) (f s (FInt.ofInt (lo + i)))) :
    P (hi + 1 - lo).toNat ((FInt.range (FInt.ofInt lo : ι) (FInt.ofInt hi)).foldl f init) := by
  rw [range_ofInt I hlo hhi, List.foldl_map]
  exact foldl_range_inv _ init P (hi + 1 - lo).toNat h0 fun i s hi hP => hstep i s (by omega) hP

end LFortran
