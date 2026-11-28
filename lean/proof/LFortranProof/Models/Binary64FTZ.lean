import LFortranProof.Models.Binary64

/-!
# IEEE-754 binary64 with flush-to-zero (FTZ) and denormals-are-zero (DAZ)

IEEE Standard for Floating-Point Arithmetic, IEEE Std 754-2019; the FTZ and
DAZ modes are non-standard hardware modes (e.g. the `FTZ` and `DAZ` bits of
the x86 `MXCSR` register, or `-ffast-math`/`-Ofast`, which enable them).

* *Flush-to-zero* (FTZ): a result that would be subnormal (tiny) is replaced
  by zero. Depending on the hardware the decision is made on the exact result
  or on the rounded result.
* *Denormals-are-zero* (DAZ): subnormal operands are treated as zero.

We model FTZ arithmetic on the set `binary64Normal` of binary64 numbers
without subnormals (zero and the normal numbers, `|x| ≥ 2^-1022`). A
flush-to-zero rounding `r'` built on a round-to-nearest `r`
(`IsFlushToZero`) either returns `r x` when that is zero or normal, or
returns `0`, which is only allowed for `|x| ≤ 2^-1022 + 2^-1075`. This
covers flushing based on the exact result (`|x| < 2^-1022`) and on the
rounded result (`|r x| < 2^-1022`, see `exists_flushToZero`). Flushing costs
an absolute error of up to about `2^-1022`, so the underflow term grows from
`2^-1075` to `η = 2^-1021` (`binary64FTZModel`).

Because `F = binary64Normal` contains no subnormals, the hypotheses `a ∈ F`
of `FPModel` mean that no operand is ever subnormal, so DAZ never changes an
input and FTZ+DAZ arithmetic is covered as well. The price is that inputs to a
program must not be subnormal (in the Legendre specification this is the
hypothesis `x ∈ fp.F`).
-/

namespace LFortran

/-- binary64 without subnormals: 0 or normal numbers. -/
def binary64Normal : Set ℝ := {x | x = 0 ∨ (x ∈ binary64 ∧ 1/2^1022 ≤ |x|)}

/-- `r'` is a flush-to-zero version of the rounding `r` on `[-M, M]`: it either
agrees with `r` where `r` gives zero or a normal number, or it flushes to `0`
an `x` of magnitude at most `2^-1022 + 2^-1075`. -/
def IsFlushToZero (M : ℝ) (r r' : ℝ → ℝ) : Prop :=
  ∀ x, |x| ≤ M → (r' x = r x ∧ r x ∈ binary64Normal) ∨
    (r' x = 0 ∧ |x| ≤ 1/2^1022 + 1/2^1075)

theorem one_div_two_pow_eq_add (n : ℕ) :
    (1:ℝ)/2^n = 1/2^(n+1) + 1/2^(n+1) := by
  rw [pow_succ]; ring

/-- The error bound of flush-to-zero rounding: for `|x| ≤ 2^1022`,
`|r' x - x| ≤ 2^-53 |x| + 2^-1021`. -/
theorem flushToZero_err {r r' : ℝ → ℝ} (hr : IsRoundToNearest binary64 (2^1022) r)
    (hr' : IsFlushToZero (2^1022) r r') {x : ℝ} (hx : |x| ≤ 2^1022) :
    |r' x - x| ≤ 1/2^53 * |x| + 1/2^1021 := by
  rcases hr' x hx with ⟨h, -⟩ | ⟨h, hsmall⟩
  · rw [h]
    refine (roundToNearest_binary64_err hr hx).trans (add_le_add_right ?_ _)
    exact one_div_le_one_div_of_le (by positivity) (pow_le_pow_right₀ (by norm_num) (by norm_num))
  · rw [h, zero_sub, abs_neg]
    calc |x| ≤ 1/2^1022 + 1/2^1075 := hsmall
      _ ≤ 1/2^1022 + 1/2^1022 := add_le_add_right
          (one_div_le_one_div_of_le (by positivity)
            (pow_le_pow_right₀ (by norm_num) (by norm_num))) _
      _ = 1/2^1021 := (one_div_two_pow_eq_add 1021).symm
      _ ≤ 1/2^53 * |x| + 1/2^1021 := le_add_of_nonneg_left (by positivity)

theorem flushToZero_mem {r r' : ℝ → ℝ} (hr' : IsFlushToZero (2^1022) r r') {x : ℝ}
    (hx : |x| ≤ 2^1022) : r' x ∈ binary64Normal := by
  rcases hr' x hx with ⟨h, hmem⟩ | ⟨h, -⟩
  · rw [h]; exact hmem
  · rw [h]; exact Or.inl rfl

/-- binary64 with flush-to-zero (and denormals-are-zero) is an `FPModel` with
`u = 2^-53`, `η = 2^-1021` and `M = 2^1022`. -/
noncomputable def binary64FTZModel (r : ℝ → ℝ) (hr : IsRoundToNearest binary64 (2^1022) r)
    (r' : ℝ → ℝ) (hr' : IsFlushToZero (2^1022) r r') : FPModel where
  F := binary64Normal
  u := 1/2^53
  η := 1/2^1021
  M := 2^1022
  round := r'
  add a b := r' (a + b)
  sub a b := r' (a - b)
  mul a b := r' (a * b)
  div a b := r' (a / b)
  round_mem _ hy := flushToZero_mem hr' hy
  round_err _ hy := flushToZero_err hr hr' hy
  add_mem _ _ _ _ h := flushToZero_mem hr' h
  add_err _ _ _ _ h := flushToZero_err hr hr' h
  sub_mem _ _ _ _ h := flushToZero_mem hr' h
  sub_err _ _ _ _ h := flushToZero_err hr hr' h
  mul_mem _ _ _ _ h := flushToZero_mem hr' h
  mul_err _ _ _ _ h := flushToZero_err hr hr' h
  div_mem _ _ _ _ _ h := flushToZero_mem hr' h
  div_err _ _ _ _ _ h := flushToZero_err hr hr' h

/-- The smallest positive normal number `2^-1022` is a binary64 number. -/
theorem one_div_two_pow_1022_mem : (1:ℝ)/2^1022 ∈ binary64 :=
  ⟨1, -1022, by norm_num, by norm_num, by norm_num, by rw [zpow_neg, zpow_ofNat]; simp⟩

theorem neg_one_div_two_pow_1022_mem : -((1:ℝ)/2^1022) ∈ binary64 :=
  ⟨-1, -1022, by norm_num, by norm_num, by norm_num, by rw [zpow_neg, zpow_ofNat]; simp⟩

/-- A round-to-nearest result smaller in magnitude than a floating-point number
`l > 0` (with `-l` also a floating-point number) comes from an `x` with
`|x| < l`. -/
theorem abs_lt_of_roundToNearest_lt {F : Set ℝ} {M : ℝ} {r : ℝ → ℝ}
    (hr : IsRoundToNearest F M r) {l : ℝ} (hl : l ∈ F) (hl' : -l ∈ F) {x : ℝ}
    (hx : |x| ≤ M) (hrx : |r x| < l) : |x| < l := by
  by_contra hge
  have hge := not_lt.mp hge
  -- `±l` with the sign of `x` is nearer to `x` than `r x`
  have hnear : ∃ f ∈ F, |f - x| = |x| - l := by
    rcases le_or_gt 0 x with h0 | h0
    · refine ⟨l, hl, ?_⟩
      rw [abs_of_nonneg h0] at hge ⊢
      rw [abs_sub_comm, abs_of_nonneg (by linarith)]
    · refine ⟨-l, hl', ?_⟩
      rw [abs_of_neg h0] at hge ⊢
      rw [abs_of_nonneg (by linarith)]
      ring
  obtain ⟨f, hf, hfx⟩ := hnear
  have h1 := (hr x hx).2 f hf
  have h2 := abs_sub_abs_le_abs_sub x (r x)
  rw [abs_sub_comm] at h2
  linarith

/-- Flushing based on the rounded result,
`r' x = if |r x| < 2^-1022 then 0 else r x`, is a flush-to-zero rounding. In
fact it only flushes `x` with `|x| < 2^-1022`. -/
theorem exists_flushToZero {r : ℝ → ℝ} (hr : IsRoundToNearest binary64 (2^1022) r) :
    ∃ r', IsFlushToZero (2^1022) r r' := by
  refine ⟨fun x => if |r x| < 1/2^1022 then 0 else r x, fun x hx => ?_⟩
  by_cases hrx : |r x| < 1/2^1022
  · right
    refine ⟨if_pos hrx, ?_⟩
    exact (abs_lt_of_roundToNearest_lt hr one_div_two_pow_1022_mem
      neg_one_div_two_pow_1022_mem hx hrx).le.trans (le_add_of_nonneg_right (by positivity))
  · left
    exact ⟨if_neg hrx, Or.inr ⟨(hr x hx).1, not_lt.mp hrx⟩⟩

end LFortran
