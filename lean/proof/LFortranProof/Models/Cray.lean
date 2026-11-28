import LFortranProof.NoGuard

/-!
# Cray arithmetic: no guard digit

CRAY-1 Hardware Reference Manual 2240004 C (Cray Research, 1977), section
"Floating point arithmetic":

* A word is a sign bit, a 15-bit exponent biased by `40000₈`, and a 48-bit
  signed-magnitude coefficient, a fraction `0.1xx…x₂` when normalized. There
  is no hidden bit. Overflow is an exponent `≥ 60000₈` (`e ≥ 2^13`),
  underflow `≤ 17777₈` (`e < -2^13`); an underflow result is zero (flush to
  zero, no subnormals), no functional unit produces `-0`, and there are no
  infinities or NaNs.
* *Addition* is done in a 49-bit register. The operand with the smaller
  exponent is shifted right, "bits shifted out of the register are lost; no
  round-up takes place". There is no guard digit.
* *Multiplication* truncates the low part of the 96-bit partial-product
  pyramid and adds a constant to compensate. It is not correctly rounded,
  and `A × B` need not equal `B × A`.
* *Division* is `a · r (2 - b r)`, with a 30-bit reciprocal approximation
  `r` refined by one Newton step to 47 bits.

N. J. Higham, *Accuracy and Stability of Numerical Algorithms*, 2nd ed.,
SIAM, 2002, §2.4, models a machine without a guard digit (Cray 1, 2, X-MP,
Y-MP, C90) by

  `fl(x ± y) = x (1 + α) ± y (1 + β)`, `|α|, |β| ≤ u`,          (2.6a)
  `fl(x op y) = (x op y)(1 + δ)`, `|δ| ≤ u`, `op = ×, ÷`.        (2.6b)

On a Cray, subtracting a power of two from the next smaller number gives a
result twice too large (X-MP, Y-MP) or zero (Cray 2): relative error `1`.

`NoGuardModel` (`LFortran/NoGuard.lean`) is (2.6) with the underflow term
`η` of `FPModel`. Every `FPModel` is a `NoGuardModel` (`FPModel.toNoGuard`),
but not conversely: `noGuardToy` below is Higham's three-digit machine, and
`noGuard_not_fpModel` shows that no accurate `FPModel` computes it.

Higham's (2.6b) is an idealization for the Cray: its multiply and divide are
not correctly rounded, so `u` must be taken as the worst of the three
operations, a few units of `2^-47`.
-/

namespace LFortran

/-- CRAY-1 floating-point numbers: zero, and `m · 2^(e-48)` with a normalized
48-bit coefficient `2^47 ≤ |m| < 2^48` and `-2^13 ≤ e < 2^13`. -/
def crayF : Set ℝ :=
  {x | x = 0 ∨ ∃ m e : ℤ, 2 ^ 47 ≤ |m| ∧ |m| < 2 ^ 48 ∧ -8192 ≤ e ∧ e ≤ 8191 ∧
    x = m * (2 : ℝ) ^ (e - 48)}

theorem zero_mem_crayF : (0 : ℝ) ∈ crayF := Or.inl rfl

theorem one_mem_crayF : (1 : ℝ) ∈ crayF := by
  refine Or.inr ⟨2 ^ 47, 1, by norm_num, by norm_num, by norm_num, by norm_num, ?_⟩
  norm_num

/-- Higham's three-digit example: binary, `t = 3`, chopping, no guard digit.
`1 - 0.111₂` is computed as `1 - 0.11₂ = 0.01₂`, twice the exact `0.001₂`.
All other operations are exact. -/
noncomputable def noGuardToy : NoGuardModel where
  F := Set.univ
  u := 1 / 4
  η := 0
  M := 8
  round := id
  add := (· + ·)
  sub a b := if a = 1 ∧ b = 7 / 8 then 1 / 4 else a - b
  mul := (· * ·)
  div := (· / ·)
  round_mem _ _ := trivial
  round_err _ _ := by simp
  add_mem _ _ _ _ _ := trivial
  add_err _ _ _ _ _ := by simp; positivity
  sub_mem _ _ _ _ _ := trivial
  sub_err a _ b _ _ := by
    split
    · rename_i h
      obtain ⟨rfl, rfl⟩ := h
      norm_num
    · simp; positivity
  mul_mem _ _ _ _ _ := trivial
  mul_err _ _ _ _ _ := by simp; positivity
  div_mem _ _ _ _ _ _ := trivial
  div_err _ _ _ _ _ _ := by simp

theorem noGuardToy_sub : noGuardToy.sub 1 (7 / 8) = 1 / 4 := by
  simp [noGuardToy]

/-- The no-guard-digit model is strictly weaker: no `FPModel` with
`u + 8 η < 1` computes the toy subtraction, so a proof about every
`FPModel` says nothing about such a machine. -/
theorem noGuard_not_fpModel :
    ¬ ∃ fp : FPModel, fp.sub = noGuardToy.sub ∧ (1 : ℝ) ∈ fp.F ∧ (7 / 8 : ℝ) ∈ fp.F ∧
      1 ≤ fp.M ∧ fp.u + 8 * fp.η < 1 := by
  rintro ⟨fp, hsub, h1, h78, hM, hsmall⟩
  have hM' : |(1 : ℝ) - 7 / 8| ≤ fp.M := by norm_num; linarith
  have h := fp.sub_err 1 h1 (7 / 8) h78 hM'
  rw [hsub, noGuardToy_sub] at h
  norm_num at h
  linarith

end LFortran
