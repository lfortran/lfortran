import Legendre.Proof.Loop

/-!
# Local rounding error of one loop iteration

The arithmetic `fp : NoGuardModel` is `Admissible` for a ceiling `U` when
`fp.u ≤ U ≤ 2^-40`, `fp.η ≤ E = U / 10^6` and `2^10 ≤ fp.M`. Every
multiplication, division and rounding then commits an error of at most
`U |exact| + E`, and every subtraction one of at most `U (|a| + |b|) + E`
(no guard digit), as long as the operands are in `fp.F` and the exact result
is at most `2^10` in magnitude.

For `2 ≤ k ≤ 100`, `|x| ≤ 1` and `|a|, |b| ≤ 1 + 10^-6` with `x, a, b ∈ fp.F`,
the computed `step fp x k a b` (including the inexact conversions of `2k-1`,
`k-1`, `k` to real) is in `fp.F` and differs from the exact
`((2k-1) x b - (k-1) a) / k` by at most `18 U`.
-/

open LFortran Legendre

namespace LegendreProof

/-- Allowed underflow error for the ceiling `U`. -/
noncomputable def E (U : ℝ) : ℝ := U / 10 ^ 6

/-- The assumptions on the arithmetic, for the ceiling `U` on its unit roundoff. -/
def Admissible (fp : NoGuardModel) (U : ℝ) : Prop :=
  0 ≤ U ∧ U ≤ 1 / 2 ^ 40 ∧ fp.u ≤ U ∧ fp.η ≤ E U ∧ (2 : ℝ) ^ 10 ≤ fp.M

lemma U_nonneg {fp : NoGuardModel} {U : ℝ} (hfp : Admissible fp U) : 0 ≤ U := hfp.1

lemma U_small {fp : NoGuardModel} {U : ℝ} (hfp : Admissible fp U) : U ≤ 1 / 10 ^ 12 := by
  have := hfp.2.1
  have h : (1 : ℝ) / 2 ^ 40 ≤ 1 / 10 ^ 12 := by norm_num
  linarith

lemma E_nonneg {U : ℝ} (hU : 0 ≤ U) : 0 ≤ E U := by unfold E; positivity

/-- Values of magnitude at most `1000` are in the range of the arithmetic. -/
lemma le_M {fp : NoGuardModel} {U : ℝ} (hfp : Admissible fp U) {t B : ℝ} (ht : |t| ≤ B)
    (hB : B ≤ 1000) : |t| ≤ fp.M := by
  have := hfp.2.2.2.2
  norm_num at this
  linarith

/-- A computed value `c` with error `≤ u S + η`, `S ≤ B`, is `t + ε` with
`|ε| ≤ U B + E`. -/
lemma decomp {fp : NoGuardModel} {U : ℝ} (hfp : Admissible fp U) {c t S B : ℝ}
    (h : |c - t| ≤ fp.u * S + fp.η) (hS0 : 0 ≤ S) (hS : S ≤ B) :
    ∃ ε, c = t + ε ∧ |ε| ≤ U * B + E U := by
  refine ⟨c - t, by ring, ?_⟩
  have h1 := mul_le_mul_of_nonneg_right hfp.2.2.1 hS0
  have h2 := mul_le_mul_of_nonneg_left hS hfp.1
  linarith [hfp.2.2.2.1]

lemma abs_mul_le' {p q P Q : ℝ} (hp : |p| ≤ P) (hq : |q| ≤ Q) : |p * q| ≤ P * Q := by
  rw [abs_mul]
  exact mul_le_mul hp hq (abs_nonneg _) ((abs_nonneg _).trans hp)

/-- Rounding error of one evaluation of the loop body, with the integer
conversions `2K-1`, `K-1`, `K` to real done by `fp.round`. -/
lemma chain_err (fp : NoGuardModel) {U : ℝ} (hfp : Admissible fp U) (x a b K : ℝ) (hK : 2 ≤ K)
    (hK' : K ≤ 100) (hxF : x ∈ fp.F) (haF : a ∈ fp.F) (hbF : b ∈ fp.F)
    (hx : |x| ≤ 1) (ha : |a| ≤ 1 + 1 / 10 ^ 6) (hb : |b| ≤ 1 + 1 / 10 ^ 6) :
    fp.div (fp.sub (fp.mul (fp.mul (fp.round (2 * K - 1)) x) b)
        (fp.mul (fp.round (K - 1)) a)) (fp.round K) ∈ fp.F ∧
    |fp.div (fp.sub (fp.mul (fp.mul (fp.round (2 * K - 1)) x) b)
        (fp.mul (fp.round (K - 1)) a)) (fp.round K)
      - ((2 * K - 1) * x * b - (K - 1) * a) / K| ≤ 18 * U := by
  have hu0 := U_nonneg hfp
  have hu1 := U_small hfp
  have hη0 := E_nonneg hu0
  have hη1 : E U = U / 10 ^ 6 := rfl
  have hKpos : (0 : ℝ) < K := by linarith
  have huK : U * K ≤ 1 / 10 ^ 12 * K := mul_le_mul_of_nonneg_right hu1 hKpos.le
  have hηK : E U ≤ U * K / 10 ^ 6 := by
    have : U * 2 ≤ U * K := mul_le_mul_of_nonneg_left hK hu0
    linarith
  -- K1 = round (2K - 1)
  have hb1 : |2 * K - 1| ≤ 2 * K := by rw [abs_of_pos (by linarith)]; linarith
  have hK1F := fp.round_mem _ (le_M hfp hb1 (by linarith))
  obtain ⟨d1, h1, hd1⟩ := decomp hfp (fp.round_err _ (le_M hfp hb1 (by linarith))) (abs_nonneg _) hb1
  rw [h1] at hK1F ⊢
  have hK1 : |2 * K - 1 + d1| ≤ 2001 / 1000 * K := by
    have := abs_add_le (2 * K - 1) d1
    linarith
  -- t1 = K1 * x
  have hx1 : |(2 * K - 1 + d1) * x| ≤ 2001 / 1000 * K * 1 := abs_mul_le' hK1 hx
  have ht1F := fp.mul_mem _ hK1F _ hxF (le_M hfp hx1 (by linarith))
  obtain ⟨e1, h2, he1⟩ :=
    decomp hfp (fp.mul_err _ hK1F _ hxF (le_M hfp hx1 (by linarith))) (abs_nonneg _) hx1
  rw [h2] at ht1F ⊢
  have ht1 : |(2 * K - 1 + d1) * x + e1| ≤ 2002 / 1000 * K := by
    have := abs_add_le ((2 * K - 1 + d1) * x) e1
    linarith
  -- t2 = t1 * b
  have hx2 : |((2 * K - 1 + d1) * x + e1) * b| ≤ 2002 / 1000 * K * (1 + 1 / 10 ^ 6) :=
    abs_mul_le' ht1 hb
  have ht2F := fp.mul_mem _ ht1F _ hbF (le_M hfp hx2 (by linarith))
  obtain ⟨e2, h3, he2⟩ :=
    decomp hfp (fp.mul_err _ ht1F _ hbF (le_M hfp hx2 (by linarith))) (abs_nonneg _) hx2
  rw [h3] at ht2F ⊢
  -- K2 = round (K - 1)
  have hb2 : |K - 1| ≤ K := by rw [abs_of_pos (by linarith)]; linarith
  have hK2F := fp.round_mem _ (le_M hfp hb2 (by linarith))
  obtain ⟨d2, h4, hd2⟩ := decomp hfp (fp.round_err _ (le_M hfp hb2 (by linarith))) (abs_nonneg _) hb2
  rw [h4] at hK2F ⊢
  have hK2 : |K - 1 + d2| ≤ 1001 / 1000 * K := by
    have := abs_add_le (K - 1) d2
    linarith
  -- t3 = K2 * a
  have hx3 : |(K - 1 + d2) * a| ≤ 1001 / 1000 * K * (1 + 1 / 10 ^ 6) := abs_mul_le' hK2 ha
  have ht3F := fp.mul_mem _ hK2F _ haF (le_M hfp hx3 (by linarith))
  obtain ⟨e3, h5, he3⟩ :=
    decomp hfp (fp.mul_err _ hK2F _ haF (le_M hfp hx3 (by linarith))) (abs_nonneg _) hx3
  rw [h5] at ht3F ⊢
  -- t4 = t2 - t3, with the no-guard bound `u (|t2| + |t3|)`
  have hS4 : |((2 * K - 1 + d1) * x + e1) * b + e2| + |(K - 1 + d2) * a + e3|
      ≤ 301 / 100 * K := by
    have := abs_add_le (((2 * K - 1 + d1) * x + e1) * b) e2
    have := abs_add_le ((K - 1 + d2) * a) e3
    linarith
  have hx4 : |((2 * K - 1 + d1) * x + e1) * b + e2 - ((K - 1 + d2) * a + e3)|
      ≤ 301 / 100 * K := by
    have := abs_sub (((2 * K - 1 + d1) * x + e1) * b + e2) ((K - 1 + d2) * a + e3)
    linarith
  have ht4F := fp.sub_mem _ ht2F _ ht3F (le_M hfp hx4 (by linarith))
  obtain ⟨e4, h6, he4⟩ :=
    decomp hfp (fp.sub_err _ ht2F _ ht3F (le_M hfp hx4 (by linarith))) (by positivity) hS4
  rw [h6] at ht4F ⊢
  have ht4 : |((2 * K - 1 + d1) * x + e1) * b + e2 - ((K - 1 + d2) * a + e3) + e4|
      ≤ 302 / 100 * K := by
    have := abs_add_le (((2 * K - 1 + d1) * x + e1) * b + e2 - ((K - 1 + d2) * a + e3)) e4
    linarith
  -- K3 = round K
  have hb3 : |K| ≤ K := (abs_of_pos hKpos).le
  have hK3F := fp.round_mem _ (le_M hfp hb3 (by linarith))
  obtain ⟨d3, h7, hd3⟩ := decomp hfp (fp.round_err _ (le_M hfp hb3 (by linarith))) (abs_nonneg _) hb3
  rw [h7] at hK3F ⊢
  have hK3 : 999 / 1000 * K ≤ K + d3 := by
    have := neg_abs_le d3
    linarith
  have hK3pos : 0 < K + d3 := by linarith
  -- c = t4 / K3
  have hx5 : |(((2 * K - 1 + d1) * x + e1) * b + e2 - ((K - 1 + d2) * a + e3) + e4)
      / (K + d3)| ≤ 31 / 10 := by
    rw [abs_div, abs_of_pos hK3pos, div_le_iff₀ hK3pos]
    linarith
  have hcF := fp.div_mem _ ht4F _ hK3F hK3pos.ne' (le_M hfp hx5 (by norm_num))
  refine ⟨hcF, ?_⟩
  obtain ⟨e5, h8, he5⟩ :=
    decomp hfp (fp.div_err _ ht4F _ hK3F hK3pos.ne' (le_M hfp hx5 (by norm_num))) (abs_nonneg _) hx5
  rw [h8]
  have key : (((2 * K - 1 + d1) * x + e1) * b + e2 - ((K - 1 + d2) * a + e3) + e4)
      / (K + d3) + e5 - ((2 * K - 1) * x * b - (K - 1) * a) / K
      = e5 + (d1 * x * b + e1 * b + e2 - d2 * a - e3 + e4) / (K + d3)
        - ((2 * K - 1) * x * b - (K - 1) * a) / K * (d3 / (K + d3)) := by
    field_simp
    ring
  rw [key]
  -- the propagated errors
  have hD : |d1 * x * b + e1 * b + e2 - d2 * a - e3 + e4| ≤ 1102 / 100 * (U * K) := by
    have k1 := abs_mul_le' (abs_mul_le' hd1 hx) hb
    have k2 := abs_mul_le' he1 hb
    have k3 := abs_mul_le' hd2 ha
    have := abs_add_le (d1 * x * b + e1 * b + e2 - d2 * a - e3) e4
    have := abs_sub (d1 * x * b + e1 * b + e2 - d2 * a) e3
    have := abs_sub (d1 * x * b + e1 * b + e2) (d2 * a)
    have := abs_add_le (d1 * x * b + e1 * b) e2
    have := abs_add_le (d1 * x * b) (e1 * b)
    linarith
  have hD' : |(d1 * x * b + e1 * b + e2 - d2 * a - e3 + e4) / (K + d3)| ≤ 1104 / 100 * U := by
    rw [abs_div, abs_of_pos hK3pos, div_le_iff₀ hK3pos]
    have := mul_le_mul_of_nonneg_left hK3 hu0
    linarith
  have hN : |((2 * K - 1) * x * b - (K - 1) * a) / K| ≤ 3001 / 1000 := by
    rw [abs_div, abs_of_pos hKpos, div_le_iff₀ hKpos]
    have k1 := abs_mul_le' (abs_mul_le' hb1 hx) hb
    have k2 := abs_mul_le' hb2 ha
    have := abs_sub ((2 * K - 1) * x * b) ((K - 1) * a)
    linarith
  have hd3' : |d3 / (K + d3)| ≤ 1002 / 1000 * U := by
    rw [abs_div, abs_of_pos hK3pos, div_le_iff₀ hK3pos]
    have := mul_le_mul_of_nonneg_left hK3 hu0
    linarith
  have hNd := abs_mul_le' hN hd3'
  have := abs_sub (e5 + (d1 * x * b + e1 * b + e2 - d2 * a - e3 + e4) / (K + d3))
    (((2 * K - 1) * x * b - (K - 1) * a) / K * (d3 / (K + d3)))
  have := abs_add_le e5 ((d1 * x * b + e1 * b + e2 - d2 * a - e3 + e4) / (K + d3))
  linarith

lemma step_err (fp : NoGuardModel) {U : ℝ} (hfp : Admissible fp U) (x a b : ℝ) (m : ℕ)
    (hm : m ≤ 98) (hxF : x ∈ fp.F) (haF : a ∈ fp.F) (hbF : b ∈ fp.F)
    (hx : |x| ≤ 1) (ha : |a| ≤ 1 + 1 / 10 ^ 6) (hb : |b| ≤ 1 + 1 / 10 ^ 6) :
    step fp x ((m : ℤ) + 2) a b ∈ fp.F ∧
    |step fp x ((m : ℤ) + 2) a b - ((2 * m + 3) * x * b - (m + 1) * a) / (m + 2)|
      ≤ 18 * U := by
  have hm' : (m : ℝ) ≤ 98 := by exact_mod_cast hm
  have hm0 : (0 : ℝ) ≤ m := m.cast_nonneg
  have he : ((2 * m + 3) * x * b - (m + 1) * a) / (m + 2) =
      ((2 * ((m : ℝ) + 2) - 1) * x * b - (((m : ℝ) + 2) - 1) * a) / ((m : ℝ) + 2) := by
    ring
  unfold step
  push_cast
  rw [he]
  exact chain_err fp hfp x a b _ (by linarith) (by linarith) hxF haF hbF hx ha hb

end LegendreProof
