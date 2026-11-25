import Legendre.Proof.Loop

/-!
# Local rounding error of one loop iteration

An arithmetic `V : VModel` (on any carrier) is `Admissible` for a ceiling `U`
when `V.u ≤ U ≤ 2^-40`, `V.η ≤ E = U / 10^6` and `2^10 ≤ V.M`. Every
multiplication, division and integer conversion then commits an error of at
most `U |exact| + E`, and every subtraction one of at most `U (|a| + |b|) + E`
(no guard digit), as long as the exact result is at most `2^10` in magnitude.

For `2 ≤ k ≤ 100`, `|x| ≤ 1` and `|a|, |b| ≤ 1 + 10^-6`, the loop body
`stepK x k a b` (with exact `integer(4)` arithmetic for `2k-1`, `k-1`, `k`
and inexact conversions of them to real) has a value within `18 U` of the
exact `((2k-1) x b - (k-1) a) / k` (`step_err`).
-/

open LFortran Legendre

namespace LegendreProof

/-- Allowed underflow error for the ceiling `U`. -/
noncomputable def E (U : ℝ) : ℝ := U / 10 ^ 6

variable {ρ : Type} [FReal ρ]

/-- The assumptions on the arithmetic, for the ceiling `U` on its unit roundoff. -/
def Admissible (V : VModel ρ) (U : ℝ) : Prop :=
  0 ≤ U ∧ U ≤ 1 / 2 ^ 40 ∧ V.u ≤ U ∧ V.η ≤ E U ∧ (2 : ℝ) ^ 10 ≤ V.M

lemma U_nonneg {V : VModel ρ} {U : ℝ} (hfp : Admissible V U) : 0 ≤ U := hfp.1

lemma U_small {V : VModel ρ} {U : ℝ} (hfp : Admissible V U) : U ≤ 1 / 10 ^ 12 := by
  have := hfp.2.1
  have h : (1 : ℝ) / 2 ^ 40 ≤ 1 / 10 ^ 12 := by norm_num
  linarith

lemma E_nonneg {U : ℝ} (hU : 0 ≤ U) : 0 ≤ E U := by unfold E; positivity

/-- Values of magnitude at most `1000` are in the range of the arithmetic. -/
lemma le_M {V : VModel ρ} {U : ℝ} (hfp : Admissible V U) {t B : ℝ} (ht : |t| ≤ B)
    (hB : B ≤ 1000) : |t| ≤ V.M := by
  have := hfp.2.2.2.2
  norm_num at this
  linarith

/-- A computed value `c` with error `≤ u S + η`, `S ≤ B`, is `t + ε` with
`|ε| ≤ U B + E`. -/
lemma decomp {V : VModel ρ} {U : ℝ} (hfp : Admissible V U) {c t S B : ℝ}
    (h : |c - t| ≤ V.u * S + V.η) (hS0 : 0 ≤ S) (hS : S ≤ B) :
    ∃ ε, c = t + ε ∧ |ε| ≤ U * B + E U := by
  refine ⟨c - t, by ring, ?_⟩
  have h1 := mul_le_mul_of_nonneg_right hfp.2.2.1 hS0
  have h2 := mul_le_mul_of_nonneg_left hS hfp.1
  linarith [hfp.2.2.2.1]

lemma abs_mul_le' {p q P Q : ℝ} (hp : |p| ≤ P) (hq : |q| ≤ Q) : |p * q| ≤ P * Q := by
  rw [abs_mul]
  exact mul_le_mul hp hq (abs_nonneg _) ((abs_nonneg _).trans hp)

set_option maxHeartbeats 1000000 in
/-- Rounding error of one evaluation of the loop body, with the integer
conversions `2k-1`, `k-1`, `k` to real done by `FReal.ofInt`. -/
lemma step_err {Ks : Kinds} [FKinds Ks] (V : VModel Ks.R8) {U : ℝ} (hfp : Admissible V U)
    (I : IntExact Ks.I4 (2 ^ 31)) (k : ℕ) (hk2 : 2 ≤ k) (hk : k ≤ 100) {xv av bv : Ks.R8}
    {x a b : ℝ} (hxv : V.val xv x) (hav : V.val av a) (hbv : V.val bv b)
    (hx : |x| ≤ 1) (ha : |a| ≤ 1 + 1 / 10 ^ 6) (hb : |b| ≤ 1 + 1 / 10 ^ 6) :
    ∃ c, V.val (stepK xv (FInt.ofInt (k : ℤ)) av bv) c ∧
      |c - ((2 * (k : ℝ) - 1) * x * b - ((k : ℝ) - 1) * a) / k| ≤ 18 * U := by
  -- the integer arithmetic is exact
  have r2 : InR (2 ^ 31) 2 := ⟨by norm_num, by norm_num⟩
  have r1 : InR (2 ^ 31) 1 := ⟨by norm_num, by norm_num⟩
  have rk : InR (2 ^ 31) (k : ℤ) := ⟨by omega, by omega⟩
  have hK1 : FInt.toInt (FInt.sub (FInt.mul (FInt.ofInt 2) (FInt.ofInt (k : ℤ))) (FInt.ofInt 1)
      : Ks.I4) = 2 * (k : ℤ) - 1 := by
    rw [I.mul 2 k r2 rk ⟨by omega, by omega⟩, I.sub _ 1 ⟨by omega, by omega⟩ r1 ⟨by omega, by omega⟩,
      I.toInt_ofInt _ ⟨by omega, by omega⟩]
  have hK2 : FInt.toInt (FInt.sub (FInt.ofInt (k : ℤ)) (FInt.ofInt 1) : Ks.I4) = (k : ℤ) - 1 := by
    rw [I.sub _ 1 rk r1 ⟨by omega, by omega⟩, I.toInt_ofInt _ ⟨by omega, by omega⟩]
  have hK3 : FInt.toInt (FInt.ofInt (k : ℤ) : Ks.I4) = k := I.toInt_ofInt _ rk
  unfold stepK
  rw [hK1, hK2, hK3]
  set K : ℝ := (k : ℝ) with hKdef
  have hK : 2 ≤ K := by rw [hKdef]; exact_mod_cast hk2
  have hK' : K ≤ 100 := by rw [hKdef]; exact_mod_cast hk
  have hu0 := U_nonneg hfp
  have hu1 := U_small hfp
  have hη0 := E_nonneg hu0
  have hη1 : E U = U / 10 ^ 6 := rfl
  have hKpos : (0 : ℝ) < K := by linarith
  have huK : U * K ≤ 1 / 10 ^ 12 * K := mul_le_mul_of_nonneg_right hu1 hKpos.le
  have hηK : E U ≤ U * K / 10 ^ 6 := by
    have : U * 2 ≤ U * K := mul_le_mul_of_nonneg_left hK hu0
    linarith
  -- K1 = real(2k - 1)
  have hc1 : (((2 * (k : ℤ) - 1 : ℤ)) : ℝ) = 2 * K - 1 := by rw [hKdef]; push_cast; ring
  have hb1 : |2 * K - 1| ≤ 2 * K := by rw [abs_of_pos (by linarith)]; linarith
  obtain ⟨w1, hw1, h1⟩ := V.ofInt_err (2 * (k : ℤ) - 1) (by rw [abs_lt]; constructor <;> omega)
    (by rw [hc1]; exact le_M hfp hb1 (by linarith))
  rw [hc1] at h1
  obtain ⟨d1, rfl, hd1⟩ := decomp hfp h1 (abs_nonneg _) hb1
  have hK1 : |2 * K - 1 + d1| ≤ 2001 / 1000 * K := by
    have := abs_add_le (2 * K - 1) d1
    linarith
  -- t1 = K1 * x
  have hx1 : |(2 * K - 1 + d1) * x| ≤ 2001 / 1000 * K * 1 := abs_mul_le' hK1 hx
  obtain ⟨w2, hw2, h2⟩ := V.mul_err _ _ _ _ hw1 hxv (le_M hfp hx1 (by linarith))
  obtain ⟨e1, rfl, he1⟩ := decomp hfp h2 (abs_nonneg _) hx1
  have ht1 : |(2 * K - 1 + d1) * x + e1| ≤ 2002 / 1000 * K := by
    have := abs_add_le ((2 * K - 1 + d1) * x) e1
    linarith
  -- t2 = t1 * b
  have hx2 : |((2 * K - 1 + d1) * x + e1) * b| ≤ 2002 / 1000 * K * (1 + 1 / 10 ^ 6) :=
    abs_mul_le' ht1 hb
  obtain ⟨w3, hw3, h3⟩ := V.mul_err _ _ _ _ hw2 hbv (le_M hfp hx2 (by linarith))
  obtain ⟨e2, rfl, he2⟩ := decomp hfp h3 (abs_nonneg _) hx2
  -- K2 = real(k - 1)
  have hc2 : (((k : ℤ) - 1 : ℤ) : ℝ) = K - 1 := by rw [hKdef]; push_cast; ring
  have hb2 : |K - 1| ≤ K := by rw [abs_of_pos (by linarith)]; linarith
  obtain ⟨w4, hw4, h4⟩ := V.ofInt_err ((k : ℤ) - 1) (by rw [abs_lt]; constructor <;> omega)
    (by rw [hc2]; exact le_M hfp hb2 (by linarith))
  rw [hc2] at h4
  obtain ⟨d2, rfl, hd2⟩ := decomp hfp h4 (abs_nonneg _) hb2
  have hK2 : |K - 1 + d2| ≤ 1001 / 1000 * K := by
    have := abs_add_le (K - 1) d2
    linarith
  -- t3 = K2 * a
  have hx3 : |(K - 1 + d2) * a| ≤ 1001 / 1000 * K * (1 + 1 / 10 ^ 6) := abs_mul_le' hK2 ha
  obtain ⟨w5, hw5, h5⟩ := V.mul_err _ _ _ _ hw4 hav (le_M hfp hx3 (by linarith))
  obtain ⟨e3, rfl, he3⟩ := decomp hfp h5 (abs_nonneg _) hx3
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
  obtain ⟨w6, hw6, h6⟩ := V.sub_err _ _ _ _ hw3 hw5 (le_M hfp hx4 (by linarith))
  obtain ⟨e4, rfl, he4⟩ := decomp hfp h6 (by positivity) hS4
  have ht4 : |((2 * K - 1 + d1) * x + e1) * b + e2 - ((K - 1 + d2) * a + e3) + e4|
      ≤ 302 / 100 * K := by
    have := abs_add_le (((2 * K - 1 + d1) * x + e1) * b + e2 - ((K - 1 + d2) * a + e3)) e4
    linarith
  -- K3 = real(k)
  have hb3 : |K| ≤ K := (abs_of_pos hKpos).le
  obtain ⟨w7, hw7, h7⟩ := V.ofInt_err (k : ℤ) (by rw [abs_lt]; constructor <;> omega)
    (by push_cast; exact le_M hfp hb3 (by linarith))
  have h7' : |w7 - K| ≤ V.u * |K| + V.η := by rw [hKdef]; exact_mod_cast h7
  obtain ⟨d3, rfl, hd3⟩ := decomp hfp h7' (abs_nonneg _) hb3
  have hK3 : 999 / 1000 * K ≤ K + d3 := by
    have := neg_abs_le d3
    linarith
  have hK3pos : 0 < K + d3 := by linarith
  -- c = t4 / K3
  have hx5 : |(((2 * K - 1 + d1) * x + e1) * b + e2 - ((K - 1 + d2) * a + e3) + e4)
      / (K + d3)| ≤ 31 / 10 := by
    rw [abs_div, abs_of_pos hK3pos, div_le_iff₀ hK3pos]
    linarith
  obtain ⟨w8, hw8, h8⟩ := V.div_err _ _ _ _ hw6 hw7 hK3pos.ne' (le_M hfp hx5 (by norm_num))
  obtain ⟨e5, rfl, he5⟩ := decomp hfp h8 (abs_nonneg _) hx5
  refine ⟨_, hw8, ?_⟩
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

end LegendreProof
