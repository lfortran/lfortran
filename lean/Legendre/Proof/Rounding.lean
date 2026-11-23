import Legendre.Proof.Loop

/-!
# Local rounding error of one loop iteration

For `2 ≤ k ≤ 100`, `|x| ≤ 1` and `|a|, |b| ≤ 9/8`, the computed
`step fp x k a b` differs from the exact `((2k-1) x b - (k-1) a) / k` by at
most `16 u`, where `u = 2^-53`.
-/

open LFortran Legendre

namespace LegendreProof

lemma u_pos : 0 < unitRoundoff := by unfold unitRoundoff; positivity
lemma u_small : unitRoundoff ≤ 1 / 10 ^ 15 := by unfold unitRoundoff; norm_num
lemma eta_nonneg : 0 ≤ underflowError := by unfold underflowError; positivity
lemma eta_small : underflowError ≤ unitRoundoff / 10 ^ 6 := by
  unfold underflowError unitRoundoff
  rw [div_div, one_div_le_one_div (by positivity) (by positivity)]
  calc (2 : ℝ) ^ 53 * 10 ^ 6 ≤ 2 ^ 53 * 2 ^ 1022 := by
        gcongr
        calc (10 : ℝ) ^ 6 ≤ 2 ^ 20 := by norm_num
          _ ≤ 2 ^ 1022 := pow_le_pow_right₀ (by norm_num) (by norm_num)
    _ = 2 ^ 1075 := by rw [← pow_add]

lemma big_ok {M : ℝ} (h : M ≤ 400) : M ≤ 2 ^ 1000 := by
  have : (400 : ℝ) ≤ 2 ^ 1000 :=
    calc (400 : ℝ) ≤ 2 ^ 9 := by norm_num
      _ ≤ 2 ^ 1000 := pow_le_pow_right₀ (by norm_num) (by norm_num)
  exact h.trans this

lemma rnd_decomp (fp : Float64Model) {t M : ℝ} (h : |t| ≤ M) (hM : M ≤ 400) :
    ∃ ε, fp.rnd t = t + ε ∧ |ε| ≤ unitRoundoff * M + underflowError := by
  refine ⟨fp.rnd t - t, by ring, ?_⟩
  have h1 := fp.rnd_err t (h.trans (big_ok hM))
  have h2 := mul_le_mul_of_nonneg_left h u_pos.le
  linarith

/-- Rounding error of one evaluation of the loop body, with the integer
conversions already resolved to the exact values `2K-1`, `K-1`, `K`. -/
lemma chain_err (fp : Float64Model) (x a b K : ℝ) (hK : 2 ≤ K) (hK' : K ≤ 100)
    (hx : |x| ≤ 1) (ha : |a| ≤ 9 / 8) (hb : |b| ≤ 9 / 8) :
    |fp.div (fp.sub (fp.mul (fp.mul (2 * K - 1) x) b) (fp.mul (K - 1) a)) K
      - ((2 * K - 1) * x * b - (K - 1) * a) / K| ≤ 16 * unitRoundoff := by
  have hu0 := u_pos
  have hu1 := u_small
  have hη0 := eta_nonneg
  have hη1 := eta_small
  have huK : unitRoundoff * K ≤ 1 / 10 ^ 15 * K :=
    mul_le_mul_of_nonneg_right hu1 (by linarith)
  have hηK : underflowError ≤ unitRoundoff * K / 10 ^ 6 := by
    have : unitRoundoff * 2 ≤ unitRoundoff * K := mul_le_mul_of_nonneg_left hK hu0.le
    linarith
  simp only [Float64Model.div, Float64Model.sub, Float64Model.mul]
  -- t1 = rnd((2K-1) x)
  have hx1 : |(2 * K - 1) * x| ≤ 2 * K := by
    rw [abs_mul, abs_of_pos (by linarith : (0 : ℝ) < 2 * K - 1)]
    nlinarith [abs_nonneg x]
  obtain ⟨e1, h1, he1⟩ := rnd_decomp fp hx1 (by linarith)
  rw [h1]
  have ht1 : |(2 * K - 1) * x + e1| ≤ 2001 / 1000 * K := by
    have := abs_add_le ((2 * K - 1) * x) e1
    linarith
  -- t2 = rnd(t1 b)
  have hx2 : |((2 * K - 1) * x + e1) * b| ≤ 2001 / 1000 * K * (9 / 8) := by
    rw [abs_mul]; exact mul_le_mul ht1 hb (abs_nonneg _) (by positivity)
  obtain ⟨e2, h2, he2⟩ := rnd_decomp fp hx2 (by linarith)
  rw [h2]
  -- t3 = rnd((K-1) a)
  have hx3 : |(K - 1) * a| ≤ K * (9 / 8) := by
    rw [abs_mul, abs_of_pos (by linarith : (0 : ℝ) < K - 1)]
    nlinarith [abs_nonneg a]
  obtain ⟨e3, h3, he3⟩ := rnd_decomp fp hx3 (by linarith)
  rw [h3]
  -- t4 = rnd(t2 - t3)
  have hx4 : |((2 * K - 1) * x + e1) * b + e2 - ((K - 1) * a + e3)| ≤ 34 / 10 * K := by
    have := abs_sub_le (((2 * K - 1) * x + e1) * b + e2) 0 ((K - 1) * a + e3)
    have := abs_add_le (((2 * K - 1) * x + e1) * b) e2
    have := abs_add_le ((K - 1) * a) e3
    simp only [sub_zero, zero_sub, abs_neg] at *
    linarith
  obtain ⟨e4, h4, he4⟩ := rnd_decomp fp hx4 (by linarith)
  rw [h4]
  have ht4 : |((2 * K - 1) * x + e1) * b + e2 - ((K - 1) * a + e3) + e4| ≤ 341 / 100 * K := by
    have := abs_add_le (((2 * K - 1) * x + e1) * b + e2 - ((K - 1) * a + e3)) e4
    linarith
  -- c = rnd(t4 / K)
  have hKpos : (0 : ℝ) < K := by linarith
  have hx5 : |(((2 * K - 1) * x + e1) * b + e2 - ((K - 1) * a + e3) + e4) / K| ≤ 341 / 100 := by
    rw [abs_div, abs_of_pos hKpos, div_le_iff₀ hKpos]
    exact ht4
  obtain ⟨e5, h5, he5⟩ := rnd_decomp fp hx5 (by norm_num)
  rw [h5]
  have key : (((2 * K - 1) * x + e1) * b + e2 - ((K - 1) * a + e3) + e4) / K + e5
      - ((2 * K - 1) * x * b - (K - 1) * a) / K = e5 + (e1 * b + e2 - e3 + e4) / K := by
    field_simp
    ring
  rw [key]
  have hS : |e1 * b + e2 - e3 + e4| ≤ 10 * unitRoundoff * K := by
    have h1' : |e1 * b| ≤ |e1| * (9 / 8) := by
      rw [abs_mul]; exact mul_le_mul_of_nonneg_left hb (abs_nonneg _)
    have := abs_add_le (e1 * b + e2 - e3) e4
    have := abs_sub_le (e1 * b + e2) 0 e3
    have := abs_add_le (e1 * b) e2
    simp only [sub_zero, zero_sub, abs_neg] at *
    linarith
  have hS' : |(e1 * b + e2 - e3 + e4) / K| ≤ 10 * unitRoundoff := by
    rw [abs_div, abs_of_pos hKpos, div_le_iff₀ hKpos]
    exact hS
  have := abs_add_le e5 ((e1 * b + e2 - e3 + e4) / K)
  linarith

lemma ofInt_exact (fp : Float64Model) (i : ℤ) (h0 : 0 ≤ i) (h : i ≤ 1000) :
    fp.ofInt i = i := by
  apply fp.rnd_int
  rw [abs_of_nonneg h0]
  exact h.trans (by norm_num)

lemma step_err (fp : Float64Model) (x a b : ℝ) (m : ℕ) (hm : m ≤ 98)
    (hx : |x| ≤ 1) (ha : |a| ≤ 9 / 8) (hb : |b| ≤ 9 / 8) :
    |step fp x ((m : ℤ) + 2) a b - ((2 * m + 3) * x * b - (m + 1) * a) / (m + 2)|
      ≤ 16 * unitRoundoff := by
  have hm' : (m : ℝ) ≤ 98 := by exact_mod_cast hm
  have hm0 : (0 : ℝ) ≤ m := m.cast_nonneg
  have h1 : fp.ofInt (2 * ((m : ℤ) + 2) - 1) = 2 * ((m : ℝ) + 2) - 1 := by
    rw [ofInt_exact fp _ (by omega) (by omega)]; push_cast; ring
  have h2 : fp.ofInt ((m : ℤ) + 2 - 1) = ((m : ℝ) + 2) - 1 := by
    rw [ofInt_exact fp _ (by omega) (by omega)]; push_cast; ring
  have h3 : fp.ofInt ((m : ℤ) + 2) = (m : ℝ) + 2 := by
    rw [ofInt_exact fp _ (by omega) (by omega)]; push_cast; ring
  have he : ((2 * m + 3) * x * b - (m + 1) * a) / (m + 2) =
      ((2 * ((m : ℝ) + 2) - 1) * x * b - (((m : ℝ) + 2) - 1) * a) / ((m : ℝ) + 2) := by
    ring
  unfold step
  rw [h1, h2, h3, he]
  exact chain_err fp x a b _ (by linarith) (by linarith) hx ha hb

end LegendreProof
