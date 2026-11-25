import Horner.Proof.Loop
import Horner.Spec
import LFortranProof.ErrorProp
import LFortranProof.Arith.Model
import Mathlib.Algebra.BigOperators.Intervals
import Mathlib.Tactic

/-!
# Correctness of `horner`

`horner_inv` (`Horner/Proof/Loop.lean`) reduces the generated code to the
loop `p₀ = c_{n-1}`, `p_{t+1} = p_t x + c_{n-2-t}`; `hval c x n t` is this
loop in exact arithmetic.

* **Exact arithmetic** (`horner_exact`): the generated template at
  `FPModel.exact` computes `Σ_{k<n} c_k x^k` for every `n ≥ 1`.
* **Rounding** (`horner_rounding`), in every `VModel` (no guard digit needed)
  with unit roundoff `≤ U`, underflow error `≤ H`, `n U, n H ≤ 1/100` and
  exact `integer(4)`: the computed value is within
  `3(n-1) U Σ |c_k| |x|^k + 3(n-1) H Σ_{k<n-1} |x|^k` of `Σ c_k x^k`. The
  invariant after `t` iterations is an error of at most
  `3t U a_t + 3t H b_t`, where `a_t` is the loop on `|c|`, `|x|` and
  `b_t = Σ_{j<t} |x|^j` (`step_err`).
-/

open LFortran Horner Finset

namespace HornerProof

/-- Horner's loop in exact arithmetic after `t` iterations. -/
noncomputable def hval (c : ℕ → ℝ) (x : ℝ) (n : ℕ) : ℕ → ℝ
  | 0 => c (n - 1)
  | t + 1 => hval c x n t * x + c (n - 2 - t)

/-- `Σ_{j<t} y^j`, the weight of the underflow errors. -/
noncomputable def hgeo (y : ℝ) : ℕ → ℝ
  | 0 => 0
  | t + 1 => hgeo y t * y + 1

theorem hval_eq (c : ℕ → ℝ) (x : ℝ) (n : ℕ) :
    ∀ t, t + 1 ≤ n → hval c x n t = ∑ j ∈ range (t + 1), c (n - 1 - t + j) * x ^ j
  | 0, _ => by simp [hval]
  | t + 1, h => by
    rw [hval, hval_eq c x n t (by omega), sum_range_succ' _ (t + 1), sum_mul]
    congr 1
    · apply sum_congr rfl
      intro j _
      rw [show n - 1 - (t + 1) + (j + 1) = n - 1 - t + j by omega]
      ring
    · simp only [add_zero, pow_zero, mul_one]
      congr 1
      omega

theorem hval_poly (c : ℕ → ℝ) (x : ℝ) {n : ℕ} (hn : 1 ≤ n) : hval c x n (n - 1) = poly c x n := by
  rw [hval_eq c x n (n - 1) (by omega), show n - 1 + 1 = n by omega, poly]
  apply sum_congr rfl
  intro j _
  congr 2
  omega

theorem hgeo_eq (y : ℝ) : ∀ t, hgeo y t = ∑ j ∈ range t, y ^ j
  | 0 => by simp [hgeo]
  | t + 1 => by
    rw [hgeo, hgeo_eq y t, sum_range_succ', sum_mul]
    simp only [pow_succ, pow_zero]

theorem hgeo_nonneg {y : ℝ} (hy : 0 ≤ y) : ∀ t, 0 ≤ hgeo y t
  | 0 => le_rfl
  | t + 1 => by rw [hgeo]; have := hgeo_nonneg hy t; positivity

theorem hval_abs_nonneg (c : ℕ → ℝ) (x : ℝ) (n : ℕ) :
    ∀ t, 0 ≤ hval (fun k => |c k|) |x| n t
  | 0 => abs_nonneg _
  | t + 1 => by
    rw [hval]; have := hval_abs_nonneg c x n t; positivity

/-- The exact value is bounded by the same loop on `|c|`, `|x|`. -/
theorem abs_hval_le (c : ℕ → ℝ) (x : ℝ) (n : ℕ) :
    ∀ t, |hval c x n t| ≤ hval (fun k => |c k|) |x| n t
  | 0 => le_rfl
  | t + 1 => by
    rw [hval, hval]
    have := abs_hval_le c x n t
    calc |hval c x n t * x + c (n - 2 - t)| ≤ |hval c x n t| * |x| + |c (n - 2 - t)| := by
          rw [← abs_mul]; exact abs_add_le _ _
      _ ≤ _ := by gcongr

/-- Crude bounds, for the range: `a_t ≤ (t+1) C X^t`, with `X = max 1 |x|`. -/
theorem hval_abs_le (c : ℕ → ℝ) (x : ℝ) (n : ℕ) {C : ℝ} (hC : ∀ k < n, |c k| ≤ C) :
    ∀ t, t + 1 ≤ n → hval (fun k => |c k|) |x| n t ≤ (t + 1) * C * max 1 |x| ^ t
  | 0, h => by
    show |c (n - 1)| ≤ ((0 : ℕ) + 1) * C * max 1 |x| ^ 0
    simpa using hC (n - 1) (by omega)
  | t + 1, h => by
    rw [hval]
    have ih := hval_abs_le c x n hC t (by omega)
    have hc := hC (n - 2 - t) (by omega)
    have hC0 : 0 ≤ C := (abs_nonneg _).trans hc
    have hX1 : 1 ≤ max 1 |x| := le_max_left _ _
    have hxX : |x| ≤ max 1 |x| := le_max_right _ _
    have hp : 1 ≤ max 1 |x| ^ (t + 1) := one_le_pow₀ hX1
    have h0 := hval_abs_nonneg c x n t
    have : hval (fun k => |c k|) |x| n t * |x| ≤ ((t + 1) * C) * (max 1 |x| ^ t * max 1 |x|) := by
      rw [← mul_assoc]; exact mul_le_mul ih hxX (abs_nonneg _) (by positivity)
    rw [← pow_succ] at this
    have : C ≤ C * max 1 |x| ^ (t + 1) := le_mul_of_one_le_right hC0 hp
    push_cast
    linarith

theorem hgeo_le (x : ℝ) : ∀ t, hgeo |x| t ≤ t * max 1 |x| ^ t
  | 0 => by simp [hgeo]
  | t + 1 => by
    rw [hgeo]
    have ih := hgeo_le x t
    have hX1 : 1 ≤ max 1 |x| := le_max_left _ _
    have hxX : |x| ≤ max 1 |x| := le_max_right _ _
    have h0 := hgeo_nonneg (abs_nonneg x) t
    have hp : 1 ≤ max 1 |x| ^ (t + 1) := one_le_pow₀ hX1
    have : hgeo |x| t * |x| ≤ t * (max 1 |x| ^ t * max 1 |x|) := by
      rw [← mul_assoc]; exact mul_le_mul ih hxX (abs_nonneg _) (by positivity)
    rw [← pow_succ] at this
    push_cast
    linarith

/-! ### Exact arithmetic -/

/-- **Exact arithmetic.** With infinitely precise operations the generated
`horner` computes `Σ_{k<n} c_k x^k` for every `n ≥ 1`. -/
theorem horner_exact (n : ℕ) (hn1 : 1 ≤ n) (cv : Arr ℝ) (c : ℕ → ℝ)
    (hc : ∀ k < n, ∃ h : k < cv.data.size, cv.data[k] = c k) (x : ℝ) :
    @horner_mod.horner proofKinds (FKinds.ofModel FPModel.exact FPModel.exact) (n : ℤ) cv x =
      poly c x n := by
  rw [← hval_poly c x hn1]
  refine @horner_inv proofKinds (FKinds.ofModel FPModel.exact FPModel.exact) ((n : ℤ) + 1)
    (intExact_int _) n hn1 (by omega) cv x (fun t p => p = hval c x n t) ?_ ?_
  · obtain ⟨h, e⟩ := hc (n - 1) (by omega)
    show cv.data[n - 1]! = c (n - 1)
    rw [getElem!_pos cv.data (n - 1) h, e]
  · intro t p ht hp
    obtain ⟨h, e⟩ := hc (n - 2 - t) (by omega)
    show p * x + cv.data[n - 2 - t]! = hval c x n t * x + c (n - 2 - t)
    rw [getElem!_pos cv.data (n - 2 - t) h, e, hp]

#print axioms horner_exact

/-! ### Rounding error -/

section Rounding

variable {ρ : Type} [FReal ρ] {V : VModel ρ} {U H : ℝ}

/-- One iteration `p ← p x + d`: an error `D = 3t U a + 3t H b` grows to
`3(t+1) U (a |x| + |d|) + 3(t+1) H (b |x| + 1)`. -/
theorem step_err (hS : Small V U H) (t : ℕ) (htU : (t : ℝ) * U ≤ 1 / 100)
    {pv dv xv : ρ} {q d x E A B : ℝ} (hp : V.val pv q) (hd : V.val dv d) (hx : V.val xv x)
    (hA0 : 0 ≤ A) (hB0 : 0 ≤ B) (hE : |E| ≤ A)
    (hq : |q - E| ≤ 3 * t * U * A + 3 * t * H * B)
    (hM1 : (A + (3 * t * U * A + 3 * t * H * B)) * |x| ≤ V.M)
    (hM2 : (A + (3 * t * U * A + 3 * t * H * B)) * |x| * (1 + U) + H + |d| ≤ V.M) :
    ∃ q', V.val (FReal.add (FReal.mul pv xv) dv) q' ∧
      |q' - (E * x + d)| ≤ 3 * (t + 1) * U * (A * |x| + |d|) + 3 * (t + 1) * H * (B * |x| + 1) := by
  have hU0 := hS.U0
  have hH0 := hS.H0
  have hUε : U ≤ 1 / 2 ^ 20 := hS.Uε
  set D := 3 * t * U * A + 3 * t * H * B with hD
  have hD0 : 0 ≤ D := by positivity
  have hqa : |q| ≤ A + D := by
    have := abs_sub_abs_le_abs_sub q E; linarith
  have hqx : |q * x| ≤ (A + D) * |x| := by
    rw [abs_mul]; exact mul_le_mul_of_nonneg_right hqa (abs_nonneg _)
  obtain ⟨m, hm, hme⟩ := V.mul_err pv q xv x hp hx (hqx.trans hM1)
  have hme' : |m - q * x| ≤ U * ((A + D) * |x|) + H := by
    have := Close.u_mul_le hS hqx; linarith [hS.η]
  have hma : |m| ≤ (A + D) * |x| * (1 + U) + H := by
    have := abs_sub_abs_le_abs_sub m (q * x); nlinarith
  have hsum : |m + d| ≤ V.M := ((abs_add_le _ _).trans (by linarith)).trans hM2
  obtain ⟨s, hs, hse⟩ := V.add_err _ m _ d hm hd hsum
  have hse' : |s - (m + d)| ≤ U * (|m| + |d|) + H := by
    have := Close.u_mul_le hS (y := |m| + |d|) (Y := |m| + |d|)
      (by rw [abs_of_nonneg (by positivity)])
    rw [abs_of_nonneg (by positivity : (0 : ℝ) ≤ |m| + |d|)] at this
    linarith [hS.η]
  refine ⟨s, hs, ?_⟩
  have e : s - (E * x + d) = (s - (m + d)) + (m - q * x) + (q - E) * x := by ring
  have hqEx : |(q - E) * x| ≤ D * |x| := by
    rw [abs_mul]; exact mul_le_mul_of_nonneg_right hq (abs_nonneg _)
  have htot : |s - (E * x + d)| ≤ U * (|m| + |d|) + H + (U * ((A + D) * |x|) + H) + D * |x| := by
    rw [e]
    have := abs_add_le ((s - (m + d)) + (m - q * x)) ((q - E) * x)
    have := abs_add_le (s - (m + d)) (m - q * x)
    linarith
  -- collect terms
  set a := A * |x| with ha
  set bx := B * |x| with hbx
  have ha0 : 0 ≤ a := by positivity
  have hbx0 : 0 ≤ bx := by positivity
  have hDx : D * |x| = 3 * t * U * a + 3 * t * H * bx := by rw [hD, ha, hbx]; ring
  have hADx : (A + D) * |x| = a + (3 * t * U * a + 3 * t * H * bx) := by rw [add_mul, hDx]
  rw [hADx] at hma htot
  rw [hDx] at htot
  have hct : (t : ℝ) * U ≤ 1 / 100 := htU
  have hU1 : U ≤ 1 / 100 := by linarith [show (1 : ℝ) / 2 ^ 20 ≤ 1 / 100 by norm_num]
  have ht0 : (0 : ℝ) ≤ t := t.cast_nonneg
  -- U |m| ≤ U (1 + U) (a + 3tUa + 3tH bx) + U H
  have hUm : U * |m| ≤ U * ((a + (3 * t * U * a + 3 * t * H * bx)) * (1 + U) + H) :=
    mul_le_mul_of_nonneg_left hma hU0
  -- the coefficient of `a`: U (2 + U) (1 + 3tU) ≤ 3U
  have hp1 : (t : ℝ) * U * U ≤ 1 / 100 * (1 / 100) := mul_le_mul hct hU1 hU0 (by norm_num)
  have h1 : (1 + U) * (1 + 3 * t * U) + (1 + 3 * t * U) ≤ 3 := by linarith
  have hA : U * ((1 + U) * (1 + 3 * t * U) + (1 + 3 * t * U)) * a ≤ 3 * U * a :=
    mul_le_mul_of_nonneg_right (by have := mul_le_mul_of_nonneg_left h1 hU0; linarith) ha0
  -- the coefficient of `bx`: 3t H U (2 + U) ≤ 3H
  have h2 : (t : ℝ) * (U * (1 + U) + U) ≤ 1 := by linarith
  have hB : 3 * H * ((t : ℝ) * (U * (1 + U) + U)) * bx ≤ 3 * H * bx :=
    mul_le_mul_of_nonneg_right (by have := mul_le_mul_of_nonneg_left h2 hH0; linarith) hbx0
  have hUH : U * H ≤ H := by
    have := mul_le_mul_of_nonneg_right hU1 hH0; linarith
  have htH : 0 ≤ (t : ℝ) * H := mul_nonneg ht0 hH0
  have hUd : U * |d| ≤ 3 * (t + 1) * U * |d| := by
    have : 0 ≤ (t : ℝ) * U * |d| := by positivity
    have : 0 ≤ U * |d| := by positivity
    linarith
  linarith

end Rounding

/-- **Rounding.** In every `VModel` with unit roundoff `≤ U` and underflow
error `≤ H`, `n U, n H ≤ 1/100`, and exact `integer(4)`, for `1 ≤ n < 2^31`
coefficients with values `c_k`, `|c_k| ≤ C`, and an input with value `x`, the
generated `horner` has a value within
`3(n-1) U Σ |c_k| |x|^k + 3(n-1) H Σ_{k<n-1} |x|^k` of `Σ c_k x^k`, provided
`4 n (C + 1) max(1, |x|)^n ≤ M`. -/
theorem horner_rounding {K : Kinds} [FKinds K] (V : VModel K.R8) (I : IntExact K.I4 (2 ^ 31))
    {U H : ℝ} (hS : Small V U H) (n : ℕ) (hn1 : 1 ≤ n) (hn : (n : ℤ) < 2 ^ 31)
    (hnU : (n : ℝ) * U ≤ 1 / 100) (hnH : (n : ℝ) * H ≤ 1 / 100) (cv : Arr K.R8) (c : ℕ → ℝ)
    (hc : ∀ k < n, ∃ h : k < cv.data.size, V.val cv.data[k] (c k)) {C : ℝ}
    (hC : ∀ k < n, |c k| ≤ C) {xv : K.R8} {x : ℝ} (hx : V.val xv x)
    (hM : 4 * n * (C + 1) * max 1 |x| ^ n ≤ V.M) :
    ∃ w, V.val (horner_mod.horner (K := K) (FInt.ofInt n) cv xv) w ∧
      |w - poly c x n| ≤ 3 * (n - 1) * U * poly (fun k => |c k|) |x| n +
        3 * (n - 1) * H * ∑ k ∈ range (n - 1), |x| ^ k := by
  have hU0 := hS.U0
  have hH0 := hS.H0
  have hC0 : 0 ≤ C := (abs_nonneg _).trans (hC 0 hn1)
  set X := max 1 |x| with hXdef
  have hX1 : 1 ≤ X := le_max_left _ _
  have hxX : |x| ≤ X := le_max_right _ _
  have hn' : (1 : ℝ) ≤ n := by exact_mod_cast hn1
  have hW1 : 1 ≤ (n : ℝ) * (C + 1) * X ^ n := by
    have := one_le_pow₀ (n := n) hX1
    have : (1 : ℝ) ≤ n * (C + 1) := by nlinarith
    nlinarith
  have hval_of : ∀ k < n, V.val cv.data[k]! (c k) := fun k hk => by
    obtain ⟨h, hv⟩ := hc k hk
    rw [getElem!_pos cv.data k h]; exact hv
  have key := horner_inv I n hn1 hn cv xv
    (fun t p => ∃ q, V.val p q ∧ |q - hval c x n t| ≤
      3 * t * U * hval (fun k => |c k|) |x| n t + 3 * t * H * hgeo |x| t) ?_ ?_
  · obtain ⟨w, hw, he⟩ := key
    refine ⟨w, hw, ?_⟩
    rw [hval_poly c x hn1, hgeo_eq, show (((n - 1 : ℕ)) : ℝ) = (n : ℝ) - 1 by
      rw [Nat.cast_sub hn1]; simp] at he
    rwa [hval_poly (fun k => |c k|) |x| hn1] at he
  · exact ⟨c (n - 1), hval_of (n - 1) (by omega), by simp [hval]⟩
  · intro t p ht ⟨q, hq, he⟩
    have htn : (t : ℝ) + 1 ≤ n := by
      have : t + 1 ≤ n := by omega
      exact_mod_cast this
    have htU : (t : ℝ) * U ≤ 1 / 100 := by nlinarith
    have htH : (t : ℝ) * H ≤ 1 / 100 := by nlinarith
    set A := hval (fun k => |c k|) |x| n t with hAdef
    set B := hgeo |x| t with hBdef
    have hA0 : 0 ≤ A := hval_abs_nonneg c x n t
    have hB0 : 0 ≤ B := hgeo_nonneg (abs_nonneg x) t
    have hAle : A ≤ (t + 1) * C * X ^ t := hval_abs_le c x n hC t (by omega)
    have hBle : B ≤ t * X ^ t := hgeo_le x t
    have hd := hC (n - 2 - t) (by omega)
    -- the range: everything is below `4 n (C + 1) X^n`
    have hXt : X ^ (t + 1) ≤ X ^ n := pow_le_pow_right₀ hX1 (by omega)
    have hDle : 3 * t * U * A + 3 * t * H * B ≤ 3 / 100 * A + 3 / 100 * B := by
      have := mul_le_mul_of_nonneg_right htU hA0
      have := mul_le_mul_of_nonneg_right htH hB0
      nlinarith
    have hABX : (A + B) * |x| ≤ n * (C + 1) * X ^ n := by
      have h1 : (A + B) * |x| ≤ ((t + 1) * C * X ^ t + t * X ^ t) * X :=
        mul_le_mul (by linarith) hxX (abs_nonneg _) (by positivity)
      have h2 : ((t + 1) * C * X ^ t + t * X ^ t) * X ≤ n * (C + 1) * X ^ (t + 1) := by
        rw [pow_succ]
        have hXt0 : 0 ≤ X ^ t * X := by positivity
        have : ((t : ℝ) + 1) * C + t ≤ n * (C + 1) := by nlinarith
        nlinarith
      have h3 : (n : ℝ) * (C + 1) * X ^ (t + 1) ≤ n * (C + 1) * X ^ n :=
        mul_le_mul_of_nonneg_left hXt (by positivity)
      linarith
    have hAD : (A + (3 * t * U * A + 3 * t * H * B)) * |x| ≤ 103 / 100 * (n * (C + 1) * X ^ n) := by
      have : (A + (3 * t * U * A + 3 * t * H * B)) * |x| ≤ (103 / 100 * (A + B)) * |x| :=
        mul_le_mul_of_nonneg_right (by linarith) (abs_nonneg _)
      linarith
    have hU1 : U ≤ 1 / 100 := by linarith [hS.Uε, show εU ≤ 1 / 100 by norm_num [εU]]
    have hH1 : H ≤ 1 / 100 := by linarith [hS.Hε, show εU ≤ 1 / 100 by norm_num [εU]]
    have hCW : C ≤ n * (C + 1) * X ^ n := by
      have h1 : C + 1 ≤ n * (C + 1) := le_mul_of_one_le_left (by positivity) hn'
      have h2 : (n : ℝ) * (C + 1) ≤ n * (C + 1) * X ^ n :=
        le_mul_of_one_le_right (by positivity) (one_le_pow₀ hX1)
      linarith
    have hADU : (A + (3 * t * U * A + 3 * t * H * B)) * |x| * U ≤
        103 / 100 * (n * (C + 1) * X ^ n) * (1 / 100) :=
      mul_le_mul hAD hU1 hU0 (by positivity)
    obtain ⟨q', hq', he'⟩ := step_err hS t htU hq (hval_of (n - 2 - t) (by omega)) hx hA0 hB0
      (abs_hval_le c x n t) he (by linarith) (by linarith)
    refine ⟨q', hq', ?_⟩
    simp only [hval, hgeo]
    push_cast
    linarith

#print axioms horner_rounding

end HornerProof
