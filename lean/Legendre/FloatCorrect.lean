import Legendre.Proof
import LFortran.FloatModel

set_option exponentiation.threshold 2048

/-!
# The `Float` run of the same `legendre_poly`

`Legendre/Run.lean` executes the generated template
`legendre_mod.legendre_poly` at `LFortran.machine` (`Int32` and Lean's `Float`),
exactly as the `main` emitted by `lfortran --backend=lean` does. This file
proves, without axioms beyond Lean's standard ones, that this run has the
accuracy proved for every `FPModel`:

* `floatModel` (`LFortran/FloatModel.lean`) is an `FPModel` whose operations
  are `Float`'s own `+ - * /` (`u = 2^-53`, `η = 2^-1075`, `M = 2^1022`);
* `legendre_float_sim`: the `Float` run has a finite value, and that value is
  exactly the generated template in `floatModel`, `legendre_fortran floatModel`.
  Each step is a `Float` operation whose exact result is far below `2^1022`
  (the computed values stay below `1 + 10^-6` by the accuracy invariant),
  so no step overflows; the `Int32` loop indices and conversions are exact
  for `n ≤ 100`;
* `legendre_float_correct`: hence `|run - legendreP n x| ≤ 10^-11`, against
  Mathlib's Legendre polynomial.

The trusted step is Lean's: the compiled `Float` operations implement the
logical model `Float.Model`.
-/

open LFortran Legendre LegendreProof

namespace LegendreFloat

/-- The loop body at `Int32` and `Float`, as `Run.lean` executes it. -/
def stepF (x : Float) (k : Int32) (a b : Float) : Float :=
  FReal.div
    (FReal.sub
      (FReal.mul
        (FReal.mul (FReal.ofInt (FInt.toInt (FInt.sub (FInt.mul (FInt.ofInt 2) k) (FInt.ofInt 1))))
          x) b)
      (FReal.mul (FReal.ofInt (FInt.toInt (FInt.sub k (FInt.ofInt 1)))) a))
    (FReal.ofInt (FInt.toInt k))

/-- The values of `b` computed by the `Float` loop. -/
def yF (x : Float) : ℕ → Float
  | 0 => FReal.ofInt 1
  | 1 => x
  | m + 2 => stepF x (Int32.ofInt ((m : ℤ) + 2)) (yF x m) (yF x (m + 1))

lemma toInt_small (k : ℤ) (h0 : -2 ^ 31 ≤ k) (h1 : k < 2 ^ 31) :
    FInt.toInt (FInt.ofInt k : Int32) = k :=
  Int32.toInt_ofInt_of_le h0 h1

/-- For `n ≤ 100` the `Int32` loop runs over `2, …, n` without overflow. -/
lemma range32 (m : ℕ) (hm : m ≤ 99) :
    FInt.range (FInt.ofInt 2 : Int32) (Int32.ofInt ((m : ℤ) + 1)) =
      (intRange 2 ((m : ℤ) + 1)).map Int32.ofInt := by
  have h2 : FInt.toInt (FInt.ofInt 2 : Int32) = 2 := toInt_small 2 (by norm_num) (by norm_num)
  have hn : FInt.toInt (Int32.ofInt ((m : ℤ) + 1)) = (m : ℤ) + 1 :=
    toInt_small _ (by omega) (by omega)
  simp only [FInt.range, intRange, h2, hn, List.map_map]
  congr 1
  funext i
  show (Int32.ofInt 2 + Int32.ofInt (i : ℤ)) = Int32.ofInt (2 + (i : ℤ))
  rw [Int32.ofInt_add]

lemma fold_eqF (x : Float) (m : ℕ) :
    ∃ c, List.foldl (fun (s : Float × Float × Float) (k : Int32) =>
        (s.2.1, stepF x k s.1 s.2.1, stepF x k s.1 s.2.1)) (FReal.ofInt 1, x, FReal.zero)
        ((intRange 2 ((m : ℤ) + 1)).map Int32.ofInt) = (yF x m, yF x (m + 1), c) := by
  induction m with
  | zero => exact ⟨FReal.zero, rfl⟩
  | succ m ih =>
    obtain ⟨c, hc⟩ := ih
    refine ⟨yF x (m + 2), ?_⟩
    rw [show ((m + 1 : ℕ) : ℤ) + 1 = ((m : ℤ) + 1) + 1 by push_cast; ring,
      intRange_succ _ _ (by omega), List.map_append, List.foldl_append, hc]
    simp only [List.map_cons, List.map_nil, List.foldl_cons, List.foldl_nil]
    have : yF x (m + 2) = stepF x (Int32.ofInt ((m : ℤ) + 2)) (yF x m) (yF x (m + 1)) := rfl
    rw [this, add_assoc, one_add_one_eq_two]

lemma beq32 (a b : Int32) : FInt.beq a b = (a == b) := rfl

lemma legendre_float_zero (x : Float) :
    legendre_mod.legendre_poly (K := machine) (Int32.ofInt 0) x = FReal.ofInt 1 := by
  simp [legendre_mod.legendre_poly, beq32]
  rfl

lemma legendre_float_succ (x : Float) (m : ℕ) (hm : m ≤ 99) :
    legendre_mod.legendre_poly (K := machine) (Int32.ofInt ((m : ℤ) + 1)) x = yF x (m + 1) := by
  have hne' : Int32.ofInt ((m : ℤ) + 1) ≠ FInt.ofInt 0 := by
    intro h
    have := congrArg Int32.toInt h
    rw [Int32.toInt_ofInt_of_le (by omega) (by omega)] at this
    have h0 : (FInt.ofInt 0 : Int32).toInt = 0 := by decide
    omega
  have hne : FInt.beq (Int32.ofInt ((m : ℤ) + 1)) (FInt.ofInt 0) = false := by
    rw [beq32]; exact beq_eq_false_iff_ne.2 hne'
  obtain ⟨c, hc⟩ := fold_eqF x m
  simp only [stepF] at hc
  simp [legendre_mod.legendre_poly, hne, range32 m hm, hc, -Int32.ofInt_add]

/-! ## One step of `Float` is one step of `floatModel` -/

/-- The arithmetic of the proof, for `floatModel`. -/
noncomputable abbrev fm : NoGuardModel := floatModel.toNoGuard

lemma fm_admissible : Admissible fm (1 / 2 ^ 53) := by
  refine ⟨by positivity, by norm_num, max_le le_rfl (by positivity), ?_, ?_⟩
  · show (1 : ℝ) / 2 ^ 1075 ≤ 1 / 2 ^ 53 / 10 ^ 6
    rw [div_div, one_div_le_one_div (by positivity) (by positivity)]
    calc (2 : ℝ) ^ 53 * 10 ^ 6 ≤ 2 ^ 53 * 2 ^ 20 := by gcongr; norm_num
      _ ≤ 2 ^ 1075 := by rw [← pow_add]; exact pow_le_pow_right₀ (by norm_num) (by norm_num)
  · show (2 : ℝ) ^ 10 ≤ 2 ^ 1022
    exact pow_le_pow_right₀ (by norm_num) (by norm_num)

/-- A result within the `FPModel` error of `t`, `|t| ≤ B`, is at most `2B + 1`. -/
lemma abs_le_of_err {v t B : ℝ} (h : |v - t| ≤ (1 / 2 ^ 53) * |t| + 1 / 2 ^ 1075)
    (ht : |t| ≤ B) : |v| ≤ 2 * B + 1 := by
  have h1 : (1 : ℝ) / 2 ^ 53 ≤ 1 := by norm_num
  have h2 : (1 : ℝ) / 2 ^ 1075 ≤ 1 := by norm_num
  have h3 : (1 / 2 ^ 53) * |t| ≤ |t| :=
    mul_le_of_le_one_left (abs_nonneg _) h1
  have := abs_sub_abs_le_abs_sub v t
  linarith

lemma le_M' {t : ℝ} (h : |t| ≤ 1000) : |t| ≤ 2 ^ 1022 := by
  have : (1000 : ℝ) ≤ 2 ^ 1022 :=
    le_trans (by norm_num : (1000 : ℝ) ≤ 2 ^ 10) (pow_le_pow_right₀ (by norm_num) (by norm_num))
  linarith

lemma floatValue_ofInt' (n : ℕ) (hn : 0 < n) (h53 : n < 2 ^ 53) :
    floatValue (FReal.ofInt (n : ℤ) : Float) = some (n : ℝ) :=
  floatValue_ofInt n hn h53

/-- One loop iteration in `Float` has a finite value, equal to the iteration
in `floatModel`. -/
lemma step_sim (k : ℕ) (hk2 : 2 ≤ k) (hk : k ≤ 100) {xf af bf : Float} {x a b : ℝ}
    (hxv : floatValue xf = some x) (hav : floatValue af = some a) (hbv : floatValue bf = some b)
    (hx1 : |x| ≤ 1) (ha1 : |a| ≤ 2) (hb1 : |b| ≤ 2) :
    floatValue (stepF xf (Int32.ofInt (k : ℤ)) af bf) = some (step fm x (k : ℤ) a b) := by
  have hK1 : FInt.toInt (FInt.sub (FInt.mul (FInt.ofInt 2) (Int32.ofInt (k : ℤ))) (FInt.ofInt 1))
      = (((2 * k - 1 : ℕ)) : ℤ) := by
    show Int32.toInt (Int32.ofInt 2 * Int32.ofInt (k : ℤ) - Int32.ofInt 1) = _
    rw [← Int32.ofInt_mul, ← Int32.ofInt_sub, Int32.toInt_ofInt_of_le (by omega) (by omega)]
    omega
  have hK2 : FInt.toInt (FInt.sub (Int32.ofInt (k : ℤ)) (FInt.ofInt 1)) = (((k - 1 : ℕ)) : ℤ) := by
    show Int32.toInt (Int32.ofInt (k : ℤ) - Int32.ofInt 1) = _
    rw [← Int32.ofInt_sub, Int32.toInt_ofInt_of_le (by omega) (by omega)]
    omega
  have hK3 : FInt.toInt (Int32.ofInt (k : ℤ)) = (k : ℤ) :=
    Int32.toInt_ofInt_of_le (by omega) (by omega)
  have hk' : (k : ℝ) ≤ 100 := by exact_mod_cast hk
  have hk2' : (2 : ℝ) ≤ k := by exact_mod_cast hk2
  -- the three integer conversions are exact, in `Float` and in `floatModel`
  have v1 := floatValue_ofInt' (2 * k - 1) (by omega) (by omega)
  have v2 := floatValue_ofInt' (k - 1) (by omega) (by omega)
  have v3 := floatValue_ofInt' k (by omega) (by omega)
  have r1 : fm.round (2 * ((k : ℤ) : ℝ) - 1) = ((2 * k - 1 : ℕ) : ℝ) := by
    have := floatModel_round_int (2 * k - 1) (by omega)
    rw [show (2 * ((k : ℤ) : ℝ) - 1) = ((2 * k - 1 : ℕ) : ℝ) by push_cast [Nat.cast_sub (by omega : 1 ≤ 2 * k)]; ring]
    exact this
  have r2 : fm.round (((k : ℤ) : ℝ) - 1) = ((k - 1 : ℕ) : ℝ) := by
    have := floatModel_round_int (k - 1) (by omega)
    rw [show (((k : ℤ) : ℝ) - 1) = ((k - 1 : ℕ) : ℝ) by push_cast [Nat.cast_sub (by omega : 1 ≤ k)]; ring]
    exact this
  have r3 : fm.round ((k : ℤ) : ℝ) = (k : ℝ) := by
    have := floatModel_round_int k (by omega)
    push_cast
    exact this
  have hn1 : ((2 * k - 1 : ℕ) : ℝ) = 2 * k - 1 := by
    push_cast [Nat.cast_sub (by omega : 1 ≤ 2 * k)]; ring
  have hn2 : ((k - 1 : ℕ) : ℝ) = k - 1 := by
    push_cast [Nat.cast_sub (by omega : 1 ≤ k)]; ring
  -- t1 = (2k-1) * x
  have b1 : |((2 * k - 1 : ℕ) : ℝ) * x| ≤ 200 := by
    rw [abs_mul, hn1, abs_of_pos (by linarith)]
    nlinarith [abs_nonneg x]
  obtain ⟨w1, hw1, -, he1⟩ := float_mul_value v1 hxv (le_M' (by linarith))
  have hw1' : |w1| ≤ 401 := by have := abs_le_of_err he1 b1; linarith
  -- t2 = t1 * b
  have b2 : |w1 * b| ≤ 802 := by
    rw [abs_mul]; nlinarith [abs_nonneg w1, abs_nonneg b]
  obtain ⟨w2, hw2, -, he2⟩ := float_mul_value hw1 hbv (le_M' (by linarith))
  have hw2' : |w2| ≤ 1605 := by have := abs_le_of_err he2 b2; linarith
  -- t3 = (k-1) * a
  have b3 : |((k - 1 : ℕ) : ℝ) * a| ≤ 200 := by
    rw [abs_mul, hn2, abs_of_pos (by linarith)]
    nlinarith [abs_nonneg a]
  obtain ⟨w3, hw3, -, he3⟩ := float_mul_value v2 hav (le_M' (by linarith))
  have hw3' : |w3| ≤ 401 := by have := abs_le_of_err he3 b3; linarith
  -- t4 = t2 - t3
  have b4 : |w2 - w3| ≤ 2006 := by have := abs_sub w2 w3; linarith
  have b4' : |w2 - w3| ≤ 2 ^ 1022 :=
    le_trans b4 (le_trans (by norm_num : (2006 : ℝ) ≤ 2 ^ 11)
      (pow_le_pow_right₀ (by norm_num) (by norm_num)))
  obtain ⟨w4, hw4, -, he4⟩ := float_sub_value hw2 hw3 b4'
  have hw4' : |w4| ≤ 4013 := by have := abs_le_of_err he4 b4; linarith
  have hk0 : (k : ℝ) ≠ 0 := by positivity
  have b5 : |w4 / (k : ℝ)| ≤ 2 ^ 1022 := by
    rw [abs_div, abs_of_pos (by positivity : (0 : ℝ) < k)]
    have : |w4| / k ≤ |w4| := div_le_self (abs_nonneg _) (by linarith)
    have : (4013 : ℝ) ≤ 2 ^ 1022 :=
      le_trans (by norm_num : (4013 : ℝ) ≤ 2 ^ 12) (pow_le_pow_right₀ (by norm_num) (by norm_num))
    linarith
  obtain ⟨w5, hw5, -, -⟩ := float_div_value hw4 v3 hk0 b5
  have e1 := floatModel_mul_eq v1 hxv hw1
  have e2 := floatModel_mul_eq hw1 hbv hw2
  have e3 := floatModel_mul_eq v2 hav hw3
  have e4 := floatModel_sub_eq hw2 hw3 hw4
  have e5 := floatModel_div_eq hk0 hw4 v3 hw5
  have hstep : step fm x (k : ℤ) a b = w5 := by
    unfold step
    rw [r1, r2, r3]
    show floatModel.div (floatModel.sub (floatModel.mul (floatModel.mul _ x) b)
      (floatModel.mul _ a)) _ = w5
    rw [e1, e2, e3, e4, e5]
  unfold stepF
  rw [hK1, hK2, hK3, hstep]
  exact hw5

lemma yF_zero (xf : Float) : floatValue (yF xf 0) = some 1 := by
  have := floatValue_ofInt' 1 (by norm_num) (by norm_num)
  show floatValue (FReal.ofInt 1 : Float) = some 1
  simpa using this

lemma y_zero (x : ℝ) : y fm x 0 = 1 := by
  have := floatModel_round_int 1 (by norm_num)
  show floatModel.round 1 = 1
  simpa using this

/-- Induction over the loop: the `Float` values are the `floatModel` values. -/
theorem yF_sim (xf : Float) {x : ℝ} (hxv : floatValue xf = some x) (hx1 : |x| ≤ 1) :
    ∀ m : ℕ, m ≤ 99 → floatValue (yF xf m) = some (y fm x m) ∧
      floatValue (yF xf (m + 1)) = some (y fm x (m + 1))
  | 0, _ => ⟨by rw [yF_zero, y_zero], hxv⟩
  | m + 1, hm => by
    obtain ⟨h0, h1⟩ := yF_sim xf hxv hx1 m (by omega)
    refine ⟨h1, ?_⟩
    have hxF : x ∈ fm.F := float_mem hxv
    have hInv := Inv_all fm fm_admissible hxF hx1 m (by omega)
    have ha := y_abs_le fm fm_admissible hx1 (m := m) (by omega) hInv.2.2.2.2
    have hb := y_abs_le fm fm_admissible hx1 (m := m) (by omega) hInv.2.2.2.1
    have hs := step_sim (m + 2) (by omega) (by omega) hxv h0 h1 hx1
      (by norm_num at ha ⊢; linarith) (by norm_num at hb ⊢; linarith)
    push_cast at hs
    exact hs

end LegendreFloat

open LegendreFloat

/-- **The `Float` run is the `floatModel` program.** For `n ≤ 100` and a
finite `Float` `x` with `|x| ≤ 1`, the value `Run.lean` computes is finite and
equals the shared polynomial evaluated in `floatModel`. -/
theorem legendre_float_sim (n : ℕ) (hn : n ≤ 100) (xf : Float) {x : ℝ}
    (hxv : floatValue xf = some x) (hx1 : |x| ≤ 1) :
    floatValue (legendre_mod.legendre_poly (K := machine) (Int32.ofInt (n : ℤ)) xf) =
      some (legendre_fortran floatModel n x) := by
  cases n with
  | zero =>
    rw [Nat.cast_zero, legendre_float_zero]
    have h : legendre_fortran floatModel 0 x = legendre_ng fm 0 x := rfl
    have hr : fm.round 1 = 1 := y_zero x
    rw [h, legendre_zero, hr]
    exact yF_zero xf
  | succ m =>
    rw [Nat.cast_succ, legendre_float_succ xf m (by omega)]
    have h : legendre_fortran floatModel ((m : ℤ) + 1) x = legendre_ng fm ((m : ℤ) + 1) x := rfl
    rw [h, legendre_succ]
    exact (yF_sim xf hxv hx1 m (by omega)).2

/-- **Accuracy of the `Float` run.** The value computed by `Run.lean`'s
`legendre_poly` at `Int32` and `Float` is within `10^-11` of Mathlib's
Legendre polynomial `legendreP n x`. -/
theorem legendre_float_correct (n : ℕ) (hn : n ≤ 100) (xf : Float) {x : ℝ}
    (hxv : floatValue xf = some x) (hx1 : |x| ≤ 1) :
    ∃ v, floatValue (legendre_mod.legendre_poly (K := machine) (Int32.ofInt (n : ℤ)) xf) = some v ∧
      |v - legendreP n x| ≤ 1 / 10 ^ 11 := by
  refine ⟨_, legendre_float_sim n hn xf hxv hx1, ?_⟩
  have hη : floatModel.η ≤ 1 / 2 ^ 1000 :=
    one_div_le_one_div_of_le (by positivity) (pow_le_pow_right₀ (by norm_num) (by norm_num))
  have hM : (2 : ℝ) ^ 10 ≤ floatModel.M := pow_le_pow_right₀ (by norm_num) (by norm_num)
  have := legendre_correct floatModel le_rfl hη hM n x (float_mem hxv) (by omega)
    (by exact_mod_cast hn) hx1
  simpa using this

#print axioms legendre_float_correct
