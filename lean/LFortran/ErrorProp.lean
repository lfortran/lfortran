import LFortran.VModel
import Mathlib.Tactic.Linarith
import Mathlib.Tactic.Positivity
import Mathlib.Tactic.GCongr

/-!
# Forward error propagation in a `VModel`

`Close V U H v t α κ T` says that the computed `v` (an element of the
carrier: a real number in a proof, a `Float` in the run) has a value within
`α U + κ H` of the exact value `t`, and that `|t| ≤ T`. Here `U` bounds the
unit roundoff and `H` the underflow error `η` (`Small V U H`); keeping them
apart keeps the absolute underflow errors of small intermediate values from
being amplified into the relative bound. Second-order terms are bounded using
`U, H ≤ 2^-20`.

The lemmas propagate `Close` through one operation of the generated code
each (`FReal.lit`, `FReal.ofInt`, `FReal.mul`, `FReal.add`, `FReal.sub`). A
proof chains them along the code and simplifies each pair of coefficients
with `Close.mono`. The result holds in every `VModel`: every `NoGuardModel`
(so every `FPModel`) on `ℝ`, and Lean's `Float`.
-/

namespace LFortran

/-- The bound on `U` and `H`, used for second-order terms. -/
noncomputable def εU : ℝ := 1 / 2 ^ 20

theorem εU_pos : 0 < εU := by norm_num [εU]

variable {ρ : Type} [FReal ρ]

/-- An arithmetic whose unit roundoff is at most `U` and whose underflow error
is at most `H`. -/
structure Small (V : VModel ρ) (U H : ℝ) : Prop where
  U0 : 0 ≤ U
  H0 : 0 ≤ H
  Uε : U ≤ εU
  Hε : H ≤ εU
  u : V.u ≤ U
  η : V.η ≤ H

/-- `v` has a value within `α U + κ H` of `t`, and `|t| ≤ T`. -/
def Close (V : VModel ρ) (U H : ℝ) (v : ρ) (t α κ T : ℝ) : Prop :=
  ∃ w, V.val v w ∧ |w - t| ≤ α * U + κ * H ∧ |t| ≤ T

namespace Close

variable {V : VModel ρ} {U H : ℝ}

theorem mono {v : ρ} {t α κ T α' κ' T' : ℝ} (hS : Small V U H) (h : Close V U H v t α κ T)
    (hα : α ≤ α') (hκ : κ ≤ κ') (hT : T ≤ T') : Close V U H v t α' κ' T' := by
  obtain ⟨w, hw, he, hb⟩ := h
  refine ⟨w, hw, he.trans ?_, hb.trans hT⟩
  have := mul_le_mul_of_nonneg_right hα hS.U0
  have := mul_le_mul_of_nonneg_right hκ hS.H0
  linarith

/-- An input: a number with value `x`, with no error. -/
theorem input {xv : ρ} {x T : ℝ} (hx : V.val xv x) (hT : |x| ≤ T) : Close V U H xv x 0 0 T :=
  ⟨x, hx, by simp, hT⟩

/-- A value within `α U + κ H` of `t` is at most `T + (α + κ) ε`. -/
theorem abs_le' (hS : Small V U H) {w t α κ T : ℝ} (hα : 0 ≤ α) (hκ : 0 ≤ κ)
    (he : |w - t| ≤ α * U + κ * H) (ht : |t| ≤ T) : |w| ≤ T + (α + κ) * εU := by
  have := mul_le_mul_of_nonneg_left hS.Uε hα
  have := mul_le_mul_of_nonneg_left hS.Hε hκ
  have h1 := abs_sub_abs_le_abs_sub w t
  linarith

lemma u_mul_le (hS : Small V U H) {y Y : ℝ} (hy : |y| ≤ Y) : V.u * |y| ≤ U * Y := by
  rcases le_total 0 V.u with hu | hu
  · exact mul_le_mul hS.u hy (abs_nonneg _) hS.U0
  · nlinarith [abs_nonneg y, hS.U0, (abs_nonneg y).trans hy]

/-- A literal `m · 2^e`. -/
theorem lit (hS : Small V U H) {m e : ℤ} (hok : LitOk m e) {C : ℝ}
    (hc : |(m : ℝ) * (2 : ℝ) ^ e| ≤ C) (hM : C ≤ V.M) :
    Close V U H (FReal.lit m e) ((m : ℝ) * (2 : ℝ) ^ e) C 1 C := by
  obtain ⟨w, hw, he⟩ := V.lit_err m e hok (hc.trans hM)
  refine ⟨w, hw, ?_, hc⟩
  have := u_mul_le hS hc
  linarith [hS.η]

/-- An integer conversion `real(n)`. -/
theorem ofInt (hS : Small V U H) {n : ℤ} (hn : |n| < 2 ^ 53) {C : ℝ} (hc : |(n : ℝ)| ≤ C)
    (hM : C ≤ V.M) : Close V U H (FReal.ofInt n) n C 1 C := by
  obtain ⟨w, hw, he⟩ := V.ofInt_err n hn (hc.trans hM)
  refine ⟨w, hw, ?_, hc⟩
  have := u_mul_le hS hc
  linarith [hS.η]

theorem mul {va vb : ρ} {a b α κ β l A B : ℝ} (hS : Small V U H) (hα : 0 ≤ α) (hκ : 0 ≤ κ)
    (hβ : 0 ≤ β) (hl : 0 ≤ l) (ha : Close V U H va a α κ A) (hb : Close V U H vb b β l B)
    (hM : (A + (α + κ) * εU) * (B + (β + l) * εU) ≤ V.M) :
    Close V U H (FReal.mul va vb) (a * b)
      (α * (B + (β + l) * εU) + A * β + (A + (α + κ) * εU) * (B + (β + l) * εU))
      (κ * (B + (β + l) * εU) + A * l + 1) (A * B) := by
  obtain ⟨wa, hwa, hea, hA'⟩ := ha
  obtain ⟨wb, hwb, heb, hB'⟩ := hb
  have hva := abs_le' hS hα hκ hea hA'
  have hvb := abs_le' hS hβ hl heb hB'
  have hA : 0 ≤ A := (abs_nonneg a).trans hA'
  have hprod : |wa * wb| ≤ (A + (α + κ) * εU) * (B + (β + l) * εU) := by
    rw [abs_mul]; exact mul_le_mul hva hvb (abs_nonneg _) ((abs_nonneg _).trans hva)
  obtain ⟨v, hv, h1⟩ := V.mul_err va wa vb wb hwa hwb (hprod.trans hM)
  refine ⟨v, hv, ?_, ?_⟩
  · have h2 := u_mul_le hS hprod
    have h3 : |wa * wb - a * b| ≤ (α * U + κ * H) * (B + (β + l) * εU) + A * (β * U + l * H) := by
      have e : wa * wb - a * b = (wa - a) * wb + a * (wb - b) := by ring
      rw [e]
      calc |(wa - a) * wb + a * (wb - b)| ≤ |wa - a| * |wb| + |a| * |wb - b| := by
            rw [← abs_mul, ← abs_mul]; exact abs_add_le _ _
        _ ≤ (α * U + κ * H) * (B + (β + l) * εU) + A * (β * U + l * H) :=
            add_le_add (mul_le_mul hea hvb (abs_nonneg _)
                (add_nonneg (mul_nonneg hα hS.U0) (mul_nonneg hκ hS.H0)))
              (mul_le_mul hA' heb (abs_nonneg _) hA)
    have h4 := abs_sub_le v (wa * wb) (a * b)
    have := hS.η
    nlinarith
  · rw [abs_mul]; exact mul_le_mul hA' hB' (abs_nonneg _) hA

theorem add {va vb : ρ} {a b α κ β l A B : ℝ} (hS : Small V U H) (hα : 0 ≤ α) (hκ : 0 ≤ κ)
    (hβ : 0 ≤ β) (hl : 0 ≤ l) (ha : Close V U H va a α κ A) (hb : Close V U H vb b β l B)
    (hM : (A + (α + κ) * εU) + (B + (β + l) * εU) ≤ V.M) :
    Close V U H (FReal.add va vb) (a + b)
      (α + β + (A + (α + κ) * εU) + (B + (β + l) * εU)) (κ + l + 1) (A + B) := by
  obtain ⟨wa, hwa, hea, hA'⟩ := ha
  obtain ⟨wb, hwb, heb, hB'⟩ := hb
  have hva := abs_le' hS hα hκ hea hA'
  have hvb := abs_le' hS hβ hl heb hB'
  have hsum : |wa + wb| ≤ (A + (α + κ) * εU) + (B + (β + l) * εU) :=
    (abs_add_le _ _).trans (add_le_add hva hvb)
  obtain ⟨v, hv, h1⟩ := V.add_err va wa vb wb hwa hwb (hsum.trans hM)
  refine ⟨v, hv, ?_, (abs_add_le _ _).trans (add_le_add hA' hB')⟩
  have h2 := u_mul_le hS (y := |wa| + |wb|) (Y := (A + (α + κ) * εU) + (B + (β + l) * εU))
    (by rw [abs_of_nonneg (by positivity)]; exact add_le_add hva hvb)
  rw [abs_of_nonneg (by positivity : (0 : ℝ) ≤ |wa| + |wb|)] at h2
  have h3 : |wa + wb - (a + b)| ≤ (α * U + κ * H) + (β * U + l * H) := by
    have e : wa + wb - (a + b) = (wa - a) + (wb - b) := by ring
    rw [e]; exact (abs_add_le _ _).trans (add_le_add hea heb)
  have h4 := abs_sub_le v (wa + wb) (a + b)
  nlinarith [hS.η]

theorem sub {va vb : ρ} {a b α κ β l A B : ℝ} (hS : Small V U H) (hα : 0 ≤ α) (hκ : 0 ≤ κ)
    (hβ : 0 ≤ β) (hl : 0 ≤ l) (ha : Close V U H va a α κ A) (hb : Close V U H vb b β l B)
    (hM : (A + (α + κ) * εU) + (B + (β + l) * εU) ≤ V.M) :
    Close V U H (FReal.sub va vb) (a - b)
      (α + β + (A + (α + κ) * εU) + (B + (β + l) * εU)) (κ + l + 1) (A + B) := by
  obtain ⟨wa, hwa, hea, hA'⟩ := ha
  obtain ⟨wb, hwb, heb, hB'⟩ := hb
  have hva := abs_le' hS hα hκ hea hA'
  have hvb := abs_le' hS hβ hl heb hB'
  have hdiff : |wa - wb| ≤ (A + (α + κ) * εU) + (B + (β + l) * εU) :=
    (abs_sub _ _).trans (add_le_add hva hvb)
  obtain ⟨v, hv, h1⟩ := V.sub_err va wa vb wb hwa hwb (hdiff.trans hM)
  refine ⟨v, hv, ?_, (abs_sub _ _).trans (add_le_add hA' hB')⟩
  have h2 := u_mul_le hS (y := |wa| + |wb|) (Y := (A + (α + κ) * εU) + (B + (β + l) * εU))
    (by rw [abs_of_nonneg (by positivity)]; exact add_le_add hva hvb)
  rw [abs_of_nonneg (by positivity : (0 : ℝ) ≤ |wa| + |wb|)] at h2
  have h3 : |wa - wb - (a - b)| ≤ (α * U + κ * H) + (β * U + l * H) := by
    have e : wa - wb - (a - b) = (wa - a) - (wb - b) := by ring
    rw [e]; exact (abs_sub _ _).trans (add_le_add hea heb)
  have h4 := abs_sub_le v (wa - wb) (a - b)
  nlinarith [hS.η]

/-- The last multiplication by an input `x`: the error stays relative to
`|x|`, plus the underflow error `η ≤ H` of the product. -/
theorem mul_input {xv vb : ρ} {x b β l B : ℝ} (hS : Small V U H) (hβ : 0 ≤ β) (hl : 0 ≤ l)
    (hx : V.val xv x) (hb : Close V U H vb b β l B) (hM : |x| * (B + (β + l) * εU) ≤ V.M) :
    ∃ w, V.val (FReal.mul xv vb) w ∧
      |w - x * b| ≤ |x| * ((β + B + (β + l) * εU) * U + l * H) + H := by
  obtain ⟨wb, hwb, heb, hB'⟩ := hb
  have hvb := abs_le' hS hβ hl heb hB'
  have hprod : |x * wb| ≤ |x| * (B + (β + l) * εU) := by
    rw [abs_mul]; exact mul_le_mul_of_nonneg_left hvb (abs_nonneg _)
  obtain ⟨v, hv, h1⟩ := V.mul_err xv x vb wb hx hwb (hprod.trans hM)
  refine ⟨v, hv, ?_⟩
  have h2 := u_mul_le hS hprod
  have h3 : |x * wb - x * b| ≤ |x| * (β * U + l * H) := by
    rw [← mul_sub, abs_mul]; exact mul_le_mul_of_nonneg_left heb (abs_nonneg _)
  have h4 := abs_sub_le v (x * wb) (x * b)
  have := hS.η
  nlinarith

end Close

end LFortran
