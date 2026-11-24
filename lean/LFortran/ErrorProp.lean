import LFortran.NoGuard
import Mathlib.Tactic.Linarith
import Mathlib.Tactic.Positivity
import Mathlib.Tactic.GCongr

/-!
# Forward error propagation in a `NoGuardModel`

`Close m U H v t α κ T` says that the computed `v` is a floating-point number
of `m`, within `α U + κ H` of the exact value `t`, and that `|t| ≤ T`. Here
`U` bounds the unit roundoff and `H` the underflow error `η` (`Small m U H`);
keeping them apart keeps the absolute underflow errors of small intermediate
values from being amplified into the relative bound. Second-order terms are
bounded using `U, H ≤ 2^-20`.

The lemmas propagate `Close` through one operation each. A proof chains
them along the generated code and simplifies each pair of coefficients
with `Close.mono`.

Every `FPModel` is a `NoGuardModel` (`FPModel.toNoGuard`), so these bounds
hold in every arithmetic with or without a guard digit.
-/

namespace LFortran

/-- The bound on `U` and `H`, used for second-order terms. -/
noncomputable def εU : ℝ := 1 / 2 ^ 20

theorem εU_pos : 0 < εU := by norm_num [εU]

/-- An arithmetic whose unit roundoff is at most `U` and whose underflow error
is at most `H`. -/
structure Small (m : NoGuardModel) (U H : ℝ) : Prop where
  U0 : 0 ≤ U
  H0 : 0 ≤ H
  Uε : U ≤ εU
  Hε : H ≤ εU
  u : m.u ≤ U
  η : m.η ≤ H

/-- `v ∈ F` approximates `t` within `α U + κ H`, and `|t| ≤ T`. -/
structure Close (m : NoGuardModel) (U H v t α κ T : ℝ) : Prop where
  mem : v ∈ m.F
  err : |v - t| ≤ α * U + κ * H
  bnd : |t| ≤ T

namespace Close

variable {m : NoGuardModel} {U H : ℝ}

theorem mono {v t α κ T α' κ' T' : ℝ} (hS : Small m U H) (h : Close m U H v t α κ T)
    (hα : α ≤ α') (hκ : κ ≤ κ') (hT : T ≤ T') : Close m U H v t α' κ' T' := by
  refine ⟨h.mem, h.err.trans ?_, h.bnd.trans hT⟩
  have := mul_le_mul_of_nonneg_right hα hS.U0
  have := mul_le_mul_of_nonneg_right hκ hS.H0
  linarith

/-- An input: a floating-point number, with no error. -/
theorem input {x T : ℝ} (hx : x ∈ m.F) (hT : |x| ≤ T) : Close m U H x x 0 0 T :=
  ⟨hx, by simp, hT⟩

/-- The error is at most `(α + κ) ε`. -/
theorem err_le {v t α κ T : ℝ} (hS : Small m U H) (hα : 0 ≤ α) (hκ : 0 ≤ κ)
    (h : Close m U H v t α κ T) : |v - t| ≤ (α + κ) * εU := by
  have := mul_le_mul_of_nonneg_left hS.Uε hα
  have := mul_le_mul_of_nonneg_left hS.Hε hκ
  linarith [h.err]

/-- `|v| ≤ T + (α + κ) ε` for the computed value. -/
theorem abs_le {v t α κ T : ℝ} (hS : Small m U H) (hα : 0 ≤ α) (hκ : 0 ≤ κ)
    (h : Close m U H v t α κ T) : |v| ≤ T + (α + κ) * εU := by
  have h1 := abs_sub_abs_le_abs_sub v t
  linarith [h.err_le hS hα hκ, h.bnd]

lemma u_mul_le (hS : Small m U H) {y Y : ℝ} (hy : |y| ≤ Y) : m.u * |y| ≤ U * Y := by
  rcases le_total 0 m.u with hu | hu
  · exact mul_le_mul hS.u hy (abs_nonneg _) hS.U0
  · nlinarith [abs_nonneg y, hS.U0, (abs_nonneg y).trans hy]

/-- A rounded literal or integer conversion `round c`. -/
theorem round {c C : ℝ} (hS : Small m U H) (hc : |c| ≤ C) (hM : C ≤ m.M) :
    Close m U H (m.round c) c C 1 C := by
  refine ⟨m.round_mem c (hc.trans hM), ?_, hc⟩
  have h := m.round_err c (hc.trans hM)
  have := u_mul_le hS hc
  linarith [hS.η]

/-- An exactly representable literal or integer conversion. -/
theorem round_exact {c C : ℝ} (hc : |c| ≤ C) (hM : C ≤ m.M) (he : m.round c = c) :
    Close m U H (m.round c) c 0 0 C :=
  ⟨m.round_mem c (hc.trans hM), by simp [he], hc⟩

theorem mul {va a vb b α κ β l A B : ℝ} (hS : Small m U H) (hα : 0 ≤ α) (hκ : 0 ≤ κ)
    (hβ : 0 ≤ β) (hl : 0 ≤ l) (ha : Close m U H va a α κ A) (hb : Close m U H vb b β l B)
    (hM : (A + (α + κ) * εU) * (B + (β + l) * εU) ≤ m.M) :
    Close m U H (m.mul va vb) (a * b)
      (α * (B + (β + l) * εU) + A * β + (A + (α + κ) * εU) * (B + (β + l) * εU))
      (κ * (B + (β + l) * εU) + A * l + 1) (A * B) := by
  have hva := ha.abs_le hS hα hκ
  have hvb := hb.abs_le hS hβ hl
  have hA : 0 ≤ A := (abs_nonneg a).trans ha.bnd
  have hB' : 0 ≤ B + (β + l) * εU := (abs_nonneg _).trans hvb
  have hprod : |va * vb| ≤ (A + (α + κ) * εU) * (B + (β + l) * εU) := by
    rw [abs_mul]; exact mul_le_mul hva hvb (abs_nonneg _) ((abs_nonneg _).trans hva)
  refine ⟨m.mul_mem va ha.mem vb hb.mem (hprod.trans hM), ?_, ?_⟩
  · have h1 := m.mul_err va ha.mem vb hb.mem (hprod.trans hM)
    have h2 := u_mul_le hS hprod
    have h3 : |va * vb - a * b| ≤ (α * U + κ * H) * (B + (β + l) * εU) + A * (β * U + l * H) := by
      have e : va * vb - a * b = (va - a) * vb + a * (vb - b) := by ring
      rw [e]
      calc |(va - a) * vb + a * (vb - b)| ≤ |va - a| * |vb| + |a| * |vb - b| := by
            rw [← abs_mul, ← abs_mul]; exact abs_add_le _ _
        _ ≤ (α * U + κ * H) * (B + (β + l) * εU) + A * (β * U + l * H) :=
            add_le_add (mul_le_mul ha.err hvb (abs_nonneg _)
                (add_nonneg (mul_nonneg hα hS.U0) (mul_nonneg hκ hS.H0)))
              (mul_le_mul ha.bnd hb.err (abs_nonneg _) hA)
    have h4 := abs_sub_le (m.mul va vb) (va * vb) (a * b)
    have := hS.η
    nlinarith
  · rw [abs_mul]; exact mul_le_mul ha.bnd hb.bnd (abs_nonneg _) hA

theorem add {va a vb b α κ β l A B : ℝ} (hS : Small m U H) (hα : 0 ≤ α) (hκ : 0 ≤ κ)
    (hβ : 0 ≤ β) (hl : 0 ≤ l) (ha : Close m U H va a α κ A) (hb : Close m U H vb b β l B)
    (hM : (A + (α + κ) * εU) + (B + (β + l) * εU) ≤ m.M) :
    Close m U H (m.add va vb) (a + b)
      (α + β + (A + (α + κ) * εU) + (B + (β + l) * εU)) (κ + l + 1) (A + B) := by
  have hva := ha.abs_le hS hα hκ
  have hvb := hb.abs_le hS hβ hl
  have hsum : |va + vb| ≤ (A + (α + κ) * εU) + (B + (β + l) * εU) :=
    (abs_add_le _ _).trans (add_le_add hva hvb)
  refine ⟨m.add_mem va ha.mem vb hb.mem (hsum.trans hM), ?_, ?_⟩
  · have h1 := m.add_err va ha.mem vb hb.mem (hsum.trans hM)
    have h2 := u_mul_le hS (y := |va| + |vb|) (Y := (A + (α + κ) * εU) + (B + (β + l) * εU))
      (by rw [abs_of_nonneg (by positivity)]; exact add_le_add hva hvb)
    rw [abs_of_nonneg (by positivity : (0 : ℝ) ≤ |va| + |vb|)] at h2
    have h3 : |va + vb - (a + b)| ≤ (α * U + κ * H) + (β * U + l * H) := by
      have e : va + vb - (a + b) = (va - a) + (vb - b) := by ring
      rw [e]; exact (abs_add_le _ _).trans (add_le_add ha.err hb.err)
    have h4 := abs_sub_le (m.add va vb) (va + vb) (a + b)
    nlinarith [hS.η]
  · exact (abs_add_le _ _).trans (add_le_add ha.bnd hb.bnd)

theorem sub {va a vb b α κ β l A B : ℝ} (hS : Small m U H) (hα : 0 ≤ α) (hκ : 0 ≤ κ)
    (hβ : 0 ≤ β) (hl : 0 ≤ l) (ha : Close m U H va a α κ A) (hb : Close m U H vb b β l B)
    (hM : (A + (α + κ) * εU) + (B + (β + l) * εU) ≤ m.M) :
    Close m U H (m.sub va vb) (a - b)
      (α + β + (A + (α + κ) * εU) + (B + (β + l) * εU)) (κ + l + 1) (A + B) := by
  have hva := ha.abs_le hS hα hκ
  have hvb := hb.abs_le hS hβ hl
  have hdiff : |va - vb| ≤ (A + (α + κ) * εU) + (B + (β + l) * εU) :=
    (abs_sub _ _).trans (add_le_add hva hvb)
  refine ⟨m.sub_mem va ha.mem vb hb.mem (hdiff.trans hM), ?_, ?_⟩
  · have h1 := m.sub_err va ha.mem vb hb.mem (hdiff.trans hM)
    have h2 := u_mul_le hS (y := |va| + |vb|) (Y := (A + (α + κ) * εU) + (B + (β + l) * εU))
      (by rw [abs_of_nonneg (by positivity)]; exact add_le_add hva hvb)
    rw [abs_of_nonneg (by positivity : (0 : ℝ) ≤ |va| + |vb|)] at h2
    have h3 : |va - vb - (a - b)| ≤ (α * U + κ * H) + (β * U + l * H) := by
      have e : va - vb - (a - b) = (va - a) - (vb - b) := by ring
      rw [e]; exact (abs_sub _ _).trans (add_le_add ha.err hb.err)
    have h4 := abs_sub_le (m.sub va vb) (va - vb) (a - b)
    nlinarith [hS.η]
  · exact (abs_sub _ _).trans (add_le_add ha.bnd hb.bnd)

/-- The last multiplication by an input `x`: the error stays relative to
`|x|`, plus the underflow error `η ≤ H` of the product. -/
theorem mul_input {x vb b β l B : ℝ} (hS : Small m U H) (hβ : 0 ≤ β) (hl : 0 ≤ l)
    (hx : x ∈ m.F) (hb : Close m U H vb b β l B) (hM : |x| * (B + (β + l) * εU) ≤ m.M) :
    m.mul x vb ∈ m.F ∧
      |m.mul x vb - x * b| ≤ |x| * ((β + B + (β + l) * εU) * U + l * H) + H := by
  have hvb := hb.abs_le hS hβ hl
  have hprod : |x * vb| ≤ |x| * (B + (β + l) * εU) := by
    rw [abs_mul]; exact mul_le_mul_of_nonneg_left hvb (abs_nonneg _)
  refine ⟨m.mul_mem x hx vb hb.mem (hprod.trans hM), ?_⟩
  have h1 := m.mul_err x hx vb hb.mem (hprod.trans hM)
  have h2 := u_mul_le hS hprod
  have h3 : |x * vb - x * b| ≤ |x| * (β * U + l * H) := by
    rw [← mul_sub, abs_mul]; exact mul_le_mul_of_nonneg_left hb.err (abs_nonneg _)
  have h4 := abs_sub_le (m.mul x vb) (x * vb) (x * b)
  have := hS.η
  nlinarith

end Close

end LFortran
