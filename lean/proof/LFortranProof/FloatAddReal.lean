import LFortranProof.FloatAdd
import Init.Data.Float.Model.Unpacked.Operations.Add

set_option exponentiation.threshold 2048

/-!
# Addition of finite binary64 values, up to Lean's `Float`

`UnpackedFloat.add` aligns by a left shift, adds the integer significands, and
rounds with `normalize`. The left shift is exact, so the real sum of two
finite values — integer or not — is an integer significand times a power of
two.

`represents_round_err` is the rounding step: once the significand sits at its
target exponent, with the discarded bits stored in `Accuracy`, replacing it by
the round-to-nearest integer costs at most half a unit in the last place.
`halfUlp_le_model` turns that half-binade into the `FPModel` addition bound
`2^-53 |x| + 2^-1075`, for every magnitude, normal or subnormal.

The chain `round_value` → `normalize_value` → `add_value` lifts the bound to
`UnpackedFloat.add` for every pair of finite or zero operands, of either sign.
The result is also canonical (`Canon`), and a binary64 number when the exact
sum is at most `2^1022`. `canon_unpack` and `unpack_pack_finite` show that
`unpack` only produces canonical floats and that `pack` then `unpack` returns
them unchanged, so the bound transfers to `Float.Model.add` and, because
`Float.add a b` is `.ofModel (a.toModel + b.toModel)`, to Lean's `Float`:

* `float_add_value`: for finite `a b : Float` whose exact sum `x + y` has
  `|x + y| ≤ 2^1022`, `a + b` is finite, lies in `binary64`, and
  `|fl(x + y) - (x + y)| ≤ 2^-53 |x + y| + 2^-1075`.

These are the `add_mem` and `add_err` fields of `binary64Model`. Only the
standard axioms are used. The compiled `Float.add` is the C operator; that it
agrees with this logical model is Lean's trusted assumption, not ours.
-/

open LFortran
open Float.Model
open Float.Model.UnpackedFloat hiding round add ofInt

namespace LFortran

/-- Real value of a finite significand. -/
noncomputable def bitValue (s : Sign) (m : Nat) (e : Int) : ℝ :=
  (s.apply (m : ℤ) : ℝ) * (2 : ℝ) ^ e

theorem apply_shift (s : Sign) (m k : Nat) :
    (s.apply ((m <<< k : Nat) : ℤ) : ℝ) = (s.apply (m : ℤ) : ℝ) * (2 : ℝ) ^ (k : ℤ) := by
  cases s <;> simp [Sign.apply, Nat.shiftLeft_eq, zpow_natCast, Int.cast_neg,
    Int.cast_mul, Int.cast_pow, Int.cast_ofNat, Int.cast_natCast]

/-- Left-aligning a significand does not change its value. -/
theorem decreaseExponent_bitValue (s : Sign) (m : Nat) (e target : Int) :
    bitValue s (decreaseExponent m e target).1 (decreaseExponent m e target).2 =
      bitValue s m e := by
  by_cases h : e ≤ target
  · simp [decrease_le m e target h, bitValue]
  · have hshift : 0 ≤ e - target := by omega
    have hd := decrease_to m (e - target).toNat e target
      (by rw [Int.toNat_of_nonneg hshift])
    simp only [hd, bitValue]
    rw [apply_shift]
    have hexp : ((e - target).toNat : ℤ) = e - target := Int.toNat_of_nonneg hshift
    rw [hexp]
    have hpow : (2 : ℝ) ^ (e - target) * (2 : ℝ) ^ target = (2 : ℝ) ^ e := by
      rw [← zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0)]
      congr 1
      omega
    calc (s.apply (m : ℤ) : ℝ) * (2 : ℝ) ^ (e - target) * (2 : ℝ) ^ target
        = (s.apply (m : ℤ) : ℝ) * ((2 : ℝ) ^ (e - target) * (2 : ℝ) ^ target) := by ring
      _ = (s.apply (m : ℤ) : ℝ) * (2 : ℝ) ^ e := by rw [hpow]

/-- Where `δ` sits in the bin of width one, as recorded by `Accuracy`. -/
def Matches (acc : Accuracy) (δ : ℝ) : Prop :=
  0 ≤ δ ∧ δ < 1 ∧
    ((acc = .exact ∧ δ = 0) ∨
      (acc = .inexact .lt ∧ 0 < δ ∧ δ < 1 / 2) ∨
      (acc = .inexact .eq ∧ δ = 1 / 2) ∨
      (acc = .inexact .gt ∧ 1 / 2 < δ))

/-- `em` at exponent `e` is the real `x`. -/
def Represents (em : ExtendedMantissa) (e : Int) (x : ℝ) : Prop :=
  ∃ δ : ℝ, Matches em.accuracy δ ∧ x = ((em.mantissa : ℝ) + δ) * (2 : ℝ) ^ e

instance : DecidableEq Accuracy := fun a b => by
  cases a with
  | exact =>
    cases b with
    | exact => exact isTrue rfl
    | inexact _ => exact isFalse (by intro h; cases h)
  | inexact o1 =>
    cases b with
    | exact => exact isFalse (by intro h; cases h)
    | inexact o2 =>
      cases o1 <;> cases o2 <;>
        first | exact isTrue rfl | exact isFalse (by intro h; cases h)

theorem accuracy_exact_iff (m : Nat) (r s : Bool) :
    (⟨m, r, s⟩ : ExtendedMantissa).accuracy = .exact ↔ r = false ∧ s = false := by
  cases r <;> cases s <;> simp [ExtendedMantissa.accuracy]

theorem shiftRightOne_accuracy (m : Nat) (r s : Bool) :
    (⟨m, r, s⟩ : ExtendedMantissa).shiftRightOne.accuracy =
      if m % 2 = 0 then
        if r || s then .inexact .lt else .exact
      else
        if r || s then .inexact .gt else .inexact .eq := by
  simp only [ExtendedMantissa.shiftRightOne, ExtendedMantissa.accuracy]
  by_cases h2 : m % 2 = 0
  · cases r <;> cases s <;> simp [h2]
  · have hone : m % 2 = 1 := by omega
    cases r <;> cases s <;> simp [hone]

/-- One right shift keeps the real value and updates the discarded-bit record. -/
theorem matches_shift {acc : Accuracy} {δ : ℝ} (h : Matches acc δ) {b : Nat}
    (hb : b = 0 ∨ b = 1) :
    Matches (if b = 0 then (if acc = .exact then .exact else .inexact .lt)
        else (if acc = .exact then .inexact .eq else .inexact .gt))
      (((b : ℝ) + δ) / 2) := by
  rcases h with ⟨h0, h1, hacc | hacc | hacc | hacc⟩
  · rcases hacc with ⟨rfl, rfl⟩
    rcases hb with rfl | rfl
    · simp [Matches]
    · simp [Matches]
      norm_num
  · rcases hacc with ⟨rfl, hlo, hhi⟩
    rcases hb with rfl | rfl
    · refine ⟨by linarith, by linarith, Or.inr <| Or.inl ⟨rfl, by linarith, by linarith⟩⟩
    · refine ⟨by linarith, by linarith, Or.inr <| Or.inr <| Or.inr ⟨rfl, by linarith⟩⟩
  · rcases hacc with ⟨rfl, rfl⟩
    rcases hb with rfl | rfl
    · refine ⟨by norm_num, by norm_num, Or.inr <| Or.inl ⟨rfl, by norm_num, by norm_num⟩⟩
    · refine ⟨by norm_num, by norm_num, Or.inr <| Or.inr <| Or.inr ⟨rfl, by norm_num⟩⟩
  · rcases hacc with ⟨rfl, hhi⟩
    rcases hb with rfl | rfl
    · refine ⟨by linarith, by linarith, Or.inr <| Or.inl ⟨rfl, by linarith, by linarith⟩⟩
    · refine ⟨by linarith, by linarith, Or.inr <| Or.inr <| Or.inr ⟨rfl, by linarith⟩⟩

theorem shiftRightOne_represents {em : ExtendedMantissa} {e : Int} {x : ℝ}
    (h : Represents em e x) : Represents em.shiftRightOne (e + 1) x := by
  rcases em with ⟨m, rb, sb⟩
  rcases h with ⟨δ, hδ, hx⟩
  have hsplit : 2 * (m / 2) + m % 2 = m := Nat.div_add_mod m 2
  set q : Nat := m / 2
  set b : Nat := m % 2
  have hb : b = 0 ∨ b = 1 := by have := Nat.mod_lt m (by decide : 0 < 2); omega
  have hx' : x = ((q : ℝ) + ((b : ℝ) + δ) / 2) * (2 : ℝ) ^ (e + 1) := by
    have hpow : (2 : ℝ) ^ (e + 1) = (2 : ℝ) ^ e * 2 := by
      rw [zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0), zpow_one]
    have hm : (m : ℝ) = 2 * (q : ℝ) + b := by
      have hnat : m = 2 * q + b := by simpa [q, b] using hsplit.symm
      simp [hnat, Nat.cast_add, Nat.cast_mul]
    rw [hx, hm, hpow]
    ring
  refine ⟨((b : ℝ) + δ) / 2, ?_, hx'⟩
  have hacc := shiftRightOne_accuracy m rb sb
  have hexact : (⟨m, rb, sb⟩ : ExtendedMantissa).accuracy = .exact ↔ rb = false ∧ sb = false :=
    accuracy_exact_iff m rb sb
  have hflag : (rb || sb) = true ↔ (⟨m, rb, sb⟩ : ExtendedMantissa).accuracy ≠ .exact := by
    cases rb <;> cases sb <;> simp [hexact]
  -- The boolean step and `matches_shift` name the same accuracy.
  have hsame : (⟨m, rb, sb⟩ : ExtendedMantissa).shiftRightOne.accuracy =
      (if b = 0 then
        (if (⟨m, rb, sb⟩ : ExtendedMantissa).accuracy = .exact then .exact else .inexact .lt)
      else
        (if (⟨m, rb, sb⟩ : ExtendedMantissa).accuracy = .exact then .inexact .eq else .inexact .gt)) := by
    rw [hacc]
    by_cases h0 : m % 2 = 0
    · have hb0 : b = 0 := by simpa [b] using h0
      simp only [h0, hb0, if_true]
      by_cases he : (⟨m, rb, sb⟩ : ExtendedMantissa).accuracy = .exact
      · simp [he, hflag]
      · simp [he, hflag]
    · have hb1 : b = 1 := by omega
      simp only [h0, hb1, if_false]
      by_cases he : (⟨m, rb, sb⟩ : ExtendedMantissa).accuracy = .exact
      · simp [he, hflag]
      · simp [he, hflag]
  rw [hsame]
  exact matches_shift hδ hb

theorem repeat_represents {em : ExtendedMantissa} {e : Int} {x : ℝ} (k : Nat)
    (h : Represents em e x) :
    Represents (Nat.repeat ExtendedMantissa.shiftRightOne k em) (e + k) x := by
  induction k generalizing em e with
  | zero =>
    rw [show Nat.repeat ExtendedMantissa.shiftRightOne 0 em = em from rfl]
    simpa using h
  | succ k ih =>
    rw [show Nat.repeat ExtendedMantissa.shiftRightOne (k + 1) em =
      ExtendedMantissa.shiftRightOne (Nat.repeat ExtendedMantissa.shiftRightOne k em) from rfl]
    have h2 := shiftRightOne_represents (ih h)
    simpa [Int.add_assoc, Int.add_comm, Int.add_left_comm] using h2

/-- An exact significand represents its own value. -/
theorem represents_exact (m : Nat) (e : Int) :
    Represents ⟨m, false, false⟩ e ((m : ℝ) * (2 : ℝ) ^ e) := by
  refine ⟨0, ⟨by norm_num, by norm_num, Or.inl ⟨by simp [ExtendedMantissa.accuracy], rfl⟩⟩, by ring⟩

/-- Moving an exact significand up to a higher exponent keeps the value and
records the discarded bits in `Accuracy`. -/
theorem shiftExact_represents (m k : Nat) (e : Int) :
    Represents ((⟨m, false, false⟩ : ExtendedMantissa) >>> k) (e + k)
      ((m : ℝ) * (2 : ℝ) ^ e) := by
  simpa [HShiftRight.hShiftRight] using
    repeat_represents k (represents_exact m e)

/-- Rounding to a neighboring integer significand costs at most half a unit.
Ties may break either way. -/
theorem roundChoice_err {δ : ℝ} {acc : Accuracy} (h : Matches acc δ) (inc : ℕ)
    (hinc : inc ≤ 1)
    (hlow : acc = .exact ∨ acc = .inexact .lt → inc = 0)
    (hhigh : acc = .inexact .gt → inc = 1) :
    |(inc : ℝ) - δ| ≤ 1 / 2 := by
  rcases h with ⟨h0, h1, hacc | hacc | hacc | hacc⟩
  · rcases hacc with ⟨rfl, rfl⟩
    have : inc = 0 := hlow (Or.inl rfl)
    simp [this]
  · rcases hacc with ⟨rfl, hlo, hhi⟩
    have : inc = 0 := hlow (Or.inr rfl)
    simp only [this, Nat.cast_zero]
    rw [abs_of_nonpos (by linarith), neg_sub]
    linarith
  · rcases hacc with ⟨rfl, rfl⟩
    have h01 : inc = 0 ∨ inc = 1 := by omega
    rcases h01 with rfl | rfl <;> norm_num
  · rcases hacc with ⟨rfl, hhi⟩
    have : inc = 1 := hhigh rfl
    simp only [this, Nat.cast_one]
    rw [abs_of_nonneg (by linarith)]
    linarith

theorem roundToNearestEven_le (m : Nat) (acc : Accuracy) :
    acc.roundToNearestEven m = m ∨ acc.roundToNearestEven m = m + 1 := by
  cases acc with
  | exact => simp [Accuracy.roundToNearestEven]
  | inexact o =>
    cases o with
    | lt => simp [Accuracy.roundToNearestEven]
    | gt => simp [Accuracy.roundToNearestEven]
    | eq =>
      simp [Accuracy.roundToNearestEven]
      have : m % 2 = 0 ∨ m % 2 = 1 := by omega
      rcases this with h | h <;> simp [h]

/-- Half a binade at the binary64 target is at most the `FPModel` addition error. -/
theorem halfUlp_le_model {m : Nat} {e t : Int} (hm : 0 < m)
    (ht : t = max ((m.log2 : Int) + 1 + e - 53) (-1074))
    {v x : ℝ} (hx : x = (m : ℝ) * (2 : ℝ) ^ e) (hv : |v - x| ≤ (2 : ℝ) ^ (t - 1))
    (hxabs : |x| = x) :
    |v - x| ≤ (1 / 2 ^ 53) * |x| + 1 / 2 ^ 1075 := by
  have hpow : (2 : ℝ) ^ (m.log2 : ℤ) ≤ m := by
    exact_mod_cast Nat.log2_self_le (by omega)
  rcases max_cases ((m.log2 : Int) + 1 + e - 53) (-1074) with ⟨hmx, -⟩ | ⟨hmx, -⟩
  · rw [ht, hmx] at hv
    have hbase : (2 : ℝ) ^ ((m.log2 : ℤ) + e) ≤ x := by
      rw [hx, zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0)]
      exact mul_le_mul_of_nonneg_right hpow (zpow_nonneg (by norm_num) _)
    have hhalf : (2 : ℝ) ^ (((m.log2 : ℤ) + 1 + e - 53) - 1) ≤ (1 / 2 ^ 53) * x := by
      have hrew : ((m.log2 : ℤ) + 1 + e - 53) - 1 = (m.log2 : ℤ) + e - 53 := by omega
      rw [hrew, zpow_sub₀ (by norm_num : (2 : ℝ) ≠ 0), zpow_ofNat]
      calc (2 : ℝ) ^ ((m.log2 : ℤ) + e) / 2 ^ 53
          ≤ x / 2 ^ 53 := by
            exact div_le_div_of_nonneg_right hbase (by positivity)
        _ = (1 / 2 ^ 53) * x := by ring
    have habs : |x| = x := hxabs
    calc |v - x| ≤ (2 : ℝ) ^ (((m.log2 : ℤ) + 1 + e - 53) - 1) := hv
      _ ≤ (1 / 2 ^ 53) * x := hhalf
      _ ≤ (1 / 2 ^ 53) * |x| + 1 / 2 ^ 1075 := by
        rw [habs]
        exact le_add_of_nonneg_right (by positivity)
  · rw [ht, hmx] at hv
    have hsub : (2 : ℝ) ^ (((-1074 : ℤ) - 1)) = 1 / 2 ^ 1075 := by
      norm_num
    calc |v - x| ≤ (2 : ℝ) ^ ((-1074 : ℤ) - 1) := hv
      _ = 1 / 2 ^ 1075 := hsub
      _ ≤ (1 / 2 ^ 53) * |x| + 1 / 2 ^ 1075 := by
        exact le_add_of_nonneg_left (by positivity)

theorem target_eq (m : Nat) (e : Int) (_hm : 0 < m) :
    Format.binary64.targetExponent (totalExponent m e) =
      max ((m.log2 : Int) + 1 + e - 53) (-1074) := by
  simp [totalExponent, Format.targetExponent, binary64_mantissaBits, binary64_minExponent]

/-- Right shift by the distance to the target, for an exact significand. -/
theorem shiftToTarget_represents (m : Nat) (e : Int) (_hm : 0 < m) :
    let t := Format.binary64.targetExponent (totalExponent m e)
    Represents ((⟨m, false, false⟩ : ExtendedMantissa) >>> (t - e).toNat) (e + (t - e).toNat)
      ((m : ℝ) * (2 : ℝ) ^ e) :=
  shiftExact_represents m (Format.binary64.targetExponent (totalExponent m e) - e).toNat e

/-- The rounded significand is the original bin plus at most one. -/
theorem rounded_sub (em : ExtendedMantissa) :
    em.roundedMantissa = em.mantissa ∨ em.roundedMantissa = em.mantissa + 1 := by
  simpa [ExtendedMantissa.roundedMantissa] using roundToNearestEven_le em.mantissa em.accuracy

theorem div_target_lt (m k : Nat) (hm : 0 < m) (hk : m.log2 + 1 ≤ 53 + k) :
    m / 2 ^ k < 2 ^ 53 := by
  have hlt : m < 2 ^ (m.log2 + 1) := (Nat.log2_lt (by omega)).1 (by omega)
  have hm' : m < 2 ^ 53 * 2 ^ k := by
    refine lt_of_lt_of_le hlt ?_
    calc 2 ^ (m.log2 + 1) ≤ 2 ^ (53 + k) := Nat.pow_le_pow_right (by decide) hk
      _ = 2 ^ 53 * 2 ^ k := by rw [Nat.pow_add]
  exact (Nat.div_lt_iff_lt_mul (Nat.two_pow_pos k)).2 hm'

theorem roundWithAccuracy_expand (s : Sign) (m : Nat) (e : Int) :
    roundWithAccuracy Format.binary64 s m e .exact =
      (let (em, e1) := shiftToTargetExponent Format.binary64 m e .exact
       let mr := em.roundedMantissa
       let (em2, e2) := shiftToTargetExponent Format.binary64 mr e1 .exact
       if h : em2.mantissa = 0 then .zero s
       else .finite s em2.mantissa e2 (Nat.pos_of_ne_zero h)) := by
  rfl

/-- Discarding the fraction costs at most half a unit in the last place. -/
theorem represents_round_err {em : ExtendedMantissa} {e : Int} {x : ℝ}
    (h : Represents em e x) :
    |((em.roundedMantissa : ℝ) * (2 : ℝ) ^ e) - x| ≤ (2 : ℝ) ^ (e - 1) := by
  rcases h with ⟨δ, hδ, hx⟩
  rw [hx]
  have hpow : (2 : ℝ) ^ e ≥ 0 := zpow_nonneg (by norm_num) _
  rcases hδ with ⟨-, hlt, hacc | hacc | hacc | hacc⟩
  · rcases hacc with ⟨hacc, hδ⟩
    simp [ExtendedMantissa.roundedMantissa, hacc, hδ, Accuracy.roundToNearestEven, abs_zero]
    exact (zpow_pos (by norm_num) (e - 1)).le
  · rcases hacc with ⟨hacc, hlo, hhi⟩
    simp [ExtendedMantissa.roundedMantissa, hacc, Accuracy.roundToNearestEven]
    have hdist : (em.mantissa : ℝ) * (2 : ℝ) ^ e -
        ((em.mantissa : ℝ) + δ) * (2 : ℝ) ^ e = -(δ * (2 : ℝ) ^ e) := by ring
    rw [hdist, abs_neg, abs_of_nonneg (mul_nonneg (by linarith) hpow)]
    calc δ * (2 : ℝ) ^ e ≤ (1 / 2) * (2 : ℝ) ^ e :=
          mul_le_mul_of_nonneg_right (by linarith) hpow
      _ = (2 : ℝ) ^ (e - 1) := by
        rw [zpow_sub₀ (by norm_num : (2 : ℝ) ≠ 0), zpow_one]; ring
  · rcases hacc with ⟨hacc, hδ⟩
    have hround := roundToNearestEven_le em.mantissa em.accuracy
    simp [hacc] at hround
    rcases hround with h | h
    · simp [ExtendedMantissa.roundedMantissa, hacc, h, hδ]
      have hdist : (em.mantissa : ℝ) * (2 : ℝ) ^ e -
          ((em.mantissa : ℝ) + (2 : ℝ)⁻¹) * (2 : ℝ) ^ e =
          -((2 : ℝ)⁻¹ * (2 : ℝ) ^ e) := by ring
      rw [hdist, abs_neg, abs_of_nonneg (mul_nonneg (by norm_num) hpow),
        zpow_sub₀ (by norm_num : (2 : ℝ) ≠ 0), zpow_one]
      apply le_of_eq
      ring
    · simp [ExtendedMantissa.roundedMantissa, hacc, h, hδ, Nat.cast_add]
      have hdist : ((em.mantissa : ℝ) + 1) * (2 : ℝ) ^ e -
          ((em.mantissa : ℝ) + (2 : ℝ)⁻¹) * (2 : ℝ) ^ e =
          (2 : ℝ)⁻¹ * (2 : ℝ) ^ e := by ring
      rw [hdist, abs_of_nonneg (mul_nonneg (by norm_num) hpow),
        zpow_sub₀ (by norm_num : (2 : ℝ) ≠ 0), zpow_one]
      apply le_of_eq
      ring
  · rcases hacc with ⟨hacc, hhi⟩
    simp [ExtendedMantissa.roundedMantissa, hacc, Accuracy.roundToNearestEven, Nat.cast_add]
    have hdist : ((em.mantissa : ℝ) + 1) * (2 : ℝ) ^ e -
        ((em.mantissa : ℝ) + δ) * (2 : ℝ) ^ e = (1 - δ) * (2 : ℝ) ^ e := by ring
    rw [hdist, abs_of_nonneg (mul_nonneg (by linarith) hpow)]
    calc (1 - δ) * (2 : ℝ) ^ e ≤ (1 / 2) * (2 : ℝ) ^ e :=
          mul_le_mul_of_nonneg_right (by linarith) hpow
      _ = (2 : ℝ) ^ (e - 1) := by
        rw [zpow_sub₀ (by norm_num : (2 : ℝ) ≠ 0), zpow_one]; ring

/-- `±1`, as a real. -/
def sgn : Sign → ℝ
  | .positive => 1
  | .negative => -1

theorem abs_sgn (s : Sign) : |sgn s| = 1 := by
  cases s <;> simp [sgn]

theorem bitValue_eq (s : Sign) (m : Nat) (e : Int) :
    bitValue s m e = sgn s * ((m : ℝ) * (2 : ℝ) ^ e) := by
  cases s <;> simp [bitValue, sgn, Sign.apply]

/-- Real value of an unpacked float: `none` for infinities and NaN. -/
noncomputable def uvalue : UnpackedFloat → Option ℝ
  | .finite s m e _ => some (bitValue s m e)
  | .zero _ => some 0
  | _ => none

/-- An unpacked float in the form `unpack` produces and `pack` accepts:
binary64 significand, exponent in range, and normal or at the minimum
exponent. -/
def Canon : UnpackedFloat → Prop
  | .finite _ m e _ => m < 2 ^ 53 ∧ -1074 ≤ e ∧ e ≤ 971 ∧ (2 ^ 52 ≤ m ∨ e = -1074)
  | .zero _ => True
  | _ => False

theorem repeat_mantissa (k : Nat) (em : ExtendedMantissa) :
    (Nat.repeat ExtendedMantissa.shiftRightOne k em).mantissa = em.mantissa / 2 ^ k := by
  induction k with
  | zero => simp [Nat.repeat]
  | succ k ih =>
    rw [show Nat.repeat ExtendedMantissa.shiftRightOne (k + 1) em =
      ExtendedMantissa.shiftRightOne (Nat.repeat ExtendedMantissa.shiftRightOne k em) from rfl]
    simp only [ExtendedMantissa.shiftRightOne, ih]
    rw [Nat.div_div_eq_div_mul, ← Nat.pow_succ]

theorem target_ge (m : Nat) (e : Int) :
    -1074 ≤ Format.binary64.targetExponent (totalExponent m e) := by
  simp [Format.targetExponent, binary64_minExponent]

theorem renorm_cases (mr : Nat) (t : Int) (hmr : mr ≤ 2 ^ 53) (ht : -1074 ≤ t) (h0 : mr ≠ 0) :
    let k := (Format.binary64.targetExponent (totalExponent mr t) - t).toNat
    (mr = 2 ^ 53 ∧ k = 1) ∨ (mr < 2 ^ 53 ∧ k = 0) := by
  intro k
  have htgt : Format.binary64.targetExponent (totalExponent mr t) =
      max ((mr.log2 : Int) + 1 + t - 53) (-1074) := by
    simp [totalExponent, Format.targetExponent, binary64_mantissaBits, binary64_minExponent]
  by_cases hbig : mr = 2 ^ 53
  · have hlog : mr.log2 = 53 := by
      rw [hbig]; exact Nat.log2_two_pow
    left
    refine ⟨hbig, ?_⟩
    simp only [k, htgt, hlog]
    omega
  · have hlt : mr < 2 ^ 53 := lt_of_le_of_ne hmr hbig
    have hlog : mr.log2 < 53 := (Nat.log2_lt h0).2 hlt
    right
    refine ⟨hlt, ?_⟩
    simp only [k, htgt]
    omega

theorem div_target_ge (m k : Nat) (hk : 52 + k ≤ m.log2) (hm : 0 < m) : 2 ^ 52 ≤ m / 2 ^ k := by
  have hpow : 2 ^ m.log2 ≤ m := Nat.log2_self_le (by omega)
  have : 2 ^ 52 * 2 ^ k ≤ m := by
    rw [← Nat.pow_add]
    exact le_trans (Nat.pow_le_pow_right (by decide) (by omega)) hpow
  exact (Nat.le_div_iff_mul_le (Nat.two_pow_pos k)).2 this

/-- Re-normalizing a rounded significand of at most `2^53` loses nothing. -/
theorem renormalize_exact (mr : Nat) (t : Int) (hmr : mr ≤ 2 ^ 53) (ht : -1074 ≤ t) :
    let k := (Format.binary64.targetExponent (totalExponent mr t) - t).toNat
    ((mr / 2 ^ k : Nat) : ℝ) * (2 : ℝ) ^ (t + k) = (mr : ℝ) * (2 : ℝ) ^ t := by
  intro k
  have htgt : Format.binary64.targetExponent (totalExponent mr t) =
      max ((mr.log2 : Int) + 1 + t - 53) (-1074) := by
    simp [totalExponent, Format.targetExponent, binary64_mantissaBits, binary64_minExponent]
  have hdvd : 2 ^ k ∣ mr := by
    by_cases hbig : mr = 2 ^ 53
    · have hlog : mr.log2 = 53 := by
        rw [hbig]; exact Nat.log2_two_pow
      have hk : k = 1 := by
        simp only [k, htgt, hlog]
        omega
      rw [hk, hbig]
      exact Nat.pow_dvd_pow 2 (by decide)
    · have hlt : mr < 2 ^ 53 := lt_of_le_of_ne hmr hbig
      by_cases h0 : mr = 0
      · rw [h0]; exact Nat.dvd_zero _
      · have hlog : mr.log2 < 53 := (Nat.log2_lt h0).2 hlt
        have hk : k = 0 := by
          simp only [k, htgt]
          omega
        simp [hk]
  obtain ⟨q, hq⟩ := hdvd
  have hdiv : mr / 2 ^ k = q := by
    rw [hq, Nat.mul_div_cancel_left _ (Nat.two_pow_pos k)]
  rw [hdiv, hq, zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0), zpow_natCast]
  push_cast
  ring

theorem shiftRight_mantissa (em : ExtendedMantissa) (k : Nat) :
    (em >>> k).mantissa = em.mantissa / 2 ^ k := by
  simp only [HShiftRight.hShiftRight]
  exact repeat_mantissa k em

/-- `roundWithAccuracy` of an exact significand at or below its target exponent
is within half a unit in the last place of that target. -/
theorem roundWithAccuracy_value (s : Sign) (m : Nat) (e : Int) (hm : 0 < m)
    (hle : e ≤ Format.binary64.targetExponent (totalExponent m e)) :
    ∃ w : ℝ, uvalue (roundWithAccuracy Format.binary64 s m e .exact) = some (sgn s * w) ∧
      |w - (m : ℝ) * (2 : ℝ) ^ e| ≤
        (2 : ℝ) ^ (Format.binary64.targetExponent (totalExponent m e) - 1) ∧
      (∃ n : Nat, n ≤ 2 ^ 53 ∧
        w = (n : ℝ) * (2 : ℝ) ^ Format.binary64.targetExponent (totalExponent m e)) ∧
      (Format.binary64.targetExponent (totalExponent m e) ≤ 970 →
        Canon (roundWithAccuracy Format.binary64 s m e .exact)) := by
  have htgt := target_eq m e hm
  set t := Format.binary64.targetExponent (totalExponent m e) with ht
  set k := (t - e).toNat with hk
  have hke : e + (k : Int) = t := by omega
  have hs1 : shiftToTargetExponent Format.binary64 m e .exact =
      ((⟨m, false, false⟩ : ExtendedMantissa) >>> k, t) := by
    simp only [shiftToTargetExponent, shiftToExponent, ExtendedMantissa.ofMantissaAndAccuracy]
    rw [← ht, ← hk, hke]
  have hrep : Represents ((⟨m, false, false⟩ : ExtendedMantissa) >>> k) t
      ((m : ℝ) * (2 : ℝ) ^ e) := by
    have := shiftExact_represents m k e
    rwa [hke] at this
  set em1 := ((⟨m, false, false⟩ : ExtendedMantissa) >>> k) with hem1
  have hman : em1.mantissa = m / 2 ^ k := by
    rw [hem1, shiftRight_mantissa]
  have hlt : em1.mantissa < 2 ^ 53 := by
    rw [hman]
    apply div_target_lt m k hm
    omega
  have hmr : em1.roundedMantissa ≤ 2 ^ 53 := by
    rcases rounded_sub em1 with h | h <;> omega
  have herr := represents_round_err hrep
  have hren := renormalize_exact em1.roundedMantissa t hmr (by rw [ht]; exact target_ge m e)
  have hlow : t ≠ -1074 → 2 ^ 52 ≤ em1.roundedMantissa := by
    intro hne
    have hk52 : 52 + k ≤ m.log2 := by omega
    have := div_target_ge m k hk52 hm
    rcases rounded_sub em1 with h | h <;> omega
  have htlo : -1074 ≤ t := by rw [ht]; exact target_ge m e
  have hcases := renorm_cases em1.roundedMantissa t hmr htlo
  rw [roundWithAccuracy_expand, hs1]
  simp only [shiftToTargetExponent, shiftToExponent, ExtendedMantissa.ofMantissaAndAccuracy,
    shiftRight_mantissa]
  simp only [] at hren hcases
  split
  · rename_i h0
    refine ⟨0, by simp [uvalue], ?_, ⟨em1.roundedMantissa, hmr, ?_⟩, fun _ => trivial⟩
    · rw [h0, Nat.cast_zero, zero_mul] at hren
      rw [← hren] at herr
      simpa using herr
    · rw [h0, Nat.cast_zero, zero_mul] at hren
      exact hren
  · rename_i h0
    refine ⟨_, by simp only [uvalue, bitValue_eq], ?_, ⟨em1.roundedMantissa, hmr, hren⟩, ?_⟩
    · rw [hren]
      exact herr
    · intro h970
      have hmr0 : em1.roundedMantissa ≠ 0 := by
        intro hz; apply h0; rw [hz]; simp
      rcases hcases hmr0 with ⟨hbig, hk1⟩ | ⟨hlt, hk0⟩
      · simp only [Canon]
        rw [hk1, hbig]
        refine ⟨by norm_num, by push_cast; omega, by push_cast; omega, Or.inl (by norm_num)⟩
      · simp only [Canon]
        rw [hk0]
        simp only [pow_zero, Nat.div_one, Nat.cast_zero, add_zero]
        refine ⟨hlt, htlo, by omega, ?_⟩
        by_cases hte : t = -1074
        · exact Or.inr hte
        · exact Or.inl (hlow hte)

/-- Left alignment to the target keeps the value and the target exponent. -/
theorem decrease_props (m : Nat) (e : Int) (hm : 0 < m) :
    let t := Format.binary64.targetExponent (totalExponent m e)
    let d := decreaseExponent m e t
    0 < d.1 ∧ d.2 ≤ t ∧ Format.binary64.targetExponent (totalExponent d.1 d.2) = t ∧
      (d.1 : ℝ) * (2 : ℝ) ^ d.2 = (m : ℝ) * (2 : ℝ) ^ e := by
  intro t d
  have hval := decreaseExponent_bitValue .positive m e t
  simp only [bitValue, Sign.apply, Int.cast_natCast] at hval
  by_cases hle : e ≤ t
  · have hd : d = (m, e) := decrease_le m e t hle
    rw [hd]
    exact ⟨hm, hle, rfl, rfl⟩
  · have hshift : 0 ≤ e - t := by omega
    have hd : d = (m <<< (e - t).toNat, t) :=
      decrease_to m (e - t).toNat e t (by rw [Int.toNat_of_nonneg hshift])
    refine ⟨?_, ?_, ?_, ?_⟩
    · rw [hd, Nat.shiftLeft_eq]
      exact Nat.mul_pos hm (Nat.two_pow_pos _)
    · rw [hd]
    · rw [hd]
      simp only [totalExponent]
      rw [Nat.shiftLeft_eq, log2_mul_two_pow m _ hm]
      have : ((m.log2 + (e - t).toNat : Nat) : Int) + 1 + t = (m.log2 : Int) + 1 + e := by
        push_cast
        rw [Int.toNat_of_nonneg hshift]
        omega
      rw [this]
      rfl
    · exact hval

/-- `UnpackedFloat.round` meets the `FPModel` error bound for every positive
significand and every exponent. -/
theorem neg_mem_binary64 {x : ℝ} (hx : x ∈ binary64) : -x ∈ binary64 := by
  obtain ⟨m, e, hm, he1, he2, rfl⟩ := hx
  exact ⟨-m, e, by rwa [abs_neg], he1, he2, by push_cast; ring⟩

theorem sgn_mul_mem (s : Sign) {x : ℝ} (hx : x ∈ binary64) : sgn s * x ∈ binary64 := by
  cases s
  · simpa [sgn] using neg_mem_binary64 hx
  · simpa [sgn] using hx

/-- A significand of at most `2^53` at an exponent in `[-1074, 970]` is a
binary64 number (`2^53 · 2^t = 2^52 · 2^(t+1)`). -/
theorem mem_binary64_of (n : Nat) (t : Int) (hn : n ≤ 2 ^ 53) (hlo : -1074 ≤ t)
    (hhi : t ≤ 970) : (n : ℝ) * (2 : ℝ) ^ t ∈ binary64 := by
  by_cases hbig : n = 2 ^ 53
  · refine ⟨2 ^ 52, t + 1, by norm_num, by omega, by omega, ?_⟩
    rw [hbig, zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0)]
    push_cast
    ring
  · have hlt : n < 2 ^ 53 := lt_of_le_of_ne hn hbig
    refine ⟨n, t, ?_, hlo, by omega, by push_cast; rfl⟩
    rw [abs_of_nonneg (by positivity)]
    exact_mod_cast hlt

/-- Below `2^1022` the target exponent leaves room for a carry. -/
theorem target_le_970 (m : Nat) (e : Int) (hm : 0 < m)
    (hx : (m : ℝ) * (2 : ℝ) ^ e ≤ 2 ^ 1022) :
    Format.binary64.targetExponent (totalExponent m e) ≤ 970 := by
  have hpow : (2 : ℝ) ^ (m.log2 : ℤ) ≤ m := by
    exact_mod_cast Nat.log2_self_le (by omega)
  have hbase : (2 : ℝ) ^ ((m.log2 : ℤ) + e) ≤ 2 ^ (1022 : ℤ) := by
    rw [zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0)]
    calc (2 : ℝ) ^ (m.log2 : ℤ) * 2 ^ e ≤ m * 2 ^ e :=
          mul_le_mul_of_nonneg_right hpow (zpow_nonneg (by norm_num) _)
      _ ≤ 2 ^ 1022 := hx
      _ = 2 ^ (1022 : ℤ) := by norm_num
  have hle : (m.log2 : ℤ) + e ≤ 1022 :=
    (zpow_le_zpow_iff_right₀ (by norm_num : (1 : ℝ) < 2)).1 hbase
  rw [target_eq m e hm]
  omega

theorem round_eq (s : Sign) (m : Nat) (e : Int) :
    UnpackedFloat.round Format.binary64 s m e =
      roundWithAccuracy Format.binary64 s
        (decreaseExponent m e (Format.binary64.targetExponent (totalExponent m e))).1
        (decreaseExponent m e (Format.binary64.targetExponent (totalExponent m e))).2 .exact := by
  rfl

theorem round_value (s : Sign) (m : Nat) (e : Int) (hm : 0 < m) :
    ∃ w : ℝ, uvalue (UnpackedFloat.round Format.binary64 s m e) = some (sgn s * w) ∧
      |w - (m : ℝ) * (2 : ℝ) ^ e| ≤ (1 / 2 ^ 53) * |(m : ℝ) * (2 : ℝ) ^ e| + 1 / 2 ^ 1075 ∧
      ((m : ℝ) * (2 : ℝ) ^ e ≤ 2 ^ 1022 → w ∈ binary64) ∧
      ((m : ℝ) * (2 : ℝ) ^ e ≤ 2 ^ 1022 → Canon (UnpackedFloat.round Format.binary64 s m e)) := by
  obtain ⟨hpos, hle, htgt, hval⟩ := decrease_props m e hm
  rw [round_eq]
  generalize decreaseExponent m e (Format.binary64.targetExponent (totalExponent m e)) = d
    at hpos hle htgt hval ⊢
  obtain ⟨w, hw, herr, ⟨n, hn, hwn⟩, hcan⟩ := roundWithAccuracy_value s d.1 d.2 hpos (htgt ▸ hle)
  refine ⟨w, hw, ?_, ?_, fun hM => hcan (htgt ▸ target_le_970 m e hm hM)⟩
  · rw [htgt, hval] at herr
    have hx : (0 : ℝ) ≤ (m : ℝ) * (2 : ℝ) ^ e :=
      mul_nonneg (Nat.cast_nonneg m) (zpow_nonneg (by norm_num) e)
    exact halfUlp_le_model hm (target_eq m e hm) rfl herr (abs_of_nonneg hx)
  · intro hM
    rw [hwn, htgt]
    exact mem_binary64_of n _ hn (target_ge m e) (target_le_970 m e hm hM)

/-- `round` with its sign, against the signed exact value. -/
theorem round_signed (s : Sign) (m : Nat) (e : Int) (hm : 0 < m) :
    ∃ v : ℝ, uvalue (UnpackedFloat.round Format.binary64 s m e) = some v ∧
      |v - bitValue s m e| ≤ (1 / 2 ^ 53) * |bitValue s m e| + 1 / 2 ^ 1075 ∧
      (|bitValue s m e| ≤ 2 ^ 1022 → v ∈ binary64) ∧
      (|bitValue s m e| ≤ 2 ^ 1022 → Canon (UnpackedFloat.round Format.binary64 s m e)) := by
  obtain ⟨w, hw, herr, hmem, hcan⟩ := round_value s m e hm
  have hx : (0 : ℝ) ≤ (m : ℝ) * (2 : ℝ) ^ e :=
    mul_nonneg (Nat.cast_nonneg m) (zpow_nonneg (by norm_num) e)
  have habs : |bitValue s m e| = (m : ℝ) * (2 : ℝ) ^ e := by
    rw [bitValue_eq, abs_mul, abs_sgn, one_mul, abs_of_nonneg hx]
  refine ⟨sgn s * w, hw, ?_, ?_, fun hM => hcan (habs ▸ hM)⟩
  · rw [bitValue_eq, ← mul_sub, abs_mul, abs_mul, abs_sgn, one_mul, one_mul]
    exact herr
  · intro hM
    exact sgn_mul_mem s (hmem (habs ▸ hM))

/-- `normalize` of any signed significand meets the `FPModel` error bound. -/
theorem normalize_value (M e : Int) (zs : Sign) :
    ∃ v : ℝ, uvalue (normalize Format.binary64 M e zs) = some v ∧
      |v - (M : ℝ) * (2 : ℝ) ^ e| ≤
        (1 / 2 ^ 53) * |(M : ℝ) * (2 : ℝ) ^ e| + 1 / 2 ^ 1075 ∧
      (|(M : ℝ) * (2 : ℝ) ^ e| ≤ 2 ^ 1022 → v ∈ binary64) ∧
      (|(M : ℝ) * (2 : ℝ) ^ e| ≤ 2 ^ 1022 → Canon (normalize Format.binary64 M e zs)) := by
  unfold normalize
  rcases lt_trichotomy M 0 with h | h | h
  · have hc : compare M 0 = .lt := by simp [compare, compareOfLessAndEq, h]
    simp only [hc]
    obtain ⟨v, hv, herr, hmem, hcan⟩ := round_signed .negative (-M).toNat e (by omega)
    refine ⟨v, hv, ?_⟩
    have hb : bitValue .negative (-M).toNat e = (M : ℝ) * (2 : ℝ) ^ e := by
      have : (((-M).toNat : Int) : ℝ) = -(M : ℝ) := by
        rw [Int.toNat_of_nonneg (by omega)]; push_cast; ring
      simp only [bitValue, Sign.apply]
      push_cast
      rw [show ((-M).toNat : ℝ) = (((-M).toNat : Int) : ℝ) by push_cast; rfl, this]
      ring
    rw [hb] at herr hmem hcan
    exact ⟨herr, hmem, hcan⟩
  · subst h
    have hc : compare (0 : Int) 0 = .eq := by simp [compare, compareOfLessAndEq]
    simp only [hc]
    refine ⟨0, rfl, ?_, fun _ => zero_mem_binary64, fun _ => trivial⟩
    simp only [Int.cast_zero, zero_mul, sub_zero, abs_zero, mul_zero, zero_add]
    positivity
  · have hc : compare M 0 = .gt := by
      simp [compare, compareOfLessAndEq, show ¬ M < 0 by omega, show ¬ M = 0 by omega]
    simp only [hc]
    obtain ⟨v, hv, herr, hmem, hcan⟩ := round_signed .positive M.toNat e (by omega)
    refine ⟨v, hv, ?_⟩
    have hb : bitValue .positive M.toNat e = (M : ℝ) * (2 : ℝ) ^ e := by
      simp only [bitValue, Sign.apply]
      rw [Int.toNat_of_nonneg (by omega)]
    rw [hb] at herr hmem hcan
    exact ⟨herr, hmem, hcan⟩

theorem decrease_snd (m : Nat) (e t : Int) (h : t ≤ e) : (decreaseExponent m e t).2 = t := by
  simp only [decreaseExponent]
  omega

theorem add_finite_eq (s₁ s₂ : Sign) (m₁ m₂ : Nat) (e₁ e₂ : Int) (h₁ : 0 < m₁) (h₂ : 0 < m₂) :
    UnpackedFloat.add Format.binary64 (.finite s₁ m₁ e₁ h₁) (.finite s₂ m₂ e₂ h₂) =
      normalize Format.binary64
        (s₁.apply (decreaseExponent m₁ e₁ (min e₁ e₂)).1 +
          s₂.apply (decreaseExponent m₂ e₂ (min e₁ e₂)).1) (min e₁ e₂) .positive := by
  rfl

/-- **Addition.** For any two finite (or zero) unpacked binary64 operands, of
either sign and with integer or fractional values, `UnpackedFloat.add`
returns a finite (or zero) result within the `FPModel` addition error of the
exact sum. -/
theorem add_value {a b : UnpackedFloat} {x y : ℝ}
    (ha : uvalue a = some x) (hb : uvalue b = some y) :
    ∃ v : ℝ, uvalue (UnpackedFloat.add Format.binary64 a b) = some v ∧
      |v - (x + y)| ≤ (1 / 2 ^ 53) * |x + y| + 1 / 2 ^ 1075 ∧
      (x ∈ binary64 → y ∈ binary64 → |x + y| ≤ 2 ^ 1022 → v ∈ binary64) ∧
      (Canon a → Canon b → |x + y| ≤ 2 ^ 1022 → Canon (UnpackedFloat.add Format.binary64 a b)) := by
  have hnn : ∀ z : ℝ, (0 : ℝ) ≤ (1 / 2 ^ 53) * |z| + 1 / 2 ^ 1075 := fun z => by positivity
  cases a with
  | notANumber => simp [uvalue] at ha
  | infinity _ => simp [uvalue] at ha
  | zero s₁ =>
    simp only [uvalue, Option.some.injEq] at ha
    subst ha
    cases b with
    | notANumber => simp [uvalue] at hb
    | infinity _ => simp [uvalue] at hb
    | zero s₂ =>
      simp only [uvalue, Option.some.injEq] at hb
      subst hb
      refine ⟨0, ?_, by simp, fun _ _ _ => zero_mem_binary64, ?_⟩
      · simp only [UnpackedFloat.add]
        split <;> rfl
      · intro _ _ _
        simp only [UnpackedFloat.add]
        split <;> trivial
    | finite s₂ m₂ e₂ h₂ =>
      refine ⟨y, ?_, by simpa using hnn y, fun _ hy _ => hy, fun _ hc _ => hc⟩
      simpa [UnpackedFloat.add] using hb
  | finite s₁ m₁ e₁ h₁ =>
    cases b with
    | notANumber => simp [uvalue] at hb
    | infinity _ => simp [uvalue] at hb
    | zero s₂ =>
      simp only [uvalue, Option.some.injEq] at hb
      subst hb
      refine ⟨x, ?_, by simpa using hnn x, fun hx _ _ => hx, fun hc _ _ => hc⟩
      simpa [UnpackedFloat.add] using ha
    | finite s₂ m₂ e₂ h₂ =>
      simp only [uvalue, Option.some.injEq] at ha hb
      rw [add_finite_eq]
      obtain ⟨v, hv, herr⟩ := normalize_value
        (s₁.apply (decreaseExponent m₁ e₁ (min e₁ e₂)).1 +
          s₂.apply (decreaseExponent m₂ e₂ (min e₁ e₂)).1) (min e₁ e₂) .positive
      refine ⟨v, hv, ?_⟩
      have hv₁ := decreaseExponent_bitValue s₁ m₁ e₁ (min e₁ e₂)
      have hv₂ := decreaseExponent_bitValue s₂ m₂ e₂ (min e₁ e₂)
      rw [decrease_snd m₁ e₁ _ (min_le_left _ _)] at hv₁
      rw [decrease_snd m₂ e₂ _ (min_le_right _ _)] at hv₂
      have hsum : (((s₁.apply (decreaseExponent m₁ e₁ (min e₁ e₂)).1 +
          s₂.apply (decreaseExponent m₂ e₂ (min e₁ e₂)).1 : Int)) : ℝ) * (2 : ℝ) ^ (min e₁ e₂) =
          x + y := by
        rw [← ha, ← hb, ← hv₁, ← hv₂]
        simp only [bitValue]
        push_cast
        ring
      rw [hsum] at herr
      exact ⟨herr.1, fun _ _ => herr.2.1, fun _ _ => herr.2.2⟩

theorem unpackSign_packComponents (sign : Sign) (exponent : BitVec Format.binary64.exponentBits)
    (mantissa : BitVec Format.binary64.mantissaBitsWithoutImplicit) :
    unpackSign (packComponents Format.binary64 sign exponent mantissa) = sign.toBitVec := by
  apply BitVec.eq_of_getLsbD_eq
  intro i hi
  have hi0 : i = 0 := by omega
  subst hi0
  simp only [unpackSign, packComponents, BitVec.getLsbD_cast, BitVec.getLsbD_extractLsb]
  change (sign.toBitVec ++ (exponent : BitVec 11) ++ (mantissa : BitVec 52)).getLsbD
    (52 + 11 + 0) = _
  rw [BitVec.getLsbD_append, BitVec.getLsbD_append]
  simp

theorem ofBitVec_toBitVec (s : Sign) : Sign.ofBitVec s.toBitVec = s := by
  cases s <;> simp [Sign.ofBitVec, Sign.toBitVec]

theorem unpack_pack_zero (s : Sign) :
    UnpackedFloat.unpack Format.binary64 (UnpackedFloat.pack Format.binary64 (.zero s)) =
      .zero s := by
  simp [UnpackedFloat.pack, UnpackedFloat.unpack, packedZero, unpackSign_packComponents,
    ofBitVec_toBitVec]

theorem unpack_pack_finite (s : Sign) (m : Nat) (e : Int) (hm : 0 < m)
    (hc : Canon (.finite s m e hm)) :
    UnpackedFloat.unpack Format.binary64 (UnpackedFloat.pack Format.binary64 (.finite s m e hm)) =
      .finite s m e hm := by
  obtain ⟨h53, hlo, hhi, hnorm⟩ := hc
  have hbias : Format.binary64.exponentBias = 1023 := by decide
  have hbits : ((e + Format.binary64.exponentBias + Format.binary64.mantissaBitsWithoutImplicit).toNat)
      = (e + 1075).toNat := by
    rw [hbias]; simp; omega
  by_cases hn : 2 ^ 52 ≤ m
  · have hlog : m.log2 = 52 := by
      have h1 : 52 ≤ m.log2 := (Nat.le_log2 (by omega)).2 hn
      have h2 : m.log2 < 53 := (Nat.log2_lt (by omega)).2 h53
      omega
    have hb1 : ¬ (2 ^ Format.binary64.exponentBits ≤
        (e + Format.binary64.exponentBias + Format.binary64.mantissaBitsWithoutImplicit).toNat + 1) := by
      rw [hbits]; simp; omega
    have hnormal : m.log2 + 1 = Format.binary64.mantissaBits := by rw [hlog]; decide
    simp only [UnpackedFloat.pack, hb1, if_false, hnormal, if_true]
    simp only [UnpackedFloat.unpack, unpackMantissa_packComponents, unpackExponent_packComponents,
      unpackSign_packComponents, ofBitVec_toBitVec, hbits]
    have hn0 : 1 ≤ (e + 1075).toNat := by omega
    have hn1 : (e + 1075).toNat < 2047 := by omega
    have hexp : (BitVec.ofNat 11 (e + 1075).toNat).toNat = (e + 1075).toNat := by
      rw [BitVec.toNat_ofNat]; exact Nat.mod_eq_of_lt (by omega)
    have hne1 : BitVec.ofNat 11 (e + 1075).toNat ≠ -1#11 := by
      intro h
      have := congrArg BitVec.toNat h
      rw [hexp, BitVec.neg_one_eq_allOnes, BitVec.toNat_allOnes] at this
      omega
    have hne0 : BitVec.ofNat 11 (e + 1075).toNat ≠ 0#11 := by
      intro h
      have := congrArg BitVec.toNat h
      rw [hexp] at this
      simp at this
      omega
    have hmant : (1#1 ++ BitVec.ofNat 52 m).toNat = m := by
      have hv : (BitVec.ofNat 52 m).toNat = m - 2 ^ 52 := by
        rw [BitVec.toNat_ofNat, Nat.mod_eq_sub_mod hn, Nat.mod_eq_of_lt (by omega)]
      have h1 : (1#1 : BitVec 1).toNat = 1 := by decide
      rw [BitVec.toNat_append, hv, h1, ← Nat.shiftLeft_add_eq_or_of_lt (by omega), Nat.shiftLeft_eq]
      omega
    simp only [hne1, hne0, if_false, hexp, hmant, hbias]
    congr 1
    omega
  · have he : e = -1074 := by omega
    have hlog : m.log2 < 52 := (Nat.log2_lt (by omega)).2 (by omega)
    have hb1 : ¬ (2 ^ Format.binary64.exponentBits ≤
        (e + Format.binary64.exponentBias + Format.binary64.mantissaBitsWithoutImplicit).toNat + 1) := by
      rw [hbits]; simp; omega
    have hsub : ¬ (m.log2 + 1 = Format.binary64.mantissaBits) := by
      have : Format.binary64.mantissaBits = 53 := by decide
      omega
    simp only [UnpackedFloat.pack, hb1, if_false, hsub]
    simp only [UnpackedFloat.unpack, unpackMantissa_packComponents, unpackExponent_packComponents,
      unpackSign_packComponents, ofBitVec_toBitVec]
    have hmv : (BitVec.ofNat 52 m).toNat = m := by
      rw [BitVec.toNat_ofNat]; exact Nat.mod_eq_of_lt (by omega)
    have hm0 : BitVec.ofNat 52 m ≠ 0#52 := by
      intro h
      have := congrArg BitVec.toNat h
      rw [hmv] at this
      simp at this
      omega
    have hne1 : (0#11 : BitVec 11) ≠ -1#11 := by decide
    simp only [hne1, if_false, if_true, hm0, dite_false, hmv, hbias]
    congr 1
    simp
    omega

/-- Every finite or zero float that `unpack` produces is canonical. -/
theorem canon_unpack (b : BitVec 64) :
    Canon (UnpackedFloat.unpack Format.binary64 b) ∨
      UnpackedFloat.unpack Format.binary64 b = .notANumber ∨
      ∃ s, UnpackedFloat.unpack Format.binary64 b = .infinity s := by
  have hbias : Format.binary64.exponentBias = 1023 := by decide
  unfold UnpackedFloat.unpack
  simp only [hbias]
  split
  · split
    · exact Or.inr (Or.inr ⟨_, rfl⟩)
    · exact Or.inr (Or.inl rfl)
  · rename_i hne1
    split
    · split
      · exact Or.inl trivial
      · rename_i hz _
        left
        have hlt : (unpackMantissa (spec := Format.binary64) b).toNat < 2 ^ 52 :=
          BitVec.isLt _
        have hz' : (unpackExponent (spec := Format.binary64) b).toNat = 0 := by
          rw [hz]; rfl
        refine ⟨by omega, ?_, ?_, Or.inr ?_⟩ <;> simp only [hz'] <;> norm_num
    · rename_i hne0
      left
      have hlt : (unpackMantissa (spec := Format.binary64) b).toNat < 2 ^ 52 := BitVec.isLt _
      have helt : (unpackExponent (spec := Format.binary64) b).toNat < 2 ^ 11 := BitVec.isLt _
      have hne0' : (unpackExponent (spec := Format.binary64) b).toNat ≠ 0 := by
        intro h; apply hne0; exact BitVec.eq_of_toNat_eq (by rw [h]; rfl)
      have hne1' : (unpackExponent (spec := Format.binary64) b).toNat ≠ 2047 := by
        intro h; apply hne1; apply BitVec.eq_of_toNat_eq
        rw [h, BitVec.neg_one_eq_allOnes, BitVec.toNat_allOnes]; rfl
      have h1 : (1#1 : BitVec 1).toNat = 1 := by decide
      have hmant : (1#1 ++ unpackMantissa (spec := Format.binary64) b).toNat =
          2 ^ 52 + (unpackMantissa (spec := Format.binary64) b).toNat := by
        rw [BitVec.toNat_append, h1, ← Nat.shiftLeft_add_eq_or_of_lt hlt, Nat.shiftLeft_eq]
        omega
      refine ⟨?_, ?_, ?_, Or.inl ?_⟩
      · rw [hmant]; omega
      · push_cast; omega
      · push_cast; omega
      · rw [hmant]; omega

theorem canon_mem {s : Sign} {m : Nat} {e : Int} {hm : 0 < m} (hc : Canon (.finite s m e hm)) :
    bitValue s m e ∈ binary64 := by
  obtain ⟨h53, hlo, hhi, -⟩ := hc
  rw [bitValue_eq]
  refine sgn_mul_mem s ⟨m, e, ?_, hlo, hhi, by push_cast; rfl⟩
  rw [abs_of_nonneg (by positivity)]
  exact_mod_cast h53

theorem model_unpack_pack (u : UnpackedFloat) (hc : Canon u) : (Float.Model.pack u).unpack = u := by
  simp only [Float.Model.unpack, Float.Model.pack, UInt64.toBitVec_ofBitVec]
  cases u with
  | notANumber => exact hc.elim
  | infinity _ => exact hc.elim
  | zero s => exact unpack_pack_zero s
  | finite s m e hm => exact unpack_pack_finite s m e hm hc

/-- Real value of a `Float.Model`: `none` for infinities and NaN. -/
noncomputable def modelValue (f : Float.Model) : Option ℝ := uvalue f.unpack

theorem canon_model {f : Float.Model} {x : ℝ} (h : modelValue f = some x) : Canon f.unpack := by
  rcases canon_unpack f.toBits.toBitVec with hc | hn | ⟨s, hi⟩
  · exact hc
  · simp only [modelValue, Float.Model.unpack, hn, uvalue] at h; cases h
  · simp only [modelValue, Float.Model.unpack, hi, uvalue] at h; cases h

/-- Every finite `Float.Model` value is a binary64 number. -/
theorem model_mem {f : Float.Model} {x : ℝ} (h : modelValue f = some x) : x ∈ binary64 := by
  have hc := canon_model h
  unfold modelValue at h
  revert hc h
  cases f.unpack with
  | notANumber => intro _ h; cases h
  | infinity _ => intro _ h; cases h
  | zero _ =>
    intro h _
    obtain rfl : (0 : ℝ) = x := Option.some.inj h
    exact zero_mem_binary64
  | finite s m e hm =>
    intro h hc
    obtain rfl : bitValue s m e = x := Option.some.inj h
    exact canon_mem hc

/-- `Float.Model.add` meets the `FPModel` addition contract with `u = 2^-53`,
`η = 2^-1075` and `M = 2^1022`: the sum of two finite values is finite, is a
binary64 number, and is within the error bound of the exact sum. -/
theorem model_add_value {a b : Float.Model} {x y : ℝ}
    (ha : modelValue a = some x) (hb : modelValue b = some y) (hM : |x + y| ≤ 2 ^ 1022) :
    ∃ v : ℝ, modelValue (a + b) = some v ∧ v ∈ binary64 ∧
      |v - (x + y)| ≤ (1 / 2 ^ 53) * |x + y| + 1 / 2 ^ 1075 := by
  obtain ⟨v, hv, herr, hmem, hcan⟩ := add_value ha hb
  refine ⟨v, ?_, hmem (model_mem ha) (model_mem hb) hM, herr⟩
  show uvalue (Float.Model.pack (UnpackedFloat.add Format.binary64 a.unpack b.unpack)).unpack = some v
  rw [model_unpack_pack _ (hcan (canon_model ha) (canon_model hb) hM)]
  exact hv

/-- Real value of a Lean `Float`: `none` for infinities and NaN. -/
noncomputable def floatValue (f : Float) : Option ℝ := modelValue f.toModel

theorem float_mem {f : Float} {x : ℝ} (h : floatValue f = some x) : x ∈ binary64 :=
  model_mem h

/-- **Lean's `Float` addition** meets the `FPModel` addition contract of
`binary64Model`: for finite `a b : Float` with `|a + b| ≤ 2^1022` (exact sum),
`a + b` is finite, is a binary64 number, and
`|fl(a + b) - (a + b)| ≤ 2^-53 |a + b| + 2^-1075`. -/
theorem float_add_value {a b : Float} {x y : ℝ}
    (ha : floatValue a = some x) (hb : floatValue b = some y) (hM : |x + y| ≤ 2 ^ 1022) :
    ∃ v : ℝ, floatValue (a + b) = some v ∧ v ∈ binary64 ∧
      |v - (x + y)| ≤ (1 / 2 ^ 53) * |x + y| + 1 / 2 ^ 1075 :=
  model_add_value ha hb hM

/-- The `Float` nearest to `0.1` has a finite value, so `float_add_value`
applies to non-integer operands. -/
theorem floatValue_tenth :
    floatValue (Float.ofBits 0x3FB999999999999A) =
      some ((7205759403792794 : ℝ) * (2 : ℝ) ^ (-56 : ℤ)) := by
  have h : (Float.ofBits 0x3FB999999999999A).toModel.unpack =
      .finite .positive 7205759403792794 (-56) (by decide) := by rfl
  simp only [floatValue, modelValue, h, uvalue, bitValue_eq, sgn, one_mul]
  norm_num


end LFortran
