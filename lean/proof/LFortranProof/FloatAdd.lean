import LFortranProof.Models.Binary64
import Init.Data.Float.Model.Float

set_option exponentiation.threshold 2048

/-!
# Addition, for exact integers

`binary64Model.add` of two integers is the exact sum whenever the inputs and
the sum have absolute value `< 2^53` (they are binary64 numbers, and
round-to-nearest fixes them).

On the bit model, every nonnegative integer `n < 2^53` is stored exactly.
`UnpackedFloat.add` of two such integers is their sum whenever the sum is
still `< 2^53`: aligning the significands only shifts in zeros, and
`normalize` shifts them back out without rounding. `Float.Model.add` packs
and unpacks around that operation; this file only checks that round trip by
execution for `0 .. 32`. `LFortran.FloatAddReal` proves it in general and
extends addition to every finite `Float`, integer or not.
-/

open LFortran
open Float.Model
open Float.Model.UnpackedFloat hiding round add ofInt

namespace LFortran

/-- Round-to-nearest does not move a value that is already in the set. -/
theorem round_fixes {F : Set ℝ} {M : ℝ} {r : ℝ → ℝ}
    (hr : IsRoundToNearest F M r) {x : ℝ}
    (hx : |x| ≤ M) (hxF : x ∈ F) : r x = x := by
  have hnear := (hr x hx).2 x hxF
  have : |r x - x| ≤ 0 := by simpa using hnear
  exact sub_eq_zero.mp (abs_eq_zero.mp (le_antisymm this (abs_nonneg _)))

theorem int_mem_binary64 {n : ℤ} (hn : |n| < 2 ^ 53) : (n : ℝ) ∈ binary64 := by
  refine ⟨n, 0, hn, by norm_num, by norm_num, ?_⟩
  simp

/-- Exact integer addition in any round-to-nearest binary64 model. -/
theorem binary64Model_add_int
    {r : ℝ → ℝ} {hr : IsRoundToNearest binary64 (2 ^ 1022) r}
    {a b : ℤ} (_ha : |a| < 2 ^ 53) (_hb : |b| < 2 ^ 53) (hs : |a + b| < 2 ^ 53) :
    (binary64Model r hr).add (a : ℝ) (b : ℝ) = (a + b : ℝ) := by
  have hsum : (a : ℝ) + (b : ℝ) = ((a + b : ℤ) : ℝ) := by simp [Int.cast_add]
  have hM : |(a : ℝ) + b| ≤ 2 ^ 1022 := by
    rw [hsum, ← Int.cast_abs]
    have hint : |(a + b : ℤ)| ≤ 2 ^ 1022 := by
      exact le_trans (le_of_lt hs)
        (pow_le_pow_right₀ (by decide : (1 : ℤ) ≤ 2) (by decide : 53 ≤ 1022))
    exact_mod_cast hint
  have hmem : (a : ℝ) + b ∈ binary64 := by
    rw [hsum]
    exact int_mem_binary64 hs
  -- `(binary64Model r hr).add x y` is `r (x + y)`.
  show r ((a : ℝ) + b) = (a + b : ℝ)
  rw [hsum]
  exact round_fixes hr (by rwa [← hsum]) (by rwa [← hsum])

/-- Exact addition of nonnegative integers whose sum is still below `2^53`. -/
theorem binary64Model_add_nat
    {r : ℝ → ℝ} {hr : IsRoundToNearest binary64 (2 ^ 1022) r}
    {a b : ℕ} (hs : a + b < 2 ^ 53) :
    (binary64Model r hr).add (a : ℝ) (b : ℝ) = (a + b : ℝ) := by
  have hab : |(a : ℤ)| < 2 ^ 53 := by
    rw [abs_of_nonneg (by exact_mod_cast (Nat.zero_le a))]
    exact_mod_cast (show a < 2 ^ 53 by omega)
  have hbb : |(b : ℤ)| < 2 ^ 53 := by
    rw [abs_of_nonneg (by exact_mod_cast (Nat.zero_le b))]
    exact_mod_cast (show b < 2 ^ 53 by omega)
  have hs' : |(a + b : ℤ)| < 2 ^ 53 := by
    have h0 : 0 ≤ (a + b : ℤ) := by exact_mod_cast (Nat.zero_le (a + b))
    rw [abs_of_nonneg h0]
    exact_mod_cast hs
  have h := binary64Model_add_int (r := r) (hr := hr) hab hbb hs'
  simpa [Int.cast_natCast, Int.cast_add] using h

theorem binary64Model_add_small
    {r : ℝ → ℝ} {hr : IsRoundToNearest binary64 (2 ^ 1022) r}
    {a b : ℕ} (ha : a ≤ 32) (hb : b ≤ 32) :
    (binary64Model r hr).add (a : ℝ) (b : ℝ) = (a + b : ℝ) :=
  binary64Model_add_nat (by omega)

/-- Integer read of a finite unpacked float, when the significand times the
power of two is an integer. -/
def intValue : UnpackedFloat → Option Int
  | .zero _ => some 0
  | .finite .positive m e _ =>
      if 0 ≤ e then some ((m : Int) * 2 ^ e.toNat)
      else
        let d := 2 ^ (-e).toNat
        if m % d = 0 then some ((m / d : Nat) : Int) else none
  | .finite .negative m e _ =>
      if 0 ≤ e then some (-((m : Int) * 2 ^ e.toNat))
      else
        let d := 2 ^ (-e).toNat
        if m % d = 0 then some (-((m / d : Nat) : Int)) else none
  | _ => none

def valueOk (a b : Nat) : Bool :=
  match intValue (Float.Model.ofInt (a : Int) + Float.Model.ofInt (b : Int)).unpack with
  | some v => decide (v = (a + b : Int))
  | none => false

/-- All pairs of integers in `0 .. 32`, computed in `Float.Model`. -/
theorem valueOk_small :
    ((List.range 33).all fun a => (List.range 33).all fun b => valueOk a b) = true := by
  native_decide

theorem valueOk_of_le {a b : Nat} (ha : a ≤ 32) (hb : b ≤ 32) : valueOk a b = true := by
  have h := valueOk_small
  simp only [List.all_eq_true, List.mem_range] at h
  exact h a (by omega) b (by omega)

/-- Bit-level addition of these integers yields their sum, and so does
`binary64Model`. -/
theorem floatModel_add_agrees_small
    {r : ℝ → ℝ} {hr : IsRoundToNearest binary64 (2 ^ 1022) r}
    {a b : ℕ} (ha : a ≤ 32) (hb : b ≤ 32) :
    intValue (Float.Model.ofInt (a : Int) + Float.Model.ofInt (b : Int)).unpack
        = some ((a + b : Int)) ∧
      (binary64Model r hr).add (a : ℝ) (b : ℝ) = (a + b : ℝ) := by
  refine ⟨?_, binary64Model_add_small ha hb⟩
  have h := valueOk_of_le ha hb
  simp only [valueOk] at h
  split at h
  · rename_i v hv
    have hvEq : v = (a + b : Int) := of_decide_eq_true h
    rw [hv, hvEq]
  · simp at h

theorem log2_lt_53 {n : Nat} (hn : 0 < n) (h : n < 2 ^ 53) : n.log2 ≤ 52 := by
  have : n.log2 < 53 := (Nat.log2_lt (by omega)).2 h
  omega

theorem log2_mul_two_pow (n k : Nat) (hn : 0 < n) : (n * 2 ^ k).log2 = n.log2 + k := by
  induction k with
  | zero => simp
  | succ k ih =>
    have hpos : n * 2 ^ k ≠ 0 := by
      exact Nat.mul_ne_zero (by omega) (Nat.ne_of_gt (Nat.two_pow_pos _))
    have hrewrite : n * 2 ^ (k + 1) = 2 * (n * 2 ^ k) := by
      calc n * 2 ^ (k + 1) = n * (2 ^ k * 2) := by rw [Nat.pow_succ]
        _ = n * (2 * 2 ^ k) := by rw [Nat.mul_comm (2 ^ k) 2]
        _ = (n * 2) * 2 ^ k := by rw [← Nat.mul_assoc]
        _ = (2 * n) * 2 ^ k := by rw [Nat.mul_comm n 2]
        _ = 2 * (n * 2 ^ k) := by rw [Nat.mul_assoc]
    rw [hrewrite, Nat.log2_two_mul hpos, ih]
    omega

theorem binary64_mantissaBits : Format.binary64.mantissaBits = 53 := by decide

theorem binary64_minExponent : Format.binary64.minExponent = -1074 := by decide

theorem target_of_small (n : Nat) (hn : 0 < n) (h53 : n < 2 ^ 53) :
    Format.binary64.targetExponent (totalExponent n 0) = (n.log2 : Int) - 52 := by
  have hk : n.log2 ≤ 52 := log2_lt_53 hn h53
  simp [totalExponent, Format.targetExponent, binary64_mantissaBits, binary64_minExponent]
  have hge : (-1074 : Int) ≤ (n.log2 : Int) + 1 - 53 := by omega
  rw [Int.max_eq_left hge]
  omega

theorem roundExact_at_target (m : Nat) (e : Int) (hm : 0 < m)
    (ht : Format.binary64.targetExponent (totalExponent m e) = e) :
    roundWithAccuracy Format.binary64 .positive m e .exact =
      .finite .positive m e (Nat.pos_of_ne_zero (by omega : m ≠ 0)) := by
  have hm0 : m ≠ 0 := by omega
  simp [roundWithAccuracy, shiftToTargetExponent, shiftToExponent, ht,
    HShiftRight.hShiftRight, Nat.repeat, ExtendedMantissa.ofMantissaAndAccuracy,
    ExtendedMantissa.roundedMantissa, ExtendedMantissa.accuracy,
    Accuracy.roundToNearestEven, hm0]

theorem decrease_to (n shift : Nat) (e target : Int) (h : e - target = shift) :
    decreaseExponent n e target = (n <<< shift, target) := by
  simp [decreaseExponent, h, Int.toNat_natCast]
  omega

/-- No left shift when the exponent is already at most the target. -/
theorem decrease_le (n : Nat) (e target : Int) (h : e ≤ target) :
    decreaseExponent n e target = (n, e) := by
  have h0 : (e - target).toNat = 0 := by
    rw [Int.toNat_eq_zero]
    omega
  simp [decreaseExponent, h0, Nat.shiftLeft_eq]

theorem compare_pos (n : Nat) (hn : 0 < n) : compare (n : Int) 0 = Ordering.gt := by
  simp [compare, compareOfLessAndEq, show ¬ (n : Int) < 0 by omega, show ¬ n = 0 by omega]

theorem ofInt_pos (n : Nat) (hn : 0 < n) (h53 : n < 2 ^ 53) :
    UnpackedFloat.ofInt Format.binary64 (n : Int) =
      .finite .positive (n <<< (52 - n.log2)) ((n.log2 : Int) - 52)
        (Nat.pos_of_ne_zero (by
          rw [Nat.shiftLeft_eq]
          exact Nat.ne_of_gt (Nat.mul_pos hn (Nat.pow_pos (by decide))) :
            n <<< (52 - n.log2) ≠ 0)) := by
  have hk : n.log2 ≤ 52 := log2_lt_53 hn h53
  unfold UnpackedFloat.ofInt normalize
  rw [compare_pos n hn]
  simp only [Int.toNat_natCast]
  unfold UnpackedFloat.round
  have ht : Format.binary64.targetExponent (totalExponent n 0) = (n.log2 : Int) - 52 :=
    target_of_small n hn h53
  have hshift : (0 : Int) - ((n.log2 : Int) - 52) = ↑(52 - n.log2) := by omega
  have hd := decrease_to n (52 - n.log2) 0 ((n.log2 : Int) - 52) hshift
  simp only [ht, hd]
  have hm : 0 < n <<< (52 - n.log2) := by
    rw [Nat.shiftLeft_eq]
    exact Nat.mul_pos hn (Nat.pow_pos (by decide))
  have htarget : Format.binary64.targetExponent
      (totalExponent (n <<< (52 - n.log2)) ((n.log2 : Int) - 52)) =
      (n.log2 : Int) - 52 := by
    have hlog : (n <<< (52 - n.log2)).log2 = 52 := by
      rw [Nat.shiftLeft_eq, log2_mul_two_pow n (52 - n.log2) hn]
      omega
    simp [totalExponent, Format.targetExponent, binary64_mantissaBits,
      binary64_minExponent, hlog]
    omega
  simpa using
    roundExact_at_target (n <<< (52 - n.log2)) ((n.log2 : Int) - 52) hm htarget

theorem ofInt_zero : UnpackedFloat.ofInt Format.binary64 0 = .zero .positive := by
  unfold UnpackedFloat.ofInt normalize
  simp [compare, compareOfLessAndEq]

theorem intValue_ofInt (n : Nat) (hn : 0 < n) (h53 : n < 2 ^ 53) :
    intValue (UnpackedFloat.ofInt Format.binary64 (n : Int)) = some (n : Int) := by
  have hform := ofInt_pos n hn h53
  have hkbound := log2_lt_53 hn h53
  rw [hform]
  simp only [intValue]
  split
  · rename_i hpos
    have hk : n.log2 = 52 := by omega
    simp [hk, Nat.shiftLeft_eq]
  · rename_i hneg
    have heq : (-((n.log2 : Int) - 52)).toNat = 52 - n.log2 := by omega
    have hd : n * 2 ^ (52 - n.log2) % 2 ^ (52 - n.log2) = 0 := by
      rw [Nat.mul_comm]
      exact Nat.mul_mod_right _ _
    have hdiv : n * 2 ^ (52 - n.log2) / 2 ^ (52 - n.log2) = n :=
      Nat.mul_div_left n (Nat.two_pow_pos _)
    simp [Nat.shiftLeft_eq, hd, hdiv]

theorem shiftLeft_shiftLeft (n i j : Nat) : (n <<< i) <<< j = n <<< (i + j) := by
  simp only [Nat.shiftLeft_eq]
  rw [Nat.mul_assoc, ← Nat.pow_add]

theorem shiftLeft_div (n k r : Nat) (hr : r ≤ k) :
    (n <<< k) / 2 ^ r = n <<< (k - r) := by
  simp only [Nat.shiftLeft_eq]
  have hd : 2 ^ r ∣ 2 ^ k := Nat.pow_dvd_pow 2 hr
  rw [Nat.mul_div_assoc n hd]
  have hpow : 2 ^ k / 2 ^ r = 2 ^ (k - r) := by
    have hk : k = (k - r) + r := by omega
    have h1 : 2 ^ k = 2 ^ ((k - r) + r) := congrArg (fun t => 2 ^ t) hk
    rw [h1, Nat.pow_add, Nat.mul_div_cancel _ (Nat.two_pow_pos r)]
  rw [hpow]

/-- Right shift of an exact extended mantissa drops no bits. -/
theorem em_rshift_exact (k m : Nat) (h : m % 2 ^ k = 0) :
    Nat.repeat ExtendedMantissa.shiftRightOne k ⟨m, false, false⟩ =
      ⟨m / 2 ^ k, false, false⟩ := by
  induction k generalizing m with
  | zero =>
    rw [show Nat.repeat ExtendedMantissa.shiftRightOne 0 ⟨m, false, false⟩ =
      ⟨m, false, false⟩ from rfl]
    simp [Nat.div_one]
  | succ k ih =>
    have hdiv : 2 ^ (k + 1) ∣ m := (Nat.dvd_iff_mod_eq_zero).2 h
    have hmodk : m % 2 ^ k = 0 :=
      Nat.mod_eq_zero_of_dvd
        (Nat.dvd_trans (Nat.pow_dvd_pow 2 (Nat.le_succ k)) hdiv)
    rw [show Nat.repeat ExtendedMantissa.shiftRightOne (k + 1) ⟨m, false, false⟩ =
      ExtendedMantissa.shiftRightOne
        (Nat.repeat ExtendedMantissa.shiftRightOne k ⟨m, false, false⟩) from rfl]
    rw [ih m hmodk]
    have hmul : 2 ^ (k + 1) ∣ m := hdiv
    rw [Nat.pow_succ] at hmul
    obtain ⟨q, hq⟩ := hmul
    have hq' : m = 2 ^ k * (2 * q) := by rw [hq, Nat.mul_assoc]
    have hdivk : m / 2 ^ k = 2 * q := by
      rw [hq', Nat.mul_div_cancel_left _ (Nat.two_pow_pos k)]
    have heven : m / 2 ^ k % 2 = 0 := by
      rw [hdivk]
      exact Nat.mul_mod_right 2 q
    simp only [ExtendedMantissa.shiftRightOne, heven]
    rw [show (0 != 0) = false by decide, show (false || false) = false by decide]
    rw [Nat.div_div_eq_div_mul, ← Nat.pow_succ]

theorem shiftExact (m r : Nat) (e : Int) (hdiv : m % 2 ^ r = 0) :
    shiftToExponent m e .exact (e + ↑r) =
      (⟨m / 2 ^ r, false, false⟩, e + ↑r) := by
  have hAmt : ((e + ↑r) - e).toNat = r := by
    have : (e + ↑r) - e = ↑r := by
      rw [Int.add_comm]
      simp
    simp [this, Int.toNat_natCast]
  simp only [shiftToExponent, hAmt, ExtendedMantissa.ofMantissaAndAccuracy,
    HShiftRight.hShiftRight]
  rw [em_rshift_exact r m hdiv]

theorem roundExact_rshift (m r : Nat) (e : Int) (hdiv : m % 2 ^ r = 0)
    (hm : 0 < m / 2 ^ r)
    (ht : Format.binary64.targetExponent (totalExponent m e) = e + ↑r)
    (ht2 : Format.binary64.targetExponent
      (totalExponent (m / 2 ^ r) (e + ↑r)) = e + ↑r) :
    roundWithAccuracy Format.binary64 .positive m e .exact =
      .finite .positive (m / 2 ^ r) (e + ↑r) (Nat.pos_of_ne_zero (Nat.ne_of_gt hm)) := by
  have hshift : shiftToTargetExponent Format.binary64 m e .exact =
      (⟨m / 2 ^ r, false, false⟩, e + ↑r) := by
    simp only [shiftToTargetExponent, ht]
    exact shiftExact m r e hdiv
  unfold UnpackedFloat.roundWithAccuracy
  rw [hshift]
  simp only [ExtendedMantissa.roundedMantissa, ExtendedMantissa.accuracy,
    Accuracy.roundToNearestEven]
  have hm0 : m / 2 ^ r ≠ 0 := Nat.ne_of_gt hm
  simp [shiftToTargetExponent, shiftToExponent, ht2, HShiftRight.hShiftRight, Nat.repeat,
    ExtendedMantissa.ofMantissaAndAccuracy, hm0]

theorem normalize_pos (m : Nat) (e : Int) (hm : 0 < m) :
    normalize Format.binary64 (m : Int) e .positive = UnpackedFloat.round Format.binary64 .positive m e := by
  unfold normalize
  rw [compare_pos m hm]
  simp [Int.toNat_natCast]

/-- `n · 2^k` at exponent `-k` is the same float as the integer `n`, for `n < 2^53`. -/
theorem normalize_exact (n k : Nat) (hn : 0 < n) (h53 : n < 2 ^ 53) :
    normalize Format.binary64 ((n <<< k : Nat) : Int) (-(k : Int)) .positive =
      UnpackedFloat.ofInt Format.binary64 (n : Int) := by
  rw [ofInt_pos n hn h53]
  have hk := log2_lt_53 hn h53
  have hm : 0 < n <<< k := by
    rw [Nat.shiftLeft_eq]
    exact Nat.mul_pos hn (Nat.two_pow_pos _)
  rw [normalize_pos _ _ hm]
  unfold UnpackedFloat.round
  have hlog : (n <<< k).log2 = n.log2 + k := by
    rw [Nat.shiftLeft_eq]
    exact log2_mul_two_pow n k hn
  have htot : totalExponent (n <<< k) (-(k : Int)) = (n.log2 : Int) + 1 := by
    simp only [totalExponent, hlog]
    omega
  have ht : Format.binary64.targetExponent (totalExponent (n <<< k) (-(k : Int))) =
      (n.log2 : Int) - 52 := by
    rw [htot]
    simp [Format.targetExponent, binary64_mantissaBits, binary64_minExponent]
    have hge : (-1074 : Int) ≤ (n.log2 : Int) + 1 - 53 := by omega
    rw [Int.max_eq_left hge]
    omega
  rw [ht]
  dsimp only
  by_cases hsmall : k ≤ 52 - n.log2
  · have hdiff : (-(k : Int)) - ((n.log2 : Int) - 52) = ↑((52 - n.log2) - k) := by omega
    have hd := decrease_to (n <<< k) ((52 - n.log2) - k) (-(k : Int))
      ((n.log2 : Int) - 52) hdiff
    have hcomp : (n <<< k) <<< ((52 - n.log2) - k) = n <<< (52 - n.log2) := by
      have hsum : k + ((52 - n.log2) - k) = 52 - n.log2 := by omega
      simpa [hsum] using shiftLeft_shiftLeft n k ((52 - n.log2) - k)
    rw [hd, hcomp]
    have hm2 : 0 < n <<< (52 - n.log2) := by
      rw [Nat.shiftLeft_eq]
      exact Nat.mul_pos hn (Nat.two_pow_pos _)
    have htarget : Format.binary64.targetExponent
        (totalExponent (n <<< (52 - n.log2)) ((n.log2 : Int) - 52)) =
        (n.log2 : Int) - 52 := by
      have hlog2 : (n <<< (52 - n.log2)).log2 = 52 := by
        rw [Nat.shiftLeft_eq, log2_mul_two_pow n (52 - n.log2) hn]
        omega
      simp [totalExponent, Format.targetExponent, binary64_mantissaBits,
        binary64_minExponent, hlog2]
      omega
    exact roundExact_at_target (n <<< (52 - n.log2)) ((n.log2 : Int) - 52) hm2 htarget
  · have hle : (-(k : Int)) ≤ (n.log2 : Int) - 52 := by omega
    have hd := decrease_le (n <<< k) (-(k : Int)) ((n.log2 : Int) - 52) hle
    rw [hd]
    let r := k - (52 - n.log2)
    have hdiv : (n <<< k) % 2 ^ r = 0 := by
      rw [Nat.shiftLeft_eq]
      apply Nat.mod_eq_zero_of_dvd
      rw [Nat.mul_comm]
      exact Nat.dvd_mul_right_of_dvd (Nat.pow_dvd_pow 2 (by omega : r ≤ k)) n
    have hquot : (n <<< k) / 2 ^ r = n <<< (52 - n.log2) := by
      rw [shiftLeft_div n k r (by omega : r ≤ k)]
      have : k - r = 52 - n.log2 := by omega
      rw [this]
    have hm' : 0 < (n <<< k) / 2 ^ r := by
      rw [hquot, Nat.shiftLeft_eq]
      exact Nat.mul_pos hn (Nat.two_pow_pos _)
    have htR : Format.binary64.targetExponent (totalExponent (n <<< k) (-(k : Int))) =
        (-(k : Int)) + ↑r := by
      rw [ht]
      omega
    have hexp : (-(k : Int)) + ↑r = (n.log2 : Int) - 52 := by omega
    have ht2 : Format.binary64.targetExponent
        (totalExponent ((n <<< k) / 2 ^ r) ((-(k : Int)) + ↑r)) =
        (-(k : Int)) + ↑r := by
      rw [hquot, hexp]
      have hlog2 : (n <<< (52 - n.log2)).log2 = 52 := by
        rw [Nat.shiftLeft_eq, log2_mul_two_pow n (52 - n.log2) hn]
        omega
      simp [totalExponent, Format.targetExponent, binary64_mantissaBits,
        binary64_minExponent, hlog2]
      omega
    have hround := roundExact_rshift (n <<< k) r (-(k : Int)) hdiv hm' htR ht2
    simp only [hquot, hexp] at hround
    exact hround

theorem add_finite_pos (m1 m2 : Nat) (e1 e2 : Int) (h1 : 0 < m1) (h2 : 0 < m2) :
    UnpackedFloat.add Format.binary64
        (.finite .positive m1 e1 h1) (.finite .positive m2 e2 h2) =
      (let smallerExponent := min e1 e2
       let (m₁, _) := decreaseExponent m1 e1 smallerExponent
       let (m₂, _) := decreaseExponent m2 e2 smallerExponent
       let mantissa := Sign.positive.apply m₁ + Sign.positive.apply m₂
       normalize Format.binary64 mantissa smallerExponent .positive) := by
  rfl

theorem add_pos (a b : Nat) (ha : 0 < a) (hb : 0 < b) (hs : a + b < 2 ^ 53) :
    UnpackedFloat.add Format.binary64
        (UnpackedFloat.ofInt Format.binary64 (a : Int))
        (UnpackedFloat.ofInt Format.binary64 (b : Int)) =
      UnpackedFloat.ofInt Format.binary64 ((a + b : Nat) : Int) := by
  have ha53 : a < 2 ^ 53 := by omega
  have hb53 : b < 2 ^ 53 := by omega
  rw [ofInt_pos a ha ha53, ofInt_pos b hb hb53]
  let minL := min a.log2 b.log2
  have hmina : minL ≤ a.log2 := Nat.min_le_left _ _
  have hminb : minL ≤ b.log2 := Nat.min_le_right _ _
  have ha52 := log2_lt_53 ha ha53
  have hb52 := log2_lt_53 hb hb53
  have hmine : min ((a.log2 : Int) - 52) ((b.log2 : Int) - 52) =
      ((min a.log2 b.log2 : Nat) : Int) - 52 := by
    rcases Nat.le_total a.log2 b.log2 with h | h
    · rw [Nat.min_eq_left h, Int.min_eq_left (by omega)]
    · rw [Nat.min_eq_right h, Int.min_eq_right (by omega)]
  have halign (n : Nat) (hn : 0 < n) (h53 : n < 2 ^ 53)
      (hmin : min a.log2 b.log2 ≤ n.log2) :
      decreaseExponent (n <<< (52 - n.log2)) ((n.log2 : Int) - 52)
          (((min a.log2 b.log2 : Nat) : Int) - 52) =
        (n <<< (52 - min a.log2 b.log2), ((min a.log2 b.log2 : Nat) : Int) - 52) := by
    have h52 := log2_lt_53 hn h53
    have hdiff : ((n.log2 : Int) - 52) - (((min a.log2 b.log2 : Nat) : Int) - 52) =
        ↑(n.log2 - min a.log2 b.log2) := by omega
    have hd := decrease_to (n <<< (52 - n.log2)) (n.log2 - min a.log2 b.log2)
      ((n.log2 : Int) - 52) (((min a.log2 b.log2 : Nat) : Int) - 52) hdiff
    have hcomp : (n <<< (52 - n.log2)) <<< (n.log2 - min a.log2 b.log2) =
        n <<< (52 - min a.log2 b.log2) := by
      have hsum : (52 - n.log2) + (n.log2 - min a.log2 b.log2) =
          52 - min a.log2 b.log2 := by omega
      simpa [hsum] using shiftLeft_shiftLeft n (52 - n.log2) (n.log2 - min a.log2 b.log2)
    simpa [hcomp] using hd
  rw [add_finite_pos]
  rw [hmine]
  simp only [halign a ha ha53 hmina, halign b hb hb53 hminb, Sign.apply]
  have hsum : (↑(a <<< (52 - minL)) : Int) + ↑(b <<< (52 - minL)) =
      ↑((a + b) <<< (52 - minL)) := by
    rw [← Int.natCast_add]
    congr 1
    simp only [Nat.shiftLeft_eq]
    exact (Nat.add_mul a b (2 ^ (52 - minL))).symm
  have hexp : (minL : Int) - 52 = -↑(52 - minL) := by omega
  rw [hsum, hexp]
  exact normalize_exact (a + b) (52 - minL) (by omega) hs

theorem add_zero_left (n : Nat) (hn : n < 2 ^ 53) :
    UnpackedFloat.add Format.binary64
        (UnpackedFloat.ofInt Format.binary64 0)
        (UnpackedFloat.ofInt Format.binary64 (n : Int)) =
      UnpackedFloat.ofInt Format.binary64 (n : Int) := by
  cases n with
  | zero =>
    rw [ofInt_zero]
    rfl
  | succ n =>
    rw [ofInt_zero, ofInt_pos (n + 1) (by omega) hn]
    rfl

theorem add_zero_right (n : Nat) (hn : n < 2 ^ 53) :
    UnpackedFloat.add Format.binary64
        (UnpackedFloat.ofInt Format.binary64 (n : Int))
        (UnpackedFloat.ofInt Format.binary64 0) =
      UnpackedFloat.ofInt Format.binary64 (n : Int) := by
  cases n with
  | zero =>
    rw [ofInt_zero]
    rfl
  | succ n =>
    rw [ofInt_zero, ofInt_pos (n + 1) (by omega) hn]
    rfl

/-- Addition of nonnegative integers in the exact range of binary64. -/
theorem unpacked_add_nat (a b : Nat) (hs : a + b < 2 ^ 53) :
    UnpackedFloat.add Format.binary64
        (UnpackedFloat.ofInt Format.binary64 (a : Int))
        (UnpackedFloat.ofInt Format.binary64 (b : Int)) =
      UnpackedFloat.ofInt Format.binary64 ((a + b : Nat) : Int) := by
  rcases Nat.eq_zero_or_pos a with ha0 | ha
  · rw [ha0, Nat.zero_add]
    exact add_zero_left b (by omega)
  · rcases Nat.eq_zero_or_pos b with hb0 | hb
    · rw [hb0, Nat.add_zero]
      exact add_zero_right a (by omega)
    · exact add_pos a b ha hb hs

theorem intValue_unpacked_add (a b : Nat) (hs : a + b < 2 ^ 53) :
    intValue (UnpackedFloat.add Format.binary64
        (UnpackedFloat.ofInt Format.binary64 (a : Int))
        (UnpackedFloat.ofInt Format.binary64 (b : Int))) =
      some (a + b : Int) := by
  rw [unpacked_add_nat a b hs]
  rcases Nat.eq_zero_or_pos (a + b) with h0 | hpos
  · have hcast : (a + b : Int) = 0 := by exact_mod_cast h0
    simp [h0, hcast, ofInt_zero, intValue]
  · exact intValue_ofInt (a + b) hpos hs

/-- `+0` is immediate from the clauses of `UnpackedFloat.add`. A finite sum is not. -/
theorem add_pos_zero_finite (s : UnpackedFloat.Sign) (m : Nat) (e : Int) (hm : 0 < m) :
    UnpackedFloat.add Format.binary64 (.zero .positive) (.finite s m e hm)
      = .finite s m e hm := by
  cases s <;> rfl

/-- Bit-level and real-model addition agree on nonnegative integers below `2^53`. -/
theorem add_nat_exact
    {r : ℝ → ℝ} {hr : IsRoundToNearest binary64 (2 ^ 1022) r}
    {a b : ℕ} (hs : a + b < 2 ^ 53) :
    intValue (UnpackedFloat.add Format.binary64
        (UnpackedFloat.ofInt Format.binary64 (a : Int))
        (UnpackedFloat.ofInt Format.binary64 (b : Int))) = some (a + b : Int) ∧
      (binary64Model r hr).add (a : ℝ) (b : ℝ) = (a + b : ℝ) :=
  ⟨intValue_unpacked_add a b hs, binary64Model_add_nat hs⟩

end LFortran
