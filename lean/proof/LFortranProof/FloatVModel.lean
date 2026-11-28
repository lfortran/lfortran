import LFortranProof.FloatExact
import LFortranProof.VModel
import LFortranProof.ErrorProp

/-!
# Lean's `Float` is a `VModel`

`floatV` reads a `Float` through its real value (`floatValue`, from Lean's
logical `Float.Model`): `val f x` is `floatValue f = some x`. Every
operation the generated templates call meets the `VModel` bounds with
`u = 2^-53`, `η = 2^-1075`, `M = 2^1022` (`LFortran/FloatOps.lean`,
`LFortran/FloatAddReal.lean`), so a rounding theorem proved for every
`VModel` is a theorem about the `Float` run. Moreover:

* literals `FReal.lit m e = Float.ofInt m * floatPow2 e` are exact
  (`float_lit_value`, from the bit pattern of `floatPow2 e`);
* `floatV_signed`: negation, `abs`, the sign bit and `floatTrunc` act on
  values;
* `floatV_guard`: `+` and `-` have the `FPModel` (guard digit) bound;
* `floatV_binary64`: `Float` is correctly rounded binary64;
* `intExact_int32`: `Int32` computes exactly on `[-2^31, 2^31)`.
-/

open Float.Model Float.Model.UnpackedFloat

set_option exponentiation.threshold 2048

namespace LFortran

/-! ## Literals -/

theorem pow2_canon (k : Nat) (hk1 : 1 ≤ k) (hk : k ≤ 2046) :
    Canon (.finite .positive (2 ^ 52) ((k : Int) - 1075) (Nat.two_pow_pos 52)) :=
  ⟨by norm_num, by omega, by omega, Or.inl le_rfl⟩

/-- The bits of `floatPow2` are the packed normal float `2^52 · 2^(k-1075)`. -/
theorem pow2_pack (k : Nat) (hk1 : 1 ≤ k) (hk : k ≤ 2046) :
    UnpackedFloat.pack Format.binary64 (.finite .positive (2 ^ 52) ((k : Int) - 1075)
      (Nat.two_pow_pos 52)) = ((k.toUInt64 <<< 52 : UInt64)).toBitVec := by
  have hbias : Format.binary64.exponentBias = 1023 := by decide
  have hbits : (((k : Int) - 1075) + Format.binary64.exponentBias +
      Format.binary64.mantissaBitsWithoutImplicit).toNat = k := by
    rw [hbias]; simp; omega
  have hlog : (2 ^ 52 : Nat).log2 = 52 := by decide
  have hnormal : (2 ^ 52 : Nat).log2 + 1 = Format.binary64.mantissaBits := by rw [hlog]; decide
  simp only [UnpackedFloat.pack, hnormal, if_true, hbits]
  rw [if_neg (by omega)]
  apply BitVec.eq_of_toNat_eq
  have hk' : (BitVec.ofNat 11 k).toNat = k := by rw [BitVec.toNat_ofNat]; omega
  have hR : (k.toUInt64 <<< 52 : UInt64).toBitVec.toNat = k * 2 ^ 52 := by
    simp [UInt64.toBitVec_shiftLeft, BitVec.toNat_shiftLeft, Nat.shiftLeft_eq]; omega
  have h0 : (BitVec.ofNat 52 (2 ^ 52)).toNat = 0 := by decide
  have hs : (Sign.positive.toBitVec).toNat = 0 := by decide
  rw [hR]
  show (Sign.positive.toBitVec ++ BitVec.ofNat 11 k ++ BitVec.ofNat 52 (2 ^ 52)).toNat = _
  rw [BitVec.toNat_append, BitVec.toNat_append, hk', h0, hs]
  simp [Nat.shiftLeft_eq]

/-- `floatPow2 e` is the `Float` `2^e`, for `-1022 ≤ e ≤ 1023`. -/
theorem floatPow2_value (e : Int) (h1 : -1022 ≤ e) (h2 : e ≤ 1023) :
    FVal (floatPow2 e) ((2 : ℝ) ^ e) := by
  obtain ⟨k, hk⟩ : ∃ k : Nat, (k : Int) = e + 1023 := ⟨(e + 1023).toNat, by omega⟩
  have hk1 : 1 ≤ k := by omega
  have hk2 : k ≤ 2046 := by omega
  have hb : (floatPow2 e).toModel = Float.Model.pack (.finite .positive (2 ^ 52)
      ((k : Int) - 1075) (Nat.two_pow_pos 52)) := by
    have : ((e + 1023).toNat.toUInt64 <<< 52 : UInt64) = k.toUInt64 <<< 52 := by
      rw [show (e + 1023).toNat = k by omega]
    show Float.Model.ofBits _ = _
    rw [this]
    unfold Float.Model.ofBits
    have hu : (k.toUInt64 <<< 52 : UInt64).toBitVec = UnpackedFloat.pack Format.binary64
        (.finite .positive (2 ^ 52) ((k : Int) - 1075) (Nat.two_pow_pos 52)) :=
      (pow2_pack k hk1 hk2).symm
    rw [hu, unpack_pack_finite _ _ _ _ (pow2_canon k hk1 hk2)]
  unfold FVal floatValue modelValue
  rw [hb, model_unpack_pack _ (pow2_canon k hk1 hk2)]
  simp only [uvalue, bitValue_eq, sgn, one_mul, Option.some.injEq]
  rw [show (((2 ^ 52 : ℕ) : ℝ)) = (2 : ℝ) ^ (52 : ℤ) by norm_num, ← zpow_add₀ (by norm_num)]
  congr 1
  omega

/-- **Literals are exact.** `FReal.lit m e` is the `Float` `m · 2^e`. -/
theorem float_lit_value {m e : ℤ} (h : LitOk m e) (hM : |(m : ℝ) * (2 : ℝ) ^ e| ≤ 2 ^ 1022) :
    FVal (FReal.lit m e : Float) ((m : ℝ) * (2 : ℝ) ^ e) := by
  obtain ⟨hm, h1, h2⟩ := h
  exact float_mul_exact (float_ofInt_value m hm) (floatPow2_value e h1 (by omega))
    ⟨m, e, hm, by omega, h2, rfl⟩ hM

/-! ## The instance -/

/-- A `u |exact| + η` error is a `u (|x| + |y|) + η` error. -/
theorem err_noGuard {v s x y : ℝ} (hs : |s| ≤ |x| + |y|)
    (h : |v - s| ≤ 1 / 2 ^ 53 * |s| + 1 / 2 ^ 1075) :
    |v - s| ≤ 1 / 2 ^ 53 * (|x| + |y|) + 1 / 2 ^ 1075 := by
  have := mul_le_mul_of_nonneg_left hs (by norm_num : (0 : ℝ) ≤ 1 / 2 ^ 53)
  linarith

/-- Lean's `Float`, read through `floatValue`. -/
noncomputable def floatV : VModel Float where
  val f x := floatValue f = some x
  u := 1 / 2 ^ 53
  η := 1 / 2 ^ 1075
  M := 2 ^ 1022
  ofInt_err n hn _ := ⟨n, float_ofInt_value n hn, by simp; positivity⟩
  lit_err m e h hM := ⟨_, float_lit_value h hM, by simp; positivity⟩
  add_err _ _ _ _ ha hb hM := by
    obtain ⟨v, hv, -, he⟩ := float_add_value ha hb hM
    exact ⟨v, hv, err_noGuard (abs_add_le _ _) he⟩
  sub_err _ _ _ _ ha hb hM := by
    obtain ⟨v, hv, -, he⟩ := float_sub_value ha hb hM
    exact ⟨v, hv, err_noGuard (abs_sub _ _) he⟩
  mul_err _ _ _ _ ha hb hM := by
    obtain ⟨v, hv, -, he⟩ := float_mul_value ha hb hM
    exact ⟨v, hv, he⟩
  div_err _ _ _ _ ha hb hy hM := by
    obtain ⟨v, hv, -, he⟩ := float_div_value ha hb hy hM
    exact ⟨v, hv, he⟩

@[simp] theorem floatV_val (f : Float) (x : ℝ) : floatV.val f x ↔ floatValue f = some x :=
  Iff.rfl

/-- `Float`'s unit roundoff and underflow error, for `LFortranProof/ErrorProp.lean`. -/
theorem floatV_small : Small floatV (1 / 2 ^ 53) (1 / 2 ^ 1075) :=
  ⟨by positivity, by positivity, by norm_num [εU], by norm_num [εU], le_rfl, le_rfl⟩

theorem floatV_signed : floatV.Signed where
  neg _ _ h := float_neg_value h
  abs _ _ h := float_abs_value h
  signbit f x h hx := by
    show floatSignbit f = true ↔ x < 0
    rw [floatSignbit_eq h hx]; simp
  toInt _ _ h h63 := floatTrunc_eq h h63

theorem floatV_guard : floatV.Guard where
  add _ _ _ _ ha hb hM := by
    obtain ⟨v, hv, -, he⟩ := float_add_value ha hb hM
    exact ⟨v, hv, he⟩
  sub _ _ _ _ ha hb hM := by
    obtain ⟨v, hv, -, he⟩ := float_sub_value ha hb hM
    exact ⟨v, hv, he⟩

theorem floatV_binary64 : floatV.Binary64 where
  u := le_rfl
  η := le_rfl
  M := le_rfl
  mem _ _ h := float_mem h
  ofInt n hn := float_ofInt_value n hn
  lit _ _ h hM := float_lit_value h hM
  sub _ _ _ _ ha hb hX hM := float_sub_exact ha hb hX hM
  mul _ _ _ _ ha hb hX hM := float_mul_exact ha hb hX hM

/-! ## `Int32` -/

theorem int32_toInt {n : ℤ} (h : InR (2 ^ 31) n) : (Int32.ofInt n).toInt = n :=
  Int32.toInt_ofInt_of_le h.1 h.2

theorem intExact_int32 : IntExact Int32 (2 ^ 31) where
  toInt_ofInt _ h := int32_toInt h
  add a b _ _ _ := (Int32.ofInt_add a b).symm
  sub a b _ _ _ := (Int32.ofInt_sub a b).symm
  mul a b _ _ _ := (Int32.ofInt_mul a b).symm
  modulo a b ha hb hb0 := by
    have ht := @Int.tmod_eq_emod a b
    have h0 := Int.emod_nonneg a hb0.ne'
    have h1 := Int.emod_lt_of_pos a hb0
    have hn : (b.natAbs : ℤ) = b := Int.natAbs_of_nonneg hb0.le
    have hT : -b < a.tmod b ∧ a.tmod b < b := by constructor <;> split_ifs at ht <;> omega
    have hr' : Int32.ofInt a % Int32.ofInt b = Int32.ofInt (a.tmod b) := by
      apply Int32.toInt_inj.1
      rw [Int32.toInt_mod, int32_toInt ha, int32_toInt hb,
        int32_toInt ⟨by have := hb.2; omega, by have := hb.2; omega⟩]
    have hb' : ¬ Int32.ofInt b < 0 := by
      rw [Int32.lt_iff_toInt_lt, int32_toInt hb]; simp; omega
    have hT' : InR (2 ^ 31) (a.tmod b) := ⟨by have := hb.2; omega, by have := hb.2; omega⟩
    have key : Int32.ofInt (a.tmod b) < 0 ↔ a.tmod b < 0 := by
      rw [Int32.lt_iff_toInt_lt, int32_toInt hT']; simp
    have hz : Int32.ofInt (a.tmod b) = 0 → a.tmod b = 0 := by
      intro h; rw [← int32_toInt hT', h]; rfl
    apply Int32.toInt_inj.1
    rw [show (FInt.ofInt (a.fmod b) : Int32) = Int32.ofInt (a.fmod b) from rfl,
      Int.fmod_eq_emod_of_nonneg _ hb0.le, int32_toInt ⟨by have := hb.1; omega,
        by have := hb.2; omega⟩]
    show (if ((Int32.ofInt a % Int32.ofInt b != 0) &&
        (decide (Int32.ofInt a % Int32.ofInt b < 0) != decide (Int32.ofInt b < 0))) = true
      then Int32.ofInt a % Int32.ofInt b + Int32.ofInt b
      else Int32.ofInt a % Int32.ofInt b).toInt = _
    rw [hr']
    simp only [hb', decide_false, Bool.and_eq_true, bne_iff_ne, ne_eq, decide_eq_false_iff_not,
      not_not, key]
    split_ifs with hc
    · rw [← Int32.ofInt_add, int32_toInt ⟨by have := hb.1; omega, by have := hb.2; omega⟩]
      split_ifs at ht <;> omega
    · have hnn : ¬ a.tmod b < 0 := fun h => hc ⟨fun h0 => by have := hz h0; omega, h⟩
      rw [int32_toInt hT']
      split_ifs at ht <;> omega
  beq a b ha hb := by
    show (Int32.ofInt a == Int32.ofInt b) = true ↔ a = b
    rw [beq_iff_eq]
    constructor
    · intro h; rw [← int32_toInt ha, ← int32_toInt hb, h]
    · intro h; rw [h]

end LFortran
