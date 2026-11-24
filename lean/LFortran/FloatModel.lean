import LFortran.FloatOps

set_option exponentiation.threshold 2048

/-!
# Lean's `Float` as an `FPModel`

`floatModel` is an `FPModel` whose operations are Lean's `Float` operations,
read through `floatValue`. Its `F` is `binary64`, `u = 2^-53`,
`η = 2^-1075` and `M = 2^1022`, the parameters of `binary64Model`.
-/

open LFortran
open Float.Model
open Float.Model.UnpackedFloat hiding round add ofInt sub mul div

namespace LFortran

/-! ## Integer conversion -/

theorem one_unpack :
    (Float.exactlyRepresentablePowersOfTen[0]'(by simp [Float.exactlyRepresentablePowersOfTen])).toModel.unpack =
      .finite .positive (2 ^ 52) (-52) (by decide) := by
  rfl

theorem canon_ofInt (n : Nat) (hn : 0 < n) (h53 : n < 2 ^ 53) :
    Canon (UnpackedFloat.ofInt Format.binary64 (n : Int)) := by
  rw [ofInt_pos n hn h53]
  have hk : n.log2 ≤ 52 := log2_lt_53 hn h53
  have hlog : (n <<< (52 - n.log2)).log2 = 52 := by
    rw [Nat.shiftLeft_eq, log2_mul_two_pow n (52 - n.log2) hn]
    omega
  have hpos : 0 < n <<< (52 - n.log2) := by
    rw [Nat.shiftLeft_eq]; exact Nat.mul_pos hn (Nat.two_pow_pos _)
  refine ⟨(Nat.log2_lt (by omega)).1 (by omega), by omega, by omega, Or.inl ?_⟩
  exact (Nat.le_log2 (by omega)).1 (by omega)

/-- Multiplying a canonical normal positive float by `1.0` returns it. -/
theorem mul_one_normal (M : Nat) (E : Int) (hM : 0 < M) (hlog : M.log2 = 52)
    (hE : -1074 ≤ E) :
    UnpackedFloat.mul Format.binary64 (.finite .positive M E hM)
        (.finite .positive (2 ^ 52) (-52) (by decide)) =
      .finite .positive M E hM := by
  rw [mul_finite_eq]
  have hdiv : M * 2 ^ 52 % 2 ^ 52 = 0 := Nat.mul_mod_left _ _
  have hq : M * 2 ^ 52 / 2 ^ 52 = M := Nat.mul_div_cancel _ (Nat.two_pow_pos 52)
  have hlog2 : (M * 2 ^ 52).log2 = M.log2 + 52 := log2_mul_two_pow M 52 hM
  have ht : Format.binary64.targetExponent (totalExponent (M * 2 ^ 52) (E + -52)) =
      (E + -52) + ((52 : Nat) : Int) := by
    rw [target_eq', hlog2, hlog]; omega
  have ht2 : Format.binary64.targetExponent
      (totalExponent (M * 2 ^ 52 / 2 ^ 52) ((E + -52) + ((52 : Nat) : Int))) =
      (E + -52) + ((52 : Nat) : Int) := by
    rw [hq, target_eq', hlog]; omega
  have h := roundExact_rshift (M * 2 ^ 52) 52 (E + -52) hdiv (by rw [hq]; exact hM) ht ht2
  have hs : (Sign.positive * Sign.positive) = Sign.positive := rfl
  rw [hs, h]
  congr 1
  · omega

/-- `Float.ofInt n` is exactly `n` for `0 < n < 2^53`. -/
theorem floatValue_ofInt (n : Nat) (hn : 0 < n) (h53 : n < 2 ^ 53) :
    floatValue (Float.ofInt (n : Int)) = some (n : ℝ) := by
  have hfast : Float.ofInt (n : Int) =
      n.toUInt64.toFloat *
        Float.exactlyRepresentablePowersOfTen[0]'(by simp [Float.exactlyRepresentablePowersOfTen]) := by
    simp only [Float.ofInt, Float.ofNat, OfScientific.ofScientific, Float.ofScientific]
    rw [dif_pos ⟨h53, by norm_num⟩]
    rfl
  have hnat : n.toUInt64.toNat = n := by
    rw [Nat.toUInt64_eq]
    exact UInt64.toNat_ofNat_of_lt' (by unfold UInt64.size; omega)
  have hunp : n.toUInt64.toFloat.toModel.unpack = UnpackedFloat.ofInt Format.binary64 (n : Int) := by
    show (Float.Model.pack (UnpackedFloat.ofUInt64 Format.binary64 n.toUInt64)).unpack = _
    simp only [UnpackedFloat.ofUInt64, UnpackedFloat.ofNat, hnat]
    exact model_unpack_pack _ (canon_ofInt n hn h53)
  have hform := ofInt_pos n hn h53
  have hk : n.log2 ≤ 52 := log2_lt_53 hn h53
  have hlog : (n <<< (52 - n.log2)).log2 = 52 := by
    rw [Nat.shiftLeft_eq, log2_mul_two_pow n (52 - n.log2) hn]
    omega
  have hcan := canon_ofInt n hn h53
  rw [hform] at hunp hcan
  rw [hfast]
  unfold floatValue modelValue
  show uvalue (Float.Model.pack (UnpackedFloat.mul Format.binary64
    n.toUInt64.toFloat.toModel.unpack
    (Float.exactlyRepresentablePowersOfTen[0]'(by simp [Float.exactlyRepresentablePowersOfTen])).toModel.unpack)).unpack = _
  rw [hunp, one_unpack, mul_one_normal _ _ _ hlog (by omega), model_unpack_pack _ hcan]
  simp only [uvalue, bitValue_eq, sgn, one_mul, Option.some.injEq]
  rw [Nat.shiftLeft_eq]
  push_cast
  rw [mul_assoc, ← zpow_natCast, ← zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0)]
  have : ((52 - n.log2 : Nat) : ℤ) + ((n.log2 : ℤ) - 52) = 0 := by omega
  rw [this, zpow_zero, mul_one]

/-! ## Values determine canonical floats -/

/-- Canonical finite floats with the same value are equal. -/
theorem canon_unique {s s' : Sign} {m m' : Nat} {e e' : Int} {h : 0 < m} {h' : 0 < m'}
    (hc : Canon (.finite s m e h)) (hc' : Canon (.finite s' m' e' h'))
    (hv : bitValue s m e = bitValue s' m' e') : s = s' ∧ m = m' ∧ e = e' := by
  obtain ⟨h53, hlo, -, hn⟩ := hc
  obtain ⟨h53', hlo', -, hn'⟩ := hc'
  rw [bitValue_eq, bitValue_eq] at hv
  have hp : (0 : ℝ) < (m : ℝ) * (2 : ℝ) ^ e := by
    have : (0 : ℝ) < m := by exact_mod_cast h
    exact mul_pos this (zpow_pos (by norm_num) _)
  have hp' : (0 : ℝ) < (m' : ℝ) * (2 : ℝ) ^ e' := by
    have : (0 : ℝ) < m' := by exact_mod_cast h'
    exact mul_pos this (zpow_pos (by norm_num) _)
  have hs : s = s' := by
    cases s <;> cases s' <;> simp only [sgn] at hv <;> first | rfl | (exfalso; linarith)
  subst hs
  have hv' : (m : ℝ) * (2 : ℝ) ^ e = (m' : ℝ) * (2 : ℝ) ^ e' := by
    cases s <;> simp only [sgn] at hv <;> linarith
  -- if `e < e'` then `m = m' 2^(e'-e) ≥ 2^53`
  have key : ∀ {m m' : Nat} {e e' : Int}, m < 2 ^ 53 → (2 ^ 52 ≤ m' ∨ e' = -1074) →
      -1074 ≤ e → (m : ℝ) * (2 : ℝ) ^ e = (m' : ℝ) * (2 : ℝ) ^ e' → ¬ e < e' := by
    intro m m' e e' h53 hn' hlo hv hlt
    have hn'' : 2 ^ 52 ≤ m' := by rcases hn' with h | h <;> [exact h; omega]
    set k := (e' - e).toNat
    have hk : (k : ℤ) = e' - e := Int.toNat_of_nonneg (by omega)
    have hmk : (m : ℝ) = (m' : ℝ) * 2 ^ k := by
      have h2 : (2 : ℝ) ^ e' = 2 ^ e * 2 ^ k := by
        rw [← zpow_natCast, ← zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0), hk]; congr 1; ring
      have hz : (2 : ℝ) ^ e ≠ 0 := zpow_ne_zero _ (by norm_num)
      rw [h2] at hv
      field_simp at hv
      linarith
    have hmk' : m = m' * 2 ^ k := by exact_mod_cast hmk
    have hk1 : 1 ≤ k := by omega
    have : 2 ^ 53 ≤ m' * 2 ^ k :=
      calc 2 ^ 53 = 2 ^ 52 * 2 ^ 1 := by norm_num
        _ ≤ m' * 2 ^ k := Nat.mul_le_mul hn'' (Nat.pow_le_pow_right (by decide) hk1)
    omega
  have he : e = e' := by
    rcases lt_trichotomy e e' with hlt | heq | hgt
    · exact absurd hlt (key h53 hn' hlo hv')
    · exact heq
    · exact absurd hgt (key h53' hn hlo' hv'.symm)
  subst he
  have : (m : ℝ) = m' := by
    have hz : (2 : ℝ) ^ e ≠ 0 := zpow_ne_zero _ (by norm_num)
    exact mul_right_cancel₀ hz hv'
  exact ⟨rfl, by exact_mod_cast this, rfl⟩

/-- Equal, or both zeros (of possibly different signs). -/
def ZEquiv (u u' : UnpackedFloat) : Prop :=
  u = u' ∨ ∃ s s', u = .zero s ∧ u' = .zero s'

theorem zequiv_of_value {u u' : UnpackedFloat} {x : ℝ} (hc : Canon u) (hc' : Canon u')
    (hv : uvalue u = some x) (hv' : uvalue u' = some x) : ZEquiv u u' := by
  cases u with
  | notANumber => simp [uvalue] at hv
  | infinity _ => simp [uvalue] at hv
  | zero s =>
    cases u' with
    | notANumber => simp [uvalue] at hv'
    | infinity _ => simp [uvalue] at hv'
    | zero s' => exact Or.inr ⟨s, s', rfl, rfl⟩
    | finite s' m' e' h' =>
      simp only [uvalue, Option.some.injEq] at hv hv'
      subst hv
      exfalso
      rw [bitValue_eq] at hv'
      have : (0 : ℝ) < (m' : ℝ) * (2 : ℝ) ^ e' := by
        have : (0 : ℝ) < m' := by exact_mod_cast h'
        exact mul_pos this (zpow_pos (by norm_num) _)
      cases s' <;> simp only [sgn] at hv' <;> linarith
  | finite s m e h =>
    cases u' with
    | notANumber => simp [uvalue] at hv'
    | infinity _ => simp [uvalue] at hv'
    | zero s' =>
      simp only [uvalue, Option.some.injEq] at hv hv'
      subst hv'
      exfalso
      rw [bitValue_eq] at hv
      have : (0 : ℝ) < (m : ℝ) * (2 : ℝ) ^ e := by
        have : (0 : ℝ) < m := by exact_mod_cast h
        exact mul_pos this (zpow_pos (by norm_num) _)
      cases s <;> simp only [sgn] at hv <;> linarith
    | finite s' m' e' h' =>
      simp only [uvalue, Option.some.injEq] at hv hv'
      obtain ⟨rfl, rfl, rfl⟩ := canon_unique hc hc' (hv.trans hv'.symm)
      exact Or.inl rfl

theorem zequiv_add {a a' b b' : UnpackedFloat} (ha : ZEquiv a a') (hb : ZEquiv b b') :
    ZEquiv (UnpackedFloat.add Format.binary64 a b) (UnpackedFloat.add Format.binary64 a' b') := by
  rcases ha with rfl | ⟨s, s', rfl, rfl⟩ <;> rcases hb with rfl | ⟨t, t', rfl, rfl⟩
  · exact Or.inl rfl
  · cases a <;> simp only [UnpackedFloat.add] <;>
      first | exact Or.inl rfl | (right; split <;> split <;> exact ⟨_, _, rfl, rfl⟩)
  · cases b <;> simp only [UnpackedFloat.add] <;>
      first | exact Or.inl rfl | (right; split <;> split <;> exact ⟨_, _, rfl, rfl⟩)
  · right; simp only [UnpackedFloat.add]; split <;> split <;> exact ⟨_, _, rfl, rfl⟩

theorem zequiv_sub {a a' b b' : UnpackedFloat} (ha : ZEquiv a a') (hb : ZEquiv b b') :
    ZEquiv (UnpackedFloat.sub Format.binary64 a b) (UnpackedFloat.sub Format.binary64 a' b') := by
  rcases ha with rfl | ⟨s, s', rfl, rfl⟩ <;> rcases hb with rfl | ⟨t, t', rfl, rfl⟩
  · exact Or.inl rfl
  · cases a <;> simp only [UnpackedFloat.sub] <;>
      first | exact Or.inl rfl | (right; split <;> split <;> exact ⟨_, _, rfl, rfl⟩)
  · cases b <;> simp only [UnpackedFloat.sub] <;>
      first | exact Or.inl rfl | (right; split <;> split <;> exact ⟨_, _, rfl, rfl⟩)
  · right; simp only [UnpackedFloat.sub]; split <;> split <;> exact ⟨_, _, rfl, rfl⟩

theorem zequiv_mul {a a' b b' : UnpackedFloat} (ha : ZEquiv a a') (hb : ZEquiv b b') :
    ZEquiv (UnpackedFloat.mul Format.binary64 a b) (UnpackedFloat.mul Format.binary64 a' b') := by
  rcases ha with rfl | ⟨s, s', rfl, rfl⟩ <;> rcases hb with rfl | ⟨t, t', rfl, rfl⟩
  · exact Or.inl rfl
  · cases a <;> simp only [UnpackedFloat.mul] <;>
      first | exact Or.inl rfl | exact Or.inr ⟨_, _, rfl, rfl⟩
  · cases b <;> simp only [UnpackedFloat.mul] <;>
      first | exact Or.inl rfl | exact Or.inr ⟨_, _, rfl, rfl⟩
  · exact Or.inr ⟨_, _, rfl, rfl⟩

theorem zequiv_div {a a' b : UnpackedFloat} (ha : ZEquiv a a') :
    ZEquiv (UnpackedFloat.div Format.binary64 a b) (UnpackedFloat.div Format.binary64 a' b) := by
  rcases ha with rfl | ⟨s, s', rfl, rfl⟩
  · exact Or.inl rfl
  · cases b <;> simp only [UnpackedFloat.div] <;>
      first | exact Or.inl rfl | exact Or.inr ⟨_, _, rfl, rfl⟩

theorem modelValue_pack_zequiv {u u' : UnpackedFloat} (h : ZEquiv u u') :
    modelValue (Float.Model.pack u) = modelValue (Float.Model.pack u') := by
  rcases h with rfl | ⟨s, s', rfl, rfl⟩
  · rfl
  · unfold modelValue
    show uvalue (UnpackedFloat.unpack Format.binary64
        (UInt64.ofBitVec (UnpackedFloat.pack Format.binary64 (.zero s))).toBitVec) =
      uvalue (UnpackedFloat.unpack Format.binary64
        (UInt64.ofBitVec (UnpackedFloat.pack Format.binary64 (.zero s'))).toBitVec)
    simp only [unpack_pack_zero, uvalue]

theorem zequiv_float {f f' : Float} {x : ℝ} (h : floatValue f = some x)
    (h' : floatValue f' = some x) : ZEquiv f.toModel.unpack f'.toModel.unpack :=
  zequiv_of_value (canon_model h) (canon_model h') h h'

/-- The value of `f + g` depends only on the values of `f` and `g`. -/
theorem floatValue_add_congr {f f' g g' : Float} {x y : ℝ}
    (hf : floatValue f = some x) (hf' : floatValue f' = some x)
    (hg : floatValue g = some y) (hg' : floatValue g' = some y) :
    floatValue (f + g) = floatValue (f' + g') :=
  modelValue_pack_zequiv (zequiv_add (zequiv_float hf hf') (zequiv_float hg hg'))

theorem floatValue_sub_congr {f f' g g' : Float} {x y : ℝ}
    (hf : floatValue f = some x) (hf' : floatValue f' = some x)
    (hg : floatValue g = some y) (hg' : floatValue g' = some y) :
    floatValue (f - g) = floatValue (f' - g') :=
  modelValue_pack_zequiv (zequiv_sub (zequiv_float hf hf') (zequiv_float hg hg'))

theorem floatValue_mul_congr {f f' g g' : Float} {x y : ℝ}
    (hf : floatValue f = some x) (hf' : floatValue f' = some x)
    (hg : floatValue g = some y) (hg' : floatValue g' = some y) :
    floatValue (f * g) = floatValue (f' * g') :=
  modelValue_pack_zequiv (zequiv_mul (zequiv_float hf hf') (zequiv_float hg hg'))

theorem floatValue_div_congr {f f' g g' : Float} {x y : ℝ} (hy : y ≠ 0)
    (hf : floatValue f = some x) (hf' : floatValue f' = some x)
    (hg : floatValue g = some y) (hg' : floatValue g' = some y) :
    floatValue (f / g) = floatValue (f' / g') := by
  have hgg : g.toModel.unpack = g'.toModel.unpack := by
    rcases zequiv_float hg hg' with h | ⟨t, t', h1, -⟩
    · exact h
    · exfalso
      unfold floatValue modelValue at hg
      rw [h1] at hg
      simp only [uvalue, Option.some.injEq] at hg
      exact hy hg.symm
  show modelValue (Float.Model.pack (UnpackedFloat.div Format.binary64 f.toModel.unpack
      g.toModel.unpack)) = modelValue (Float.Model.pack (UnpackedFloat.div Format.binary64
      f'.toModel.unpack g'.toModel.unpack))
  rw [hgg]
  exact modelValue_pack_zequiv (zequiv_div (zequiv_float hf hf'))

/-! ## Every binary64 number is a `Float` -/

theorem exists_canon_of_mem {x : ℝ} (hx : x ∈ binary64) : ∃ u, Canon u ∧ uvalue u = some x := by
  obtain ⟨m, e, hm, he1, he2, rfl⟩ := hx
  by_cases h0 : m = 0
  · exact ⟨.zero .positive, trivial, by simp [uvalue, h0]⟩
  · have hmabs : ((m.natAbs : ℤ)) = |m| := Int.natCast_natAbs m
    have hn : 0 < m.natAbs := Int.natAbs_pos.2 h0
    have hn53 : m.natAbs < 2 ^ 53 := by
      have : ((m.natAbs : ℤ)) < 2 ^ 53 := by rw [hmabs]; exact hm
      exact_mod_cast this
    have hL : m.natAbs.log2 ≤ 52 := log2_lt_53 hn hn53
    set n := m.natAbs
    set L := n.log2
    set k : Nat := min (52 - L) (e + 1074).toNat with hk
    have hk1 : k ≤ 52 - L := min_le_left _ _
    have hk2 : (k : ℤ) ≤ e + 1074 := by
      have := min_le_right (52 - L) (e + 1074).toNat
      omega
    have hMpos : 0 < n * 2 ^ k := Nat.mul_pos hn (Nat.two_pow_pos _)
    refine ⟨.finite (if 0 < m then .positive else .negative) (n * 2 ^ k) (e - k) hMpos, ?_, ?_⟩
    · have hnlt : n < 2 ^ (L + 1) := Nat.lt_log2_self
      have hnge : 2 ^ L ≤ n := Nat.log2_self_le (by omega)
      refine ⟨?_, by omega, by omega, ?_⟩
      · calc n * 2 ^ k < 2 ^ (L + 1) * 2 ^ k := Nat.mul_lt_mul_of_pos_right hnlt (Nat.two_pow_pos _)
          _ = 2 ^ (L + 1 + k) := by rw [← Nat.pow_add]
          _ ≤ 2 ^ 53 := Nat.pow_le_pow_right (by decide) (by omega)
      · by_cases hkk : k = 52 - L
        · left
          calc 2 ^ 52 = 2 ^ L * 2 ^ k := by rw [← Nat.pow_add]; congr 1; omega
            _ ≤ n * 2 ^ k := Nat.mul_le_mul_right _ hnge
        · right
          have : k = (e + 1074).toNat := by
            rcases min_choice (52 - L) (e + 1074).toNat with h | h <;> [exact absurd h hkk; exact h]
          omega
    · simp only [uvalue, bitValue_eq, Option.some.injEq]
      have hpow : (2 : ℝ) ^ (k : ℕ) * (2 : ℝ) ^ (e - k) = (2 : ℝ) ^ e := by
        rw [← zpow_natCast, ← zpow_add₀ (by norm_num : (2 : ℝ) ≠ 0)]; congr 1; ring
      have hnm : (n : ℝ) = |(m : ℝ)| := by
        rw [← Int.cast_abs, ← hmabs]; push_cast; rfl
      push_cast
      rw [mul_assoc (n : ℝ), hpow, hnm]
      split
      · rename_i hpos
        rw [abs_of_pos (by exact_mod_cast hpos)]; simp [sgn]
      · rename_i hneg
        have : (m : ℝ) < 0 := by
          have : m < 0 := lt_of_le_of_ne (not_lt.1 hneg) h0
          exact_mod_cast this
        rw [abs_of_neg this]; simp [sgn]

theorem exists_float_of_mem {x : ℝ} (hx : x ∈ binary64) : ∃ f : Float, floatValue f = some x := by
  obtain ⟨u, hc, hv⟩ := exists_canon_of_mem hx
  refine ⟨Float.ofModel (Float.Model.pack u), ?_⟩
  show uvalue (Float.Model.pack u).unpack = some x
  rw [model_unpack_pack u hc]
  exact hv

/-! ## `floatModel` -/

open Classical in
/-- A `Float` whose value is `x`, for `x ∈ binary64`. -/
noncomputable def toFloat (x : ℝ) : Float :=
  if h : ∃ f : Float, floatValue f = some x then Classical.choose h else 0

theorem toFloat_spec {x : ℝ} (hx : x ∈ binary64) : floatValue (toFloat x) = some x := by
  have h := exists_float_of_mem hx
  simp only [toFloat, h, dite_true]
  exact Classical.choose_spec h

/-- A binary `Float` operation read on reals: the value of the `Float` result,
or the exact result if that is not finite (outside the range where `FPModel`
says anything). -/
noncomputable def liftOp (op : Float → Float → Float) (exact : ℝ → ℝ → ℝ) (x y : ℝ) : ℝ :=
  (floatValue (op (toFloat x) (toFloat y))).getD (exact x y)

/-- A round-to-nearest into `binary64`, used for `round` (conversions and
literals). -/
noncomputable def floatRound : ℝ → ℝ := Classical.choose exists_roundToNearest_binary64

theorem floatRound_spec : IsRoundToNearest binary64 (2 ^ 1022) floatRound :=
  Classical.choose_spec exists_roundToNearest_binary64

/-- **Lean's `Float` as an `FPModel`.** `add sub mul div` are `Float`'s own
operations on representatives of the operands; the axioms are
`float_add_value`, `float_sub_value`, `float_mul_value`, `float_div_value`. -/
noncomputable def floatModel : FPModel where
  F := binary64
  u := 1 / 2 ^ 53
  η := 1 / 2 ^ 1075
  M := 2 ^ 1022
  round := floatRound
  add := liftOp (· + ·) (· + ·)
  sub := liftOp (· - ·) (· - ·)
  mul := liftOp (· * ·) (· * ·)
  div := liftOp (· / ·) (· / ·)
  round_mem := (binary64Model floatRound floatRound_spec).round_mem
  round_err := (binary64Model floatRound floatRound_spec).round_err
  add_mem a ha b hb h := by
    obtain ⟨v, hv, hmem, -⟩ := float_add_value (toFloat_spec ha) (toFloat_spec hb) h
    simp only [liftOp, hv, Option.getD_some]; exact hmem
  add_err a ha b hb h := by
    obtain ⟨v, hv, -, herr⟩ := float_add_value (toFloat_spec ha) (toFloat_spec hb) h
    simp only [liftOp, hv, Option.getD_some]; exact herr
  sub_mem a ha b hb h := by
    obtain ⟨v, hv, hmem, -⟩ := float_sub_value (toFloat_spec ha) (toFloat_spec hb) h
    simp only [liftOp, hv, Option.getD_some]; exact hmem
  sub_err a ha b hb h := by
    obtain ⟨v, hv, -, herr⟩ := float_sub_value (toFloat_spec ha) (toFloat_spec hb) h
    simp only [liftOp, hv, Option.getD_some]; exact herr
  mul_mem a ha b hb h := by
    obtain ⟨v, hv, hmem, -⟩ := float_mul_value (toFloat_spec ha) (toFloat_spec hb) h
    simp only [liftOp, hv, Option.getD_some]; exact hmem
  mul_err a ha b hb h := by
    obtain ⟨v, hv, -, herr⟩ := float_mul_value (toFloat_spec ha) (toFloat_spec hb) h
    simp only [liftOp, hv, Option.getD_some]; exact herr
  div_mem a ha b hb hb0 h := by
    obtain ⟨v, hv, hmem, -⟩ := float_div_value (toFloat_spec ha) (toFloat_spec hb) hb0 h
    simp only [liftOp, hv, Option.getD_some]; exact hmem
  div_err a ha b hb hb0 h := by
    obtain ⟨v, hv, -, herr⟩ := float_div_value (toFloat_spec ha) (toFloat_spec hb) hb0 h
    simp only [liftOp, hv, Option.getD_some]; exact herr

/-! ## `floatModel` computes what `Float` computes -/

theorem floatModel_add_eq {f g : Float} {x y v : ℝ} (hf : floatValue f = some x)
    (hg : floatValue g = some y) (hv : floatValue (f + g) = some v) : floatModel.add x y = v := by
  show (floatValue (toFloat x + toFloat y)).getD (x + y) = v
  rw [floatValue_add_congr (toFloat_spec (float_mem hf)) hf (toFloat_spec (float_mem hg)) hg, hv]
  rfl

theorem floatModel_sub_eq {f g : Float} {x y v : ℝ} (hf : floatValue f = some x)
    (hg : floatValue g = some y) (hv : floatValue (f - g) = some v) : floatModel.sub x y = v := by
  show (floatValue (toFloat x - toFloat y)).getD (x - y) = v
  rw [floatValue_sub_congr (toFloat_spec (float_mem hf)) hf (toFloat_spec (float_mem hg)) hg, hv]
  rfl

theorem floatModel_mul_eq {f g : Float} {x y v : ℝ} (hf : floatValue f = some x)
    (hg : floatValue g = some y) (hv : floatValue (f * g) = some v) : floatModel.mul x y = v := by
  show (floatValue (toFloat x * toFloat y)).getD (x * y) = v
  rw [floatValue_mul_congr (toFloat_spec (float_mem hf)) hf (toFloat_spec (float_mem hg)) hg, hv]
  rfl

theorem floatModel_div_eq {f g : Float} {x y v : ℝ} (hy : y ≠ 0) (hf : floatValue f = some x)
    (hg : floatValue g = some y) (hv : floatValue (f / g) = some v) : floatModel.div x y = v := by
  show (floatValue (toFloat x / toFloat y)).getD (x / y) = v
  rw [floatValue_div_congr hy (toFloat_spec (float_mem hf)) hf (toFloat_spec (float_mem hg)) hg, hv]
  rfl

/-- `floatModel.round` fixes the integers that `Float.ofInt` converts exactly. -/
theorem floatModel_round_int (n : Nat) (h53 : n < 2 ^ 53) : floatModel.round (n : ℝ) = n := by
  have hmem : ((n : ℤ) : ℝ) ∈ binary64 := int_mem_binary64 (by
    rw [abs_of_nonneg (by positivity)]; exact_mod_cast h53)
  have hle : |((n : ℤ) : ℝ)| ≤ 2 ^ 1022 := by
    rw [abs_of_nonneg (by positivity)]
    have : (n : ℝ) < 2 ^ 53 := by exact_mod_cast h53
    have : (2 : ℝ) ^ 53 ≤ 2 ^ 1022 := pow_le_pow_right₀ (by norm_num) (by norm_num)
    push_cast; linarith
  have := round_fixes floatRound_spec hle hmem
  push_cast at this
  exact this

end LFortran
