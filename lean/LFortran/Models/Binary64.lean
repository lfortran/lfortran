import LFortran.Prelude
import Mathlib.Algebra.Order.Round
import Mathlib.Data.Int.Interval
import Mathlib.Data.Int.Log
import Mathlib.Algebra.Order.Archimedean.Real.Basic
import Mathlib.Data.Finite.Prod
import Mathlib.Data.Set.Finite.Lemmas
import Mathlib.Tactic.Linarith
import Mathlib.Tactic.NormNum
import Mathlib.Tactic.Positivity

/-!
# IEEE-754 binary64 arithmetic with rounding to nearest

IEEE Standard for Floating-Point Arithmetic, IEEE Std 754-2019.

The finite binary64 ("double precision") numbers are the reals `m · 2^e` with
an integer significand `|m| < 2^53` and an exponent `-1074 ≤ e ≤ 971`
(`binary64`); this covers both normal and subnormal numbers, and the largest
finite number is `(2^53 - 1) · 2^971 = 2^1024 (1 - 2^-53)`.

IEEE 754-2019 §4.3.1 defines `roundTiesToEven` as delivering the
floating-point number nearest to the infinitely precise result (with ties
going to an even significand), and §4 and §5 define every arithmetic operation
(`addition`, `subtraction`, `multiplication`, `division`, `convertFromInt`, ...)
as computing the exact result and then rounding it. Hence binary64 arithmetic
in `roundTiesToEven` mode is `r ∘ exact` for a function `r` that rounds to a
nearest element of `binary64`, i.e. satisfies `IsRoundToNearest binary64 M r`.
The predicate allows any tie-breaking rule, so it also covers
`roundTiesToAway`. Rounding overflows to `±∞` only for results of magnitude at
least `2^1024 (1 - 2^-54)`, far beyond the range `M = 2^1022` used here.

Main results:

* `exists_roundToNearest_binary64`: a round-to-nearest function exists
  (the hypotheses below are not vacuous);
* `roundToNearest_binary64_err`: if `|x| ≤ 2^1022` then
  `|r x - x| ≤ 2^-53 |x| + 2^-1075`;
* `binary64Model`: binary64 round-to-nearest arithmetic is an
  `LFortran.FPModel` with `u = 2^-53`, `η = 2^-1075` and `M = 2^1022`.
-/

namespace LFortran

/-- Finite binary64 numbers: `m · 2^e`, integer `m` with `|m| < 2^53`,
`-1074 ≤ e ≤ 971`. -/
def binary64 : Set ℝ :=
  {x | ∃ m e : ℤ, |m| < 2^53 ∧ -1074 ≤ e ∧ e ≤ 971 ∧ x = m * (2:ℝ)^e}

/-- `r` rounds to a nearest element of `F` on `[-M, M]` (any tie-breaking rule,
so this includes IEEE roundTiesToEven and roundTiesToAway). -/
def IsRoundToNearest (F : Set ℝ) (M : ℝ) (r : ℝ → ℝ) : Prop :=
  ∀ x, |x| ≤ M → r x ∈ F ∧ ∀ f ∈ F, |r x - x| ≤ |f - x|

theorem zero_mem_binary64 : (0 : ℝ) ∈ binary64 :=
  ⟨0, 0, by norm_num, by norm_num, by norm_num, by simp⟩

/-- There are finitely many binary64 numbers. -/
theorem binary64_finite : binary64.Finite := by
  refine ((Set.toFinite (Set.Icc (-2^53 : ℤ) (2^53))).prod
    (Set.toFinite (Set.Icc (-1074 : ℤ) 971))).image
    (fun p : ℤ × ℤ => (p.1 : ℝ) * (2:ℝ)^p.2) |>.subset ?_
  rintro x ⟨m, e, hm, he1, he2, rfl⟩
  have := abs_lt.mp hm
  exact ⟨(m, e), ⟨⟨by omega, by omega⟩, ⟨he1, he2⟩⟩, rfl⟩

/-- A round-to-nearest function for binary64 exists (on all of `ℝ`, hence on
any range). -/
theorem exists_roundToNearest_binary64 : ∃ r, IsRoundToNearest binary64 (2^1022) r := by
  have h x := Set.exists_min_image binary64 (fun f => |f - x|) binary64_finite
    ⟨0, zero_mem_binary64⟩
  choose r hr using h
  exact ⟨r, fun x _ => hr x⟩

/-- For `|x| ≤ 2^1022` the binary64 number `f = round (x / 2^e) · 2^e`, with
`e = max (⌊log₂ |x|⌋ - 52) (-1074)`, is near `x`: either within the relative
error `2^-53 |x|` (normal range), or within the absolute error `2^-1075`
(subnormal range), and then `f = x` if `x` is an integer multiple of
`2^-1074`. -/
theorem exists_binary64_near_cases (x : ℝ) (hx : |x| ≤ 2^1022) :
    ∃ f ∈ binary64, |f - x| ≤ 1/2^53 * |x| ∨
      (|f - x| ≤ 1/2^1075 ∧ ((∃ k : ℤ, x = k / 2^1074) → f = x)) := by
  rcases eq_or_ne x 0 with rfl | hx0
  · exact ⟨0, zero_mem_binary64, Or.inl (by simp)⟩
  have hax : 0 < |x| := abs_pos.mpr hx0
  set L := Int.log 2 |x| with hL
  have hL1 : (2:ℝ)^L ≤ |x| := by
    simpa using Int.zpow_log_le_self (b := 2) (by norm_num) hax
  have hL2 : |x| < (2:ℝ)^(L+1) := by
    simpa using Int.lt_zpow_succ_log_self (b := 2) (by norm_num) |x|
  have hL3 : L ≤ 1022 := by
    have h : (2:ℝ)^L ≤ (2:ℝ)^(1022:ℤ) := by
      rw [zpow_ofNat]; exact hL1.trans hx
    exact (zpow_le_zpow_iff_right₀ (by norm_num : (1:ℝ) < 2)).mp h
  set e : ℤ := max (L - 52) (-1074) with he
  have he1 : L - 52 ≤ e := le_max_left _ _
  have he2 : -1074 ≤ e := le_max_right _ _
  have he3 : e ≤ 970 := max_le (by omega) (by norm_num)
  set s : ℝ := (2:ℝ)^e with hs
  have hs0 : 0 < s := zpow_pos (by norm_num) e
  set q : ℤ := round (x / s) with hq
  have hqx : |x / s - q| ≤ 1/2 := abs_sub_round _
  -- the error bound
  have herr : |(q : ℝ) * s - x| ≤ 1/2^53 * |x| ∨
      (|(q : ℝ) * s - x| ≤ 1/2^1075 ∧ ((∃ k : ℤ, x = k / 2^1074) → q * s = x)) := by
    have h1 : |(q : ℝ) * s - x| ≤ s / 2 := by
      rw [show (q : ℝ) * s - x = -((x / s - q) * s) by field_simp; ring, abs_neg, abs_mul,
        abs_of_pos hs0]
      nlinarith
    rcases max_cases (L - 52) (-1074) with ⟨hm, -⟩ | ⟨hm, -⟩
    · left
      refine h1.trans ?_
      have : s = (2:ℝ)^L / 2^52 := by
        rw [hs, he, hm, zpow_sub₀ (by norm_num), zpow_ofNat]
      rw [this]
      calc (2:ℝ)^L / 2^52 / 2 = 1/2^53 * (2:ℝ)^L := by ring
        _ ≤ 1/2^53 * |x| := mul_le_mul_of_nonneg_left hL1 (by positivity)
    · right
      have hs1074 : s = 1/2^1074 := by
        rw [hs, he, hm, zpow_neg, zpow_ofNat, one_div]
      refine ⟨h1.trans (le_of_eq ?_), ?_⟩
      · rw [hs1074, div_div, ← pow_succ]
      · rintro ⟨k, rfl⟩
        have hk : (k : ℝ) / 2^1074 / s = k := by
          rw [hs1074]; field_simp
        rw [hq, hk, round_intCast, hs1074, mul_one_div]
  refine ⟨q * s, ?_, herr⟩
  -- membership
  have hxs : |x / s| < 2^53 := by
    rw [abs_div, abs_of_pos hs0, div_lt_iff₀ hs0]
    calc |x| < (2:ℝ)^(L+1) := hL2
      _ ≤ (2:ℝ)^((53:ℕ) + e) := zpow_le_zpow_right₀ (by norm_num) (by push_cast; omega)
      _ = 2^53 * s := by rw [zpow_add₀ (by norm_num), zpow_natCast]
  have hq53 : |q| ≤ 2^53 := by
    have h1 : |(q : ℝ)| < 2^53 + 1 := by
      have := abs_sub_abs_le_abs_sub (q : ℝ) (x / s)
      rw [abs_sub_comm] at hqx
      linarith
    have h2 : ((|q| : ℤ) : ℝ) < ((2^53 + 1 : ℤ) : ℝ) := by
      rw [Int.cast_add, Int.cast_pow, Int.cast_ofNat, Int.cast_one, Int.cast_abs]; exact h1
    have := Int.cast_lt.mp h2
    omega
  rcases lt_or_eq_of_le hq53 with h | h
  · exact ⟨q, e, h, he2, by omega, rfl⟩
  · rcases abs_eq (by norm_num) |>.mp h with h' | h'
    · refine ⟨2^52, e + 1, by norm_num, by omega, by omega, ?_⟩
      rw [h', hs, zpow_add_one₀ (by norm_num)]
      push_cast; ring
    · refine ⟨-2^52, e + 1, by norm_num, by omega, by omega, ?_⟩
      rw [h', hs, zpow_add_one₀ (by norm_num)]
      push_cast; ring

/-- For `|x| ≤ 2^1022` there is a binary64 number within `2^-53 |x| + 2^-1075`
of `x`. -/
theorem exists_binary64_near (x : ℝ) (hx : |x| ≤ 2^1022) :
    ∃ f ∈ binary64, |f - x| ≤ 1/2^53 * |x| + 1/2^1075 := by
  obtain ⟨f, hf, h | ⟨h, -⟩⟩ := exists_binary64_near_cases x hx
  · exact ⟨f, hf, h.trans (le_add_of_nonneg_right (by positivity))⟩
  · exact ⟨f, hf, h.trans (le_add_of_nonneg_left (by positivity))⟩

/-- The error bound of binary64 round-to-nearest: for `|x| ≤ 2^1022`,
`|r x - x| ≤ 2^-53 |x| + 2^-1075`. -/
theorem roundToNearest_binary64_err {r : ℝ → ℝ} (hr : IsRoundToNearest binary64 (2^1022) r)
    {x : ℝ} (hx : |x| ≤ 2^1022) : |r x - x| ≤ 1/2^53 * |x| + 1/2^1075 := by
  obtain ⟨f, hf, hfx⟩ := exists_binary64_near x hx
  exact ((hr x hx).2 f hf).trans hfx

/-- IEEE-754 binary64 arithmetic with rounding to nearest is an `FPModel` with
`u = 2^-53`, `η = 2^-1075` and `M = 2^1022`. -/
noncomputable def binary64Model (r : ℝ → ℝ) (hr : IsRoundToNearest binary64 (2^1022) r) : FPModel where
  F := binary64
  u := 1/2^53
  η := 1/2^1075
  M := 2^1022
  round := r
  add a b := r (a + b)
  sub a b := r (a - b)
  mul a b := r (a * b)
  div a b := r (a / b)
  round_mem _ hy := (hr _ hy).1
  round_err _ hy := roundToNearest_binary64_err hr hy
  add_mem _ _ _ _ h := (hr _ h).1
  add_err _ _ _ _ h := roundToNearest_binary64_err hr h
  sub_mem _ _ _ _ h := (hr _ h).1
  sub_err _ _ _ _ h := roundToNearest_binary64_err hr h
  mul_mem _ _ _ _ h := (hr _ h).1
  mul_err _ _ _ _ h := roundToNearest_binary64_err hr h
  div_mem _ _ _ _ _ h := (hr _ h).1
  div_err _ _ _ _ _ h := roundToNearest_binary64_err hr h

end LFortran
