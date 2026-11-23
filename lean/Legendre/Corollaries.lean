import Legendre.Proof
import LFortran.Models.Binary64FTZ
import LFortran.Models.HighamBinary64

/-!
# `legendre_correct` in concrete arithmetics

`legendre_correct` holds for every `FPModel` with `u ≤ 2^-53`, `η ≤ 2^-1000`
and `M ≥ 2^10`. These corollaries instantiate it with the arithmetic systems
of `LFortran/Models/`: the error of `legendre_poly` is at most `10^-11` for
`0 ≤ n ≤ 100` and floating-point `|x| ≤ 1` in each of them.
-/

open LFortran Legendre

lemma one_div_pow_le (a b : ℕ) (h : b ≤ a) : (1 : ℝ) / 2 ^ a ≤ 1 / 2 ^ b :=
  one_div_le_one_div_of_le (by positivity) (pow_le_pow_right₀ (by norm_num) h)

/-- IEEE-754 binary64 with any round-to-nearest (e.g. roundTiesToEven). -/
theorem legendre_correct_binary64 (r : ℝ → ℝ) (hr : IsRoundToNearest binary64 (2^1022) r)
    (n : ℤ) (x : ℝ) (hx : x ∈ binary64) (hn0 : 0 ≤ n) (hn : n ≤ 100) (hx1 : |x| ≤ 1) :
    |legendre_fortran (binary64Model r hr) n x - P n.toNat x| ≤ 1 / 10 ^ 11 :=
  legendre_correct (binary64Model r hr) le_rfl
    (one_div_pow_le 1075 1000 (by norm_num))
    (pow_le_pow_right₀ (by norm_num) (by norm_num)) n x hx hn0 hn hx1

/-- binary64 with flush-to-zero (and denormals-are-zero), for a non-subnormal `x`. -/
theorem legendre_correct_binary64FTZ (r : ℝ → ℝ) (hr : IsRoundToNearest binary64 (2^1022) r)
    (r' : ℝ → ℝ) (hr' : IsFlushToZero (2^1022) r r')
    (n : ℤ) (x : ℝ) (hx : x ∈ binary64Normal) (hn0 : 0 ≤ n) (hn : n ≤ 100) (hx1 : |x| ≤ 1) :
    |legendre_fortran (binary64FTZModel r hr r' hr') n x - P n.toNat x| ≤ 1 / 10 ^ 11 :=
  legendre_correct (binary64FTZModel r hr r' hr') le_rfl
    (one_div_pow_le 1021 1000 (by norm_num))
    (pow_le_pow_right₀ (by norm_num) (by norm_num)) n x hx hn0 hn hx1

/-- Higham's standard model with underflow, at least as accurate as binary64. -/
theorem legendre_correct_higham (H : Higham) (hu : H.u ≤ 1 / 2 ^ 53)
    (hη : H.lam * H.u ≤ 1 / 2 ^ 1000) (hM : 2 ^ 10 ≤ H.M)
    (n : ℤ) (x : ℝ) (hx : x ∈ H.F) (hn0 : 0 ≤ n) (hn : n ≤ 100) (hx1 : |x| ≤ 1) :
    |legendre_fortran H.toFPModel n x - P n.toNat x| ≤ 1 / 10 ^ 11 :=
  legendre_correct H.toFPModel hu hη hM n x hx hn0 hn hx1

/-- Trefethen & Bau's arithmetic with `ε_machine ≤ 2^-53`. -/
theorem legendre_correct_trefethenBau (A : TrefethenBau) (hε : A.εmach ≤ 1 / 2 ^ 53)
    (n : ℤ) (x : ℝ) (hx : x ∈ A.F) (hn0 : 0 ≤ n) (hn : n ≤ 100) (hx1 : |x| ≤ 1) :
    |legendre_fortran (A.toFPModel (2 ^ 10)) n x - P n.toNat x| ≤ 1 / 10 ^ 11 :=
  legendre_correct (A.toFPModel (2 ^ 10)) hε
    (show (0 : ℝ) ≤ 1 / 2 ^ 1000 by norm_num) le_rfl n x hx hn0 hn hx1

#print axioms legendre_correct_binary64
#print axioms legendre_correct_binary64FTZ
#print axioms legendre_correct_higham
#print axioms legendre_correct_trefethenBau
