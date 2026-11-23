import Legendre.Shared
import Legendre.SharedProof
import Legendre.Corollaries
import LFortran.Models.Binary64

/-!
# The `Float` run of the same `legendre_poly`

`Float.add` (and `sub` / `mul` / `div`) is definitionally
`Float.ofModel (Float.toModel a + Float.toModel b)`, and `Float.Model` is
Lean's bit-level binary64. This file does **not** prove that those bit-level
operations are "round the exact real result": that is the gap between
`Float.Model` and `binary64Model`. It is stated as `FloatAgrees`.

From that statement, the error bound already proved for every `FPModel`
applies to the `Float` value that `Legendre/Run.lean` computes.
-/

open LFortran Legendre

/-- The real represented by a VM `Float`. -/
axiom floatToReal : Float → ℝ

/-- `Float` operations, read through `floatToReal`, are the operations of `fp`.

The hypotheses are the ones `legendre_poly` needs for `0 ≤ n ≤ 100` and
`|x| ≤ 1`: integer arguments are small enough to be exact integers in
binary64, and the real operands stay in the range where `fp` is specified.
-/
structure FloatAgrees (fp : FPModel) : Prop where
  ofInt : ∀ n : ℤ, n.natAbs ≤ 2 ^ 53 →
    floatToReal (FReal.ofInt n : Float) = fp.ofInt n
  add : ∀ a b : Float,
    floatToReal (FReal.add a b) = fp.add (floatToReal a) (floatToReal b)
  sub : ∀ a b : Float,
    floatToReal (FReal.sub a b) = fp.sub (floatToReal a) (floatToReal b)
  mul : ∀ a b : Float,
    floatToReal (FReal.mul a b) = fp.mul (floatToReal a) (floatToReal b)
  div : ∀ a b : Float, floatToReal b ≠ 0 →
    floatToReal (FReal.div a b) = fp.div (floatToReal a) (floatToReal b)

/-- The VM's binary64 is some round-to-nearest `binary64Model`.

Discharge this by proving `Float.Model`'s unpack/operate/pack implements
`round (exact real result)` for a tie-to-even `r`. -/
axiom float_is_binary64 :
  ∃ r : ℝ → ℝ, ∃ hr : IsRoundToNearest binary64 (2 ^ 1022) r,
    FloatAgrees (binary64Model r hr)

/-- Running the shared polynomial at `Float` agrees with running it at a model
it implements. This is the induction over `legendre_poly`'s loop; each step
uses `FloatAgrees`. -/
axiom legendre_float_sim
    {r : ℝ → ℝ} {hr : IsRoundToNearest binary64 (2 ^ 1022) r}
    (h : FloatAgrees (binary64Model r hr))
    (n : ℕ) (x : Float) :
    floatToReal (legendre_shared.legendre_poly (Int32.ofInt (n : Int)) x)
      = legendre_at (binary64Model r hr) (n : Int) (floatToReal x)

/-- The `Float` value computed by the shared code has the proved accuracy. -/
theorem legendre_float_correct
    (n : ℕ) (hn : n ≤ 100) (x : Float)
    (hx1 : |floatToReal x| ≤ 1)
    (hxF : floatToReal x ∈ binary64) :
    |floatToReal (legendre_shared.legendre_poly (Int32.ofInt (n : Int)) x)
        - P n (floatToReal x)| ≤ 1 / 10 ^ 11 := by
  obtain ⟨r, hr, hagree⟩ := float_is_binary64
  have hbound :=
    legendre_shared_correct (binary64Model r hr) le_rfl
      (one_div_pow_le 1075 1000 (by norm_num))
      (pow_le_pow_right₀ (by norm_num) (by norm_num))
      (n : Int) (floatToReal x) hxF (by omega) (by exact_mod_cast hn) hx1
  rw [legendre_float_sim hagree n x]
  exact hbound

#print axioms legendre_float_correct
