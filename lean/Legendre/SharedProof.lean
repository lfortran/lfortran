import Legendre.Shared
import Legendre.Generated
import Legendre.Proof
import LFortran.Arith.Model

/-!
# The shared function is the function already proved

At `ℤ` and `ℝ`, with `FReal.ofModel fp`, `legendre_shared.legendre_poly`
is definitionally the generated proof term. `legendre_correct` therefore
applies to this one definition, not to a second translation.
-/

open LFortran

/-- The shared polynomial at one `FPModel`. -/
noncomputable def legendre_at (fp : FPModel) (n : ℤ) (x : ℝ) : ℝ :=
  @legendre_shared.legendre_poly ℤ ℝ _ _ (FReal.ofModel fp) n x

theorem legendre_at_eq (fp : FPModel) (n : ℤ) (x : ℝ) :
    legendre_at fp n x = legendre_mod.legendre_poly fp n x := by
  delta legendre_at
  simp only [legendre_shared.legendre_poly, legendre_mod.legendre_poly,
    range_eq_intRange, ofModel_zero, ofModel_ofInt, ofModel_sub, ofModel_mul,
    ofModel_div, ofInt_int, toInt_int, mul_int, sub_int, FPModel.ofInt,
    Int.cast_one]

/-- Accuracy of the shared definition, for every admissible arithmetic. -/
theorem legendre_shared_correct :
    Legendre.LegendreSpec (fun fp n x => legendre_at fp n x) := by
  intro fp hu hη hM n x hxF hn0 hn100 hx
  simpa [legendre_at_eq] using
    legendre_correct fp hu hη hM n x hxF hn0 hn100 hx
