import LFortran.Arr
import Mathlib.Data.Int.Basic

/-!
# Array lemmas

`Arr` (`runtime/LFortran/Arr.lean`) stores a column-major array with its
bounds. These lemmas evaluate element access as generated code performs it.
-/

namespace LFortran.Arr

theorem offset_one (lb : ℤ) (e : ℕ) (k : ℤ) : offset #[lb] #[e] #[k] = (k - lb).toNat := by
  simp [offset]

/-- An element of a one-dimensional explicit-shape dummy `a(lb:)`: the
`(k - lb)`-th element of the actual argument's data. -/
theorem get_reshape_one {α : Type} [Inhabited α] (a : Arr α) (lb : ℤ) (e : ℕ) (k : ℤ) :
    (a.reshape #[lb] #[e]).get #[k] = a.data[(k - lb).toNat]! := by
  simp [get, reshape, offset_one]

end LFortran.Arr
