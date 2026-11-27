/-!
# Fortran arrays

A Fortran array is a column-major `Arr α` with the lower bound of each
dimension stored beside the data. `α` is the element type, for example
`K.R8` in a template. Arrays are values: assignment copies, and Lean updates
an unshared array in place.

Procedures receive arrays by value and return the `intent(out)` and
`intent(inout)` ones (copy-in/copy-out, which is faithful because Fortran
forbids aliasing a modified dummy argument). A dummy array takes the bounds
of its declaration on entry: `reshape` for an explicit-shape dummy `a(n)`
(the data is reinterpreted, as Fortran's sequence association does),
`rebase` for an assumed-shape dummy `a(:)`. After the call the caller keeps
its own bounds and takes the data back (`copyBack`).
-/

namespace LFortran

/-- Column-major array with a lower bound per dimension. -/
structure Arr (α : Type) where
  lbs : Array Int
  exts : Array Nat
  data : Array α

instance {α : Type} : Inhabited (Arr α) where
  default := { lbs := #[], exts := #[], data := #[] }

namespace Arr

def map {α β : Type} (f : α → β) (a : Arr α) : Arr β :=
  { lbs := Array.replicate a.exts.size 1, exts := a.exts, data := a.data.map f }

def zipWith {α β γ : Type} (f : α → β → γ) (a : Arr α) (b : Arr β) : Arr γ :=
  { lbs := Array.replicate a.exts.size 1, exts := a.exts, data := Array.zipWith f a.data b.data }

def fill {α : Type} (a : Arr α) (v : α) : Arr α :=
  { a with data := Array.replicate a.data.size v }

def offset (lbs : Array Int) (exts : Array Nat) (idxs : Array Int) : Nat := Id.run do
  let mut off : Nat := 0
  let mut stride : Nat := 1
  for d in [:exts.size] do
    let delta := idxs[d]! - lbs[d]!
    off := off + delta.toNat * stride
    stride := stride * exts[d]!
  return off

def make {α : Type} (lbs : Array Int) (exts : Array Nat) (fill : α) : Arr α := Id.run do
  let mut n : Nat := 1
  for e in exts do
    n := n * e
  return { lbs, exts, data := Array.replicate n fill }

def get {α : Type} [Inhabited α] (a : Arr α) (idxs : Array Int) : α :=
  a.data[offset a.lbs a.exts idxs]!

def set {α : Type} (a : Arr α) (idxs : Array Int) (v : α) : Arr α :=
  { a with data := a.data.set! (offset a.lbs a.exts idxs) v }

/-- The same data with the bounds of an explicit-shape dummy argument. -/
def reshape {α : Type} (a : Arr α) (lbs : Array Int) (exts : Array Nat) : Arr α :=
  { lbs, exts, data := a.data }

/-- The same array with the lower bounds of an assumed-shape dummy argument. -/
def rebase {α : Type} (a : Arr α) (lbs : Array Int) : Arr α :=
  { a with lbs }

/-- The actual argument `a` after a call that returned `r` for it. -/
def copyBack {α : Type} (a r : Arr α) : Arr α :=
  { a with data := r.data }

/-- `size(a, dim)`. -/
def sizeDim {α : Type} (a : Arr α) (dim : Int) : Int :=
  Int.ofNat a.exts[dim.toNat - 1]!

def size {α : Type} (a : Arr α) : Int :=
  Int.ofNat (a.exts.foldl (fun n e => n * e) 1)

def lbound {α : Type} (a : Arr α) (dim : Int) : Int :=
  let i := dim.toNat - 1
  if a.exts[i]! == 0 then 1 else a.lbs[i]!

def ubound {α : Type} (a : Arr α) (dim : Int) : Int :=
  let i := dim.toNat - 1
  if a.exts[i]! == 0 then 0 else a.lbs[i]! + Int.ofNat (a.exts[i]!) - 1

private def sliceLayout {α : Type} (a : Arr α)
    (specs : Array (Int ⊕ (Int × Int × Int)))
    : Array Nat × Int × Array Int × Nat := Id.run do
  let mut newExts : Array Nat := #[]
  let mut strides : Array Nat := #[]
  let mut stride : Nat := 1
  for d in [:a.exts.size] do
    strides := strides.push stride
    stride := stride * a.exts[d]!
  let mut baseOff : Int := 0
  let mut sliceStrides : Array Int := #[]
  for d in [:specs.size] do
    let lb := a.lbs[d]!
    let st : Int := Int.ofNat strides[d]!
    match specs.getD d (Sum.inl 0) with
    | Sum.inl idx =>
      baseOff := baseOff + (idx - lb) * st
    | Sum.inr (lo, hi, step) =>
      let count : Nat := if step == 0 then 0 else ((hi - lo + step).tdiv step).toNat
      newExts := newExts.push count
      baseOff := baseOff + (lo - lb) * st
      sliceStrides := sliceStrides.push (step * st)
  let mut total : Nat := 1
  for e in newExts do total := total * e
  return (newExts, baseOff, sliceStrides, total)

/-- Slice an array along each dimension.
`specs[d]` is `Sum.inl idx` for a fixed scalar index (rank-reducing) or
`Sum.inr (start, stop, step)` for a triplet slice `start:stop:step`.
Resulting dimensions have lower bound 1, as in Fortran. -/
def slice {α : Type} [Inhabited α] (a : Arr α)
    (specs : Array (Int ⊕ (Int × Int × Int))) : Arr α := Id.run do
  let (newExts, baseOff, sliceStrides, total) := sliceLayout a specs
  let mut out : Array α := Array.mkEmpty total
  for flat in [:total] do
    let mut rem := flat
    let mut off := baseOff
    for k in [:newExts.size] do
      let e := newExts[k]!
      let idx := rem % e
      rem := rem / e
      off := off + Int.ofNat idx * sliceStrides[k]!
    out := out.push a.data[off.toNat]!
  return { lbs := Array.replicate newExts.size 1, exts := newExts, data := out }

/-- Assign an array into a slice of `a`, keeping `a`'s shape and bounds. -/
def setSlice {α : Type} [Inhabited α] (a : Arr α)
    (specs : Array (Int ⊕ (Int × Int × Int))) (src : Arr α) : Arr α := Id.run do
  let (newExts, baseOff, sliceStrides, total) := sliceLayout a specs
  let mut data := a.data
  for flat in [:total] do
    let mut rem := flat
    let mut off := baseOff
    for k in [:newExts.size] do
      let e := newExts[k]!
      let idx := rem % e
      rem := rem / e
      off := off + Int.ofNat idx * sliceStrides[k]!
    data := data.set! off.toNat src.data[flat]!
  return { a with data := data }

/-- Fill a slice of `a` with a scalar value, keeping `a`'s shape and bounds. -/
def fillSlice {α : Type} (a : Arr α)
    (specs : Array (Int ⊕ (Int × Int × Int))) (v : α) : Arr α := Id.run do
  let (newExts, baseOff, sliceStrides, total) := sliceLayout a specs
  let mut data := a.data
  for flat in [:total] do
    let mut rem := flat
    let mut off := baseOff
    for k in [:newExts.size] do
      let e := newExts[k]!
      let idx := rem % e
      rem := rem / e
      off := off + Int.ofNat idx * sliceStrides[k]!
    data := data.set! off.toNat v
  return { a with data := data }

/-- Fortran `matmul` for 2D×2D, 2D×1D, and 1D×2D arrays. -/
def matmul {α : Type} [Inhabited α] (z : α) (add mul : α → α → α)
    (a b : Arr α) : Arr α := Id.run do
  if a.exts.size == 2 && b.exts.size == 2 then
    let m := a.exts[0]!
    let k := a.exts[1]!
    let n := b.exts[1]!
    let mut out : Array α := Array.mkEmpty (m * n)
    for j in [:n] do
      for i in [:m] do
        let mut s := z
        for p in [:k] do
          s := add s (mul a.data[i + m * p]! b.data[p + k * j]!)
        out := out.push s
    return { lbs := #[1, 1], exts := #[m, n], data := out }
  else if a.exts.size == 2 && b.exts.size == 1 then
    let m := a.exts[0]!
    let k := a.exts[1]!
    let mut out : Array α := Array.mkEmpty m
    for i in [:m] do
      let mut s := z
      for p in [:k] do
        s := add s (mul a.data[i + m * p]! b.data[p]!)
      out := out.push s
    return { lbs := #[1], exts := #[m], data := out }
  else
    let k := a.exts[0]!
    let n := b.exts[1]!
    let mut out : Array α := Array.mkEmpty n
    for j in [:n] do
      let mut s := z
      for p in [:k] do
        s := add s (mul a.data[p]! b.data[p + k * j]!)
      out := out.push s
    return { lbs := #[1], exts := #[n], data := out }

/-- Fortran `spread(source, dim, ncopies)` for an array `source`. -/
def spread {α : Type} [Inhabited α] (a : Arr α) (dim ncopies : Int) : Arr α := Id.run do
  let d := (dim - 1).toNat
  let nc := ncopies.toNat
  let mut newExts : Array Nat := Array.mkEmpty (a.exts.size + 1)
  for i in [:d] do newExts := newExts.push a.exts[i]!
  newExts := newExts.push nc
  for i in [d:a.exts.size] do newExts := newExts.push a.exts[i]!
  let mut total : Nat := 1
  for e in newExts do total := total * e
  let mut out : Array α := Array.mkEmpty total
  for flat in [:total] do
    let mut rem := flat
    let mut srcOff : Nat := 0
    let mut srcStride : Nat := 1
    for k in [:newExts.size] do
      let e := newExts[k]!
      let idx := rem % e
      rem := rem / e
      if k != d then
        srcOff := srcOff + idx * srcStride
        srcStride := srcStride * e
    out := out.push a.data[srcOff]!
  return { lbs := Array.replicate newExts.size 1, exts := newExts, data := out }

/-- 1-based coordinates of the first element maximizing/minimizing according to
`better cur cand` (returns all zeros if no element is selected). -/
def extremaLoc {α : Type} [Inhabited α] (better : α → α → Bool)
    (a : Arr α) (mask : Option (Arr Bool) := none) : Array Int := Id.run do
  let rank := a.exts.size
  let mut found := false
  let mut bestIdx : Nat := 0
  let mut bestVal : α := default
  for i in [:a.data.size] do
    let ok := match mask with
      | none => true
      | some m => m.data.getD i false
    if ok then
      let v := a.data[i]!
      if !found || better bestVal v then
        found := true
        bestIdx := i
        bestVal := v
  if !found then
    return Array.replicate rank (0 : Int)
  let mut coords : Array Int := Array.mkEmpty rank
  let mut rem := bestIdx
  for k in [:rank] do
    let e := a.exts[k]!
    coords := coords.push (Int.ofNat (rem % e + 1))
    rem := rem / e
  return coords

/-- Reduce `a` along 1-based dimension `dim` using `(init, f)`, removing that
dimension from the result shape. When `mask` is `some m`, only elements where
`m` is `true` contribute. -/
def reduceDim {α β : Type} [Inhabited α] (init : β) (f : β → α → β)
    (a : Arr α) (dim : Int) (mask : Option (Arr Bool) := none) : Arr β := Id.run do
  let d := (dim - 1).toNat
  let mut newExts : Array Nat := Array.mkEmpty (a.exts.size - 1)
  for i in [:a.exts.size] do
    if i != d then newExts := newExts.push a.exts[i]!
  let mut total : Nat := 1
  for e in newExts do total := total * e
  let dExt := a.exts.getD d 0
  let mut out : Array β := Array.mkEmpty total
  for flat in [:total] do
    let mut rem := flat
    let mut baseOff : Nat := 0
    let mut stride : Nat := 1
    let mut dStride : Nat := 1
    for k in [:a.exts.size] do
      let e := a.exts[k]!
      if k == d then
        dStride := stride
      else
        let idx := rem % e
        rem := rem / e
        baseOff := baseOff + idx * stride
      stride := stride * e
    let mut acc := init
    for j in [:dExt] do
      let off := baseOff + j * dStride
      let ok := match mask with
        | some m => m.data.getD off false
        | none => true
      if ok then
        acc := f acc a.data[off]!
    out := out.push acc
  return { lbs := Array.replicate newExts.size 1, exts := newExts, data := out }

/-- Reduce `a` along 1-based dimension `dim` seeding from the first selected
element, returning `emptyVal` when a slice has no selected elements (used by
`maxval` / `minval`). -/
def reduceDim1 {α : Type} [Inhabited α] (emptyVal : α) (f : α → α → α)
    (a : Arr α) (dim : Int) (mask : Option (Arr Bool) := none) : Arr α :=
  let res := reduceDim (none : Option α)
    (fun acc x => match acc with | none => some x | some v => some (f v x))
    a dim mask
  { res with data := res.data.map (·.getD emptyVal) }

/-- 1-based index along 1-based dimension `dim` of the first element maximizing
or minimizing according to `better cur cand` (0 when a slice has no selected
elements). -/
def extremaLocDim {α : Type} [Inhabited α] (better : α → α → Bool)
    (a : Arr α) (dim : Int) (mask : Option (Arr Bool) := none) : Arr Int := Id.run do
  let d := (dim - 1).toNat
  let mut newExts : Array Nat := Array.mkEmpty (a.exts.size - 1)
  for i in [:a.exts.size] do
    if i != d then newExts := newExts.push a.exts[i]!
  let mut total : Nat := 1
  for e in newExts do total := total * e
  let dExt := a.exts.getD d 0
  let mut out : Array Int := Array.mkEmpty total
  for flat in [:total] do
    let mut rem := flat
    let mut baseOff : Nat := 0
    let mut stride : Nat := 1
    let mut dStride : Nat := 1
    for k in [:a.exts.size] do
      let e := a.exts[k]!
      if k == d then
        dStride := stride
      else
        let idx := rem % e
        rem := rem / e
        baseOff := baseOff + idx * stride
      stride := stride * e
    let mut found := false
    let mut bestPos : Int := 0
    let mut bestVal : α := default
    for j in [:dExt] do
      let off := baseOff + j * dStride
      let ok := match mask with
        | some m => m.data.getD off false
        | none => true
      if ok then
        let v := a.data[off]!
        if !found || better bestVal v then
          found := true
          bestPos := Int.ofNat (j + 1)
          bestVal := v
    out := out.push bestPos
  return { lbs := Array.replicate newExts.size 1, exts := newExts, data := out }

end Arr

end LFortran
