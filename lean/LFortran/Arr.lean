/-!
# Fortran arrays

A Fortran array is a column-major `Arr α` with the lower bound of each
dimension stored beside the data. `α` is the element type, for example
`K.R8` in a template. Arrays are values: assignment copies.
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
  { lbs := a.lbs, exts := a.exts, data := a.data.map f }

def zipWith {α β γ : Type} (f : α → β → γ) (a : Arr α) (b : Arr β) : Arr γ :=
  { lbs := a.lbs, exts := a.exts, data := Array.zipWith f a.data b.data }

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

def size {α : Type} (a : Arr α) : Int :=
  Int.ofNat (a.exts.foldl (fun n e => n * e) 1)

def lbound {α : Type} (a : Arr α) (dim : Int) : Int :=
  a.lbs[dim.toNat - 1]!

def ubound {α : Type} (a : Arr α) (dim : Int) : Int :=
  a.lbs[dim.toNat - 1]! + Int.ofNat (a.exts[dim.toNat - 1]!) - 1

end Arr

end LFortran
