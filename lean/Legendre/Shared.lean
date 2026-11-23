import LFortran.Arith

/-!
# `legendre_poly`, one definition

Prototype of the Lean the backend should emit for `lean/legendre.f90`.
`lfortran` does not emit this yet. The runner and the proof both import this
file; neither has a second copy of the recurrence.
-/

namespace legendre_shared

def legendre_poly {ι α : Type} [LFortran.FInt ι] [DecidableEq ι] [LFortran.FReal α]
    (n : ι) (x : α) : α := Id.run do
  let mut p : α := LFortran.FReal.zero
  let mut a : α := LFortran.FReal.zero
  let mut b : α := LFortran.FReal.zero
  let mut c : α := LFortran.FReal.zero
  a := LFortran.FReal.ofInt 1
  b := x
  if n = LFortran.FInt.ofInt 0 then
    p := a
    return p
  for k in LFortran.FInt.range (LFortran.FInt.ofInt 2) n do
    c := LFortran.FReal.div
      (LFortran.FReal.sub
        (LFortran.FReal.mul
          (LFortran.FReal.mul
            (LFortran.FReal.ofInt (LFortran.FInt.toInt
              (LFortran.FInt.sub (LFortran.FInt.mul (LFortran.FInt.ofInt 2) k)
                (LFortran.FInt.ofInt 1))))
            x)
          b)
        (LFortran.FReal.mul
          (LFortran.FReal.ofInt (LFortran.FInt.toInt
            (LFortran.FInt.sub k (LFortran.FInt.ofInt 1))))
          a))
      (LFortran.FReal.ofInt (LFortran.FInt.toInt k))
    a := b
    b := c
  p := b
  return p

end legendre_shared
