import LFortran.Runtime

/-!
# Design check: the template shape for all of Fortran

Hand-written: what `lfortran --backend=lean` is to emit for the Fortran in
the comments. Every procedure is a template over `K : Kinds` and asks only
for the class it needs. `main` is the single instantiation at
`LFortran.machine`. `Demo/DesignProof.lean` instantiates the same
templates at `ℤ`/`ℝ` with an `FPModel`.

Each feature below is a pattern the generator repeats:

* kinds: `integer(1/2/4/8)` → `K.I1/I2/I4/I8`, `real(4/8)` → `K.R4/R8`;
  conversions between kinds go through `FInt.toInt`/`FReal.ofInt`,
  `FKinds.r4to8`/`r8to4`;
* `logical` → `Bool`, `character` → `String` (not kind-generic);
* arrays → `Arr α`, derived types → `structure T (K : Kinds)`,
  `complex` → `Cplx K.R8`;
* `intent(out)`/`intent(inout)` → returned in a tuple;
* module variables → one record `Globals K`, threaded as `StateM`/`StateT`;
* math intrinsics → `[FKindsMath K]`, `print` → `[FKindsIO K]` and `IO`;
* `recursive` → `partial def` (needs `Inhabited`, which `FKinds` gives);
* `stop`/`error stop` → `halt`;
* every call of a template passes `(K := K)`: an argument such as
  `FReal.ofInt 1` does not determine `K`.
-/

open LFortran

namespace design_mod

/-- `type :: point; real(8) :: x, y; end type` -/
structure point (K : Kinds) where
  x : K.R8
  y : K.R8

/-- `complex(8)`: a pair of `K.R8`. (Runtime library in the real backend.) -/
structure Cplx (α : Type) where
  re : α
  im : α

def Cplx.mul {α : Type} [FReal α] (a b : Cplx α) : Cplx α :=
  ⟨FReal.sub (FReal.mul a.re b.re) (FReal.mul a.im b.im),
   FReal.add (FReal.mul a.re b.im) (FReal.mul a.im b.re)⟩

/-- Module variables: `integer :: ncalls = 0`, `real(8) :: total = 0`. -/
structure Globals (K : Kinds) where
  ncalls : K.I4
  total : K.R8

def Globals.init {K : Kinds} [FKinds K] : Globals K :=
  ⟨FInt.ofInt 0, FReal.zero⟩

/-- `real(8) function mix(a, b, i)`, `real(4) :: a`, `integer(8) :: i`:
mixed kinds. -/
def mix {K : Kinds} [FKinds K] (a : K.R4) (b : K.R8) (i : K.I8) : K.R8 := Id.run do
  let mut r : K.R8 := FReal.zero
  r := FReal.add (FKinds.r4to8 a) (FReal.mul b (FReal.ofInt (FInt.toInt i)))
  return r

/-- `function hyp(x, y)` uses `sqrt`: needs `FKindsMath`. -/
def hyp {K : Kinds} [FKindsMath K] (x y : K.R8) : K.R8 :=
  FRealMath.sqrt (FReal.add (FReal.mul x x) (FReal.mul y y))

/-- `function norm1(v)`, `real(8) :: v(:)`: arrays and their bounds. -/
def norm1 {K : Kinds} [FKinds K] (v : Arr K.R8) : K.R8 := Id.run do
  let mut s : K.R8 := FReal.zero
  for i in FInt.range (FInt.ofInt (Arr.lbound v 1) : K.I4) (FInt.ofInt (Arr.ubound v 1)) do
    s := FReal.add s (FReal.abs (Arr.get v #[FInt.toInt i]))
  return s

/-- `function dist(p)`, `type(point) :: p`: derived types. -/
def dist {K : Kinds} [FKindsMath K] (p : point K) : K.R8 :=
  hyp (K := K) p.x p.y

/-- `subroutine swap(a, b)`, `intent(inout)`: results returned as a tuple. -/
def swap {K : Kinds} [FKinds K] (a b : K.R8) : K.R8 × K.R8 := Id.run do
  let mut a := a
  let mut b := b
  let t := a
  a := b
  b := t
  return (a, b)

/-- `recursive function fact(n)`, `integer(8)`. -/
partial def fact {K : Kinds} [FKinds K] (n : K.I8) : K.I8 :=
  if FInt.le n (FInt.ofInt 1) then FInt.ofInt 1
  else FInt.mul n (fact (K := K) (FInt.sub n (FInt.ofInt 1)))

/-- `subroutine record(x)` updates module variables: `StateM (Globals K)`. -/
def record {K : Kinds} [FKinds K] (x : K.R8) : StateM (Globals K) Unit :=
  modify fun g => { g with ncalls := FInt.add g.ncalls (FInt.ofInt 1),
                            total := FReal.add g.total x }

/-- `function divmod(a, b)`: Fortran integer `/` and `mod`. -/
def divmod {K : Kinds} [FKinds K] (a b : K.I4) : K.I4 × K.I4 :=
  (FInt.div a b, FInt.mod a b)

end design_mod

open design_mod in
/-- `program design`: prints, uses module state, stops on error. -/
def design_main {K : Kinds} [FKindsIO K] : StateT (Globals K) IO Unit := do
  let x : K.R8 := FReal.lit 3 (-1)          -- 1.5_8
  let y : K.R4 := FReal.lit 1 (-1)          -- 0.5_4
  let r := mix (K := K) y x (FInt.ofInt 2)
  IO.println s!"mix = {FKindsIO.fmtR8 r}"
  if !FReal.beq r (FReal.lit 7 (-1)) then LFortran.halt 1     -- 0.5 + 1.5*2 = 3.5
  let h := dist (K := K) ⟨FReal.ofInt 3, FReal.ofInt 4⟩
  IO.println s!"dist = {FKindsIO.fmtR8 h}"
  if !FReal.beq h (FReal.ofInt 5) then LFortran.halt 2
  let v : Arr K.R8 := Arr.make #[1] #[3] FReal.zero
  let v := (v.set #[1] (FReal.ofInt 1)).set #[2] (FReal.neg (FReal.ofInt 2))
  let v := v.set #[3] (FReal.ofInt 3)
  if !FReal.beq (norm1 (K := K) v) (FReal.ofInt 6) then LFortran.halt 3
  let (a, b) := swap (K := K) (FReal.ofInt 1) (FReal.ofInt 2)
  if !(FReal.beq a (FReal.ofInt 2) && FReal.beq b (FReal.ofInt 1)) then LFortran.halt 4
  if !FInt.beq (fact (K := K) (FInt.ofInt 10)) (FInt.ofInt 3628800) then LFortran.halt 5
  let z : Cplx K.R8 := Cplx.mul ⟨FReal.zero, FReal.ofInt 1⟩ ⟨FReal.zero, FReal.ofInt 1⟩
  if !FReal.beq z.re (FReal.neg (FReal.ofInt 1)) then LFortran.halt 6
  let (q, m) := divmod (K := K) (FInt.ofInt (-7)) (FInt.ofInt 2)
  if !(FInt.beq q (FInt.ofInt (-3)) && FInt.beq m (FInt.ofInt (-1))) then LFortran.halt 7
  for i in FInt.range (FInt.ofInt 1 : K.I4) (FInt.ofInt 4) do
    modify fun g => (record (K := K) (FReal.ofInt (FInt.toInt i))).run g |>.2
  let g ← get
  if !(FInt.beq g.ncalls (FInt.ofInt 4) && FReal.beq g.total (FReal.ofInt 10)) then
    LFortran.halt 8
  IO.println "design ok"

/-- The single instantiation. -/
def main : IO UInt32 :=
  LFortran.run ((design_main (K := LFortran.machine)).run' design_mod.Globals.init)
