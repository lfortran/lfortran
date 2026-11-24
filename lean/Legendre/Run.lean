import Legendre.Generated

/-!
Run the generated template `legendre_poly` at `LFortran.machine`
(`Int32`, `Float`), the instantiation `legendre_float_correct` is about.

`P_n(1) = 1` and `P_2(1/2) = -1/8`. Exit 1 if the `Float` value disagrees.
-/

open LFortran

def step (n : Int32) (x : Float) : Float :=
  legendre_mod.legendre_poly (K := machine) n x

def close (got expected : Float) : Bool :=
  let d := got - expected
  d.abs < 1e-12

def main : IO UInt32 := do
  -- P_n(1) = 1 for every n.
  for n in [0, 1, 2, 5, 10, 20, 50, 100] do
    let y := step (Int32.ofInt n) 1.0
    if !close y 1.0 then
      IO.eprintln s!"P_{n}(1) = {y}, expected 1"
      return 1
  -- P_2(x) = (3x^2 - 1) / 2, so P_2(1/2) = -1/8.
  let y2 := step (Int32.ofInt 2) 0.5
  if !close y2 (-0.125) then
    IO.eprintln s!"P_2(1/2) = {y2}, expected -0.125"
    return 1
  IO.println "legendre Float run ok"
  return 0
