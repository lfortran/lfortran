# Verifying Fortran code with Lean (LFortran prototype)

This directory is a working prototype of the workflow discussed in
[Verifying Fortran code with Lean](https://fortran-lang.discourse.group/t/verifying-fortran-code-with-lean/11327/3):
a Fortran function is translated to Lean by LFortran, a human reviews a short
mathematical specification, and Lean checks a proof that the translated code
meets it, including floating-point rounding. The example is the Legendre
polynomial `legendre_poly(n, x)`.

Run everything (accuracy test, translation check, proof check) from the
repository root:

```bash
pixi run -e llvm11 bash lean/run.sh
```

There is one Lean backend, and it emits only **templates**. Every Fortran
procedure becomes one Lean definition generic over `K : Kinds`, a bundle of
one Lean type per Fortran kind (`K.I4` for `integer(4)`, `K.R8` for
`real(8)`, ...) with the arithmetic as type classes (`runtime/LFortran/Arith.lean`).
The same definition is used in two ways:

* **run**: at `LFortran.machine` (`Int8`…`Int64`, `Float32`, `Float`;
  `runtime/LFortran/Runtime.lean`). A program's `main` is a single line,
  `def main : IO UInt32 := LFortran.run (prog (K := LFortran.machine))`.
* **prove**: at `proofKinds` (`ℤ` and `ℝ`) with an arithmetic model,
  `FKinds.ofModel fp fp` for an `FPModel` or `FKinds.ofNoGuard m m` for a
  `NoGuardModel` (`proof/LFortranProof/Arith/Model.lean`). `FPModel.exact` is exact real
  arithmetic, which gives the value the code computes with infinitely
  precise operations.

`lfortran --show-lean file.f90` and `lfortran --backend=lean file.f90` emit
the same text (only the header comment differs); for a program it ends with
the one-line `main`, which `lean --run` executes. Integration tests labeled
`lean` are run that way, locally and in the Linux LLVM 11 Quick job:

```bash
pixi run -e llvm11 python lean/run_integration_tests.py
```

The runner checks the exit code, like every integration test; the test
program checks its own results. `lean_sin_kernels_01`, `lean_names_01` and
`lean_arrays_01` are the only tests labeled `lean` while the template backend
grows; `--all`
tries every integration test and `--apply` updates the labels.

The backend accepts the subset the examples need: module functions and
subroutines with `integer`/`real`/`logical` scalar and array arguments,
locals (including arrays), assignments (to variables, array elements and
whole arrays), `if`, unit-step `do` loops, `return`, calls (callees are
emitted first; self-recursion is `partial`), `+ - * /`, comparisons, integer/real conversions, `int`, `abs`,
`sign`, `mod`, `modulo`, the math intrinsics (`sin`, `sqrt`, ...; these need
`FKindsMath` and are not modelled in proofs), and in a program also `print`,
`stop` and `error stop`. Anything else is rejected with an error rather than
translated approximately. Fortran names that are Lean keywords are emitted
as `«name»`. Real literals are emitted exactly (`-0.0` keeps its sign,
which `sign(a, b)` observes through `FReal.signbit`), as `m · 2^e`
(`FReal.lit m e`), or `FReal.ofInt n` for integers. `print` uses Lean's
`toString`, which is shorter than Fortran list-directed output.

**Procedures, arguments and arrays** use value semantics, so every template
is a pure function and proofs stay about functions:

* A Fortran array is an `Arr α` value (`runtime/LFortran/Arr.lean`):
  column-major data with the bounds of each dimension. Element access and
  update are `Arr.get a #[i, j]` and `a := Arr.set a #[i, j] v`; Lean updates
  an unshared array in place. `size`, `lbound` and `ubound` read the stored
  bounds, and `a = b` and `a = v` assign whole arrays.
* A subroutine returns its `intent(out)` and `intent(inout)` arguments, in
  order (a tuple if there are several), and `call s(a, b)` assigns them back
  to the actual arguments (copy-in/copy-out). This is faithful for conforming
  programs because Fortran forbids aliasing between a modified dummy argument
  and anything else the procedure accesses. An `intent(out)` scalar is not
  passed in; it starts as a fresh local.
* A dummy array takes its declared bounds on entry (`Arr.reshape` for an
  explicit-shape `a(n)`, which reinterprets the data as sequence association
  does; `Arr.rebase` for an assumed-shape `a(:)`); after the call, the actual
  argument keeps its own bounds and takes the data back (`Arr.copyBack`).
* Not yet translated: module variables (planned as an explicit state record
  passed in and returned), derived types, `complex`, functions with
  `intent(out)`/`intent(inout)` arguments, array sections and array
  expressions, `allocatable`/`pointer`, and `print`/`stop` inside
  procedures (planned through a monad class only where needed).

`examples/Demo/Design.lean` and `examples/Demo/DesignProof.lean` are a hand-written
spike of these and further features (module state, derived types, arrays,
complex, `intent(out)`), each run at `machine` and instantiated at
`proofKinds`.

The directory has three Lake packages:

| Package | Contents | Depends on |
|---|---|---|
| `runtime/` (`lfortran_runtime`) | what generated code imports: `LFortran.Arith` (the interface), `LFortran.Arr` (arrays), `LFortran.Runtime` (the executable instance `machine`) | nothing (no Mathlib) |
| `proof/` (`lfortran_proof`) | the proof library `LFortranProof.*`: arithmetic models, proof instances, error propagation, `Float` is a `VModel`, loop lemmas | `runtime/`, Mathlib |
| `examples/` (`lfortran_examples`) | verified programs: Fortran sources, generated templates, specifications, proofs, and their test scripts | `proof/` |

Run the Lean part alone (no LFortran build needed):

```bash
cd lean/examples
bash fetch_deps.sh   # first time: Lean dependencies and prebuilt Mathlib files
lake build --wfail   # builds runtime/, proof/ and the examples
```

| File | Role | Written by |
|---|---|---|
| `examples/legendre.f90`, `examples/sin_kernels.f90`, `examples/horner.f90` | the implementations | programmer (or AI) |
| `examples/legendre_accuracy.f90` | measured accuracy of `legendre_poly` | programmer |
| `examples/Legendre/Generated.lean`, `examples/SinKernels/Generated.lean` | the implementations in Lean (templates) | `lfortran --show-lean` |
| `runtime/LFortran/Arith.lean` | `FInt`, `FReal`, `Kinds`, `FKinds` | human, reviewed |
| `runtime/LFortran/Runtime.lean` | the executable instance `machine` | human, reviewed |
| `proof/LFortranProof/Arith/Model.lean` | the proof instances `proofKinds`, `FKinds.ofModel`, `FKinds.ofNoGuard` | human, reviewed |
| `examples/Legendre/Spec.lean` | what `legendre_poly` must satisfy, against Mathlib's Legendre polynomial | human, reviewed |
| `proof/LFortranProof/Prelude.lean` | the arithmetic model `FPModel`, and `FPModel.exact` | human, reviewed |
| `proof/LFortranProof/NoGuard.lean` | the weaker model `NoGuardModel` (no guard digit), implied by `FPModel` | human, reviewed |
| `proof/LFortranProof/VModel.lean` | `VModel`: the same error bounds on any carrier (`ℝ` in a proof, `Float` in the run), read through values | human, reviewed |
| `proof/LFortranProof/Models/*.lean` | proofs that IEEE binary64 and textbook models are `FPModel`s, and the Cray model (not trusted) | proofs (AI), checked by Lean |
| `proof/LFortranProof/FloatExact.lean`, `proof/LFortranProof/PiBounds.lean`, `proof/pi_witness.py` | exact `Float` `-`/`*`, Sterbenz's lemma; `π` to 32 decimals | proofs (AI), checked by Lean |
| `proof/LFortranProof/PolyBound.lean`, `proof/LFortranProof/SinTaylor.lean`, `proof/LFortranProof/ErrorProp.lean` | kernel-checked polynomial bounds, Taylor bounds for `sin`, forward error propagation in a `VModel` | proofs (AI), checked by Lean |
| `proof/LFortranProof/Loop.lean`, `proof/LFortranProof/Arr.lean` | loop invariants for generated `do` loops (`range_foldl_inv`); array element access | proofs (AI), checked by Lean |
| `examples/Legendre/Proof*.lean`, `examples/Legendre/Corollaries.lean` | the Legendre proofs | proofs (AI), checked by Lean |
| `examples/SinKernels/Proof/*.lean` | the sin kernel proofs | proofs (AI), checked by Lean |
| `examples/Legendre/Run.lean` | run `legendre_poly` at `machine` | human |
| `proof/LFortranProof/FloatAdd*.lean`, `proof/LFortranProof/FloatOps.lean`, `proof/LFortranProof/FloatModel.lean` | Lean's `Float` `+ − × ÷` meet the `FPModel` bounds; `floatModel` (not trusted) | proofs (AI), checked by Lean |
| `proof/LFortranProof/FloatVal.lean`, `proof/LFortranProof/FloatVModel.lean` | Lean's `Float` (and `Int32`) is a `VModel`: every operation the templates use, literals included | proofs (AI), checked by Lean |
| `examples/Legendre/FloatCorrect.lean`, `examples/SinKernels/FloatRun.lean` | the `Float` runs meet the `Float` specifications: the general theorems at `Float` | proofs (AI), checked by Lean |
| `examples/SinKernels/Spec.lean` | what the sin kernels' `Float` runs must satisfy | human, reviewed |
| `examples/Horner/Spec.lean` | what `horner`'s `Float` run must satisfy | human, reviewed |
| `examples/Horner/Proof*.lean`, `examples/Horner/FloatRun.lean` | the Horner proofs and its `Float` run | proofs (AI), checked by Lean |
| `examples/check_float_runs.py`, `examples/float_runs_check.f90` | bit-for-bit and bound tests of the `Float` runs | human |

## 1. The idea

Tests check a program on finitely many inputs. A proof covers all of them:
there are about `2^63` doubles in `[-1, 1]`, so `legendre_poly` cannot be
tested exhaustively even for a single `n`, but the theorem below holds for
every one of them and every `0 <= n <= 100`.

The roles are split so that a human only has to read short, mathematical
statements:

1. **The implementation** is ordinary Fortran, written by a person or by AI.
2. **The translation** to Lean is produced mechanically by the compiler from
   the actual code, not written by hand or by AI. In CI it is regenerated and
   compared with the checked-in file, so it cannot drift from the Fortran.
3. **The specification** states what the code must do: the mathematical
   function, the supported inputs, the required accuracy, and the assumptions
   about the arithmetic. This is what a human reviews and approves.
4. **The proof** that the translation meets the specification can be produced
   by AI. Nobody needs to read it: Lean's kernel checks it, and a proof that
   does not establish exactly the specification is rejected.

The function's signature in Lean follows from the Fortran signature; it is not
part of the specification. The specification is a property of any function of
that type, `LegendreSpec : (FPModel → ℤ → ℝ → ℝ) → Prop`, and the theorem
`legendre_correct : LegendreSpec legendre_fortran` connects it to the
generated code.

## 2. Worked example: `legendre_poly`

### 2.1 Implementation and measured accuracy

`legendre.f90` evaluates Legendre polynomials by Bonnet's recurrence
`P_0 = 1`, `P_1 = x`, `k P_k = (2k-1) x P_{k-1} - (k-1) P_{k-2}`:

```fortran
a = 1.0_dp
b = x
if (n == 0) then
    p = a
    return
end if
do k = 2, n
    c = ((2*k-1)*x*b - (k-1)*a)/k
    a = b
    b = c
end do
p = b
```

`legendre_accuracy.f90` compares it with a double-double (about 106-bit)
evaluation of the same recurrence on 20,000 points per `n`, uniform in
`[-1, 1]` and clustered near `±1`. The largest error for `n <= 100` is
**1.78e-13** (about 1600 units of `2^-53`), at `n = 99`, `x ≈ 1 - 7·10^-11`.
GFortran and LFortran give identical results.

### 2.2 Translation to Lean

`lfortran --show-lean examples/legendre.f90` produces `examples/Legendre/Generated.lean`:

```lean
def legendre_poly {K : Kinds} [FKinds K] (n : K.I4) (x : K.R8) : K.R8 := Id.run do
  let mut p : K.R8 := FReal.zero
  let mut a : K.R8 := FReal.zero
  let mut b : K.R8 := FReal.zero
  let mut c : K.R8 := FReal.zero
  a := FReal.ofInt 1
  b := x
  if FInt.beq n (FInt.ofInt 0) then
    p := a
    return p
  for k in FInt.range (FInt.ofInt 2 : K.I4) n do
    c := FReal.div (FReal.sub (FReal.mul (FReal.mul (FReal.ofInt (FInt.toInt (FInt.sub (FInt.mul (FInt.ofInt 2) k) (FInt.ofInt 1)))) x) b) (FReal.mul (FReal.ofInt (FInt.toInt (FInt.sub k (FInt.ofInt 1)))) a)) (FReal.ofInt (FInt.toInt k))
    a := b
    b := c
  p := b
  return p
```

The template is one definition with three uses. `examples/Legendre/Run.lean` runs
`legendre_poly (K := LFortran.machine)` (`Int32`, `Float`). The proof uses
`legendre_fortran fp = @legendre_poly proofKinds (FKinds.ofModel fp fp)`, in
which every real operation is the corresponding `FPModel` field, and
`legendre_exact = legendre_fortran FPModel.exact`, the same code in exact
arithmetic (`examples/Legendre/Proof.lean`).

The translation preserves the operation tree chosen by LFortran's ASR rather
than replacing floating-point expressions by exact real arithmetic:

* `real(8)` values are of type `K.R8`, and **every operation is rounded**: in a
  proof `FReal.mul` is `fp.mul`. The ASR groups `(2*k-1)*x*b` as
  `((2*k-1)*x)*b`, with the integer `2*k-1` converted to real
  (`FReal.ofInt`, in a proof `fp.round`) and each product rounded. Literals
  such as `1.0_dp` are rounded too (`FReal.ofInt 1`, or `FReal.lit m e` for
  the exact value `m · 2^e` of a non-integer constant).
* `integer` is `K.I4`: exact `ℤ` in the proof (overflow is out of scope there),
  `Int32` when run. Comparisons return `Bool` (`FInt.beq`, `FReal.lt`, ...).
* The arithmetic `fp : FPModel` is a parameter (Section 3), so one proof covers
  every arithmetic satisfying the model, including IEEE binary64.
* The backend runs on LFortran's internal representation (ASR) before any
  lowering or optimization, and rejects what it does not support rather
  than translating it approximately.

Fortran permits mathematically equivalent rearrangements subject to its rules
on parentheses, even when floating-point results differ; it does not require
this particular evaluation order (see the
[Fortran discussion of mathematical equivalence](https://fortran.bcs.org/2012/math_equiv2.pdf)).
The theorem therefore applies to this operation tree. Applying it to an
executable requires that compilation preserve the tree's values under the
stated arithmetic assumptions (Sections 5 and 6), not merely that fast-math
be disabled.

### 2.3 The specification

`examples/Legendre/Spec.lean` is the part a human reads:

```lean
noncomputable def legendreP (n : ℕ) (x : ℝ) : ℝ :=
  Polynomial.aeval ((1 - x) / 2) (Polynomial.shiftedLegendre n)

def LegendreSpec (f : FPModel → ℤ → ℝ → ℝ) : Prop :=
  ∀ (fp : FPModel), fp.u ≤ 1 / 2 ^ 53 → fp.η ≤ 1 / 2 ^ 1000 → 2 ^ 10 ≤ fp.M →
  ∀ (n : ℤ) (x : ℝ), x ∈ fp.F → 0 ≤ n → n ≤ 100 → |x| ≤ 1 →
    |f fp n x - legendreP n.toNat x| ≤ 1 / 10 ^ 11
```

It contains four decisions to review:

* **The mathematical function.** `legendreP` is Mathlib's Legendre
  polynomial: the shifted Legendre polynomial `shiftedLegendre n`, defined by
  its explicit coefficients `(-1)^k C(n,k) C(n+k,n)`, evaluated at
  `(1 - x)/2`. It is independent of the algorithm: the proof establishes
  Bonnet's recurrence for it.
* **The inputs.** `0 <= n <= 100`, and `x` is a floating-point number
  (`x ∈ fp.F`, for binary64 "a finite double") with `|x| <= 1`. Nothing is
  claimed outside this domain.
* **The accuracy.** Absolute error at most `10^-11` (Section 2.4).
* **The arithmetic.** Any `FPModel` with unit roundoff `u <= 2^-53`,
  underflow error `η <= 2^-1000` and range `M >= 2^10`. The relative bound
  matches binary64 round-to-nearest, but the underflow allowance is looser
  and the required range smaller. These hypotheses also admit arithmetic
  such as flush-to-zero (Section 4).

### 2.4 Choosing the tolerance

A requested accuracy can be false, unproved, or proved. Its status depends on
whether it is required only for IEEE binary64 or for every admissible
`FPModel`:

* **1e-14 is false.** The measured error 1.78e-13 already exceeds it, so no
  proof of that accuracy for the measured implementation can exist.
* **1e-12 is unproved here for IEEE binary64.** It holds on all tested points,
  but that is not a proof over all inputs. A sharper proof could exploit
  IEEE-specific facts that the abstract model does not require.
* **1e-12 is false for the full `FPModel` specification.** The model admits
  artificial arithmetics that exceed this tolerance, as shown below.
* **1e-11 is proved for every admissible `FPModel`.** The proof gives about
  9.9e-12; no optimality claim is made for this bound.

For an analytic counterexample to the universal `1e-12` bound, take `F = ℝ`,
`u = 2^-53`, `η = 0`, `M = 2^10`, exact rounding/conversions and addition,
and let multiplication, subtraction and division return `q·exact`, where
`q = 1+u`. These operations satisfy the `FPModel` error and closure bounds.
This is not IEEE binary64. At `x = 1`, the generated code has `y_0 = y_1 = 1`
and

```
k·y_k = (2k-1)q^4·y_(k-1) - (k-1)q^3·y_(k-2).
```

Writing `Δ_k = y_k - y_(k-1)` gives

```
k·Δ_k = (k-1)q^3·Δ_(k-1)
        + ((2k-1)q^4 - (k-1)q^3 - k)·y_(k-1).
```

For `k >= 2`, the coefficient of `y_(k-1)` is at least `(5k-1)u`.
Starting with `Δ_1 = 0`, induction gives `y_k >= 1` and `Δ_k >= 2ku`
for `k >= 2`. Thus `y_100 - P_100(1) >= 10098u > 1e-12`.
This calculation explains the distinction; it is not an additional
Lean-checked theorem in this directory.

A proof of a weaker statement does not establish a stronger one: checking
`9.9e-12 <= 1e-12` fails. Failure of that argument alone would mean "not
proven", not "false". The counterexample above separately rules out the
stronger universal specification, while leaving the IEEE-specific conjecture
open.

### 2.5 How the proof works

The proof has two parts, following the
[Discourse discussion](https://fortran-lang.discourse.group/t/verifying-fortran-code-with-lean/11327/15):
the code in exact arithmetic is the mathematical function, and rounding
changes the result by a bounded amount.

**Exact arithmetic** (`legendre_exact_eq`): `legendre_exact n x =
legendreP n x` for every `n ≥ 0` and every real `x`.
`examples/Legendre/Proof/Bonnet.lean` proves the coefficient identity
`(n+2) b_{n+2,k} + (n+1) b_{n,k} = (2n+3)(b_{n+1,k} + 2 b_{n+1,k-1})` for
`b_{m,k} = C(m,k) C(m+k,m)` (through factorials), lifts it to Bonnet's
recurrence for `shiftedLegendre`, and concludes that the recurrence `P` the
loop evaluates equals `legendreP` (`P_eq_legendreP`).

**Rounding** (`legendre_rounding`): the computed value is within `89201 U`
of `P_n(x)`, the exact-arithmetic value. It is done once, for every `VModel`
(Section 3: the template on any carrier, `ℝ` with an arithmetic model or
Lean's `Float`) without a guard digit, with a ceiling `U ≤ 2^-40` on the unit
roundoff and `E = U/10^6` on the underflow term. Its corollaries are
`legendre_ng_correct : LegendreSpecNoGuard legendre_ng` (every
`NoGuardModel`), `legendre_correct` (every `FPModel`, `U = 2^-53`, `10^-11`,
through `FPModel.toNoGuard`), and `legendre_float_spec` (the `Float` run,
Section 5). The bounds use `U` and `E`, not the individual model's `u` and
`η`. The rounding part has four steps:

1. **Loop to recursion.** The `for` loop of the generated code is rewritten as
   a recursive sequence `y_m` of computed values, on any carrier whose
   `integer(4)` is exact in range (`IntExact`: `ℤ`, or `Int32` for `n ≤ 100`).
   This uses the generic loop invariant `range_foldl_inv`
   (`proof/LFortranProof/Loop.lean`): after `simp` unfolds the template, a
   `do k = lo, hi` loop is a `List.foldl` over `FInt.range lo hi`, and a
   property `P i s` of the state after `i` iterations that holds initially and
   is preserved by iteration `i` (index `lo + i`) holds at the end. Only `lo`
   and `hi` must be in range.
2. **One step of the loop.** Using only the error bound of each of its eight
   rounded operations (five arithmetic operations, three conversions), the
   computed `y_k` differs from the exact recurrence applied to the computed
   `y_{k-1}`, `y_{k-2}` by at most `|r_k| ≤ 18U`, as long as the computed
   values stay within `1 + 10^-6` in magnitude, which the induction
   maintains. The subtraction is bounded through `|a| + |b|`, so the step
   holds without a guard digit. This bound includes the allowed absolute underflow error;
   it is not a bound of `18·u` when `u` is smaller than `U`.
3. **Propagation.** The errors `e_k = y_k - P_k(x)` (`P_k(x)` is the exact
   value) satisfy
   `e_k = ((2k-1) x e_{k-1} - (k-1) e_{k-2})/k + r_k`. Bounding this term by
   term, `|e_k| <= (2k-1)/k |e_{k-1}| + (k-1)/k |e_{k-2}| + |r_k|`, is too
   weak: that recurrence grows like `(1 + √2)^k`, about `10^38` at `k = 100`.
   Instead the proof uses the quadratic form
   `T(p, q) = p^2 + q^2 - 2xpq = (q - xp)^2 + (1 - x^2) p^2`, which does not
   increase along the exact recurrence when `|x| <= 1`. Its square root is a
   seminorm, so each rounding error adds at most `|r_k|` to it, and the errors
   grow only quadratically: `|e_n| <= n·ε_0 + n(n-1)/2·18U` for
   `1 <= n <= 100`, where `ε_0 = U + E` bounds the rounding error of the
   literal `1.0`. The case `n = 0` is handled separately by `|e_0| <= ε_0`.
   The same form also proves `|P_n(x)| <= 1`, which keeps the intermediate
   values (and the per-step errors) bounded.
4. **The numbers.** The largest bound for `0 <= n <= 100` is
   `100(U+E) + 4950·18U ≤ 89201 U`, about `9.9e-12` for `U = 2^-53`, below
   `10^-11`.

The bound is roughly 50 times larger than the measured error. The generic
proof permits worst-case operation errors, allows rounding errors in `1.0`
and the small integer conversions that are exact in IEEE binary64, and uses
conservative propagation inequalities. Rounding errors can be correlated;
the measurements alone do not establish cancellation as the explanation for
the gap. A sharper IEEE-specific proof could exploit exactness and additional
structure.

### 2.6 Second example: fast `sin` kernels

`sin_kernels.f90` has two scalar kernels:

* `kernel_sin(x) = x·S(x²)`, a degree-7 polynomial in `x²` with binary64
  coefficients, for `|x| ≤ π/2`;
* `kernel_fast(x0)`: range reduction `N = int(x0/π_d + sign(1, x0)/2)`
  (`π_d` the double nearest `π`), Cody–Waite reduction
  `r = ((x0 - N p1) - N p2) - N p3` with a three-part `π` (`p1`, `p2` have
  21-bit significands), `r → -r` for odd `N` (`modulo(int(N), 2) == 1`),
  then `kernel_sin(r)`.

Each is proved in two parts, exact arithmetic against `Real.sin` and
rounding. Both rounding proofs are written once, for every `VModel`
(Section 3). `kernel_sin`'s holds with unit roundoff `≤ U` and underflow
error `≤ H` (`U, H ≤ 2^-20`), without a guard digit, so for every
`NoGuardModel` (`kernel_sin_correct_ng`) and for `Float`;
`kernel_fast`'s for every correctly rounded binary64 `VModel` with a guard
digit:

| | exact arithmetic vs `Real.sin` | rounding | combined | the `Float` run |
|---|---|---|---|---|
| `kernel_sin`, `\|x\| ≤ π/2` | `\|x S(x²) - sin x\| ≤ 4.6·10^-16 \|x\|`, relative `≤ 10^-15` (`kernel_sin_approx`, `_rel`) | `≤ \|x\|(6.14 U + 2107 H) + H` (`kernel_sin_rounding`) | `kernel_sin_correct` | `≤ 1.15·10^-15 \|x\| + 2^-1075` (`kernel_sin_spec`) |
| `kernel_fast`, `\|x0\| ≤ 6·10^9` | `≤ 7.3·10^-16` (`kernel_fast_exact_correct`, `\|x0\| ≤ 10^12`) | binary64-specific, see below | `kernel_fast_binary64` | `≤ 2.2·10^-15` (`kernel_fast_spec`) |

The ingredients:

* **Taylor bound** (`proof/LFortranProof/SinTaylor.lean`): `|sin r - r T_n(r²)| ≤
  |r|^(2n+1)/(2n+1)!` for `|r| ≤ 2`, from Mathlib's power series of `sin`
  and the alternating series bound.
* **Polynomial bound** (`proof/LFortranProof/PolyBound.lean`): the difference between
  the kernel's polynomial and the Taylor polynomial has rational
  coefficients; `check` bounds it on an interval by bisection and Taylor
  shifts in exact rational arithmetic, and `check_sound` proves the checker
  correct. The kernel evaluates the check (`decide +kernel`; no
  `native_decide`, no floating point): `|S(z) - T_11(z)| ≤ 4.5·10^-16` on
  `0 ≤ z ≤ 1.571²` (measured maximum `4.22·10^-16`), slightly beyond
  `(π/2)²` because the computed reduced argument can exceed `π/2` by
  rounding.
* **Error propagation** (`proof/LFortranProof/ErrorProp.lean`): `Close V U H v t α κ T`
  (the computed `v`, on any carrier, has a value within `α U + κ H` of `t`,
  `|t| ≤ T`) through each rounded operation. Keeping `H` separate from `U` matters: the literal
  `S8 ≈ 7·10^-13` has an absolute underflow error that the Horner scheme
  multiplies by `z^7`.
* **Choice of `N`** (`chooseN`, `examples/SinKernels/Proof/Reduction.lean`): `int`
  truncates, and the shift by `±1/2` makes `|x0/π_d - N| ≤ 1/2 + ε` in every
  `VModel` in which integers up to `X` convert exactly and negation, `abs`,
  the sign bit and `int` act on values (`VModel.Signed`), so `|r| ≤ 1.571`. Then `sin x0 = (-1)^N sin(x0 - Nπ)`, and negating `r` for
  odd `N` multiplies `sin r` by `(-1)^N` (`fast_core`, `parity_sin`).

`kernel_fast` is proved for binary64 only, because Cody–Waite relies on
binary64 exactness that the generic models do not provide
(`VModel.Binary64`: values are binary64 numbers and an operation whose exact
result is one returns it; Lean's `Float` satisfies it by
`proof/LFortranProof/FloatExact.lean`). `N p1` and `N p2` are exact, and `x0 - N p1` is
exact by Sterbenz's lemma (`sterbenz`). The last two subtractions need the
guard-digit bound `u|x - y|` (`VModel.Guard`); the no-guard bound
`u(|x| + |y|)` would charge the rounding of `N p2 ≈ 10^3` and give `10^-13`. The two remaining subtractions and
`N p3` add at most `3.6·10^-16`, and `p1 + p2 + p3` differs from `π` by
`1.7·10^-31`, which needs `π` to 32 decimals (`proof/LFortranProof/PiBounds.lean`, checked
by Mathlib's `pi_lower_bound`/`pi_upper_bound`, witnesses from
`proof/pi_witness.py`). The bound holds while `int(N)` fits in 32 bits.

The computed and the exact run can choose different `N` when `x0/π_d` is
within rounding error of a half-integer, so `kernel_fast` is stated against
`sin x0` directly; both results are then accurate, since `sin` is
`π`-antiperiodic.

The bounds are worst-case. `kernel_sin`'s `6.14 U` is about 2.7 times the
measured `2.3 u`, and `kernel_fast`'s `2.2·10^-15` about 3 times its measured
`7.5·10^-16`.

### 2.7 Third example: Horner's rule, with an array and `intent(out)`

`examples/horner.f90` evaluates `p = c(1) + c(2) x + … + c(n) x^(n-1)` in a
subroutine with an explicit-shape array argument `c(n)` and an
`intent(out)` result `p`. The generated template takes `c` as an `Arr K.R8`
and returns `p`:

```lean
def horner {K : Kinds} [FKinds K] (n : K.I4) (c : Arr K.R8) (x : K.R8) : K.R8 := Id.run do
  let c := Arr.reshape c #[1] #[(FInt.toInt n).toNat]
  let mut p : K.R8 := FReal.zero
  p := Arr.get c #[FInt.toInt n]
  for i in FInt.range (FInt.ofInt 1 : K.I4) (FInt.sub n (FInt.ofInt 1)) do
    p := FReal.add (FReal.mul p x) (Arr.get c #[FInt.toInt (FInt.sub n i)])
  return p
```

* `horner_inv` (`examples/Horner/Proof/Loop.lean`) states the loop once, on
  any carrier: with exact `integer(4)`, the result satisfies every invariant
  that holds for `c(n)` and is preserved by `p ↦ p x + c(n-1-t)`. It uses the
  generic `range_foldl_inv` and `Arr.get_reshape_one`.
* `horner_exact`: in exact arithmetic the template computes `Σ_{k<n} c_k x^k`
  for every `n ≥ 1`.
* `horner_rounding`, in every `VModel` (no guard digit needed): the error is
  at most `3(n-1) U Σ |c_k| |x|^k + 3(n-1) H Σ_{k<n-1} |x|^k` for
  `n U, n H ≤ 1/100`, the textbook `γ_{2n}`-type bound with an underflow
  term. The invariant after `t` iterations is an error of at most
  `3t U a_t + 3t H b_t` (`step_err`).
* `horner_spec` (`examples/Horner/FloatRun.lean`): the `Float` run meets
  `HornerFloatSpec` (`examples/Horner/Spec.lean`), for `1 ≤ n < 2^31` and
  coefficients and `x` for which no intermediate value can overflow.

## 3. The floating-point model

Generated code evaluates every `real(8)` operation with an abstract arithmetic
`fp : FPModel` (`proof/LFortranProof/Prelude.lean`):

| Field | Meaning |
|---|---|
| `F : Set ℝ` | the floating-point numbers |
| `u` | unit roundoff |
| `η` | absolute error allowed by underflow |
| `M` | range: the bounds hold for exact results with `\|exact\| ≤ M` |
| `round` | rounding of literals and integer-to-real conversions |
| `add sub mul div` | the operations |

The only assumptions are, for operands in `F` and `|exact| ≤ M` (and a
nonzero divisor):

```
|computed − exact| ≤ u·|exact| + η        and        computed ∈ F
```

Nothing is assumed about overflow, division by zero, or operands outside `F`;
a proof has to show these never occur. Each operation may round differently,
and no particular rounding rule, radix or tie-breaking is assumed. A theorem
proved for every `FPModel` (with `u`, `η`, `M` in some range) therefore holds
for every concrete arithmetic that is an instance.

**One proof for the model and the run** (`proof/LFortranProof/VModel.lean`). The
template is instantiated at `ℝ` for a proof but at `Float` for the run, so a
theorem about `ℝ` with an `FPModel` is not yet a theorem about the run. The
rounding proofs are therefore written against `VModel α`: the `FReal α`
operations the generated code calls, on any carrier `α`, read through a
relation `val a x` ("`a` is finite with value `x`") with the error bounds of
`NoGuardModel`:

```
val a x → val b y → |x ∘ y| ≤ M → ∃ v, val (a ∘ b) v ∧ |v − x ∘ y| ≤ u·(…) + η
```

and the same for literals and integer conversions. Its instances are
`VModel.ofNoGuard m` (the carrier `ℝ`, `val a x := a = x ∧ x ∈ m.F`, for every
`NoGuardModel`, hence every `FPModel`) and `floatV` (Lean's `Float`,
`val f x := floatValue f = some x`, `proof/LFortranProof/FloatVModel.lean`). A rounding
theorem then reads "the run has a value `w`, and `|w − f(x)| ≤ …`", and is a
theorem about the `Float` run by instantiation. Programs that need more state
it as hypotheses: `VModel.Signed` (negation, `abs`, the sign bit and `int` act
on values), `VModel.Guard` (`+`, `-` have the `FPModel` bound),
`VModel.Binary64` (correctly rounded binary64), and `IntExact` for the
integer kind (`ℤ`; `Int32` on `[-2^31, 2^31)`).

## 4. Arithmetic systems and how they relate

| System | Source | Error form | Underflow | Overflow | In Lean | As an `FPModel` |
|---|---|---|---|---|---|---|
| Trefethen & Bau | *Numerical Linear Algebra*, Lecture 13 | `fl(x) = x(1+ε)`, `x ⊛ y = (x∗y)(1+ε)`, `\|ε\| ≤ ε_machine` | none (unbounded exponents) | none | `TrefethenBau` | `u = ε_machine`, `η = 0`, any `M` |
| Higham (standard model with underflow) | *Accuracy and Stability of Numerical Algorithms*, Ch. 2 | `(x op y)(1+δ) + η`, `\|δ\| ≤ u`, `\|η\| ≤ λu`, `δη = 0`, `η = 0` for `±` | absolute error `≤ λu` | excluded (`\|x op y\| ≤ M`) | `Higham` | `u`, `η = λu`, `M` |
| IEEE 754 binary64, round to nearest | IEEE 754-2019 §4.3.1, §5 | nearest element of binary64 | gradual (subnormals) | beyond `2^1024(1 − 2^-54)` | `binary64Model` | `u = 2^-53`, `η = 2^-1075`, `M = 2^1022` |
| binary64 with flush-to-zero (FTZ, also DAZ) | `-ffast-math`, MXCSR FTZ/DAZ, ARM FZ | as binary64, tiny results become 0 | results below `2^-1022` flushed | as binary64 | `binary64FTZModel` | `F` = binary64 without subnormals, `u = 2^-53`, `η = 2^-1021`, `M = 2^1022` |
| `FPModel` | this prototype | `\|computed − exact\| ≤ u·\|exact\| + η` | absolute term `η` | excluded (`M`) | `FPModel` | — |
| No guard digit (Cray 1, 2, X-MP, Y-MP, C90) | Higham §2.4 (2.6); CRAY-1 Hardware Reference 2240004 C | `fl(x ± y) = x(1+α) ± y(1+β)`, i.e. `\|error\| ≤ u(\|x\| + \|y\|) + η`; `×`, `÷` as `FPModel` | Cray: flushed to 0 | Cray: exponent `≥ 2^13` | `NoGuardModel`, `crayF` | not an `FPModel` (`noGuard_not_fpModel`) |

Proved implications (each arrow is a Lean definition that builds the target
from the source and proves its axioms):

```
Trefethen & Bau ──► Higham ──► FPModel ◄── binary64 with flush-to-zero (+ DAZ)
                      ▲           ▲
                      └── IEEE binary64 round-to-nearest
```

| Arrow | Lean (`proof/LFortranProof/Models/`) |
|---|---|
| Trefethen & Bau → Higham | `TrefethenBau.toHigham` (`λ = 0`) |
| Trefethen & Bau → `FPModel` | `TrefethenBau.toFPModel`, equal to going through Higham (`toHigham_toFPModel`) |
| Higham → `FPModel` | `Higham.toFPModel` |
| binary64 → Higham | `binary64Higham` (including `δη = 0` and exact `±` on underflow) |
| binary64 → `FPModel` | `binary64Model`, equal to going through Higham (`binary64Higham_toFPModel`) |
| binary64 FTZ → `FPModel` | `binary64FTZModel` |
| `FPModel` → no guard digit | `FPModel.toNoGuard` (the converse fails: `noGuard_not_fpModel`) |
| Lean's `Float` → `FPModel` | `floatModel` (`proof/LFortranProof/FloatModel.lean`): its `+ − × ÷` are `Float`'s, proved from `Float.Model` |
| Lean's `Float` → `VModel` | `floatV` (`proof/LFortranProof/FloatVModel.lean`), with `Signed`, `Guard`, `Binary64` |
| `NoGuardModel` → `VModel` | `VModel.ofNoGuard` |

`examples/Legendre/Corollaries.lean` applies `legendre_correct` to each of them
(`legendre_correct_binary64`, `legendre_correct_binary64FTZ`,
`legendre_correct_higham`, `legendre_correct_trefethenBau`), and
`legendre_correct_noGuard` gives `1.3e-9` without a guard digit at a Cray's
`u ≤ 2^-46`. The proof gives
an error bound of about `9.9e-12` using the fixed ceiling `U = 2^-53`; the
measured error is `1.78e-13`.

Differences in detail:

* **Trefethen & Bau** idealize: no underflow and no overflow, and only the
  relative form `(1+ε)`. Real binary64 violates their axioms for subnormal
  results (the relative error of a subnormal result can be close to 1). Their
  stability results are asymptotic, `O(ε_machine)` as `ε_machine → 0`; since
  `FPModel` keeps `u` a parameter, such statements can be expressed, but the
  Lean proofs give explicit constants instead.
* **Higham** adds a finite range and an absolute underflow term, with two extra
  facts: only one of `δ`, `η` is nonzero, and addition and subtraction are
  exact when the result underflows. `FPModel` does not assume these two
  facts; proofs that do not need them (such as the Legendre proof) then apply
  to more arithmetics.
* **IEEE binary64** is modelled concretely: `binary64` is the set of finite
  doubles `m·2^e` (`|m| < 2^53`, `-1074 ≤ e ≤ 971`) and each operation is
  any round-to-nearest of the exact result (any tie-breaking, so this includes
  roundTiesToEven). The error bound is proved, not assumed.
* **Flush-to-zero** replaces subnormal results by 0, which increases the
  absolute error from `2^-1075` to about `2^-1022`. Denormals-are-zero (DAZ)
  treats subnormal *inputs* as 0, which can change a result by much more than
  `η` (for example `2^-1074 · 2^1000`). It is covered by excluding subnormals
  from `F`: then no operand is ever subnormal and DAZ never applies, but a
  program's inputs must be non-subnormal (the hypothesis `x ∈ fp.F`).
* **No guard digit** (`proof/LFortranProof/Models/Cray.lean`) bounds the error of `x ± y`
  by `u(|x| + |y|)`, not by `u|x ± y|`: under cancellation the relative error
  can be 1 (the CRAY X-MP returns twice the exact `1 − 0.111…1₂`). So
  theorems about every `FPModel` do not apply to a Cray. Without cancellation
  (`xy ≥ 0` for `+`) the bounds agree (`NoGuardModel.add_err_same_sign`).
  The Legendre proof only bounds `|x − y|` through `|x| + |y|`, so it is
  proved for every `NoGuardModel` (`legendre_ng_correct`, error `≤ 89201 U`);
  the `FPModel` theorem is its corollary. An emulation of the CRAY-1 add,
  multiply and reciprocal divide measures `1.5e-11` against the proved
  `6.3e-10` for `u = 2^-47`.
* **Not automatically covered:** directed rounding needs the larger relative
  bound `u = 2^-52`, outside the Legendre specification's `u ≤ 2^-53`.
  Transformations such as reassociation, contraction into fused multiply-add,
  and replacement of division by approximate reciprocals can change which
  operations run. The current proof does not automatically transfer to the
  changed operation tree; modelling and verifying such transformations is
  possible (Section 6).

## 5. What the theorem guarantees, and what is trusted

Lean's kernel checks every proof. CI builds the proofs with `--wfail`, so an
incomplete proof (`sorry`) fails, and each theorem reports the axioms it uses:
only Lean's standard `propext`, `Classical.choice` and `Quot.sound`.

A human still has to trust:

* **the specification**: that `Spec.lean` and the arithmetic hypotheses say
  what is wanted;
* **the arithmetic**: that the machine's double-precision arithmetic, as the
  program uses it, satisfies the `FPModel` assumptions required by the
  specification: every operation, conversion and literal yields a double, with
  error at most `2^-53·|exact| + 2^-1000` when `|exact| <= 2^10` (and the
  divisor is nonzero). This is much weaker than IEEE conformance: within a
  normal binary64 binade, the relative term alone ranges from half a unit in
  the last place to almost a full unit. The absolute term can dominate near
  underflow, so the complete bound is not a half-to-one-ULP constraint.
  Roundings other than round-to-nearest can qualify, and operations may round
  differently from one another;
* **the translation**: that LFortran's Lean backend correctly models the ASR
  operation tree chosen for the Fortran source; the backend itself is not
  verified;
* **the compilation**: that the executable implements that operation tree's
  values under the stated arithmetic assumptions, or that a separate proof
  establishes the required accuracy after transformations (Section 6).

`proof/LFortranProof/Models/` is not part of what has to be trusted: it proves that
IEEE 754 round-to-nearest (and flush-to-zero) arithmetic satisfies these
assumptions. Someone who justifies the assumption by the hardware's IEEE
conformance can rely on that proof, and then only has to check that the
definitions `binary64` and `IsRoundToNearest` in
`proof/LFortranProof/Models/Binary64.lean` match the standard.

**The `Float` run.** The final theorems are about the functions that
`--backend=lean` executes: the generated templates at `LFortran.machine`
(`Int32`, Lean's `Float`). Their statements are in the human-reviewed spec
files, so what is guaranteed can be read off the theorem's type:

| Theorem | Statement (spec file) | Guarantee for every finite `Float` input |
|---|---|---|
| `legendre_float_spec` | `LegendreFloatSpec` (`examples/Legendre/Spec.lean`) | `n ≤ 100`, `\|x\| ≤ 1`: finite, within `10^-11` of `legendreP n x` |
| `kernel_sin_spec` | `KernelSinFloatSpec` (`examples/SinKernels/Spec.lean`) | `\|x\| ≤ π/2`: finite, within `1.15·10^-15 \|x\| + 2^-1075` of `sin x` |
| `kernel_fast_spec` | `KernelFastFloatSpec` (`examples/SinKernels/Spec.lean`) | `\|x0\| ≤ 6·10^9`: finite, within `2.2·10^-15` of `sin x0` (measured `7.5·10^-16`) |
| `horner_spec` | `HornerFloatSpec` (`examples/Horner/Spec.lean`) | `1 ≤ n < 2^31`, finite `c(1..n)`, no overflow: finite, within `3(n-1) 2^-53 Σ \|c_k\| \|x\|^k + 3(n-1) 2^-1075 Σ_{k<n-1} \|x\|^k` of `Σ c_k x^k` |

`floatValue` (the real number a `Float` denotes, from Lean's `Float.Model`)
is part of these statements. Each proof is the general rounding theorem
(proved once for every `VModel`, Section 3) instantiated at `floatV` and
`Int32`; there is no separate argument relating the run to a real-valued
model. That `Float` is a `VModel` is proved once, for all programs
(`proof/LFortranProof/FloatVModel.lean`): every operation the templates use (`+ − × ÷`,
negation, `abs`, the sign bit, `int` truncation, `Float.ofInt`, and literals
`Float.ofInt m * floatPow2 e`, exact by the bit pattern of `floatPow2`) meets
the bounds, and `Int32` is exact on `[-2^31, 2^31)` (including Fortran
`modulo`). The sign bit and `int` are read through `Float.Model` because
`Float.toInt64` has no logical model. The `Float` sign of `-0.0` differs from
`ℝ`, so the choice of `N` is proved for either sign choice at `x0 = 0`
(`SignOk`, `chooseN`). All use only the standard axioms.

What remains trusted is Lean's: that the compiled `Float` operations (C
`double`) implement `Float.Model`. The proofs cover the Lean run, not an
LFortran- or GFortran-compiled executable, which performs the same IEEE
operations only if the compiler preserves them. `examples/check_float_runs.py` tests
both links on 20,000 inputs (random, `±0`, subnormals, `±π/2`, half-integer
multiples of `π_d`, powers of two, up to `6·10^9`): the Lean run, an LFortran
build and a GFortran build agree bit for bit, and every result in a
specification's domain is within its proved bound against exact rational
(Legendre) or 90-digit (`sin`) references. It runs in the Linux LLVM 11
Quick job.

The theorem says nothing about inputs outside the specified domain (for
example `n > 100`, NaN or infinite `x`), or about performance.

## 6. Compiler optimizations, fast-math, and levels of verification

The proof is about the source-level ASR operation tree: the backend translates
it before any lowering or optimization. It is not a proof about every
evaluation permitted by Fortran or about the final machine code. What runs
is the compiled program, so the question is which compiler transformations
preserve the proved property.

**Value-preserving transformations** (register allocation, inlining, loop
transformations that keep the order of floating-point operations, ...) do not
change any computed value, so the theorem applies unchanged. Such a
transformation can in principle be verified once for all programs, as in the
CompCert C compiler, whose correctness proof in Coq includes floating-point
semantics (via the Flocq library). Another approach is to check individual
transformation instances, as
[Alive2](https://web.ist.utl.pt/nuno.lopes/pubs/alive2-pldi21.pdf) does for LLVM.
Alive2 performs **bounded** translation validation: its loop-unrolling bounds
can leave errors undetected, so a successful bounded check is not a general
proof for arbitrarily many loop iterations.

**Value-changing transformations**, typically enabled by fast-math, change the
computed values:

* *flush-to-zero* and *denormals-are-zero* change the rounding of tiny values.
  This is a different arithmetic, and it is covered (Section 4): the theorem
  holds for binary64 with flush-to-zero for non-subnormal inputs.
* *reassociation* (`(a+b)+c` to `a+(b+c)`), *contraction* of `a*b+c` into a
  fused multiply-add, and *approximate reciprocals* change which operations
  are performed. The current operation tree and proof do not automatically
  cover the transformed program.

These are not unconditional value-preserving transformations: they can break
programs whose accuracy depends on the exact order of operations, such as
compensated summation and the error-free transformations in the double-double
reference in `legendre_accuracy.f90`. `kernel_fast` is another example: its
Cody–Waite reduction relies on `x - N p1` and `N p1` being exact. Compiled
with `lfortran --fast`, its error grows from `4·10^-16` to `1.9·10^-8` at
`x0 = 1.7·10^9`; with `--fast --no-fast-math` it keeps the proved accuracy,
so `lean_sin_kernels_01` is registered `NO_FAST_MATH`. This does not preclude reusable proofs
under sufficient side conditions. A transformation can be proved to preserve
values, or a specified accuracy contract, whenever those conditions hold.

Arithmetic models can also be extended or instantiated to cover optimized
operations. FMA can be represented explicitly and its error analyzed. An
approximate division implementation can implement `fp.div` if its aggregate
error and result membership satisfy the existing contract on the required
domain. A looser error bound would instead require checking whether the
specification and proof still apply.

Another route is verification **per program**: the compiler can emit Lean
code after transformations (after ASR passes, and in principle for LLVM IR and
assembly), and that code can be proved against the same specification. If an
optimized program violates the specification, no sound proof can establish
it; inability to find a proof does not by itself establish a violation.
A practical design can combine reusable pass proofs, discharge their side
conditions for each use, and use program-specific accuracy proofs where
needed. Fast-math does not impose an absolute boundary between reusable and
per-program verification.

Verifying the **machine code** directly, using formal models of the
instruction set (for example the Sail models of the Arm, RISC-V and x86
architectures), would remove the compiler from what has to be trusted and
would not depend on the source language. It requires modelling the processor's
floating-point behaviour (rounding modes, flush-to-zero), and optimized
machine code is larger and less structured than the source, so the proofs are
harder.

## 7. Scope and outlook

**The Fortran subset.** The backend translates only what this example needs.
Arrays, derived types and procedure calls have well-defined semantics and can
be translated as well (an array as a Lean array or a function from indices to
values, loops as folds); the proofs then also have to handle indexing,
bounds and aliasing. How far this scales to large array codes is an open
question.

**Mathematical function libraries** (`sin`, `cos`, `erf`, ...) are a natural
target (Section 2.6 proves two `sin` kernels): they are small, heavily optimized, and their accuracy is usually only
tested on samples, while a proof covers every double. Such proofs combine the
error of the approximation (range reduction, polynomial or rational
approximation) with rounding errors, and need `sqrt` and fused multiply-add
in the model. This has been done before in other provers, for example John
Harrison's HOL Light proofs of Intel's floating-point algorithms, and the
CRlibm library with the Gappa tool.

**Why Fortran rather than Lean?** Lean can compile programs. Since Lean 4.33,
`Float` has a logical `Float.Model` for basic arithmetic and square root,
usable in proofs; this includes the 4.33.1 version pinned here (see the
[Lean floating-point documentation](https://lean-lang.org/doc/reference/4.33.1/Basic-Types/Floating-Point-Numbers/)).
Numerical analysis still needs supporting mathematical lemmas, and agreement
between the logical model and native execution remains part of the trusted
implementation, supported by empirical testing rather than a proof about the
target machine. The reason for using Fortran here is its optimizing compilers
and large body of existing scientific code that could be verified without
being rewritten, not an absence of Lean floating-point semantics. Writing
algorithms in Lean and translating them to Fortran is also possible; unless
verified, that translation would have to be trusted.

**Next steps** for the infrastructure: `sqrt` and fused multiply-add in
`FPModel`; Higham's `γ_n` lemma to make proofs shorter (for `n` factors with
`|δ_i| <= u`, `0 <= u` and `nu < 1`, it gives
`∏(1+δ_i)^{±1} = 1+θ_n`, `|θ_n| <= nu/(1-nu)`); a connection to a more
comprehensive floating-point formalization, such as
[FloatSpec](https://github.com/Beneficial-AI-Foundation/FloatSpec), an ongoing
Lean port of Flocq whose roadmap still lists correctness work; and a larger
Fortran subset in the backend.
