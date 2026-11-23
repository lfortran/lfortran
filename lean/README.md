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

`--show-lean` is the proof translation above: noncomputable code over an
abstract `FPModel`, which Lean cannot execute. `--backend=lean` is a second
translation of the same ASR, before any ASR pass, into computable Lean with a
`def main` that `lean --run` executes. The two translations are independent.
`stop` and `error stop` leave the process. Integration tests labeled `lean`
are run that way, locally and in the Linux LLVM 11 Quick job:

```bash
pixi run -e llvm11 python lean/run_integration_tests.py
pixi run -e llvm11 python lean/run_integration_tests.py --all
```

`--all` tries every integration test and reports what the backend still
rejects. `--apply` adds the `lean` label to tests that pass and removes it
from tests that no longer do. A test is labeled only when its exit code
agrees with the `RUN()` entry (zero, unless the entry is `FAIL`). The latest
sweep passes 1695 integration tests and labels 1691 of them; the other four
have no `LABELS` list to attach the word to. The runtime is
`lean/LFortran/Exec.lean` (no Mathlib). The toolchain is `lean/lean-toolchain`.

### Executable backend decisions

These are deliberate limits of `--backend=lean`. A test that depends on a
behavior below and gets a different result is not labeled.

- Integers are `Int8`/`Int16`/`Int32`/`Int64` and wrap. Division truncates
  toward zero. `real(4)` is `Float32` and `real(8)` is `Float`, built with
  `Float.ofBits` so the bits match IEEE. `real(16)` is rejected. `complex`
  is a pair struct with the usual arithmetic. `logical` is `Bool`.
  `character` is `String`, blank-padded and truncated to its declared length.
- Arrays are column-major values (`LFortran.Exec.Arr`) with a lower bound
  per dimension. A scalar assigned to an array fills every element.
- An allocatable or a pointer is the value itself, not an alias. `allocate`
  replaces an array (`Arr.make`); a deferred shape starts empty. Scalar
  `allocate` does nothing. Pointer assignment copies. A later write through
  one name is not visible through the other. `allocated()` is not tracked.
- An assumed-shape or deferred-shape `intent(out)` dummy is still passed in,
  because the caller owns the shape. A scalar broadcast onto an array whose
  shape is not known stays a scalar: an operation maps it across the other
  operand, and assigning it fills the array that is already there.
- A module variable, or a local used by a contained procedure, lives in a
  process-wide table in `Exec` keyed by `owner.name`. Each `lean --run` is
  its own process. The first read returns the type's zero unless `main` has
  stored the initializer. A procedure that touches one of these, prints,
  stops, or calls such a procedure is an `IO` action. Block and associate
  locals do not escape. A derived-type value is not put in those tables, so
  a module variable or a host-associated variable of derived type is rejected.
- List-directed `write` to `*`, unit 6, or unit 0 is one line on stdout, with
  the same spacing as `print *`. Any other unit, namelist, unformatted I/O,
  an internal file, or a format string is rejected. `iostat`, when present,
  is set to 0.
- An array section with exactly one subscript range is a rank-1 copy whose
  lower bound is 1. Assigning to that section writes the elements back.
  Several ranges, and implied-do constructors, are rejected.
- `reshape` without `pad` or `order` reuses the source elements and takes
  the extents from the shape array. Lower bounds are 1. A size mismatch is
  not checked.
- `transfer` / bit cast uses the folded value when ASR has one, and is the
  identity when the two Lean types are the same. Anything else is rejected.
- `do concurrent` with no reduction clause runs as a sequential counted loop.
  Reductions and an empty head are rejected. Named `cycle` of an outer loop
  is rejected.
- `where` evaluates the mask once. Each assignment in the body is computed
  for the whole array and then stored only at elements where the mask is
  true. The `elsewhere` arm stores where it is false. A nested `where`, or a
  statement that is not an assignment, is rejected. `<`, `>`, `<=`, and `>=`
  are emitted with `decide`, because those Lean operators are propositions
  and a mask has to be a `Bool` array.
- A derived type is a Lean `structure`. Components inherited with `extends`
  are fields of the child, in parent-then-child order. There is no dynamic
  type. A component, including a pointer or allocatable component, is a
  value, so two names never alias. A type that contains itself (a linked
  list node, for example) is rejected. `class(*)` is rejected. Enums and
  unions are rejected. Type-bound calls are rejected.

or only the Lean part (no LFortran build needed):

```bash
cd lean
bash fetch_deps.sh   # first time: Lean dependencies and prebuilt Mathlib files
lake build --wfail
```

| File | Role | Written by |
|---|---|---|
| `legendre.f90` | the implementation | programmer (or AI) |
| `legendre_accuracy.f90` | measured accuracy | programmer |
| `Legendre/Generated.lean` | the implementation in Lean | `lfortran --show-lean` |
| `Legendre/Spec.lean` | what the code must satisfy | human, reviewed |
| `LFortran/Exec.lean` | runtime for `--backend=lean` (`Arr`, complex, cells) | human, reviewed |
| `LFortran/Prelude.lean` | the arithmetic model `FPModel` | human, reviewed |
| `LFortran/Models/*.lean` | proofs that IEEE binary64 and textbook models are `FPModel`s (not trusted) | proofs (AI), checked by Lean |
| `Legendre/Proof*.lean`, `Legendre/Corollaries.lean` | the proofs | proofs (AI), checked by Lean |

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

`lfortran --show-lean legendre.f90` produces `Legendre/Generated.lean`:

```lean
noncomputable def legendre_poly (fp : FPModel) (n : ℤ) (x : ℝ) : ℝ := Id.run do
  let mut p : ℝ := 0
  let mut a : ℝ := 0
  let mut b : ℝ := 0
  let mut c : ℝ := 0
  a := fp.round 1
  b := x
  if n = 0 then
    p := a
    return p
  for k in intRange 2 n do
    c := fp.div (fp.sub (fp.mul (fp.mul (fp.ofInt (2 * k - 1)) x) b) (fp.mul (fp.ofInt (k - 1)) a)) (fp.ofInt k)
    a := b
    b := c
  p := b
  return p
```

The translation preserves the operation tree chosen by LFortran's ASR rather
than replacing floating-point expressions by exact real arithmetic:

* `real(8)` values are real numbers, and **every operation is rounded**:
  the ASR groups `(2*k-1)*x*b` as `((2*k-1)*x)*b`, with the integer `2*k-1`
  converted to real (`fp.ofInt`, itself a rounding) and each product rounded
  by `fp.mul`. Literals such as `1.0_dp` are rounded too (`fp.round`).
* `integer` is `ℤ`, with exact arithmetic (overflow is out of scope in this
  prototype).
* The arithmetic `fp : FPModel` is a parameter (Section 3), so one proof covers
  every arithmetic satisfying the model, including IEEE binary64.
* The backend runs on LFortran's internal representation (ASR) before any
  lowering or optimization, and accepts only the small Fortran subset this
  example needs (module functions on `integer` and `real(8)` scalars,
  assignments, `if`, `return`, unit-step `do` loops, `+ - * /`). Anything else
  is rejected with an error rather than translated approximately.

Fortran permits mathematically equivalent rearrangements subject to its rules
on parentheses, even when floating-point results differ; it does not require
this particular evaluation order (see the
[Fortran discussion of mathematical equivalence](https://fortran.bcs.org/2012/math_equiv2.pdf)).
The theorem therefore applies to this operation tree. Applying it to an
executable requires that compilation preserve the tree's values under the
stated arithmetic assumptions (Sections 5 and 6), not merely that fast-math
be disabled.

### 2.3 The specification

`Legendre/Spec.lean` is the part a human reads:

```lean
noncomputable def P : ℕ → ℝ → ℝ
  | 0, _ => 1
  | 1, x => x
  | n + 2, x => ((2 * n + 3) * x * P (n + 1) x - (n + 1) * P n x) / (n + 2)

def LegendreSpec (f : FPModel → ℤ → ℝ → ℝ) : Prop :=
  ∀ (fp : FPModel), fp.u ≤ 1 / 2 ^ 53 → fp.η ≤ 1 / 2 ^ 1000 → 2 ^ 10 ≤ fp.M →
  ∀ (n : ℤ) (x : ℝ), x ∈ fp.F → 0 ≤ n → n ≤ 100 → |x| ≤ 1 →
    |f fp n x - P n.toNat x| ≤ 1 / 10 ^ 11
```

It contains four decisions to review:

* **The mathematical function.** `P` is the exact Legendre polynomial over
  `ℝ`, defined here by the recurrence. Mathlib defines the shifted Legendre
  polynomials (`Polynomial.shiftedLegendre`) by an explicit formula; using
  them would make the specification independent of the algorithm, at the
  cost of proving the recurrence for them.
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

The proof (`Legendre/Proof.lean`, `Legendre/Proof/*.lean`) uses the fixed
ceilings `U = 2^-53` and `E = 2^-1000`, not the individual model's `fp.u`
and `fp.η`. It has four steps:

1. **Loop to recursion.** The `for` loop of the generated code is rewritten as
   a recursive sequence `y_m` of computed values.
2. **One step of the loop.** Using only the error bound of each of its eight
   rounded operations (five arithmetic operations, three conversions), the
   computed `y_k` differs from the exact recurrence applied to the computed
   `y_{k-1}`, `y_{k-2}` by at most `|r_k| ≤ 18U`, as long as the computed
   values stay within `1 + 10^-10` in magnitude, which the induction
   maintains. This bound includes the allowed absolute underflow error;
   it is not a bound of `18·fp.u` when `fp.u` is smaller than `U`.
3. **Propagation.** The errors `e_k = y_k - P_k(x)` satisfy
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
   `100(U+E) + 4950·18U`, about `9.9e-12`, below `10^-11`.

The bound is roughly 50 times larger than the measured error. The generic
proof permits worst-case operation errors, allows rounding errors in `1.0`
and the small integer conversions that are exact in IEEE binary64, and uses
conservative propagation inequalities. Rounding errors can be correlated;
the measurements alone do not establish cancellation as the explanation for
the gap. A sharper IEEE-specific proof could exploit exactness and additional
structure.

## 3. The floating-point model

Generated code evaluates every `real(8)` operation with an abstract arithmetic
`fp : FPModel` (`LFortran/Prelude.lean`):

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

## 4. Arithmetic systems and how they relate

| System | Source | Error form | Underflow | Overflow | In Lean | As an `FPModel` |
|---|---|---|---|---|---|---|
| Trefethen & Bau | *Numerical Linear Algebra*, Lecture 13 | `fl(x) = x(1+ε)`, `x ⊛ y = (x∗y)(1+ε)`, `\|ε\| ≤ ε_machine` | none (unbounded exponents) | none | `TrefethenBau` | `u = ε_machine`, `η = 0`, any `M` |
| Higham (standard model with underflow) | *Accuracy and Stability of Numerical Algorithms*, Ch. 2 | `(x op y)(1+δ) + η`, `\|δ\| ≤ u`, `\|η\| ≤ λu`, `δη = 0`, `η = 0` for `±` | absolute error `≤ λu` | excluded (`\|x op y\| ≤ M`) | `Higham` | `u`, `η = λu`, `M` |
| IEEE 754 binary64, round to nearest | IEEE 754-2019 §4.3.1, §5 | nearest element of binary64 | gradual (subnormals) | beyond `2^1024(1 − 2^-54)` | `binary64Model` | `u = 2^-53`, `η = 2^-1075`, `M = 2^1022` |
| binary64 with flush-to-zero (FTZ, also DAZ) | `-ffast-math`, MXCSR FTZ/DAZ, ARM FZ | as binary64, tiny results become 0 | results below `2^-1022` flushed | as binary64 | `binary64FTZModel` | `F` = binary64 without subnormals, `u = 2^-53`, `η = 2^-1021`, `M = 2^1022` |
| `FPModel` | this prototype | `\|computed − exact\| ≤ u·\|exact\| + η` | absolute term `η` | excluded (`M`) | `FPModel` | — |

Proved implications (each arrow is a Lean definition that builds the target
from the source and proves its axioms):

```
Trefethen & Bau ──► Higham ──► FPModel ◄── binary64 with flush-to-zero (+ DAZ)
                      ▲           ▲
                      └── IEEE binary64 round-to-nearest
```

| Arrow | Lean (`LFortran/Models/`) |
|---|---|
| Trefethen & Bau → Higham | `TrefethenBau.toHigham` (`λ = 0`) |
| Trefethen & Bau → `FPModel` | `TrefethenBau.toFPModel`, equal to going through Higham (`toHigham_toFPModel`) |
| Higham → `FPModel` | `Higham.toFPModel` |
| binary64 → Higham | `binary64Higham` (including `δη = 0` and exact `±` on underflow) |
| binary64 → `FPModel` | `binary64Model`, equal to going through Higham (`binary64Higham_toFPModel`) |
| binary64 FTZ → `FPModel` | `binary64FTZModel` |

`Legendre/Corollaries.lean` applies `legendre_correct` to each of them
(`legendre_correct_binary64`, `legendre_correct_binary64FTZ`,
`legendre_correct_higham`, `legendre_correct_trefethenBau`). The proof gives
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

`LFortran/Models/` is not part of what has to be trusted: it proves that
IEEE 754 round-to-nearest (and flush-to-zero) arithmetic satisfies these
assumptions. Someone who justifies the assumption by the hardware's IEEE
conformance can rely on that proof, and then only has to check that the
definitions `binary64` and `IsRoundToNearest` in
`LFortran/Models/Binary64.lean` match the standard.

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
reference in `legendre_accuracy.f90`. This does not preclude reusable proofs
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
target: they are small, heavily optimized, and their accuracy is usually only
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
