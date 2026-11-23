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
| `LFortran/Prelude.lean` | the arithmetic model `FPModel` | human, reviewed |
| `LFortran/Models/*.lean` | IEEE binary64 and textbook models as `FPModel`s | definitions reviewed by a human, proofs by AI |
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

The translation is faithful to Fortran's evaluation rules rather than to the
mathematics:

* `real(8)` values are real numbers, and **every operation is rounded**:
  `(2*k-1)*x*b` is `((2*k-1)*x)*b`, evaluated left to right as in the source,
  with the integer `2*k-1` converted to real (`fp.ofInt`, itself a rounding)
  and each product rounded by `fp.mul`. Literals such as `1.0_dp` are rounded
  too (`fp.round`).
* `integer` is `ℤ`, with exact arithmetic (overflow is out of scope in this
  prototype).
* The arithmetic `fp : FPModel` is a parameter (Section 3), so one proof covers
  every arithmetic satisfying the model, including IEEE binary64.
* The backend runs on LFortran's internal representation (ASR) before any
  lowering or optimization, and accepts only the small Fortran subset this
  example needs (module functions on `integer` and `real(8)` scalars,
  assignments, `if`, `return`, unit-step `do` loops, `+ - * /`). Anything else
  is rejected with an error rather than translated approximately.

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
* **The arithmetic.** Any `FPModel` at least as accurate as binary64: unit
  roundoff `u <= 2^-53`, underflow error `η <= 2^-1000` and range
  `M >= 2^10`.

### 2.4 Choosing the tolerance

A requested accuracy can be false, true but not provable by a given argument,
or proved:

* **1e-14 is false.** The measured error 1.78e-13 already exceeds it, so no
  proof can exist.
* **1e-12 is probably true but not provable by worst-case analysis.** It holds
  on all tested points, but a proof that only uses the error bound of each
  operation must allow every rounding error to take its worst value and sign.
  The best such bound at `n = 100` is about 3e-12.
* **1e-11 is proved.** The proof gives about 9.9e-12.

A proof of a weaker statement does not establish a stronger one: if the
specification asks for `1e-12`, the final step of the proof (checking
`9.9e-12 <= 1e-12` with exact rational arithmetic) fails and Lean rejects
the proof. The result is then "not proven", which is different from "false".

### 2.5 How the proof works

The proof (`Legendre/Proof.lean`, `Legendre/Proof/*.lean`) has four steps:

1. **Loop to recursion.** The `for` loop of the generated code is rewritten as
   a recursive sequence `y_m` of computed values.
2. **One step of the loop.** Using only the error bound of each of its eight
   rounded operations (five arithmetic operations, three conversions), the
   computed `y_k` differs from the exact recurrence applied to the computed
   `y_{k-1}`, `y_{k-2}` by at most `|r_k| ≤ 18u` (to first order the constant
   is `17 - 11/k`), as long as the computed values stay within `1 + 10^-10`
   in magnitude, which the induction maintains.
3. **Propagation.** The errors `e_k = y_k - P_k(x)` satisfy
   `e_k = ((2k-1) x e_{k-1} - (k-1) e_{k-2})/k + r_k`. Bounding this term by
   term, `|e_k| <= (2k-1)/k |e_{k-1}| + (k-1)/k |e_{k-2}| + |r_k|`, is too
   weak: that recurrence grows like `(1 + √2)^k`, about `10^38` at `k = 100`.
   Instead the proof uses the quadratic form
   `T(p, q) = p^2 + q^2 - 2xpq = (q - xp)^2 + (1 - x^2) p^2`, which does not
   increase along the exact recurrence when `|x| <= 1`. Its square root is a
   seminorm, so each rounding error adds at most `|r_k|` to it, and the errors
   grow only quadratically: `|e_n| <= n·ε_0 + n(n-1)/2·18u`, where `ε_0` is
   the rounding error of the literal `1.0`. The same form also proves
   `|P_n(x)| <= 1`, which keeps the intermediate values (and the per-step
   errors) bounded.
4. **The numbers.** For `n <= 100` this is about `9.9e-12 <= 10^-11`.

The proof is 50 times more pessimistic than the measured error because it
assumes every rounding error is as large as possible and of the worst sign;
real rounding errors vary in size and sign and largely cancel. Proving bounds
close to the observed error would need a different kind of argument.

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
an error bound of about `9.9e-12` (with `u = 2^-53`); the measured error is
`1.78e-13`.

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
* **Not covered:** directed rounding (needs `u = 2^-52`; an `FPModel`, but
  outside the Legendre specification's `u ≤ 2^-53`), and the program
  transformations of `-ffast-math` (reassociation, contraction into fused
  multiply-add, approximate reciprocals). Those change which operations run,
  so the compiled program no longer matches the translated one; no rounding
  model can account for them.

## 5. What the theorem guarantees, and what is trusted

Lean's kernel checks every proof. CI builds the proofs with `--wfail`, so an
incomplete proof (`sorry`) fails, and each theorem reports the axioms it uses:
only Lean's standard `propext`, `Classical.choice` and `Quot.sound`.

A human still has to trust:

* **the specification**: that `Spec.lean` and the arithmetic hypotheses say
  what is wanted;
* **the arithmetic model**: that `binary64Model` matches IEEE 754, i.e. that
  each operation's exact result is rounded to nearest (the definitions in
  `LFortran/Models/Binary64.lean` follow the standard's wording);
* **the translation**: that LFortran's Lean backend preserves the meaning of
  the Fortran code; the backend itself is not verified;
* **the compilation**: that the executable performs the same operations in
  the same order as the source (Section 6).

The theorem says nothing about inputs outside the specified domain (for
example `n > 100`, NaN or infinite `x`), or about performance.

## 6. Compiler optimizations, fast-math, and levels of verification

The proof is about the program at the level of the source code: the backend
translates LFortran's ASR before any lowering or optimization. What runs is
the compiled program, so the question is which compiler transformations
preserve the proved property.

**Value-preserving transformations** (register allocation, inlining, loop
transformations that keep the order of floating-point operations, ...) do not
change any computed value, so the theorem applies unchanged. Such a
transformation can in principle be verified once for all programs, as in the
CompCert C compiler, whose correctness proof in Coq includes floating-point
semantics (via the Flocq library), or checked automatically per
transformation, as Alive2 does for LLVM optimizations.

**Value-changing transformations**, typically enabled by fast-math, change the
computed values:

* *flush-to-zero* and *denormals-are-zero* change the rounding of tiny values.
  This is a different arithmetic, and it is covered (Section 4): the theorem
  holds for binary64 with flush-to-zero for non-subnormal inputs.
* *reassociation* (`(a+b)+c` to `a+(b+c)`), *contraction* of `a*b+c` into a
  fused multiply-add, and *approximate reciprocals* change which operations
  are performed. No arithmetic model covers them; the program itself changes.

Such transformations cannot be verified for all programs, because they really
do break some programs, namely those whose accuracy depends on the exact
order of operations (compensated summation, error-free transformations such
as the double-double reference in `legendre_accuracy.f90`). They can be
verified **per program**: the compiler can emit Lean code after each
transformation (after each ASR pass, and in principle for LLVM IR and
assembly) and the proof is redone for that code against the same
specification. A program whose accuracy is robust to reordering is then
proved correct for the optimized code too, and a non-robust program fails to
prove, which is the right answer. This suggests a division: passes that are
correct for every program are verified once; fast-math passes are verified
per program.

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

**Why Fortran rather than Lean?** Lean can compile programs, but its built-in
`Float` type has no proved floating-point semantics, so proofs about Lean
programs that use it face the same modelling problem, and Lean is not aimed at
producing vectorized numerical code. Fortran has optimizing compilers and a
large body of existing scientific code that could be verified without being
rewritten. Writing algorithms in Lean and translating them to Fortran is also
possible; the translation would then have to be trusted instead.

**Next steps** for the infrastructure: `sqrt` and fused multiply-add in
`FPModel`; Higham's `γ_n` lemma (`∏(1+δ_i)^{±1} = 1+θ_n`, `|θ_n| <= nu/(1-nu)`)
to make proofs shorter; a connection to a complete IEEE formalization such as
FloatSpec (a Lean port of Flocq); and a larger Fortran subset in the backend.
