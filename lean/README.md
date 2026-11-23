# Verified Fortran with the LFortran Lean backend (prototype)

This directory demonstrates the workflow proposed in
[SciFort: SciPy algorithms in Fortran](https://fortran-lang.discourse.group/t/scifort-scipy-algorithms-in-fortran/11326/6):

1. `legendre.f90` implements `legendre_poly(n, x)` (Legendre polynomials by
   Bonnet's recurrence). `legendre_accuracy.f90` measures its error against a
   double-double reference: at most **1.78e-13** for `n <= 100`, `|x| <= 1`.
2. `lfortran --show-lean legendre.f90` translates it to
   `Legendre/Generated.lean`. The backend (`src/libasr/codegen/asr_to_lean.cpp`)
   supports only the small Fortran subset this example needs.
3. `Legendre/Spec.lean` is the hand-written specification a human reviews:
   error at most `1e-11` for `0 <= n <= 100`, `|x| <= 1`, in every arithmetic
   at least as accurate as binary64.
4. `Legendre/Proof.lean` proves `legendre_correct : LegendreSpec legendre_fortran`,
   and Lean checks it. `LFortran/Models/` proves that IEEE binary64 and the
   textbook models of Trefethen & Bau and Higham are instances of the
   arithmetic the proof assumes, so the theorem applies to all of them
   (`Legendre/Corollaries.lean`).

Run everything from the repository root:

```bash
pixi run -e llvm11 bash lean/run.sh
```

or only the Lean part (no LFortran build needed):

```bash
cd lean
bash fetch_deps.sh   # first time: Lean dependencies and prebuilt Mathlib files
lake build --wfail
```

## The floating-point model

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

## Arithmetic systems and how they relate

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

## What is trusted

Lean checks the proofs. A human still has to trust:

* the specification `Legendre/Spec.lean` (that it says what is wanted);
* that `binary64Model` matches IEEE 754, i.e. that the hardware rounds each
  operation's exact result to nearest (the definitions in
  `LFortran/Models/Binary64.lean` follow the standard's wording);
* that LFortran's translation preserves the meaning of the Fortran code and
  that the compiled executable performs the same operations in the same
  order (no fast-math reassociation or fused multiply-add contraction).
