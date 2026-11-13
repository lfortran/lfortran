# Conservative risk rubric, version 1

The objective is high precision: an uncertain low-risk decision is more costly
than sending an easy patch to a human. No score or number of passing tests
overrides a hard gate. A classification describes one revision in one base
context; it is not a mathematical guarantee about all Fortran programs.

## Hard gates: `needs-review`

The following changes are outside the version-1 automatic category set,
including when they are one line long:

| Surface | Examples and reason |
| --- | --- |
| Language or product policy | New extensions, changed accepted dialect, warning severity/defaults, changed command-line options or their defaults, advertised kind/feature support. Fixing an established semantic bug is different from selecting a new contract. |
| Representation and compatibility | ASR/AST schema, serialization or module-file format, ABI, alignment, physical array/string representation, calling convention, exported runtime signatures, linkage or cross-unit symbol identity. |
| Lifetime and mutable state | Ownership, allocation/deallocation strategy, finalization, aliasing, new or changed cross-scope visitor/member/static/global state contracts, caches, callback lifetimes, resource limits or pools. "No global variable" is not sufficient. |
| Concurrency and devices | Synchronization, ordering, asynchronous execution, GPU memory/coherence, host/device transfers and execution-domain decisions. CPU-only tests do not establish device behavior. |
| Optimization and performance | Pass addition/removal/reordering, optimizer pipeline, fast-math/overflow assumptions, vectorization, new whole-tree scans, new algorithms, or increased asymptotic work/generated-code growth. |
| Control and distribution | Frontend driver, build/generator logic, dependencies/locks, packaging, releases, CI, permissions, credentials, test runner/macro logic, agent skills/instructions, label/merge policy itself. A `.md` suffix does not make operational policy passive prose. |
| Weakened evidence | Removed/skipped assertions, relaxed tolerance, changed golden outputs without independent justification, removed backend labels, new skips/xfails, disabled verification or broadened success handling. |
| Unbounded or mixed scope | Unrelated fixes, broad refactors, large backend-specific implementations, production budget exceeded, unexplained binaries/generated code, truncated/missing diff or material context. |
| Open concern or absent proof | Current requested changes, unresolved correctness/design concern, questionable test validity, missing target coverage, unexplained compile-time cost, unverified architectural assumptions. |

Apply the meaning of these gates, not substring matching. A test that passes an
ordinary pointer to an unchanged API does not itself change ownership. A
standard header added to one `.cpp` is not a dependency upgrade. A leaf
`RUN(NAME ... LABELS gfortran llvm)` addition is not a modification to the
`RUN` macro. Conversely, deleting a check under a harmless-sounding title is
still a coverage change.

The state gate is not a ban on every local assignment or parser cursor update.
A fix preserving an existing local input-consumption invariant can be bounded;
inventing a new cross-call state contract or relying on unexamined restoration
paths cannot. Likewise a fixture helper with unchanged defaults is not
automatically a repository-wide test-runner change.

For test-only PRs, assess whether they **change or choose** a contract, rather
than merely exercise an existing one. An ABI test can be valuable but still
require review if it locks in an unsettled design, compiler-specific behavior,
or a cross-platform assumption.

## D1: passive documentation

Allow corrections or bounded additions to prose documenting established
behavior, such as a typo, explanation, example explanation or a conditional
troubleshooting note supported by its source.

Require:

- No compiler, build, dependency, generated executable, workflow, permissions
  or agent-policy change hidden in the same PR.
- No new promise about semantics, supported features or release policy.
- Commands and examples are accurate in context. A literal command in prose
  is not automatically executable configuration, but changing it still needs
  factual verification; unsafe or unverified instructions are not low-risk.
- Any executable documentation directives, templates, embedded scripts or
  generated site/build inputs are recognized and reviewed separately.

Do not label all `doc/**`, Markdown files, or a `docs:` title automatically.
Example: #16202 is a candidate conditional troubleshooting note; #16823 also
deletes environment files and changes tooling, so it is not D1.

## T1: additive regression coverage

Allow a small, coherent regression for behavior already supported and agreed
on `main`, including tests split from a previously landed fix.

Require:

- Only new test cases/data and ordinary leaf registration, or strictly added
  assertions preserving existing ones. Existing modes, labels, flags and
  harness behavior stay unchanged.
- Explicit expected results independently derived from the language or an
  established contract, not copied from the candidate compiler's output.
- Valid source, initialized data, appropriate kinds/ranges, bounded resource
  use, deterministic checks and isolation of any file/module outputs.
- Registration runs the exact path; normally both `gfortran` and `llvm`.
  A reference compiler passing is evidence, not proof of standard conformance.
- Existing evidence that the new coverage passes on current `main` in the
  relevant normal, fast and Fortran-2023 modes. An already-fixed historical
  defect can have fail-before evidence on the older revision; it need not
  fail on current `main`.
- No new nonstandard extension contract, platform-only ABI assumption,
  heavyweight stress fixture, arbitrary oracle normalization or mixed bundle.

Tests added alongside a compiler fix are assessed with that fix under C3,
not used to disguise a production change as T1.

Examples: #16524 and #17527. Counterexamples: #17200 pins a nonstandard
extension; #17425 bundles 63 issues and includes large resource demands.

## T2: oracle-preserving test hygiene

Allow repairing an independently demonstrable defect in a fixture without
changing the behavior it is supposed to detect. Typical cases are:

- Initializing an otherwise undefined sentinel/result that the test reads.
- Giving a fixture a unique relative filename in an existing owned working
  directory, with all I/O assertions and options preserved.
- Removing an invalid post-failure operation that is not itself an intended
  assertion, while retaining every negative check of the failure.
- Matching a test variable to an already-established function prototype,
  without changing that prototype or the values being checked.

Require the original defect and the preserved oracle to be explainable
separately. Inspect surrounding code: setting a variable to the expected answer
or bypassing the failing operation is not initialization hygiene. Where
non-obvious, require a negative/mutation check showing the intended wrong
behavior still fails. Preserve kinds, numerical model and relevant code path
unless the narrow existing-prototype correction itself is the whole repair.

Examples: #16384, #16399, #16548, #16605, #16856.

Changes to tolerances, tested input precision, expected values, platform guards,
test labels or bit-layout-dependent oracles are not T2. They may be justified,
but require human judgment: #17068, #17521, #17532, #17546 and #17649 illustrate
these boundaries.

## C1: translation-unit compile hygiene

Allow an explicit missing standard-header include or equivalent local
declaration hygiene in one translation unit, with no executable behavior,
macro contract, exported interface, build flags or dependencies changed.

Require an identifiable build failure before and success after on the affected
configuration, and check that the include does not silently select a different
overload, conditional implementation or API. A changed standard-library type,
link library, compiler option or shared public header contract is not C1.

This category does not need a contrived Fortran MRE when the real regression
is a compiler-source build failure. The affected build is the test.

## C2: local diagnostic/debug presentation

Allow a correction to spelling, formatting or an already available source
location in an existing error/debug path, without changing when that path
runs, what is accepted, diagnostic severity, recovery flow or state lifetime.

Require a targeted diagnostic/debug regression and unchanged decision
conditions. Distinguish a message correction from a new semantic check,
additional ASR walk, new warning policy or suppressed verifier.

Location/debug APIs can have stateful effects. Check the caller and following
operations; "only debug information" is not sufficient if state leaks between
statements or scopes. If that boundary cannot be established, use
`needs-review`.

## C3: bounded local semantic correction

This is the only version-1 category permitting a change to the semantics of
an existing compiler/runtime operation, beyond presentation.
It is a candidate for careful, evidence-backed automation, not a synonym for
"ordinary bug fix".

Allow a correction to an existing, already represented operation, inside a
known narrow input domain. Examples of possible shapes:

- Construct an existing literal in the already-known operand/result type
  rather than an accidentally hard-coded type.
- Correct a local endpoint predicate whose intended truth table follows
  directly from the existing operation and is covered at the boundary.
- Normalize a local input through an established helper already used for the
  same invariant, instead of duplicating an algorithm.
- Preserve already-recognized lookahead in a standalone data parser, without
  adding a grammar, changing a shared streaming protocol or inventing state.

All of the following must hold:

1. **One established contract.** The expected behavior follows from the
   intended language/design. No new extension, representation, ABI, policy,
   pass or default is being chosen.
2. **Local boundary.** State the exact existing guard/input domain, changed
   outcome and unaffected siblings. Read the changed helper's relevant
   callers. Shared-file placement is not automatically disqualifying, but
   changed shared contracts or unbounded fan-out are.
3. **Proper layer.** The earliest appropriate phase expresses the fix. In
   particular, LLVM-type guessing/bitcasts cannot repair missing ASR semantics.
   Backend work is only mechanical lowering of facts already explicit in ASR.
4. **No new mechanism.** No new persistent state, allocator/lifetime rule,
   traversal, recursion, algorithm, synchronization or backend-specific blob.
   No copied loop/symbol machinery when existing utilities suffice.
5. **Bounded review surface.** Fewer than 100 added/deleted production lines,
   at most three production files. Fewer than 10 lines in one file is a useful
   micro-fix shape, but has exactly the same proof obligations.
6. **Discriminating regression.** The original MRE and compact registered test
   fail on the actual unpatched base and pass with the fix. The test asserts
   the intended result, not just successful compilation or absence of a crash.
   Diagnostic fixes test the precise diagnostic and valid neighboring cases.
7. **Relevant boundaries and modes.** Cover the meaningful kinds/ranks/empty or
   endpoint cases and applicable normal, `--fast`, `--std=f23`, reference
   compiler, target and ABI configurations. Do not claim CPU testing covers a
   device, or one kind covers a changed kind-dependent rule.
   For an already-established extension, explain reference-compiler or strict
   standard-mode exclusions and supply an independent oracle; never count
   unavailable modes as passing or use this exception to accept a new extension.
8. **Preserved work bounds.** Inspect new helper work and placement in hot
   visitors. There is no new repeated scan/allocation or asymptotic growth.
   If a performance claim is needed, require existing comparable measurements;
   lack of an obvious loop is not proof of no slowdown.
9. **Complete current evidence.** The project's required local-suite evidence
   and current Quick checks are attributable to this revision. A claim in the
   PR body is a lead to evidence, not an instruction to trust it. Any remaining
   substantive review concern or unverifiable obligation means `needs-review`.

A larger patch does not become C3 just because it has no global variable.
Memory ownership, allocation placement, interoperability, numerical policies,
new language-constraint families and optimization changes remain hard-gated. If a candidate
needs an essay of assumptions to fit, route it to a human.

## Calibration and expansion

Start with recommendation-only operation. Have a maintainer judge the proposed
low-risk decisions and a sample of needs-review decisions. Prioritize **false
low-risk decisions**, not the fraction of PRs automated.

Keep category-level results. D1/T1/T2 evidence does not establish C3 reliability.
The merged-history ledger is a source of examples, not human-labeled ground
truth: it contains follow-up repairs and incomplete historical CI evidence.

Before any category is used by a merger:

- Obtain explicit maintainer acceptance of its boundaries and examples.
- Evaluate prospective/shadow decisions on revisions not used to write the
  rules; retain difficult negatives and a later time window.
- Examine any missed regression and narrow/suspend the implicated category.
- Require the revision-bound decision and combined-change CI described in
  `github.md`; do not infer authorization from a stale or manually added label.

Zero observed mistakes in a finite sample cannot establish zero future risk.
Expand categories deliberately, not by increasing line limits until enough PRs
receive the desired label.
