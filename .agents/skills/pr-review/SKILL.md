---
name: pr-review
description: >
  Review LFortran pull requests and code diffs using the project's architecture
  and established review rules. Use this skill whenever asked to review a PR,
  assess a patch, judge PR readiness, inspect an LLVM/codegen change, or suggest
  where an LFortran compiler fix belongs. Push decisions as early as practical
  into AST-to-ASR, explicit ASR state and verification, or a focused ASR pass;
  keep backends small and mechanical. Covers correctness, reuse, tests, scope,
  and maintainability with concrete good and bad implementation patterns.
compatibility: Requires git; use gh when reviewing a GitHub pull request.
---

# Review an LFortran Pull Request

Find correctness and design problems before style issues. The central question
for every backend change is:

> What decision is the backend making, and can the compiler make that decision
> earlier and represent it explicitly in ASR?

Read `AGENTS.md` first. Read [references/review-rules.md](references/review-rules.md)
for the detailed principles. Consult [references/examples.md](references/examples.md)
when a concrete comparison would help.

## Gather the change

For a GitHub PR, collect the description, commits, changed files, and diff:

```bash
gh pr view <number> --repo lfortran/lfortran \
    --json title,body,author,baseRefName,headRefName,commits,files,reviews
gh pr diff <number> --repo lfortran/lfortran
```

For a local branch, inspect the merge-base diff and the worktree. Do not review
only the latest commit when the PR contains several commits.

Identify:

1. The bug or feature and its MRE.
2. The first compiler phase where behavior becomes wrong.
3. The invariant the fix should establish.
4. The tests that prove the old behavior fails and the new behavior works.

Check that the description's rationale and every helper, artifact, or behavior
it names are present and accurate in the diff. Treat architectural claims as
hypotheses until the code and reproducer support them.

If these cannot be determined from the PR, report that as a review finding
rather than guessing.

## Review placement from earliest to latest

Walk this ladder in order. Stop at the earliest layer that can express the
decision cleanly:

1. **Parser / AST:** syntax and source-form distinctions. Parser actions should
   produce AST even for semantically invalid input; semantic diagnostics belong
   in AST-to-ASR.
2. **AST-to-ASR:** symbol resolution, type coercion, casts, overload selection,
   semantic diagnostics, and other source-language decisions.
3. **ASR representation:** add or use a node field, enum, structured type, or
   other explicit state when downstream code needs to know a semantic fact.
   Extend ASR verification so malformed states fail near their origin. Only
   symbol nodes own symbol tables; represent statement-local scopes with a
   `Block` or a dedicated symbol node. A new ASR node also needs minimal support
   in every exhaustive visitor, including verification, round-trip printing,
   and dependency collection.
4. **Existing ASR pass:** use or extend a pass when the decision requires a
   whole-tree transformation or canonical lowering.
5. **New ASR pass:** consider one only when it localizes a coherent algorithm,
   simplifies multiple consumers, produces a useful explicit IR, and has clear
   ordering and invariants.
6. **Backend:** keep only target-specific mechanics that cannot be represented
   earlier.

Do not accept an LLVM-type query, opportunistic bitcast, magic string, or
backend-only type inference as a substitute for understanding the ASR. Querying
LLVM for a type and casting based on the result is a warning that ASR does not
yet express what is happening.

### Decide whether to add an ASR pass

An ASR pass is justified when most of these are true:

- The transformation requires non-local knowledge or repeated tree rewriting.
- Several backends or later phases benefit from one canonical form.
- The output invariant is explicit and verifiable.
- Centralizing the algorithm removes duplicated complex logic.
- The pass can be enabled, disabled, ordered, and tested independently.

Prefer an ASR field plus verifier, an existing pass, or direct AST-to-ASR
construction when the transformation is local. A new pass adds compile time,
ordering dependencies, maintenance, and another failure boundary; LFortran
should have a few strong passes rather than many narrow ones.

If the work must remain in a backend, require the smallest target-specific
change possible. Reuse `asr_to_llvm`, `llvm_utils`, `ASRUtils`, `PassUtils`, and
existing lowering helpers. Reject duplicated loop builders, copied symbol
lookup, hard-coded argument positions, and long one-off blobs.

## Review correctness and invariants

- Trace types, ranks, kinds, ownership, physical representations, and ABI facts
  from ASR to the changed code.
- Check scalar and array forms, sections, allocatable/pointer forms, inherited
  types, type-bound procedures, nondefault kinds, and target differences when
  relevant.
- Preserve language semantics, not merely contiguous memory behavior. For
  example, character-array elements used as an internal file are separate
  records; flattening them into one byte stream is wrong.
- Prefer explicit structured state over string inspection and positional
  assumptions.
- Identify intrinsic types and modules by structured origin, not a spoofable
  name string.
- Distinguish compiler-synthesized and user-visible entities with explicit ASR
  provenance, not generated-name prefixes or node-kind guesses.
- Represent defined-layout properties such as `SEQUENCE` and `BIND(C)` in ASR;
  use that state for physical layout and interoperability decisions.
- Preserve symbol-table invariants: imported symbols point to their original
  definitions, and symbol creation does not duplicate an existing local name.
- Use assertions that test the actual boolean invariant, or ASR verification,
  for impossible states instead of quietly accepting them through defensive
  conditionals.
- Propagate failures with `Result` and diagnostics carrying source `Location`;
  do not introduce exceptions or broad try/catch recovery.
- Point diagnostics at the precise offending tokens and render types as valid
  Fortran syntax.
- When accepting a deliberate nonstandard extension, emit a located portability
  warning that explains the conforming alternative.
- In runtime C, pass actual buffer capacities to bounded APIs and avoid
  undefined conversions or writes.
- Ensure `libasr` remains frontend-independent.

## Review reuse and scope

Search before accepting a new helper or algorithm:

```bash
rg "<related symbol or operation>" src integration_tests tests
```

Ask whether the patch:

- duplicates a scalar path for arrays instead of normalizing the type once;
- adds a core utility when a pass-layer helper already exists;
- reconstructs loop nests, bounds, or array references already handled by a
  shared helper;
- mixes a bug fix with refactoring, formatting, generated-output churn, or an
  unrelated cleanup;
- adds comments that repeat the code or describe behavior not implemented; or
- introduces unused, dead, or unreachable code.

Recommend the concrete existing helper or layer, not just "simplify this."

## Review tests

Require a test that fails without the fix and passes with it.

- Prefer a compact registered integration test with at least `gfortran` and
  `llvm` labels when the behavior can run end to end.
- Verify that CI executes the exact binary, backend, options, and code path
  changed by the PR.
- Add a targeted edge case for each newly handled error or semantic branch.
- Put recoverable semantic diagnostics in a complete continue-compilation test
  and verify each error branch independently.
- Keep shared diagnostic fixtures append-only so new cases do not shift existing
  source locations.
- Test portability warnings themselves, not only the accepted runtime behavior;
  document why unsupported reference compilers are omitted from test labels.
- Differential C interoperability tests must include each compiler's own
  `ISO_Fortran_binding.h` and use its corresponding runtime.
- Keep fixtures proportional to the bug. A 1,300-line program for one
  out-of-bounds error is not an acceptable regression test.
- Avoid large reference-output churn. Make preparatory spacing or mechanical
  changes separately so the behavioral diff remains reviewable. Regeneration
  caused directly by an ASR schema change is legitimate, but should be
  mechanically consistent.
- For external-project CI failures, identify the failing toolchain and root
  cause before accepting a skip or an upstream patch that disables coverage.
- Build changes must track real inputs, quote paths, fail early when required
  artifacts are absent, and avoid committed binary or large generated blobs.
- Any CI build or generation step that can fail must run on pull requests unless
  it inherently requires main-only publishing credentials.

## Account for pragmatic follow-ups

An approval can mean "safe enough to merge and iterate," not that every line is
endorsed. Keep unresolved correctness issues visible. Classify each finding as:

- **Blocker:** wrong semantics, invalid ASR, regression, unsafe error handling,
  or an untested core behavior.
- **Rework:** maintainability or placement problem that should be fixed in this
  PR because it shapes the implementation.
- **Follow-up:** bounded debt that does not invalidate the current change; state
  the exact follow-up test or refactor.

Do not silently downgrade a correctness bug because another review approved the
PR.

A draft, pending, or otherwise unsubmitted review does not gate merging. Confirm
that any blocker-level concern was resolved or explicitly tracked before
treating a merge as evidence that the implementation is sound. Treat approval
conditioned on a fresh full run as incomplete until that run succeeds.

## Write the review

Report only actionable findings, ordered by severity. Use this shape:

```text
[blocker|rework|follow-up] Short title
path/to/file.cpp:<line>

What is wrong and the concrete input or invariant that demonstrates it.
Why the current layer or implementation is risky.
Recommended shape: the earlier phase, ASR state/pass, or existing helper to use.
```

Then add a short **Architecture assessment**:

- earliest wrong phase;
- chosen fix layer and whether it is justified;
- ASR invariant added or still missing;
- backend complexity added or removed.

If no actionable findings remain, say so and mention only meaningful residual
risks or test gaps. Treat automated-review findings as hypotheses: reproduce
and understand them before reporting them as blockers. Do not manufacture style
comments.
