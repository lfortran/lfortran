# Two-week study behind the draft

## Conclusion

There are useful low-risk patterns, but **small + tested + merged is not an
adequate category**. Start with passive prose, monotonic regression coverage,
oracle-preserving fixture repairs, and a few tightly bounded production
patterns. Default everything else to human review.

The initial screen identified **12 plausible pattern candidates out of 439
merged PRs**. That is not an estimate of defect probability or a claim that
those twelve were historically eligible for automatic merging. The other
427 fail a conservative gate or need more evidence; they are not being called
bad PRs. This is intentionally a precision-first starting point, not a target
automation percentage.

## Scope and method

- Repository: `lfortran/lfortran`.
- Fixed window: **2026-10-30 09:44:40 UTC through 2026-11-13 09:44:40 UTC**,
  corresponding to the two weeks before the request.
- All **439** merged PRs in that window, all targeting `main`; pagination,
  uniqueness and date bounds checked. No cherry-picked author or topic.
- Complete diff/file corpus: **423** checked local merge/squash reconstructions
  and **16** authoritative GitHub fallbacks where local shape/counts did not
  match. Fallback warnings were resolved, not treated as missing evidence.
- All available discussions collected: **56 issue comments, 31 reviews and
  23 review threads**, with no pagination truncation.
- Separate counts for production paths, tests, registration, snapshots,
  documentation and control-plane files. Added plus deleted lines, including
  whitespace/comments in production files; not net growth.

| Changed production-file lines | PRs |
| --- | ---: |
| 0 | 70 |
| 1-9 | 48 |
| 10-29 | 78 |
| 30-99 | 104 |
| 100-299 | 67 |
| 300 or more | 72 |
| **Total** | **439** |

The [per-PR ledger](merged-prs.csv) contains every selected PR, its size,
assessment, concrete routing reason, inspection depth and source link.
Large/control-plane PRs can be excluded by an explicit hard gate without a
line-by-line correctness proof. The 230 production changes below 100 lines
were investigated for narrow safe patterns; many were also excluded by an
architectural gate rather than fully certified. Candidate tests and relevant
callers received closer inspection. Generated reference snapshots were omitted
from ordinary review packets, but retained in the complete corpus and consulted
where relevant.

**Limits:** this was a retrospective change/risk study, not 439 fresh builds or
a reconstruction of all pre-merge CI states. Reported before/after test results
remain attributed to PR evidence. A merge, lack of a reported regression, or
an author's successful-run claim is not human-labeled ground truth. Review
comments sometimes led to meaningful changes; the merged patch can therefore
be safer than the initially proposed patch. The cohort has short follow-up and
cannot establish the absence of latent bugs.

## Positive patterns

These are candidates for the rubric, not unconditional historical approvals.
Each live use still needs current revision-bound evidence.

| Pattern | PRs | Why it is promising |
| --- | --- | --- |
| Passive prose | [#16202](https://github.com/lfortran/lfortran/pull/16202) | Conditional Windows `zstd` troubleshooting next to existing optional binary-install instructions; no executable packaging/configuration change. |
| Additive established-behavior coverage | [#16524](https://github.com/lfortran/lfortran/pull/16524), [#17527](https://github.com/lfortran/lfortran/pull/17527) | Compact self-checking regressions and ordinary `gfortran llvm` registrations; behavior already passes on main. |
| Initialize undefined fixture data | [#16384](https://github.com/lfortran/lfortran/pull/16384), [#16605](https://github.com/lfortran/lfortran/pull/16605) | Defines values the existing oracle reads without changing the exercised operation or intended assertion. |
| Remove invalid fixture side effects | [#16399](https://github.com/lfortran/lfortran/pull/16399) | Removes uses of undefined units after failed OPEN; preserves every OPEN/IOSTAT/IOMSG assertion, with reported negative checks. |
| Isolate fixture files | [#16548](https://github.com/lfortran/lfortran/pull/16548) | Three filenames change; I/O, kinds, options and assertions stay intact, under the verified owned-working-directory premise. |
| Match an established test API | [#16856](https://github.com/lfortran/lfortran/pull/16856) | Test storage/result become `int64_t` to match the already changed runtime prototype; the runtime contract itself is untouched. |
| Missing standard include | [#17210](https://github.com/lfortran/lfortran/pull/17210) | Adds `<cstdint>` before LLVM headers; discussion supplies affected LLVM 15/17/18 build results. |
| Preserve recognized parser lookahead | [#16590](https://github.com/lfortran/lfortran/pull/16590) | Do not consume the next already-recognized DWARF table header; tests cover separated/unseparated tables. This is a standalone debug-data parser, not a repository-wide test-runner change. |
| Normalize for an existing check | [#17096](https://github.com/lfortran/lfortran/pull/17096) | IOMSG validation unwraps allocatable/pointer types with the existing helper. It does not change the actual expression/descriptor or the preceding variable/scalar checks. |
| Construct a literal in the existing operand type | [#16940](https://github.com/lfortran/lfortran/pull/16940) | Replace hard-coded float `1.0` with the already-emitted operand's type. No type-mismatch repair, new cast or ABI choice. `expm1` was already an extension, so omitted reference/f23 modes must be explained, not silently counted as passes. |

C2 presentation-only corrections remain a prospective narrowly defined
category, not a class validated by many accepted examples in this window.
Both tiny debug-location examples were conservatively held for state-boundary
review rather than treating "debug-only" as automatic permission.

## Why the proposed broad categories are insufficient

| Tempting rule | Counterexample | Actual decision |
| --- | --- | --- |
| "Only two production lines" | [#16490](https://github.com/lfortran/lfortran/pull/16490) changes the default thunk pool from 64 to 8. An OpenMP test explicitly needs 16. | Resource/default/concurrency policy needs review. |
| "Only tests" | [#17532](https://github.com/lfortran/lfortran/pull/17532) replaces exact comparison with a tolerance; the first proposed tolerance was revised after review. | Numerical oracle changes need judgment, even with a compelling justification. |
| "Only a test registration" | [#17068](https://github.com/lfortran/lfortran/pull/17068) adds `NO_FAST_MATH`; [#17521](https://github.com/lfortran/lfortran/pull/17521) removes a backend label. | This changes coverage, unlike adding an ordinary new RUN entry. |
| "Small additive test" | [#17200](https://github.com/lfortran/lfortran/pull/17200) pins a nonstandard assumed-rank SELECT TYPE extension. | The supported-language contract must be chosen by a human. |
| "The title says test" | [#17468](https://github.com/lfortran/lfortran/pull/17468) also relaxes a complex(16) ABI assertion in production code. | Inspect every file and hunk, not the title. |
| "Under 100 production lines" | [#17142](https://github.com/lfortran/lfortran/pull/17142) has 44 production lines but adds ASR state and affects cross-object linking. | Schema and cross-phase contracts remain high-impact. |
| "One local helper" | [#17483](https://github.com/lfortran/lfortran/pull/17483) changes a scanner also used by formatted/complex reads, not just the new scalar test. | Trace callers and all affected domains. |
| "Diagnostics cannot affect compile speed" | [#16488](https://github.com/lfortran/lfortran/pull/16488) review identified an extra ASR walk and repeated statement walks. The implementation was revised. | Check work placement and state, not just code size. |
| "Documentation only" | [#16823](https://github.com/lfortran/lfortran/pull/16823) also removes environment files and changes tooling. [#17458](https://github.com/lfortran/lfortran/pull/17458) edits the release skill. | Build and agent-policy Markdown is control-plane code. |
| "No compiler code" | [#17202](https://github.com/lfortran/lfortran/pull/17202) changes the shared test macro's module directories. | A harness fix changes behavior across tests and needs review. |

The backend restrictions are not a ban on all backend work. #16940 is
mechanical typed-constant construction; inferring missing semantic facts from
LLVM types, changing descriptors/allocas, or adding a large special-purpose
lowering routine is different.

## A near-candidate that was rejected on closer inspection

[#17502](https://github.com/lfortran/lfortran/pull/17502) deduplicates complex
runtime-name selection in **84 changed production-file lines**. For the six
operation names and kinds 4/8/16, the new helper yields the same strings.
All eight current call sites are dominated by an existing kind guard; this is
not an ABI change merely because it manipulates runtime symbol names.

It initially looked like a good larger mechanical category. Closer inspection
found that the helper's signature does not enforce that finite domain, the
old fallback differs outside it, the inspected tests do not directly cover
kind-16 variable-integer exponentiation, and the new string construction adds
bounded work whose compile-time effect was not measured. The final ledger
therefore records **needs-review**, not a low-risk claim with conditions.

This suggests a future *finite-domain mechanical equivalence* category, but
not a blanket refactor exemption. It would need exhaustive caller/precondition
and error-path reasoning, relevant coverage, and an agreed performance-evidence
standard. None of the 104 changes in the 30-99-line bin was retained as an
initial low-risk pattern after this stricter pass.

## The merge-combination problem is real

[#16856](https://github.com/lfortran/lfortran/pull/16856) documents an interaction
between #16820 and #16821: one changed INQUIRE's POS argument to 64 bits, while
the other added a test using 32-bit storage. They merged without a textual
conflict; together they failed compilation and otherwise risked an 8-byte write
to a 4-byte object. This is not a hypothetical label-lifecycle problem.

There are also green-Quick/main follow-ups for test portability and undefined
fixture values, such as [#16605](https://github.com/lfortran/lfortran/pull/16605).
These observations support exact-target coverage and valid tests; they do not
justify automatically running Exhaustive for every PR, contrary to project
policy.

On November 13, 2026 the read-only rules snapshot showed twelve app-bound
required checks, loose freshness and no required merge queue. Neither risk
label existed. Quick had no `merge_group` trigger. Nothing in that setup was
changed by this work.

**Before eventual merging**, add independent current-head/base validation,
trusted decision provenance, invalidation on updates, and combined-change CI
or a correctly configured queue. A label is mutable metadata, not a
commit-bound certificate. See [GitHub details](github.md).

## Splitting that actually helps

1. **Already-passing regression first.** #16524 was split from #15895 after
   the compiler fix had already landed. That is a useful independent T1 PR.
2. **Separate tiny fixture hygiene.** Keep repairs like #16384/#16548 apart
   from changes to the compiler that happen to reveal them.
3. **Keep a failing regression with its fix.** Do not create a red prerequisite,
   an xfail, or a temporarily weakened oracle just to make the code PR smaller.
4. **Separate independent contracts.** #17197 combines storage-size and
   module-serialization fixes; those have distinct reasoning and regressions.
   Both may still need human review.
5. **Keep architectural invariants atomic.** For #17142-like schema changes,
   producers, verifiers and exhaustive consumers must agree in every mergeable
   intermediate state. Small dependent slices are not independent low-risk PRs.
6. **Split large test bundles.** #17425's compact existing-behavior regressions
   could be grouped by coherent purpose, separately from stress/resource cases
   and language-policy judgments.

The target is independently correct intermediate states, not ten-line PRs.

## How to decide whether this will work for the project

Run the skill in recommendation-only mode first. Have the maintainer inspect
every proposed low-risk result, plus a representative sample of rejected easy
changes. Keep the candidate category, exact revision and reason, not just the
final label. The most useful metric is **false low-risk decisions per category**;
the automation fraction is secondary.

The bundled offline evaluations exercise category traps, readiness/label
transitions and semantic boundaries with and without the skill. They are
development smoke tests, not independent human gold labels or evidence of
production precision. A later prospective window must be held out before
enabling a merger.

The draft is deliberately useful without automerge: it produces a small,
explainable low-risk queue and a human-review queue, and reveals which evidence
or design boundary prevents promotion. Expand categories from actual
maintainer disagreements and prospective outcomes, not from a desired merge
rate. No finite empirical study can guarantee 100% correctness.

## Initial offline evaluation

Three tasks contain 22 synthetic cases: category traps, readiness/label
transitions, and semantic boundaries. Each ran once with the draft skill and
once with ordinary repository guidance but without the new skill.

| Result | With skill | Without skill |
| --- | ---: | ---: |
| Expected case decisions | 22/22 | 20/22 |
| Workflow assertions | 33/33 | 30/33 |
| False low-risk decisions in these cases | 0 | 0 |

Both configurations produced the same aggregate label counts. The differences
were routing: the baseline left a green PR with unresolved correctness concerns
unlabeled instead of `needs-review`, and treated a job that merely echoed a
test-success message as eligible. Neither difference led the baseline to
recommend automatic merging.

One initial expected eligibility was corrected during analysis: replacing the
required reference-test command with `echo` violates the skill's already-written
CI-execution gate, so that fixture is ineligible, not eligible/needs-review.
The fixture and policy were not changed to fit the result. This correction,
the synthetic inputs, and the absence of independent human gold labels are
reasons not to present the scores as measured production safety.

The session artifacts include the static HTML reviewer, exact outputs and
per-assertion grading. Timing and token telemetry were unavailable and are not
claimed. The policy still needs maintainer feedback and prospective shadow
evaluation; no labels, merges or protection settings were changed.
