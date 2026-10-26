---
name: merge-pr
description: >
  Decide whether LFortran pull requests can be merged and act on the decision,
  the way the maintainer does: judge the change itself (size, layers touched,
  locality, non-local interactions, placement, completeness, design/policy
  impact) into a risk class, check the hard gates (ready, no conflict, all
  twelve Quick jobs green on a fresh enough head, fail-before/pass-after test,
  linear history, stack order, open blocking reviews), then merge, or do the
  exact remedy: post the standard conflict/CI-red note and mark draft, rebase
  after a main fix, adopt an external PR by cherry-picking it into a fork PR
  (or rewriting it with credit), consolidate duplicates, split an oversized
  PR into a bottom-up stack, close superseded PRs with a link, file leftover
  bugs, or escalate design decisions. Use when asked to merge, land, triage
  for merging, "go through the open PRs", or judge whether a PR is ready.
  Not for writing a code review only (pr-review) or fixing issues (fix-issue).
compatibility: >
  Requires git and GitHub access (authenticated gh or GitHub MCP tools) with
  merge rights on lfortran/lfortran and a user fork for adoption PRs. Local
  verification of an adopted or rewritten PR uses Pixi/LLVM 11 per AGENTS.md.
---

# Merge PR — Judge, Then Merge or Do the Exact Next Step

This skill encodes how the maintainer decides merges, distilled from the PRs
cited in [references/decision-evidence.md](references/decision-evidence.md).
The decision has two independent halves, and both must pass:

1. **Content judgment** — is this change, as written, something `main`
   should carry? Size alone never decides; what decides is *who executes the
   changed code*, *what else it can break*, and *whether it is the right fix
   in the right layer*.
2. **Gates** — is the evidence complete and current (ready, mergeable, all
   Quick jobs green on a fresh enough head, the right tests)?

Green CI never makes a PR mergeable by itself (#15819/#15820 were green and
clean, and were closed on design), and a red gate on a sound change is a
chore to do, not a reason to drop the contribution.

## Authority

- A request to **merge / land / process** PRs authorizes: merging PRs that
  pass both halves, the standard draft-marking comments, rebasing and pushing
  *the user's own* branches, opening draft adoption PRs from the user's fork,
  closing PRs that are superseded or landed (with a link), and filing issues
  for leftover bugs.
- A request to **assess / triage / check readiness** is read-only: report the
  decision cards and stop.
- Never: approve a PR review in the user's name, force-push or rewrite
  someone else's branch (adopt instead), merge a draft, merge a class-R3 PR
  without the user's explicit decision on it, bypass CI on a compiler-source
  change, or approve fork CI workflow runs on your own (tell the user which
  runs to approve; see Gate G3).
- Follow AGENTS.md's commit-authorship rules in every commit you create.

## Procedure

For each PR (batch: see "Batches" below):

1. **Collect** (`gh pr view --json ...` / `pull_request_read`): body,
   draft, mergeable state, head SHA, commits (authors, merge-from-main
   commits), files and diff, reviews and review threads, comments, check
   runs on the head SHA, linked issues (`fixes #N`), and open PRs that target
   the same issue or the same files (duplicates, stacks). Record the base SHA
   of the last green run and what merged to `main` since.
2. **Judge content** (next section) → risk class R0–R3 plus findings.
   For R2, or whenever placement is in doubt, also run the `pr-review`
   skill on the diff; treat its blockers as content findings.
3. **Check gates** G1–G9.
4. **Decide** one verdict from the decision table and **act** on it.
5. **Report** a decision card per PR (format at the end).

## 1. Content judgment

Answer each question with evidence from the diff (`rg` the callers of every
changed helper; do not guess).

**a. Size and layers.** Count files, +/- lines, and distinct layers:
tokenizer/parser, AST→ASR semantics, `ASR.asdl` schema, ASR verify, a named
ASR pass, pass pipeline/ordering, LLVM (or other) backend, runtime C,
`.mod` format, CMake/CI, tests, references, docs. Separate *mechanical*
churn (code moves, schema regeneration, reference refresh) from *behavior*
lines — a +8.7k/−8.1k pure move (#15918) is lower risk than a 20-line change
to a shared predicate.

**b. Locality — who executes the changed code?**
- *Local:* only the reported construct, behind a narrow guard (one node kind,
  one type/kind combination, one intrinsic, an opt-in flag such as `--gpu`).
  Examples: #15588, #15701, #15960, #15601 (`cast_to_wider_kind` acts only
  when kinds differ).
- *Non-local:* default compile path for every program, a shared helper in
  `asr_utils`/`pass_utils`, what a pass skips (`is_temporary_needed`, #15122),
  pass pipeline membership or order (#15870 broke the C backend), symbol
  tables and scoping/`use` import (#15323, #15888), allocation/realloc/
  finalization semantics, runtime I/O or RNG paths every program hits
  (#15806, #12439), compiler–runtime ABI, `.mod` file layout (#15351), ASR
  schema, default options or file-form defaults (#12293).

**c. Non-local interactions.** For every non-local change enumerate the
consumers and require evidence for each: other callers of the helper, later
passes, every backend that runs the pass pipeline (LLVM, C/C++, WASM,
Fortran, GPU/Metal), separate compilation and mod files, `--fast`, and
other open PRs that add `tests/asr/**/*.asr` inputs or references that a
schema change invalidates (#15868 broke `main` this way).

**d. Placement.** Is the fix in the earliest correct layer (pr-review's
ladder)? Red flags: a backend that infers or fixes types, LLVM type queries
plus casts, a backend workaround for a semantic problem (#11414 removed a
`free` → redone in semantics as #11417), name-string checks, new
`PassOptions`/CLI plumbing to dodge a stale special case (#15962 → rewritten
as #15990), duplicated implementation instead of reuse (#12403 → #14648).

**e. Completeness and trade-offs.** Sibling cases covered (integer/real/
complex, scalar/array, sections, kinds — #15924 missed integer/complex
folds)? Does it give up a capability (stops folding, disables an
optimization) or trade one bug for another ("Leaks memory. … We can't have
memory leaks.", #11414)? Does it remove or weaken existing tests ("Removing
all these tests does not seem right?", #13886)?

**f. Mitigations present.** Narrow guard; an ASR-verify rule that makes the
wrong state unrepresentable (#15875, #15877, #15913); a "not supported yet"
diagnostic instead of a miscompile (#15876, #15903); neighbouring and
negative tests; a test per affected backend label (#15951 added `c`);
mechanical churn in its own no-behavior-change commit (#15870, #15877);
ordered single-topic commits with a "why X is fixed here too" note (#15671).

**g. Design and policy.** Does it add a long-lived surface — a protocol, a
third-party dependency, a CI configuration, a CLI option, a new pass — or
change a user-visible default or product choice (`.f` as fixed form,
#12293; leading space in list output, #8572)? Standard-mandated behavior is
fine (DATA implies SAVE, #15118); a product choice is the maintainer's call.

**h. Hygiene.** Unrelated edits (#12321 dropped `*.so` from `.gitignore`),
refactor mixed with a fix, dead code, unregistered or misplaced tests ("This
is not used?", "This should not be in the root directory."), a description
that no longer matches the diff (#12439), oversized fixtures, unexplained
reference churn (#13877: 1,549 reference lines for a ~30-line fix).

### Risk classes

| Class | What it is | Merge rule |
| --- | --- | --- |
| **R0 trivial** | docs, skills, CI script, test-only, reference refresh, one-line CI exclude for a known external toolchain bug | merge on green; a test/CI-only fix that unbreaks `main` may merge before its own CI only when the user says so (#15925, #15976) |
| **R1 local** | one function/branch for one construct, earliest correct layer, guarded, with a fail-before/pass-after test | merge on green, no human review needed (#15122, #15960, #15588) |
| **R2 non-local but disciplined** | shared helper, default path, pass pipeline, schema, scoping, runtime ABI, many references, or >2 layers of behavior change | merge on green **only if** every mitigation in (f) that applies is present, each consumer in (c) has evidence, CI is fresh against current `main` (G4), and the body's Rationale explains the placement; otherwise request exactly the missing items (#15323, #15870–#15874, #15806) |
| **R3 design/policy** | new long-lived surface, default/product change, capability trade-off, or a PR too large to review as one unit | never merge on your own; escalate with the analysis, or propose a split (#14615 → 10 PRs, #15760 → split, #15819/#15820 closed for #15934) |

A PR whose behavior change spans many layers (#14615: 828 files, 9 layers)
is R3 *until split*: schema-only and interface-only pieces first, behavior
after, each piece R1/R2 on its own, merged bottom-up.

Any placement red flag (d), capability loss or bug trade (e), or test
removal is a blocker regardless of class.

## 2. Gates

- **G1 Ready.** Not a draft. Drafts are never merged (#15974). For the
  user's own agent PRs, "ready" is the user's step after review; if asked to
  merge one, mark it ready only after both halves pass.
- **G2 Mergeable.** No conflict with `main`.
- **G3 CI.** All twelve required Quick jobs (listed in
  `doc/src/installation.md#ci-coverage`) report `success` on the current head
  SHA. Fork PRs may sit at `action_required`: workflow approval is the
  maintainer's decision, so judge content first and tell the user which runs
  to approve (only for R0–R2 content that is plausibly mergeable). Exhaustive
  is not a gate unless the user asks for it.
- **G4 Freshness.** A stale green run is acceptable (#15122 merged on an
  11-day-old run) **unless** `main` has since merged an `ASR.asdl` change, a
  reference refresh, a pass-pipeline change, or edits to the same files, and
  the PR adds/updates references or `.asr` inputs or touches those areas —
  then require a rebase and a fresh green run (#15868). Re-check this after
  every merge in a batch.
- **G5 Tests.** Behavior changes carry a registered test that fails on `main`
  and passes on the branch (integration test with `gfortran` and `llvm`
  labels preferred; diagnostics appended to
  `tests/errors/continue_compilation_1.f90`, #13877, #12339). CI/docs-only
  changes are judged by their own harness tests and the linked failing run.
- **G6 History.** Linear, single-topic commits, no merge-from-main commits
  (ask for a rebase), no AI co-author trailers (CI enforces).
- **G7 Stack.** Every PR it is stacked on is merged first; the body says
  "stacked on #N". Merge bottom-up (#15870 → #15873 → #15874; #15877 →
  #15880 → #15884).
- **G8 Reviews.** No unresolved changes-requested review or blocker-level
  thread from a member or the area owner (coarrays: bonachea, #12466).
  Non-blocking points may become follow-up issues (#12222 → #12339).
- **G9 Main health.** If `main`'s Quick is red, fixing `main` comes first
  (smallest test/CI-only fix forward; a PR red only because of `main` waits
  for that fix and a rebase, #15913).

## 3. Decision table and actions

| Situation | Verdict | Action |
| --- | --- | --- |
| Both halves pass | **MERGE** | Merge with a merge commit (`gh pr merge N --merge` / `merge_pull_request` method `merge`); stacks bottom-up; then re-check G4 for the rest of the batch and watch `main`'s Quick. |
| Only G3 pending, content R0–R2 sound | **WAIT CI** | List runs to approve (fork) or pending jobs; enable auto-merge only if the user asked. |
| External PR, conflicts | **REQUEST** | Comment T1, mark draft. |
| External PR, CI red because of the PR | **REQUEST** | Comment T2, mark draft. |
| PR red or conflicting because of `main` | **REBASE** | Fix `main` first (G9); comment T3 on external PRs; rebase the user's own branches and rerun. |
| External PR, sound content (R1/R2), but stalled on process (conflicts the author can't resolve, merge-from-main history, missing LLVM 11 refs, small defects) or reached by the user's fix-issue batch | **ADOPT** | Cherry-pick its commit(s) unchanged (authorship preserved) onto a fresh branch from `main` in the user's fork; fix defects in *separate* follow-up commits; open a draft PR crediting `@author` and "builds on #N"; run the fix-issue verification; comment T4 on the original. |
| External PR whose fix is in the wrong layer, incomplete, or adds plumbing, but has a useful test | **REWRITE** | Same as ADOPT, but write the fix anew (smaller or more complete) in the user's commit, keep the contributor's test commit, credit them (#15962 → #15990, #15924 → #16002); comment T4. |
| Several PRs for one issue | **CONSOLIDATE** | Keep the most complete test and correct-layer fix (tie-break: opened first, then simpler); adopt it; credit the others; comment T5 on each and close after the winner merges (#15940/#15941 → #15987). |
| Content finding the author can fix (missing test, misplaced test, unrelated edit, missing backend label, description out of date) | **REQUEST** | One review comment per finding, each concrete and quoting the rule (style: "Please add this into continue compilation." + link to AGENTS.md test placement). |
| R2 missing mitigations | **REQUEST** | Ask for exactly the missing items (split churn commit, verifier rule, per-backend test, fresh rebase), not general rework. |
| Oversized / multi-layer | **SPLIT** | Propose the stack order (mechanical schema/interface first, independent prerequisites separately, behavior last); for the user's own PR, do the split. |
| R3 design/policy, or a blocker the user must weigh | **ESCALATE** | No action on GitHub; give the user the analysis and the decision needed. |
| Linked issue closed and fixed on `main`, or PR superseded/landed via another PR | **CLOSE** | Verify the fix is on `main` (rerun the PR's reproducer if cheap); comment T6 naming the PR that landed it, then close. Never close silently. |
| Merged, then a narrow regression appears | **FIX FORWARD** | File the issue, open the smallest scoped fix with a test for the affected backend (#15949 → #15951); revert only if a fix is not hours away (#15508 → #15510/#15511). |

Every leftover or adjacent bug found while judging goes into an issue
(search open and closed issues first) and the PR's "Not covered" list.

### Comment templates

Use the maintainer's wording; fill in numbers; end with the attribution
footer the session requires.

- **T1** "This pull request has merge conflicts with `main`. Please resolve
  the conflicts (marking this as a draft until then)."
- **T2** "CI is failing on this PR and needs to be fixed (marking as draft
  until then)." Add the failing job and the first error line.
- **T3** "Main was broken, I fixed it in #M, so you need to rebase."
- **T4** "Thanks! This is continued in #M, which builds on your commit
  (authorship preserved) and adds <what changed>. I'll close this one once
  #M is merged."
- **T5** "#M fixes the same issue and is being merged; it keeps <which>
  test. Thanks for working on this — closing as superseded."
- **T6** "Landed via #M (the fix is on `main`), so closing this one." or
  "The linked issue #I is fixed on `main` by #M, closing this PR."

## Batches

1. Resolve the full PR list (all pages); record it once.
2. Check `main` health (G9) first.
3. Group: stacks (bottom-up), duplicate sets (same issue or same diff),
   independent PRs.
4. Judge content for all PRs (independent PRs in parallel subagents, read
   only; give each the PR number, this skill, and pr-review).
5. Act in this order: unbreak `main` → R0 → R1 → R2 → stacks bottom-up.
   After each merge, re-evaluate G4 for remaining PRs that touch references,
   `.asr` inputs, `ASR.asdl`, or the same files; merges that land together
   on one base are fine only when G4 holds for each.
6. Adoption and rewrite PRs follow `fix-issue` (worktree, Pixi/LLVM 11,
   suites once, draft from the fork, Quick) — they are new PRs judged by
   this skill again once green.

## Decision card (report)

```text
#N <title> (@author, external|member|own, R<k>) — VERDICT
Content: <layers>, <+/- behavior vs mechanical>, <local|non-local: consumers>,
         placement <ok|issue>, mitigations <present/missing>, design <none|...>
Gates:   G1 ✓ G2 ✓ G3 ✓ (12/12 on abc1234) G4 ✓ G5 ✓ G6 ✓ G7 n/a G8 ✓ G9 ✓
Action:  <merged at ... | commented T1 + drafted | adopted as #M | ...>
Follow-ups: <issues filed or linked>
```

End a batch with a table of every PR and its verdict; never present a
partially processed batch as complete.
