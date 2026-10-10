---
name: fix-issue
description: >
  End-to-end fix of an LFortran bug, from issue to review-ready pull request.
  Takes an issue (most commonly a GitHub issue number or URL at
  lfortran/lfortran, but also a pasted snippet, error report, or third-party
  failure), then orchestrates fresh subagents that run repro-issue, then
  create-mre and fix-mre repeatedly (one MRE per underlying bug) until the
  original issue is fully fixed, review the branch locally with pr-review
  until it is clean, open a draft PR from the user's fork with `gh`, and keep
  fixing CI failures and review blockers until Quick checks is green and the
  PR is clean, then mark it
  ready for review. Third-party applications run in Exhaustive (main or an
  explicit request) to generate integration-test regressions, not as a PR gate. Extended PR compiler checks
  are opt-in only when the user requests them. Unrelated pre-existing bugs
  are filed as separate issues and linked from the PR.
  Triggers: fix issue, fix github issue, issue to PR, resolve issue, send PR
  for issue, end-to-end fix.
compatibility: >
  Requires git, an authenticated gh CLI, a configured LFortran build, a
  reference Fortran compiler, and an agent runtime that can spawn subagents.
---

# Fix Issue — From Issue to Review-Ready PR

Drive one LFortran issue (which may involve several compiler bugs) to a single
pull request that is green in CI and clean under `pr-review`:

```
setup ─► repro-issue ─► ┌─► create-mre ─► fix-mre ─► issue check ─┐
                        │                                          │
                        └──────── original issue still fails ◄─────┤
                                                                   │ fixed
                  ┌─► local pr-review ─ clean ─► open draft PR ◄───┘
                  │        │                          │
                  └── fix (commit, no push)           ▼
   ┌─► CI watch (returns on first failure)  +  human comments
   │        │
   │        ├─ Quick green + clean ─► mark PR ready ─► final report
   │        ▼
   └── fix subagent (commit; push per the push rules)

unrelated pre-existing bugs (any phase) ─► followups.md ─► file issues, link in PR
```

Every push reruns the full Quick checks and cancels the run in progress, so
review happens locally before publishing and pushes are batched.

## You are the orchestrator: delegate everything

**Do all the real work in subagents.** Your job as the top-level agent is to
sequence phases, pass short handoffs between them, and decide what happens
next. Keep your own context small so it stays accurate across a loop that can
last hours:

- Spawn a **fresh** subagent for every phase and for every review/fix round.
  In Claude Code use the `Agent` tool; in other runtimes, use their equivalent.
  Never reuse a reviewer's context for fixing, or a fixer's context for
  reviewing.
- Do not read source files, diffs, test logs, or CI logs yourself. Subagents
  read them and return a short report.
- Every subagent prompt must be **self-contained**: a fresh subagent does not
  see this conversation. Include the issue identifier, branch, PR number, state
  directory, which skill to load, what to do, what it is authorized to do, and
  the exact report format to return. Tell it to load skills by name (Claude
  Code: the `Skill` tool) or to read `.agents/skills/<name>/SKILL.md`.
- Ask every subagent to end with a **report of at most ~20 lines** and to put
  anything longer (logs, review text, PR body) in a file under the state
  directory, returning only the path.
- Only one subagent may modify the checkout at a time. Read-only subagents
  (review, follow-up filing) may run concurrently with a CI watch, never with
  a fixer.
- Every report ends with an **Unrelated bugs** list (or `none`): LFortran bugs
  the subagent ran into that are not needed to fix the original issue, such
  as a failure that also occurs on `main` or a separate bug seen while
  reducing or reviewing. One line each, plus a pointer to where the details
  (code, command, output) are saved. Never silently drop them, and never
  bundle their fixes into this PR. Append each new one to `followups.md`.

### State directory

Keep handoffs on disk, in `.fix-issue/<id>/` at the repository root (it is
gitignored). `<id>` is the GitHub issue number, or a short slug for other
input. Maintain `.fix-issue/<id>/state.md` yourself. Keep it to a few lines:
the input, branch, fork remote, PR URL, current round, last pushed head SHA,
and status. Update it after every phase. If your context is compacted or the
session resumes, reread `state.md` to find where you are.

Subagents write their artifacts there: `repro.md`; for each MRE iteration `j`,
`mre_<j>.md`, `fix_<j>.md`, `check_<j>.md`; for each local review round `r`,
`local_review_<r>.md`, `local_round_<r>.md`; then `pr_body.md`, and for each
review round `k`, `review_<k>.md`, `round_<k>.md`, `ci_<k>.log`. You maintain
`followups.md`: one line per unrelated bug, with its status (`unfiled`,
`filed #M`, `duplicate of #M`, or `regression, sent to fix loop`) and the
path to its details. The reproducers themselves (`run_re.sh`, `re_*.f90`,
`run.sh`, `mre_*.f90`) live in the repository root, following the conventions
of the `repro-issue` and `create-mre` skills. Before iteration `j+1` overwrites `run.sh`, the current
MRE files are archived to `.fix-issue/<id>/mre_<j>/`.

## Authorization

Invoking this skill counts as the user's consent to create a branch, commit,
push to **the user's fork**, open a draft PR against `lfortran/lfortran`, push
follow-up commits to that PR, open new issues at `lfortran/lfortran` for unrelated pre-existing bugs
found along the way, and mark the PR ready for review. This overrides the
"do not commit" default in `fix-mre`. Pass this authorization explicitly to
the subagents that need it.

Never, under any circumstances:

- push to the upstream `lfortran/lfortran` repository;
- use an unconditional force-push; when the policy below calls for rebasing,
  update the fork branch only with `git push --force-with-lease`;
- merge `main` into the PR branch;
- comment on, close, or relabel the original issue, add a label to
  the PR, comment on
  other issues or PRs (including existing issues found in
  a duplicate search), or post review comments on other people's PRs;
- run `./run_tests.py -u` without reviewing every reference change.

### Keeping the PR branch current

Keep PR history linear: the branch must never contain a merge of `main`. Do
not update the branch only because `main` moved; every push reruns CI, and
Quick on `main` catches integration breakage after merging. Update it only when
the PR has base conflicts, needs a change that landed on `main`, or CI
failures look caused by a stale base.

To update, run `git rebase <upstream-remote>/main`, resolve any conflicts,
rebuild and retest, then update the fork with `git push --force-with-lease`.
This applies in every PR state, including after review. Rebasing only moves
the existing commits; address review feedback with new commits and do not
squash or reword commits reviewers have already seen.

## Inputs

1. **The issue.** Accept any of:
   - a GitHub issue number (`12345`), `#12345`, or URL. Default repo:
     `lfortran/lfortran`. This is the common case.
   - a pasted Fortran snippet with an error or wrong output;
   - a failure in a third-party package (project, file, command, error);
   - a JupyterLite / notebook failure.

Ask the user only if the input is missing or does not describe a failure.

## Procedure

### Phase 0: Preflight (orchestrator, cheap commands only)

Run a few quick checks yourself. Each prints only a few lines:

1. `gh auth status`. If gh is not logged in, stop and ask the user to run
   `! gh auth login`.
2. `gh api user --jq .login` gives `<login>`.
3. `git status --porcelain --untracked-files=no` must be empty. Untracked
   scratch files are fine. If tracked files are modified, stop and ask the
   user. Never stash or discard their work.
4. Identify remotes from `git remote -v`:
   - **upstream remote**: the one whose URL points at `lfortran/lfortran`
     (often `origin` or `upstream`);
   - **fork remote**: the one whose URL points at `<login>/lfortran`. If none
     exists, create the fork without touching existing remotes:
     `gh repo fork lfortran/lfortran --clone=false`, then
     `git remote add <login> git@github.com:<login>/lfortran.git` (use the
     https URL if `gh auth status` reports the https protocol).
5. For a GitHub issue, look for existing work, and stop and ask the user if
   there is any:
   `gh issue view <N> --repo lfortran/lfortran --json state,title` (closed?)
   and
   `gh pr list --repo lfortran/lfortran --state open --search "<N> in:body,title" --json number,title,author`.

Create `.fix-issue/<id>/state.md`.

### Phase 1: Setup subagent

It should:

1. `git fetch <upstream-remote> main`, then create and check out
   `fix/<id>-<short-slug>` from `<upstream-remote>/main`. Derive the slug from
   the issue title or the failure. If the branch name already exists, pick a
   new suffix; do not reuse or reset an existing branch.
2. Rebuild: `cmake --build build -j &> .fix-issue/<id>/build.log`. If `build/`
   is not configured, use the dev config from `AGENTS.md`. If modfiles are
   stale, do a clean rebuild.
3. Put `build/src/bin` first on `PATH` and confirm `which lfortran`. Also
   confirm that a reference compiler (`gfortran` preferred) exists.

Report: branch name, base SHA, build OK or failed (with the log path).

### Phase 2: Reproduce subagent

- **GitHub issue:** run the `repro-issue` skill on the issue. Output:
  `run_re.sh` plus `re_*.f90` in the repository root.
- **Pasted snippet:** apply the same RE conventions as `repro-issue` (faithful
  code, `run_re.sh` that shows the reference compiler succeeds and `lfortran`
  fails), just without fetching from GitHub.
- **Third-party or notebook failure:** skip this phase. `create-mre` handles
  that input directly.

Report, also written to `repro.md`: reproduced yes/no, error type, the exact
LFortran error, the reference compiler result, and whether the issue contains
several distinct bugs.

**Stop and report to the user** if the bug does not reproduce on current
`main` (it may already be fixed), or if the reference compiler also rejects
the code (the code may be invalid). Do not open a PR in either case.

### Phase 3: MRE loop — reduce, fix, check until the issue is fixed

Many issues hide several compiler bugs. For this skill, "one PR" means one
PR per issue: each bug still gets its own MRE, commit, and integration test,
so reviewers can follow the commits one by one. Fixing the first one often just
exposes the next failure in the same code. Iterate `j = 1, 2, ...` with **no
fixed cap**. Every iteration adds **one commit with its own integration test**
to the same branch. All of them ship together in a single PR.

Stop the loop and report to the user if there is no progress: an iteration
produced no verified fix, or the issue check shows the same failure as the
previous iteration. Never open a PR for an unverified fix.

#### 3a. Reduce subagent

If `j > 1`, first move the previous `run.sh` and `mre_*.f90` to
`.fix-issue/<id>/mre_<j-1>/`. Then run the `create-mre` skill on the
**current** failure of the original reproducer:

- for `j = 1`, the RE from Phase 2, or the third-party failure;
- for `j > 1`, the failure recorded in `check_<j-1>.md`, which is produced by
  the in-tree `lfortran` that already includes the earlier fixes.

Output: `run.sh` plus `mre_*.f90`.

Report, also written to `mre_<j>.md`: the MRE files, a one-line bug
description, the error, and confirmation that `run.sh` fails with the current
in-tree LFortran and succeeds with the reference compiler.

If the current failure contains several distinct bugs, reduce only **one**
per iteration. The loop picks up the rest.

#### 3b. Fix subagent

Run the `fix-mre` skill on the MRE, with these additions to its instructions:

- Tell it which earlier iterations already committed fixes on this branch
  (from `fix_<1..j-1>.md`). It must not undo them, and must not duplicate
  their integration tests.
- It must verify that the new integration test **fails without this
  iteration's fix and passes with it**. For `j > 1`, "without" means the
  branch before this iteration's change, not `main`.
- It must run the full integration suite and the reference suite, with output
  redirected to log files. For every failure, it must determine whether this
  change caused it or whether it also fails on `main`.
- **Commit is authorized.** Make one focused commit for this bug, with an
  imperative `fix: ...` message. Do not stage scratch reproducers, logs, or
  `.fix-issue/`.
- Do not push.

Report, also written to `fix_<j>.md`: the commit SHA, the changed files, the
test name, fail-before/pass-after verified, integration and reference
results, any failures that already exist on `main`, and one paragraph of
rationale (the bug, the compiler phase at fault, and why the fix belongs
there).

#### 3c. Issue check subagent (fresh)

It verifies the **original** issue, not the MRE, against the rebuilt in-tree
`lfortran`:

- **GitHub issue or pasted snippet:** reread the issue text, including
  comments, and run `run_re.sh`. The program must now compile and run, and its
  output must match the reference compiler's output and any expected output
  stated in the issue. Also check every other program, flag combination, or
  scenario the issue mentions that `run_re.sh` does not cover. If one is
  missing, add it to `run_re.sh`.
- **Third-party failure:** re-run the original failing build or test
  command, and continue past the point that used to fail.

Report, also written to `check_<j>.md`: fully fixed yes/no. If no, report the
exact new failure (the command, the error or the output diff, and the file
involved) and whether it is the same failure as in `check_<j-1>.md`.

If it is fully fixed, go to Phase 3.5. Otherwise start iteration `j+1`.

### Phase 3.5: Local review loop (before publishing)

Review the branch locally before it is pushed, so CI normally runs once, on a
branch that is already clean under `pr-review`. Each finding fixed after
publishing would cost another push and another full Quick run.

Repeat rounds `r = 1, 2, ...` up to **5 rounds**:

1. **Review subagent (fresh, read-only):** run the `pr-review` skill on the
   local branch: all commits since the merge base with
   `<upstream-remote>/main`, not just the latest commit. It must not edit,
   commit, or push. It may build and run tests in the checkout to confirm a
   finding, as long as it leaves the tree unchanged. It writes the full review
   to `local_review_<r>.md` and reports the number of findings in each class
   (blocker / rework / follow-up), with a one-line title and file:line for
   each blocker and rework item. Tell it to review the change on its merits
   and report only real findings it has checked. It must not invent findings
   to fill a quota, and must not soften findings because an automated agent
   wrote the change.
2. If there is **no blocker and no rework** finding, go to Phase 4.
3. Otherwise spawn a **fresh fix subagent.** For each blocker and rework
   finding, it first confirms the finding is real by reproducing it. Then it
   fixes it, or records in `local_round_<r>.md` exactly why it is not valid.
   It leaves **follow-up** items unfixed and lists them for the final report.
   It rebuilds and reruns the MRE, the new integration tests, and the full
   integration and reference suites whenever compiler source changed. It
   commits focused commits. **Do not push.**

If the same finding survives two rounds, or the round cap is reached, stop
and report to the user with the outstanding items. Never publish a branch
with an open blocker or rework finding.

### Phase 4: Publish subagent

It should:

1. Write `.fix-issue/<id>/pr_body.md` from `repro.md` and all the
   `fix_<j>.md` files. Use plain Markdown with the headings Summary, Scope,
   Verification, Rationale.
   - Summary: first line `Fixes #<N>` (omit it for non-GitHub input), then
     the issue and a bullet per bug fixed, in commit order.
   - Scope: what is and is not covered, including unrelated bugs found along
     the way (linked once filed, see below) and non-bug follow-ups such as
     refactoring ideas or missing tests for existing behaviour, including
     follow-up findings from `local_review_<r>.md`.
   - Verification: the integration tests added, each failing before its fix
     and passing after; the original reproducer now passing; the suite
     results.
   - Rationale: for each bug, why the fix belongs in the chosen compiler
     phase.
2. `git push -u <fork-remote> <branch>`
3. `gh pr create --repo lfortran/lfortran --base main --head <login>:<branch> --draft --title "<title>" --body-file .fix-issue/<id>/pr_body.md`.
   The title is the commit subject when there is a single commit. Otherwise
   it is a `fix: ...` summary of the issue.

Report: the PR number, URL, and head SHA. Record them in `state.md`.

#### Follow-up issues subagent

Spawn it once the PR exists if `followups.md` has `unfiled` entries, and again
whenever a later round adds more (at the latest before Phase 6). It does not
edit tracked files; it puts scratch files under
`.fix-issue/<id>/followup_<m>/`, never in the repository root. For each
distinct bug it:

1. Confirms the bug also fails on `main`, reusing the discovering subagent's
   evidence or building `<upstream-remote>/main` in a separate worktree
   (never switch the PR checkout). If it does not fail on `main`, it is a
   regression of this PR: do not file it; mark it for the fix loop instead.
2. Searches for duplicates:
   `gh issue list --repo lfortran/lfortran --state all --search "<keywords>"`.
   If one exists, record `duplicate of #M` and do not comment on it.
3. Otherwise files one issue with `gh issue create --repo lfortran/lfortran
   --title "<bug>" --body-file <file>`. The body contains: a minimal
   self-contained reproducer, the exact LFortran command and output, the
   reference compiler's result showing the code is valid, confirmation that
   it fails on `main`, and the line `Found while working on #<PR> (#<N>)`.
4. Updates `followups.md`, adds every filed or duplicate issue to the Scope
   section of `pr_body.md`, and runs
   `gh pr edit <PR> --repo lfortran/lfortran --body-file .fix-issue/<id>/pr_body.md`.

Only bugs are filed. Non-bug follow-ups go in the PR body and the final report.

Report: issues filed, duplicates linked, regressions sent back.

### Phase 5: Review and CI loop

Repeat rounds `k = 1, 2, ...` up to **5 rounds**. Each round works on the
current head SHA.

**6a. Start the CI watch, and a fresh review when needed, concurrently.**

- **CI watch (orchestrator):** run in the background so it does not block,
  and send the output to a file, not your context. `--fail-fast` makes it
  return on the first failing check, so fixing starts immediately while the
  remaining checks keep running:
  ```bash
  gh pr checks <PR> --repo lfortran/lfortran --watch --fail-fast --interval 120 \
      > .fix-issue/<id>/ci_<k>.log 2>&1; echo "exit=$?"
  ```
  Checks can take a minute to appear after a push. If `gh`
  reports no checks yet, wait and retry. CI can take over an hour. Do not
  poll in short loops; wait for the background command to finish. Afterwards,
  get only a summary:
  ```bash
  gh pr checks <PR> --repo lfortran/lfortran --json name,bucket \
      --jq 'group_by(.bucket)[] | "\(.[0].bucket): \(length) \([.[].name] | join(", "))"'
  ```
  If checks sit in `action_required` or never start, tell the user. If the
  watch returned on a failure, go to 6d right away; do not cancel the run.
  After the fix is committed, check whether other jobs failed meanwhile, and
  restart the watch for the remaining checks while no push is pending.
- **Review subagent (fresh, read-only):** only when the current head contains
  commits that no clean review has covered. Phase 3.5 already reviewed the
  first published head, so round 1 normally needs only the CI watch. Run the
  `pr-review` skill on
  PR `<PR>` in `lfortran/lfortran`. It must not edit, commit, or push. It may
  build and run tests in the checkout to confirm a finding, as long as it
  leaves the tree unchanged. It writes the full review to `review_<k>.md` and
  reports the number of findings in each class (blocker / rework /
  follow-up), with a one-line title and file:line for each blocker and rework
  item. Tell it to review the PR on its merits and report only real findings
  it has checked. It must not invent findings to fill a quota, and must not
  soften findings because an automated agent wrote the PR.

**6b. Collect human feedback (orchestrator, summary only):**

```bash
gh pr view <PR> --repo lfortran/lfortran --json reviewDecision,mergeable,reviews,comments \
    --jq '{reviewDecision, mergeable, reviews: [.reviews[] | {author: .author.login, state}], comments: (.comments | length)}'
gh api repos/lfortran/lfortran/pulls/<PR>/comments --jq 'length'
```

Treat the comment counts as new only if they changed since the last round.
The fix subagent reads the actual comment text.

**6c. Decide.** The PR is **clean** when all of these hold for the current
head SHA:

- every applicable CI check passed, or each failing check was shown by a subagent to
  also fail on `main` (it is pre-existing, so report it but do not fix it
  here);
- the latest fresh review of the current head (Phase 3.5 or 6a) has **no
  blocker and no rework** findings;
- no human review comment or requested change is unaddressed;
- `mergeable` is not `CONFLICTING`.

The PR is **done** when it is clean and `Quick checks` ran for the current
PR revision, with every applicable job passed or shown to also fail on `main`.
Quick includes the shared compiler compatibility jobs; those are not optional.
Explicitly requested Exhaustive checks supplement Quick rather than replacing
it; require both on the same current revision.
Check with
`gh pr checks <PR> --repo lfortran/lfortran --json workflow,name,bucket`.
Do not treat missing or all-skipped Quick checks as success.
Exhaustive does not run on PRs; its absence does not block a PR.

Third-party applications are bug generators for the integration suite and
release compatibility checks on the latest main, not an ordinary PR test suite.
When the reported bug comes from an application, reduce it, add the registered
integration regression, fix the compiler and verify the original application
failure locally. Do not add that application to Quick. Caffeine-backed
coarray and GPU integration checks remain required capability tests.

Do not dispatch extended CI automatically, including for serialization,
finalization, I/O or GPU changes. Do not cancel older main runs.

Only when the user explicitly requests extended checks, dispatch Exhaustive on
the PR branch in the fork (the same checks as main, including applications):
`gh workflow run Exhaustive-Checks-CI.yml --repo <login>/lfortran --ref <branch>`.
If the fork's Actions are disabled, tell the user. Exhaustive never invokes
Quick; the PR's Quick checks must also be green for the same revision.
Record the run ID, URL and head SHA in `state.md`, wait with
`gh run watch <run-id> --repo <login>/lfortran`, and report the run URL; it
does not appear in upstream `gh pr checks`. A push invalidates the old result,
so dispatch again for the new head when needed. Failures go to the fix
subagent like any other CI failure, subject to the pre-existing-failure rule
above.

Release qualification is separate: the release commit must have green full
main CI, including applications. Quick or extended PR success is not enough.

If done, go to Phase 6.

**6d. Otherwise spawn a fresh fix subagent** with the list of what is
outstanding: failing check names, the path `review_<k>.md`, and which human
comments are new. It should:

- For CI failures: read the logs
  (`gh run view <run-id> --repo lfortran/lfortran --log-failed`, saved to a
  file), reproduce locally where possible, and fix the root cause. If a
  failure also happens on `main` or is plainly infrastructure flakiness, do
  not fix it in this PR. Re-run it once with `gh run rerun <run-id> --failed`
  and report it; a pre-existing LFortran bug goes on its Unrelated bugs list.
- For each review **blocker** and **rework** finding: first confirm the
  finding is real by reproducing it. Then fix it, or record in
  `round_<k>.md` exactly why it is not valid. Leave **follow-up** items
  unfixed; list them for the final report.
- For human review comments: address each one in code, or draft a reply in
  `round_<k>.md`. Do not post replies automatically; the user decides.
- For base conflicts, or when the PR needs a change from `main`: follow
  **Keeping the PR branch current**. Rebase onto `<upstream-remote>/main`,
  resolve conflicts, rebuild, and retest; push it with `--force-with-lease`
  under the push rules below.
- Rebuild, rerun the MRE, the new integration test, and the affected suites
  (the full integration and reference suites whenever compiler source
  changed). Update `pr_body.md` and the PR description
  (`gh pr edit <PR> --body-file ...`) if scope or rationale changed.
- Commit focused follow-up commits. Fixing always starts immediately; these
  **push rules** decide only when to push to the fork remote (a normal push):
  - **A CI job failed:** push as soon as the fix passes locally. Before
    pushing, check `gh pr checks` for other jobs that failed meanwhile and
    include their fixes in the same push. Do not wait for the remaining
    jobs; a failed job stops early, so only a new push can reveal later
    problems in it.
  - **CI is still running with no failures, and the change is not a CI fix**
    (a review finding, a human comment, a cleanup): commit locally but do not
    push yet. Pushing would cancel a run that may turn out all green. Push
    when CI finishes, or together with a CI fix if a job fails meanwhile.
  - **CI has finished:** push everything collected in one push.

Report: what was fixed, what was rejected and why, the local head SHA,
whether it was pushed (and if not, which push rule held it back), and test
results. Record the last pushed head SHA in `state.md`. Then start round
`k+1`; a round with an unpushed commit keeps watching the CI of the last
pushed head and pushes when the push rules allow.

If the same finding or CI failure survives two fix rounds, or the round cap is
reached, stop the loop and report to the user with the outstanding items.
Do not keep iterating blindly.

### Phase 6: Finish

1. Mark the PR ready: `gh pr ready <PR> --repo lfortran/lfortran`.
2. Leave the branch checked out and do not delete the scratch reproducers.
   The user may want them.
3. Print the final report:

```
PR ready for review: <URL>

Issue:   #<N> — <title>   (original reproducer now passes)
Branch:  <fork-remote>/<branch>   head <sha>
Bugs fixed (<count> MRE iterations, one commit each):
  1. <one line> — integration_tests/<test>.f90 (fails before, passes after)
  2. ...

CI:      green  (pre-existing failures on main: <none | names>)
Extended compiler CI: <not requested | green, run URL and tested SHA>
Review:  <local rounds> local + <PR rounds> PR round(s); pushes: <n>; blockers/rework fixed: <n>; rejected with reason: <n>
Follow-up issues filed: <#M, #M (duplicate of existing), ... or none>
Other follow-ups (not bugs, not filed): <list or none>
Human comments needing your reply: <list or none>
```

## Tips

- **Context discipline is the point.** If you catch yourself reading a diff
  or a log, hand that job to a subagent instead.
- **Resume:** if the session is interrupted, reread `state.md`, check
  `git log`, `gh pr view`, and the `.fix-issue/<id>/` artifacts, then continue
  from the first phase that is not finished.
- **Base freshness:** if `main` moved a lot during a long loop and CI failures
  look unrelated to the PR, have the fix subagent rebase onto `upstream/main`
  before debugging, as described in **Keeping the PR branch current**.
- **Build errors in CI only:** other platforms (Windows/MSVC, WASM, other
  backends) often fail where macOS/Linux pass. The fix subagent should read
  the CI step's exact command and reproduce that backend locally where
  feasible (see the CI-parity notes in `AGENTS.md`).
