---
name: fix-issue
description: >
  Fix LFortran issues end-to-end, singly or as a batch selected by a list,
  GitHub search URL, or filters. Use for requests to fix all matching issues,
  resolve an issue, or send issue-fix PRs, including pasted failures. Expand
  the complete selection, use fresh per-issue subagents and isolated Git
  worktrees, and build with Pixi/LLVM 11. Run repro-issue, then create-mre and
  fix-mre until every original reproducer passes. Publish independent fixes
  as separate fork PRs; consolidate shared or dependent fixes into one PR
  with focused commits. Iterate on pr-review, human feedback, and CI, then
  add Tests::Run-Exhaustive and finish exhaustive CI before marking ready.
  Deduplicate and file additional pre-existing compiler bugs, including
  discoveries from blocked jobs. Do not execute fixes for triage, plan-only
  requests, or discussions of this workflow.
compatibility: >
  Requires git, authenticated gh, Pixi, the platform prerequisites in AGENTS.md,
  and a runtime that can spawn subagents. Provision the compiler build and
  reference Fortran compiler using the repository's Pixi tasks.
---

# Fix Issue — From Issue to Review-Ready PR

Drive each independent issue or connected group to a review-ready fork PR.
For **a list, search URL, or filtered batch**, first read
[references/batch.md](references/batch.md): resolve all matches and coordinate
fresh issue-worker subagents in separate worktrees. Each worker follows the
per-job procedure below. Pass it its assigned issue IDs, not the original
batch query, so it does not recursively redispatch the batch.

Per job: setup → reproduce → (reduce → fix → original-issue check) until fixed
→ draft PR → review/CI/fix loop → exhaustive CI → ready. Track additional bugs
throughout, including jobs that stop without a PR.

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
  see this conversation. Include the issue IDs, absolute worktree and state
  paths, branch/base SHA, PR number, build directory, compiler path, exact Pixi
  invocation, job budget, skill, authorized actions, and report format.
  Explicitly pass the no-co-author commit policy below to every committer.
  Tell it to load skills by name or read `.agents/skills/<name>/SKILL.md`.
- Ask every subagent to end with a **report of at most ~20 lines** and to put
  anything longer (logs, review text, PR body) in a file under the state
  directory, returning only the path.
- Only one subagent may modify, build, or test in a given worktree at a time.
  Independent worktrees may run concurrently within the shared resource budget.
  Read-only subagents (review, follow-up filing) may run with a CI watch, never with
  a fixer.
- Every report ends with an **Unrelated bugs** list (or `none`): LFortran bugs
  the subagent ran into that are not needed to fix the original issue, such
  as a failure that also occurs on `main` or a separate bug seen while
  reducing or reviewing. One line each, plus a pointer to where the details
  (code, command, output) are saved. Never silently drop them, and never
  bundle unrelated fixes into this PR. Append each new one to `followups.md`
  and send it to the batch coordinator, which owns cross-job deduplication.

### State directory

Keep each job's handoffs in `.fix-issue/<id>/` inside its **assigned worktree**
(gitignored). `<id>` is an issue number or a job slug. Maintain `state.md`:
input/all included issues, absolute worktree, branch/base SHA, fork remote,
build directory, compiler/Pixi invocation, resource budget, PR URL, round,
last pushed SHA, and status. Update it after every phase; reread it on resume.

Subagents write their artifacts there: `repro.md`; for each MRE iteration `j`,
`mre_<j>.md`, `fix_<j>.md`, `check_<j>.md`; then `pr_body.md`, and for each
review round `k`, `review_<k>.md`, `round_<k>.md`, `ci_<k>.log`. You maintain
`followups.md`: one line per unrelated bug, with its status (`unfiled`,
`filed #M`, `duplicate of #M`, or `regression, sent to fix loop`) and the
path to its details. The reproducers themselves (`run_re.sh`, `re_*.f90`,
`run.sh`, `mre_*.f90`) live in the repository root, following the conventions
of the `repro-issue` and `create-mre` skills. Before iteration `j+1` overwrites `run.sh`, the current
MRE files are archived to `.fix-issue/<id>/mre_<j>/`.

## Authorization

For an actual fixing request, invoking this skill authorizes isolated worktrees,
branches, Pixi dependency installation/builds, commits and follow-up pushes to
**the user's fork**, draft PRs against `lfortran/lfortran`, the
`Tests::Run-Exhaustive` PR label, filing new unrelated pre-existing bugs, and
marking verified PRs ready. This overrides the "do not commit" default in
`fix-mre`. Pass the authorized subset explicitly to each subagent.

Never, under any circumstances:

- push to the upstream `lfortran/lfortran` repository;
- use an unconditional force-push; when the policy below calls for rebasing,
  update the fork branch only with `git push --force-with-lease`;
- comment on, close, or relabel the original issue, add any other label to
  the PR, comment on other issues or PRs (including existing issues found in
  a duplicate search), or post review comments on other people's PRs;
- bulk-update references blindly; use targeted LLVM 11 updates and review every
  change, including when invoking `pixi run tests -u`.

### Commit authorship

Do not add `Co-authored-by:` or `Co-author:` lines to **any** commit, including
fixes, CI follow-ups, and merges. Do not use AI author/committer identities or
alternative AI-credit stamps such as `Generated-By:` or `Assisted-By:`.
`check_ai_commit_authorship.py` explains why: AI is a tool, while the submitting
human must read, understand, and guarantee the changes. Quick Checks CI inspects
the whole PR history. Preserve the configured human identity, do not invent one,
and do not equate automated review with human sign-off. `AGENTS.md` owns this
policy; `fix-mre` applies it when preparing commits. Pass it to all other fixers.

### Keeping the PR branch current

Keep commit history clean by rebasing the PR branch onto `upstream/main` while
the PR is a draft. Run `git rebase <upstream-remote>/main`, resolve any
conflicts, rebuild and retest, then update the fork with
`git push --force-with-lease`.

Switch permanently to merging `<upstream-remote>/main` when any of these
review-sensitive events occurs:

- the PR is marked ready for review;
- a human other than the PR author submits a formal review;
- a human other than the PR author leaves an inline code-review comment;
- the user explicitly says not to rebase the PR.

Top-level PR conversation comments, reactions, bot activity, and comments by
the PR author do not change the mode. Before updating the branch, use `gh` to
inspect the PR's draft state, author, submitted reviews, and inline review
comments. Once the mode changes to `merge`, preserve the commits reviewers may
have seen: merge `<upstream-remote>/main` for all later updates and push
normally. Never return to rebasing that PR even if a review or inline comment
is later dismissed, hidden, or deleted. Record the chosen mode (`rebase` or
`merge`) in `state.md`.

## Inputs

1. **The issue.** Accept any of:
   - a GitHub issue number (`12345`), `#12345`, or URL. Default repo:
     `lfortran/lfortran`. This is the common case.
   - an explicit list of issues, an issues/search URL with filters, or a
     request to fix all matching issues; use `references/batch.md`.
   - a pasted Fortran snippet with an error or wrong output;
   - a failure in a third-party package (project, file, command, error);
   - a JupyterLite / notebook failure.

Ask the user only if the input is missing or does not describe a failure.

## Procedure

### Phase 0: Preflight (orchestrator, cheap commands only)

Run a few quick checks yourself. Each prints only a few lines:
Reuse shared preflight results supplied by a batch coordinator; do not repeat
fetches or mutate shared Git state concurrently.

1. `gh auth status`. If gh is not logged in, stop and ask the user to run
   `! gh auth login`.
2. `gh api user --jq .login` gives `<login>`.
3. Inspect `git status --porcelain` and `git worktree list`. Preserve the
   caller's branch and dirty files: create a separate worktree instead of
   stashing, resetting, or asking the user to clean an unrelated checkout.
4. Identify remotes from `git remote -v`:
   - **upstream remote**: the one whose URL points at `lfortran/lfortran`
     (often `origin` or `upstream`);
   - **fork remote**: the one whose URL points at `<login>/lfortran`. If none
     exists, create the fork without touching existing remotes:
     `gh repo fork lfortran/lfortran --clone=false`, then
     `git remote add <login> git@github.com:<login>/lfortran.git` (use the
     https URL if `gh auth status` reports the https protocol).
5. For a GitHub issue, look for existing work and inspect candidates before
   deciding they overlap:
   `gh issue view <N> --repo lfortran/lfortran --json state,title` (closed?)
   and
   `gh pr list --repo lfortran/lfortran --state open --search "<N> in:body,title" --json number,title,author`.

Do not take over someone else's PR or reopen a closed issue automatically.
For overlapping work, ask or report that job as blocked; continue other jobs.
Reserve an unused branch/worktree path and create the coordinator's state
under the invoking checkout's ignored `.fix-issue/` directory.

### Phase 1: Setup subagent

It should:

1. Use the coordinator's pinned upstream base. For a standalone job, fetch
   upstream `main` once and record its SHA. Create the reserved branch with
   `git worktree add -b fix/<id>-<slug> <absolute-worktree> <base-sha>`, never
   by checking out a branch in the caller's directory. Serialize shared Git
   operations; if the coordinator already created the worktree, verify and
   use it. Never reset or reuse unrelated existing branches.
2. In that worktree, create `.fix-issue/<id>/` and follow the **Pixi setup in
   `AGENTS.md`**: `pixi run -e llvm11 build`, saving the log. This provisions
   dependencies and builds into `build/llvm11`, not the source root. Set
   `CMAKE_BUILD_PARALLEL_LEVEL` to the worker's allocated budget.
3. Record absolute paths and the exact environment invocation. In that
   environment, verify `command -v lfortran`, its reported LLVM version,
   `llvm-config --version`, and `gfortran --version`. Every later subagent
   must select this same build explicitly, not an inherited/system compiler.

Report: worktree, branch/base SHA, build/compiler paths, Pixi command, job
budget, tool versions, and build OK or failed (with the log path).

### Phase 2: Reproduce subagent

- **GitHub issue:** run the `repro-issue` skill on the issue. Output:
  `run_re.sh` plus `re_*.f90` in the repository root.
- **Pasted snippet:** apply the same RE conventions as `repro-issue` (faithful
  code, `run_re.sh` that shows the reference compiler succeeds and `lfortran`
  fails), just without fetching from GitHub.
- **Third-party or notebook failure:** skip this phase. `create-mre` handles
  that input directly.

For a connected group, repeat for every included issue and archive each RE
and its script in `.fix-issue/<id>/repro/<issue>/` before the next one overwrites
root-level scratch files. Preserve those originals for the final issue check.

Report, also written to `repro.md`: reproduced yes/no, error type, the exact
LFortran error, the reference compiler result, and whether the issue contains
several distinct bugs.

**Stop this job and report** if the bug does not reproduce on the pinned
`main` (it may already be fixed), or if the reference compiler also rejects
the code (the code may be invalid). Do not open a PR in either case.

### Phase 3: MRE loop — reduce, fix, check until the issue is fixed

Fixing one bug can expose the next failure in the same reported code. Iterate
`j = 1, 2, ...` with **no fixed cap**, one MRE, focused commit, and integration
test per distinct bug. A single issue or connected group shares one PR;
independent issues do not. Report newly discovered cross-job dependencies to
the batch coordinator before implementing duplicate fixes or publishing.

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
- It must run the unit, full integration, and LLVM 11 reference suites with output
  redirected to log files. For every failure, it must determine whether this
  change caused it or whether it also fails on `main`.
- **Commit is authorized.** Make one focused commit for this bug, with an
  imperative `fix: ...` message. Do not stage scratch reproducers, logs, or
  `.fix-issue/`. Follow **Commit authorship**, including no co-author lines.
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

For a group, perform this check for **every included issue**, using its archived
original RE, and report per-issue results. If all are fixed, request the batch
coordinator's grouping/publication decision, then go to Phase 4. Otherwise
start iteration `j+1` on the next failure.

### Phase 4: Publish subagent

It should:

1. Write `.fix-issue/<id>/pr_body.md` from `repro.md` and all the
   `fix_<j>.md` files. Use plain Markdown with the headings Summary, Scope,
   Verification, Rationale.
   - Summary: `Fixes #<N>` for each fully verified included issue (omit for
     non-GitHub input), then a bullet per distinct bug, in commit order.
   - Scope: what is and is not covered, including unrelated bugs found along
     the way (linked once filed, see below) and non-bug follow-ups such as
     refactoring ideas or missing tests for existing behaviour.
   - Verification: the integration tests added, each failing before its fix
     and passing after; the original reproducer now passing; the suite
     results.
   - Rationale: for each bug, why the fix belongs in the chosen compiler
     phase.
2. Inspect all new commits for the authorship policy, then
   `git push -u <fork-remote> <branch>`
3. `gh pr create --repo lfortran/lfortran --base main --head <login>:<branch> --draft --title "<title>" --body-file .fix-issue/<id>/pr_body.md`.
   The title is the commit subject when there is a single commit. Otherwise
   it is a `fix: ...` summary of the issue.

Report: the PR number, URL, and head SHA. Record them in `state.md`.

#### Follow-up issues subagent

Delegate it whenever verified `unfiled` entries exist, **even if no PR exists or
the job is blocked**. In a batch, the coordinator serializes filing across all
jobs to avoid duplicate reports. It does not
edit tracked files; it puts scratch files under
`.fix-issue/<id>/followup_<m>/`, never in the repository root. For each
distinct bug it:

1. Confirms the bug also fails on `main`, reusing the discovering subagent's
   evidence or building `<upstream-remote>/main` in a separate worktree
   (never switch the PR checkout). If it does not fail on `main`, it is a
   regression of this PR: do not file it; mark it for the fix loop instead.
2. Searches for duplicates:
   `gh issue list --repo lfortran/lfortran --state all --search "<keywords>"`.
   Inspect likely matches and the batch's pending/filed entries, not just titles.
   Recheck immediately before filing. If one exists, record `duplicate of #M`
   and do not comment on it.
3. Otherwise files one issue with `gh issue create --repo lfortran/lfortran
   --title "<bug>" --body-file <file>`. The body contains: a minimal
   self-contained reproducer, the exact LFortran command and output, the
   reference compiler's result (or expected diagnostic for invalid input),
   confirmation that it fails on `main`, and the originating issue/PR links
   available so far. Do not invent a PR number when none exists.
4. Updates `followups.md`, adds every filed or duplicate issue to the Scope
   section of `pr_body.md` when there is a PR, and runs
   `gh pr edit <PR> --repo lfortran/lfortran --body-file .fix-issue/<id>/pr_body.md`.

Only bugs are filed. Non-bug follow-ups go in the PR body and the final report.

Report: issues filed, duplicates linked, regressions sent back.

### Phase 5: Review and CI loop

Repeat rounds `k = 1, 2, ...` up to **5 rounds**. Each round works on the
current head SHA.

**5a. Start the CI watch and a fresh review, concurrently.**

- **CI watch (orchestrator):** run in the background so it does not block,
  and send the output to a file, not your context:
  ```bash
  gh pr checks <PR> --repo lfortran/lfortran --watch --interval 120 \
      > .fix-issue/<id>/ci_<k>.log 2>&1; echo "exit=$?"
  ```
  Checks can take a minute to appear after a push or labeling. If `gh`
  reports no checks yet, wait and retry. CI can take over an hour. Do not
  poll in short loops; wait for the background command to finish. Afterwards,
  get only a summary:
  ```bash
  gh pr checks <PR> --repo lfortran/lfortran --json name,bucket \
      --jq 'group_by(.bucket)[] | "\(.[0].bucket): \(length) \([.[].name] | join(", "))"'
  ```
  If checks sit in `action_required` or never start, tell the user.
- **Review subagent (fresh, read-only):** run the `pr-review` skill on
  PR `<PR>` in `lfortran/lfortran`. It must not edit, commit, or push. It may
  build and run tests in the checkout to confirm a finding, as long as it
  leaves the tree unchanged. It writes the full review to `review_<k>.md` and
  reports the number of findings in each class (blocker / rework /
  follow-up), with a one-line title and file:line for each blocker and rework
  item. Tell it to review the PR on its merits and report only real findings
  it has checked. It must not invent findings to fill a quota, and must not
  soften findings because an automated agent wrote the PR.

**5b. Collect human feedback (orchestrator, summary only):**

```bash
gh pr view <PR> --repo lfortran/lfortran --json reviewDecision,mergeable,reviews,comments \
    --jq '{reviewDecision, mergeable, reviews: [.reviews[] | {author: .author.login, state}], comments: (.comments | length)}'
gh api repos/lfortran/lfortran/pulls/<PR>/comments --jq 'length'
```

Treat the comment counts as new only if they changed since the last round.
The fix subagent reads the actual comment text.

**5c. Decide.** The PR is **clean** when all of these hold for the current
head SHA:

- every CI check passed, or each failing check was shown by a subagent to
  also fail on `main` (it is pre-existing, so report it but do not fix it
  here);
- the latest fresh review has **no blocker and no rework** findings;
- no human review comment or requested change is unaddressed;
- `mergeable` is not `CONFLICTING`.

If clean and the PR does not have the label yet, add it:
`gh pr edit <PR> --repo lfortran/lfortran --add-label Tests::Run-Exhaustive`.
Wait until now because the exhaustive suite is expensive, and while the label
is present it reruns on every push (`.github/workflows/Exhaustive-Checks-CI.yml`
triggers on `labeled` and `synchronize`). If `gh` lacks permission to add
labels, tell the user and ask them to add it. Record the label in `state.md`,
then start the next round with only the CI watch; the head SHA is unchanged,
so the review stands, and this round does not count toward the cap.

The PR is **done** when it is clean, the label is present, and the
`Exhaustive checks` workflow ran on the current head SHA (not `skipped`) with
every job passed or shown to also fail on `main`. Check with
`gh pr checks <PR> --repo lfortran/lfortran --json workflow,name,bucket`.
If done, go to Phase 6. Exhaustive failures go to the fix subagent like any
other CI failure.

**5d. Otherwise spawn a fresh fix subagent** with the list of what is
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
- For base conflicts or an outdated branch: follow **Keeping the PR branch
  current**. Rebase onto `<upstream-remote>/main` and push with
  `--force-with-lease` while the PR remains in `rebase` mode; once a
  review-sensitive event switches it to `merge` mode, merge
  `<upstream-remote>/main` and push normally. Resolve conflicts, rebuild, and
  retest either way.
- Rebuild, rerun the MRE, the new integration test, and the affected suites
  (the full integration and reference suites whenever compiler source
  changed). Update `pr_body.md` and the PR description
  (`gh pr edit <PR> --body-file ...`) if scope or rationale changed.
- Commit focused follow-ups under **Commit authorship** (no co-author lines),
  inspect the new commits, and push them to the fork remote.

Report: what was fixed, what was rejected and why, the new head SHA, and test
results. Then start round `k+1`.

If the same finding or CI failure survives two fix rounds, or the round cap is
reached, stop the loop and report to the user with the outstanding items.
Do not keep iterating blindly.

### Phase 6: Finish

1. Mark the PR ready: `gh pr ready <PR> --repo lfortran/lfortran`.
2. Leave the job's worktree, branch, and scratch reproducers available.
   Do not switch or clean the caller's checkout.
3. Return the report to the batch coordinator, or print it for a standalone
   job. Include all issues in a connected group and their individual results:

```
PR ready for review: <URL>

Issues:  <each #N and title>   (every original reproducer now passes)
Branch:  <fork-remote>/<branch>   head <sha>
Worktree: <absolute path>
Bugs fixed (<count> MRE iterations, one commit each):
  1. <one line> — integration_tests/<test>.f90 (fails before, passes after)
  2. ...

CI:      <passed | pre-existing failures on main: names>
Exhaustive CI: <passed | pre-existing failures on main: names>
Review:  <rounds> round(s); blockers/rework fixed: <n>; rejected with reason: <n>
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
- **CI-only failures:** delegate reproduction using CI's exact platform/backend
  commands; see `AGENTS.md` for the alternate CI-parity setup.
