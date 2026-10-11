# Batch issue fixing

Use this procedure for a list, a filtered GitHub issues URL, or a request to
fix all matching issues. The batch coordinator handles selection, scheduling,
dependencies, and publication decisions. Fresh issue-worker subagents run the
per-job phases in `../SKILL.md`; the coordinator does not implement fixes.

## Resolve and freeze the complete selection

1. Parse an explicit list or decode the URL's `q` parameter with a URL parser.
   Preserve the repository, state, labels, author, dates, and Boolean filters.
   Normalize UI spellings such as `state:open` to `is:open` where needed for
   the API. Two required labels are an intersection, not a union. Do not pass
   a search URL to `gh issue view` or interpret query text as shell code.
2. Use `gh` to fetch every match, not its default first page. For example:
   ```bash
   gh api --method GET --paginate search/issues \
       -f q='repo:lfortran/lfortran is:issue is:open label:"derived types" label:"high priority"' \
       -f per_page=100
   ```
   Substitute the actual selection; do not hard-code these example labels.
   With GitHub MCP tools, run the same query and page through every result
   (see **GitHub access** in `SKILL.md`).
   Keep only issues, deduplicate by repository and number, and verify the
   count against `total_count` and `incomplete_results`. GitHub search is
   capped at 1,000 results: partition the same query into non-overlapping
   creation ranges or use a paginated repository listing with equivalent
   filters. Do not claim completeness while a page, rate limit, or cap is
   unresolved.
3. Save the original query, resolved query, selection time, and complete list
   of issue numbers/URLs in the coordinator's ignored
   `.fix-issue/<batch-id>/` directory in the invoking checkout. Freeze that
   list for this run. New matches do not silently expand it. An empty result
   is a no-op report, not a reason to relax the filters.
4. Check current issue state and existing PRs before assigning work. Read
   likely matches before deciding there is a conflict. Resume prior work only
   when its recorded issue, branch, worktree, and PR agree with this job.
   Do not take over other contributors' work. Mark closed, already-fixed,
   non-reproducing, invalid-input, or existing-work cases explicitly, preserving
   evidence and continuing the remaining independent jobs.

## Assign isolated workers

Run shared preflight once: authentication, fork/remotes, caller worktree state,
and a fetch of upstream `main`. Pin that base SHA for independent branches.
Leave the invoking checkout, its current branch, and dirty files untouched.

Reserve a unique `fix/<issue>-<slug>` branch and absolute worktree path per
issue, for example:

```
<invoking-checkout>/.fix-issue/<batch-id>/worktrees/<issue>/
```

Create worktrees serially with `git worktree add -b <branch> <path> <base-sha>`.
Do not check out these branches in the invoking directory. Git refs, remotes,
and the stash stack are shared even though worktree files are not; serialize
shared Git mutations and never use workers' stashes as a handoff mechanism.

Launch one fresh issue-worker subagent per issue, queued within available
CPU, RAM, and disk capacity. Allocate build/test jobs across active workers,
not all cores to each one. A worker orchestrates the phase subagents from
`SKILL.md`, with at most one writer/build/test process in its worktree.
Parallelize across isolated worktrees, not within a checkout.

Every worker handoff includes:

- Its assigned issue IDs and URLs, worker role, and stop/publication checkpoint.
  Do not give it the batch query as a new assignment.
- Absolute worktree and state paths, branch, pinned base SHA, fork remote,
  and any existing PR/head SHA.
- The Pixi environment and exact invocation, build directory, compiler path,
  and job budget. Use only Pixi, starting with
  `pixi run -e llvm11 build '-DCMAKE_CXX_FLAGS_DEBUG=-g -O2' '-DCMAKE_C_FLAGS_DEBUG=-g -O2'`
  from the assigned worktree; the build is `build/llvm11`, with dependencies
  in that worktree's `.pixi`. All reference tests and updates use `llvm11`.
  Never install dependencies outside Pixi. Do not share or copy build caches, runtime
  modules, or environments.
- Skills to load, authorized actions, and the no-co-author commit policy.
  Commits are allowed; publication waits for the coordinator's grouping
  decision and a Quick CI slot.
- A short report with verified root cause, required/shared fixes, commit/MRE/
  test evidence, original-issue status, blockers, and every additional bug.
  Keep verbose evidence in the state directory.

Maintain a coordinator registry with one row per selected issue: worker,
worktree, branch, base/head SHA, phase/status, dependency group, evidence path,
PR URL, and blocker. Update it at every handoff and reread it when resuming.
If the runtime cannot spawn subagents, report that limitation instead of
pretending to have delegated the batch.

## Group by actual dependencies, not by labels

Start with separate branches. Workers report suspected shared root causes or
dependencies as soon as found and return **before publishing** with their
verified findings. Compare their evidence; do not infer a shared bug merely
from similar error messages, labels, or edits to the same file.

| Relationship | Default publication |
| --- | --- |
| Independent fixes | Separate fork PRs |
| Several reports of one root cause | One fix PR covering all verified reports; do not duplicate the implementation |
| One fix requires another, or the reports need one coherent change | One comprehensive PR with ordered, focused commits |
| Unrelated bug found while fixing | Separate issue after duplicate checking, not an extra fix in the current PR |

Represent a dependency chain as commits in one PR, **not a stack of PRs**,
unless the user explicitly requests a stack. One distinct bug still needs one
MRE, commit, and integration test; multiple reports of that same bug need all
their original scenarios checked, not artificial duplicate commits.

For consolidation, select one group owner and branch, stop other writers to
that group, and delegate integration of the verified commits in dependency
order. Preserve the other workers' branches, REs, and evidence. Do not cherry-pick
the same shared fix twice or discard uncommitted work. Archive each original
RE separately under the group state directory. Rebuild and run the local gate
on the combined head with every included issue's original scenarios and new
tests, not only the final worker's MRE; Quick CI runs the suites. For each
dependent commit, fail-before evidence uses the build of the preceding commit.

Reassess grouping if later work exposes another dependency. Do not silently
close or supersede already-published PRs; ask before changing that publication
plan. Before publication, record the final issue-to-PR mapping and authorize
only the owner to push/open the draft. Include `Fixes #N` for every issue fully
verified by that PR, never for a partially fixed report.

## Limit concurrent Quick runs

At most **four** of the batch's PRs may have Quick checks running at once:
each run has about twelve jobs and competes with human PRs for the
organization's runners. Branches that passed the local gate wait in a FIFO
publication queue; when a running Quick run finishes, grant the next branch
its slot and let it publish the draft. Later pushes to an already published
PR do not wait: a CI-fix push replaces that PR's own run, and the push rules
already batch the rest. A queued worker may start its local review rounds.

## Track additional bugs across the whole batch

The coordinator owns a shared follow-up ledger and serializes duplicate
checking and filing. Workers report discoveries from setup, reproduction,
reduction, fixing, original-issue checks, local tests, review, and CI, including
jobs that stop without producing a PR.

- Bugs required to fix the selected issues belong in the connected fixing loop.
- Regressions introduced by the new changes return to that group's fix loop.
- Unrelated pre-existing compiler bugs need baseline evidence, a standalone
  reproducer, exact commands/results, and reference-compiler evidence. Search
  open and closed issues, inspect likely matches, check the shared ledger, and
  recheck just before filing. Record a canonical existing issue instead of
  opening duplicates from two workers. Infrastructure failures and speculative
  refactoring ideas are not compiler bug reports.

Use the follow-up filing phase in `SKILL.md`. File verified new bugs without
waiting for a parent PR to exist. Include the originating issue links, and add
PR links when available. Link filed and existing reports from all affected PRs
and the final report; do not comment on or relabel the original/existing issues.
An API or permission failure leaves an explicit `blocked` entry with the
ready-to-file body; report it to the user immediately, not only at the end.

## Finish each group and account for every issue

Each published group independently completes the existing review/CI loop at
its current head, including any explicitly requested Exhaustive checks, before
being marked ready.
One blocked group does not stop unrelated workers. Apply the per-job no-progress
and review-round limits rather than retrying a stuck batch indefinitely.

Return one compact table covering every selected issue exactly once: issue,
status, PR or connected group, branch/worktree, and blocker if any. Distinguish
ready PRs, already-fixed/non-reproducing cases, existing work, and unfinished
jobs. Report additional bugs filed, duplicates linked, and outstanding permission
or human-review actions. Preserve worktrees and state for inspection/resume,
leave the caller's checkout unchanged, and do not close issues or merge PRs.
