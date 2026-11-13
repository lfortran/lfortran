# GitHub evidence and label updates

Read this when classifying live PRs or applying labels. These are operations to
perform with authenticated `gh` (or equivalent GitHub tools), not a shell script
to execute blindly. Substitute the selected repository, PR and base branch.

## Snapshot the selected PRs

Default to `lfortran/lfortran`, open PRs targeting `main`. Preserve every user
filter. Freeze the selection once; fetch every page and deduplicate numbers.
GitHub search stops at 1,000 results: partition a larger result rather than
silently truncating it. Historical research filters on **merge time**, not
creation or update time.

For each PR record the repository, number, state, draft status, head SHA, base
branch and current base SHA, mergeability, labels, and evidence retrieval time.
Do not equate `mergeStateStatus=BLOCKED` with a failed test: determine the cause.
A required human approval is not itself a reason to exclude a PR from a
workflow whose purpose is to decide whether human review is needed.

```bash
gh pr view <number> --repo lfortran/lfortran \
  --json number,title,body,url,state,isDraft,headRefOid,baseRefName,baseRefOid,mergeable,mergeStateStatus,labels,reviewDecision,statusCheckRollup
gh pr diff <number> --repo lfortran/lfortran
gh api --paginate --slurp \
  'repos/lfortran/lfortran/pulls/<number>/files?per_page=100'
gh api --paginate --slurp \
  'repos/lfortran/lfortran/issues/<number>/comments?per_page=100'
gh api --paginate --slurp \
  'repos/lfortran/lfortran/pulls/<number>/reviews?per_page=100'
gh api --paginate --slurp \
  'repos/lfortran/lfortran/pulls/<number>/comments?per_page=100'
gh api --paginate --slurp \
  'repos/lfortran/lfortran/labels?per_page=100'
```

Fetch review-thread resolution and outdated status with GraphQL as well;
ordinary review-comment REST responses do not establish that a thread was
resolved. Follow pagination on threads and on each thread's comments. Check
GraphQL `errors` even when the process exits successfully. Read linked issues
and prior fixes when they establish the expected behavior or the remaining
scope.

Compare retrieved file counts with the PR's `changedFiles`. A missing `patch`
field can mean a binary or a large/truncated diff, not an empty change. Inspect
the raw diff and source at the recorded base/head when necessary. Renames,
deletions, executable bits, symlinks, submodules, binaries, generated files and
changes hidden by `.gitattributes` all belong to the change. Never execute PR
scripts to discover what they do.

Use the complete PR diff, not the last commit. An authoritative PR diff is
preferable to guessing that the final commit of a rebased PR contains the whole
change. A historical merge reconstruction must account for the merge method
and reconcile its changed files and additions/deletions with GitHub.

## Establish that CI is actually green

```bash
gh pr checks <number> --repo lfortran/lfortran --required
gh api 'repos/lfortran/lfortran/rules/branches/main'
gh api 'repos/lfortran/lfortran/branches/main/protection/required_status_checks'
```

Use the **effective current** required checks, combining applicable rulesets
and legacy branch protection. The last endpoint can be absent when requirements
come from rulesets; distinguish that from an authorization or network error.
Do not assume the names or number of required jobs from this skill's historical
research. Also inspect the repository's trusted Quick workflow and its required
reusable jobs. An unexpectedly empty set needs investigation, not acceptance.

For every required context:

1. Establish the latest applicable run, its repository, check provider/app,
   workflow, event, tested SHA and conclusion. Respect the expected app binding.
   An identically named check from another provider is not a substitute.
2. Verify that it belongs to the current PR head or to an identified test-merge
   commit containing that head and the recorded base. A green run on `main`,
   another PR, a previous head, or an older successful rerun is insufficient.
3. Require a completed **success**. GitHub can accept skipped or neutral checks
   for branch protection; this classifier deliberately does not treat them as
   executed, passing required tests. Missing, queued, in-progress, cancelled,
   timed-out, failed, action-required, ambiguous or unknown results are not
   green. Do not ignore a newer pending or failed attempt.
4. Verify that the changed behavior is actually exercised. A successful job
   with the relevant test filtered out is not target coverage. CPU execution
   of GPU-generated code is not proof of GPU synchronization or performance.

The lightweight classification does not build the compiler, rerun full suites,
dispatch Exhaustive, or repair CI. Reuse identifiable existing run/log evidence;
when evidence is insufficient, record the gap. If the required-check snapshot
cannot be established, the PR is **ineligible**, not automatically low-risk.
If CI eligibility is established but the fix's own correctness/coverage evidence
is missing, the verdict is **needs-review**.

## Preserve the exact risk-label invariant

Only an explicit request to **apply** or **label** authorizes mutations.
Research, recommendations, dry runs, historical audits and evaluating this
skill are read-only.

An application request authorizes adding/removing only `low-risk` and
`needs-review` within the selected scope, including replacing an old risk
verdict. Preserve all unrelated labels. Never replace the complete label list.
Do not create a missing label unless label creation was explicitly authorized;
report the missing catalog entry instead.

Suggested descriptions, when creation is authorized:

| Label | Description |
| --- | --- |
| `low-risk` | Fits the conservative PR-risk policy for the assessed revision; merge checks still apply. |
| `needs-review` | Green, non-draft PR requiring human judgment or additional correctness evidence. |

For each selected eligible PR:

1. Re-read head/base identities, draft/state, required checks and blocking
   discussion. Reclassify if the evidence changed.
2. Remove the opposite risk label **first**, if present, then add the chosen
   label if absent. For example, switching to human review removes `low-risk`
   before adding `needs-review`.
3. Re-read labels and identities. Verify the intersection of labels with
   `{low-risk, needs-review}` is exactly `{chosen_label}`, and that unrelated
   labels were not removed. Already correct labels are a no-op.
4. If the head or relevant base context changed during the operation, remove
   the stale `low-risk` authorization and re-evaluate. If an API operation fails
   or concurrent changes prevent reconciliation, report the actual state and
   stop for that PR; do not report success.

Example operations, after the above checks (omit unnecessary calls):

```bash
gh pr edit <number> --repo lfortran/lfortran --remove-label low-risk
gh pr edit <number> --repo lfortran/lfortran --add-label needs-review
```

For selected PRs that are no longer eligible, do not add a verdict label. In
application mode remove stale risk labels from those selected PRs and report
the reason. Do not broaden an explicitly green-only or numbered selection to
mutate unrelated PRs. In recommendation mode merely report necessary cleanup.

A label update is not a transaction that binds the label to a commit. Record the
decision identity separately: repository, PR, head SHA, base SHA, complete diff
identity, policy version, category, evidence and time. A one-shot skill cannot
keep labels fresh after it exits. Before any future automerge, an event-driven
consumer must invalidate decisions on head/base/policy changes and verify a
current decision, exactly one risk label, current protection/review requirements
and passing checks for the **actual merge candidate**.

## Future merging is a separate project

Do not approve, merge, enable auto-merge, edit branch protection, resolve review
threads, or change CI as part of this skill. `low-risk` never grants permission
to bypass a code owner, requested changes, or a required approval.

In the November 13, 2026 snapshot, `main` had twelve app-bound required checks,
loose status-check freshness, and no required merge queue. Quick's triggers did
not include `merge_group`. Those are historical observations, not settings to
assume or change automatically.

An eventual merger needs either up-to-date combined-change validation or a
properly configured merge queue. GitHub Actions required workflows need a
`merge_group` trigger for the latter. Neither individually green branches nor
lack of textual conflicts proves the combination works.

Primary references:

- [Required status checks and branch protection](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/managing-protected-branches/about-protected-branches)
- [Merge queues and merge-group CI](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/configuring-pull-request-merges/managing-a-merge-queue)
- [PR files API and its limits](https://docs.github.com/en/rest/pulls/pulls#list-pull-requests-files)
