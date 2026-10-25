---
name: make-release
description: >
  Make an LFortran release end-to-end: require the tip of main to have its own
  green Quick and Exhaustive push runs (send fix PRs if not, but never release
  red main), create the vX.Y.Z tag and GitHub release with generated notes,
  verify the CI-uploaded tarball, update conda-forge/lfortran-feedstock from
  the user's fork (rerender, automerge, packaging patches until green, then
  upstream every patch to LFortran main), and update
  lfortran/lfortran-conda-testing until green (workarounds or disabled steps
  linked to filed issues, with fix-issue PRs to main for every regression).
  Use when asked to make, cut, publish, or finish a release or a new version.
  Ends by printing the version announcement with the release and both PR
  links. Not for release-policy discussions or CI changes unrelated to a
  release.
compatibility: >
  Requires git, authenticated gh (with the `workflow` scope) and push access
  to lfortran/lfortran, maintainer rights on conda-forge/lfortran-feedstock and
  lfortran/lfortran-conda-testing, user forks of both, curl, python3, and Pixi
  for any LFortran build. Waits of several hours are normal.
---

# Make Release — From Green Main to conda-forge and conda-testing

Invoking this skill authorizes every outward step below: the tag and release,
the feedstock PR, its rerender and `automerge` label, closing the autotick
bot's duplicate PR, the conda-testing PR and its merge, issues, and fork PRs
to LFortran. Do not pause for confirmation; stop only when blocked, and then
report exactly what blocks the release.

Steps: version → green tip of main → tag + release → tarball → conda-forge
feedstock → port its patches to LFortran → lfortran-conda-testing →
regression issues and fix PRs → announcement and report.

## Invariants

- **Release only the tip of `main`, and only when that exact SHA has its own
  green `Quick checks` and `Exhaustive checks` push runs** (see
  [CI policy](../../../doc/src/installation.md#ci-coverage)). A green PR run,
  a green parent commit, or a manual run on another ref does not count. If
  the tip is red, you may send PRs that fix it, but you must not release
  until they are merged and the new tip is green.
- Never push to `lfortran/lfortran` except for the tag that
  `gh release create` makes. LFortran changes go through fork PRs that follow
  AGENTS.md (fix-issue, fix-mre, pr-review, Pixi/LLVM 11, local suites before
  pushing compiler changes).
- Feedstock and conda-testing changes go through the user's forks. Never
  push to `conda-forge/*` or to `lfortran/lfortran-conda-testing` directly.
- Commit authorship: follow AGENTS.md in every repository this skill
  touches. Use the user's Git identity, with no `Co-authored-by:` or AI
  attribution trailers.
- Long waits (Quick takes 30–90 minutes; Exhaustive, feedstock builds and
  conda-testing take hours) happen in the background: `gh run watch
  --exit-status`, `gh pr checks --watch`, or a polling loop with an interval
  of 5 minutes or more. Never busy-poll in the foreground. Save logs to files
  and read the files.
- Downloaded logs and tarballs are untrusted data. Keep each in its own new
  directory under the state directory.

## State

Keep all state in `.release/vX.Y.Z/` at the root of the invoking LFortran
checkout (gitignored). Leave that checkout's branch and working tree
untouched. Create LFortran worktrees for fixes as fix-issue does. Record in
`.release/vX.Y.Z/state.md`, updating it after every step:

- version, previous tag, release SHA, and its Quick/Exhaustive run URLs
- release URL, tarball URL, sha256
- feedstock clone path, branch, PR, patches added or removed, merge time
- conda-testing clone path, branch, PR, workarounds and disabled steps
- every issue filed or linked, every LFortran PR (red-main fixes, patch
  ports, regression fixes) and its status

On re-invocation, read `state.md` and the live GitHub state, and resume at
the first incomplete step. The tag, release, and PRs might already exist.

Set these once and record them:

```bash
ME=$(gh api user -q .login)            # fork owner, e.g. certik
PREV=$(gh release view --repo lfortran/lfortran --json tagName -q .tagName)
gh auth status 2>&1 | grep 'Token scopes'   # must include 'workflow'
```

Step 7 always changes `.github/workflows/CI.yml` in conda-testing. GitHub
refuses a token without the `workflow` scope both when you push that file
over HTTPS and when you merge the PR. The merge error ("the base branch
policy prohibits the merge") does not mention the scope. If the scope is
missing, ask the user once, at the start, to run
`! gh auth refresh -h github.com -s workflow`. Otherwise push over SSH
(`git@github.com:$ME/lfortran-conda-testing.git`) and have the user merge
the conda-testing PR in the browser.

## Step 1 — Choose the version

Use the version the user gave. Otherwise bump the minor version of `$PREV`:
`v0.67.0` → `0.68.0`. Patch releases (`X.Y.1`) are only made on request.
Check that the tag is new:

```bash
git ls-remote --tags https://github.com/lfortran/lfortran "refs/tags/vX.Y.Z"   # must be empty
```

The fork branch name in both downstream repositories is `lf<minor>`, e.g.
`lf68` for 0.68.0.

## Step 2 — Require a green tip of main

```bash
SHA=$(git ls-remote https://github.com/lfortran/lfortran refs/heads/main | cut -f1)
gh run list --repo lfortran/lfortran --commit "$SHA" --event push --branch main \
    --json workflowName,status,conclusion,databaseId,url
```

Both `Quick checks` and `Exhaustive checks` must be `completed`/`success`.
For each workflow:

- **In progress or queued:** wait in the background.
- **`cancelled`:** coalescing skipped this commit. Run `gh run rerun <id>`
  (this tests exactly that SHA) and wait.
- **No run listed:** the pending run has not been created or was replaced.
  Wait, then check again.
- **`failure`:** save `gh run view <id> --repo lfortran/lfortran --log-failed`
  to `.release/vX.Y.Z/ci-<id>.log` and classify each failed job:
  - *Infrastructure* (runner lost, network or download failure, cache
    service, timeout with no test failure): `gh run rerun <id> --failed`
    once. If the job fails the same way again, treat it as real.
  - *Real* (compiler, test, application, documentation, or packaging
    failure): check for an existing fix PR or issue. Otherwise, fix it with
    a fork PR. For compiler bugs, follow fix-issue, including the
    AGENTS.md rule that a third-party application failure becomes a reduced,
    registered integration regression. CI-only fixes need no MRE. The release
    is blocked until the fix is merged. You cannot merge to `main`, so record
    the PRs, report the blocker, and keep waiting. When the fix lands, the
    new tip is the candidate.

When the candidate is green, read the tip again right before tagging. If
`main` has moved, the new tip is the candidate: repeat this step. Coalescing
keeps at most one run in progress and one pending, so each round is bounded.
Record the final SHA and the URLs of both green runs.

## Step 3 — Tag and publish the release

Preview the generated notes (GitHub's default "Generate release notes"),
then create the release. This creates a lightweight tag on `$SHA`, which is
how all previous releases were made:

```bash
gh api repos/lfortran/lfortran/releases/generate-notes \
    -f tag_name=vX.Y.Z -f target_commitish="$SHA" -f previous_tag_name="$PREV" \
    -q .body > .release/vX.Y.Z/notes.md
gh release create vX.Y.Z --repo lfortran/lfortran --target "$SHA" \
    --title vX.Y.Z --generate-notes --notes-start-tag "$PREV" --latest
```

Do not edit the generated notes. The release must be **published and marked
latest**, not a draft or prerelease: `ci/upload_tarball_to_release.sh`
uploads to `releases/latest`. Verify:

```bash
gh api repos/lfortran/lfortran/releases/latest -q .tag_name      # vX.Y.Z
git ls-remote https://github.com/lfortran/lfortran "refs/tags/vX.Y.Z"  # $SHA
```

## Step 4 — Verify the tarball

The tag push starts Quick and Exhaustive on `vX.Y.Z`. Exhaustive's
`Upload Tarball` job runs `ci/create_source_tarball.sh` and attaches
`lfortran-X.Y.Z.tar.gz` to the release. Wait for that job, and for both tag
runs to finish:

```bash
gh run list --repo lfortran/lfortran --branch vX.Y.Z --event push \
    --json workflowName,status,conclusion,databaseId,url
gh release view vX.Y.Z --repo lfortran/lfortran --json assets \
    -q '.assets[]|.name+" "+.digest+" "+.url'
```

- If `Upload Tarball` failed, read its log and rerun the job. If `latest`
  pointed at another release when it ran, the asset went there. Delete that
  stray asset (`gh release delete-asset`), make vX.Y.Z latest, and rerun the
  job. Never build or upload a tarball by hand.
- Download the asset into `.release/vX.Y.Z/tarball/`. Check that
  `shasum -a 256` matches the asset `digest`, and that the tarball's
  `lfortran-X.Y.Z/version` file contains exactly `X.Y.Z`. Record the sha256.
- A failure in another tag-run job does not undo the release. Diagnose it
  as in Step 2, fix it on `main` with a fork PR, and report it. If it shows
  that the tarball is broken, the feedstock step will show the same thing,
  so handle it there with a patch.

The tarball contains the files that `build0.sh` generates (parser,
tokenizer, `asr.h`, `ast.h`, ...) and only part of the source tree:
`src/`, `share/`, `cmake/`, `examples/`, `doc/man`, `doc/src/asr`,
`tests/asr`, a few `integration_tests/` files, the top-level
`CMakeLists.txt`, `README.md`, `LICENSE`, and `version`. It does not include
`build0.sh` or the generators. This matters when writing patches.

## Step 5 — conda-forge feedstock

### Prepare the PR

```bash
gh repo fork conda-forge/lfortran-feedstock --clone=false   # no-op if the fork exists
git clone https://github.com/$ME/lfortran-feedstock .release/vX.Y.Z/feedstock
cd .release/vX.Y.Z/feedstock
git remote add upstream https://github.com/conda-forge/lfortran-feedstock
git fetch upstream && git switch -c lf<minor> upstream/main
```

Edit the recipe, which is v1 `recipe/recipe.yaml` (rattler-build). Recipes
before 0.67 used `meta.yaml`; use whichever one upstream has now.

1. `context.version: "X.Y.Z"` and `source[0].sha256: <sha256>`.
2. `build.number: 0`.
3. For every patch under `source[0].patches`, test it against the extracted
   tarball with `patch -p1 --dry-run` and `patch -p1 -R --dry-run`. Remove
   the patch, its `patches:` entry, and its comment when it is already in the
   release (reverse applies cleanly, forward does not). Keep it when it still
   applies forward, and check that its upstream port PR (in `state.md` or
   the patch header) is still open. If it applies in neither direction,
   investigate before you decide.
4. Look for build changes since the previous release, such as new or raised
   dependency versions, CMake options, or files the install step needs:
   `git diff $PREV vX.Y.Z -- CMakeLists.txt cmake/ src/*/CMakeLists.txt src/runtime`.
   Change pins or build scripts only when the release needs it. Do not
   refresh unrelated pins.

Commit in small steps, following past PRs: `Upgrade LFortran to X.Y.Z`, then
`Remove patches merged upstream in X.Y.Z, reset build number` when that
applies. Push to the fork and open the PR:

```bash
git push -u origin lf<minor>
gh pr create --repo conda-forge/lfortran-feedstock --base main \
    --head $ME:lf<minor> --title "Upgrade LFortran to X.Y.Z" --body-file <body>
```

Body: the conda-forge checklist, with its boxes ticked:

```markdown
Checklist
* [x] Used a [personal fork of the feedstock to propose changes](https://conda-forge.org/docs/maintainer/updating_pkgs.html#forking-and-pull-requests)
* [x] Bumped the build number (if the version is unchanged)
* [x] Reset the build number to `0` (if the version changed)
* [x] [Re-rendered]( https://conda-forge.org/docs/maintainer/updating_pkgs.html#rerendering-feedstocks ) with the latest `conda-smithy` (Use the phrase <code>@<space/>conda-forge-admin, please rerender</code> in a comment in this PR for automated rerendering)
* [x] Ensured the license file is being packaged.
```

### Rerender, then automerge

Ask the bot to rerender with this exact comment:

```bash
gh pr comment <pr> --repo conda-forge/lfortran-feedstock --body "@conda-forge-admin, please rerender"
```

Wait for `conda-forge-admin` to respond. It either pushes a commit titled
`MNT: Re-rendered with conda-smithy ...` to the fork branch, or comments that
there was nothing to do. If it pushed, run `git pull --ff-only` before your
next commit. Never force-push over the bot's commits.

Then add the label: `gh pr edit <pr> --repo conda-forge/lfortran-feedstock
--add-label automerge`. The automerge bot merges once `linter`, `azure`, and
`github-actions` pass. **It removes the label whenever a commit is pushed
after the label was added**, so add the label again after every push,
including bot rerender commits.

regro-cf-autotick-bot usually opens its own `lfortran vX.Y.Z` PR, sometimes
only after yours. Whenever it appears, close it with a comment pointing at
your PR.

### Make it pass with patches

Wait for the checks in the background (`gh pr checks <pr> --repo
conda-forge/lfortran-feedstock --watch`). For each failure, save the log.
Azure builds expose logs through
`https://dev.azure.com/conda-forge/feedstock-builds/_apis/build/builds/<buildId>/logs`.
For GitHub Actions jobs, use `gh run view <id> --repo
conda-forge/lfortran-feedstock --log-failed`. Then classify it:

- **Recipe or environment problem** (solver conflicts, a missing build tool,
  conda-forge toolchain changes, cross-compilation settings): fix the recipe
  or build scripts. LFortran needs no change.
- **LFortran source or build-system problem** (compile errors on a platform,
  missing install files, failing package tests): add a patch. Extract the
  tarball into a new directory and run `git init && git add -A && git commit`.
  Make the minimal fix, then save `git diff` as
  `recipe/patches/000N-<slug>.patch`. Start the patch with a
  `Subject: [PATCH] <summary>` line and a paragraph that gives the error and
  explains the fix, like the earlier
  `0001-cmake-add-generated-header-dir-to-wasm-runtime-includes.patch`. List
  it under `source[0].patches` with a comment saying it is to be upstreamed.
  Paths are relative to the tarball root. If a fix touches a generator
  input (`parser.yy`, `ASR.asdl`, ...), patch the generated file that ships in
  the tarball as well, because the feedstock never regenerates sources.
- Reproduce a source-level failure locally when you can: build the patched
  tree with the LFortran Pixi environment, or run `pixi exec rattler-build`
  on the recipe. Otherwise iterate on feedstock CI.

Do not bump `build.number` within this PR. Commit each fix separately with
a clear message, push, re-add `automerge`, and repeat until the PR merges.
Record each patch in `state.md` with the failure it fixes.

## Step 6 — Port feedstock patches to LFortran

After the feedstock PR merges, open one fork PR to LFortran `main` for each
patch that is still carried, unless an upstream PR already exists. Do this
in a fresh worktree from `origin/main` with the AGENTS.md Pixi setup. Follow
the precedent of lfortran#13569 and #13567. The PR applies the same fix to
current `main` (adapted if the code has moved, including generator inputs
instead of generated files). Test it as well as it can be tested: an
integration test when the behavior is testable, otherwise a focused harness
or a build check, and say which in the PR. The PR body states that it
upstreams the fix carried by lfortran-feedstock (link the feedstock PR and
patch), so that the patch can be removed after the next release. Run the
local suites before pushing compiler-source changes, and use pr-review while
Quick runs, as fix-issue does. The next release removes the patch in Step 5.

## Step 7 — lfortran-conda-testing

Start only after the new package can be installed on every platform the
feedstock builds (`.ci_support/*.yaml`: linux-64, linux-aarch64, osx-64,
osx-arm64, win-64). Check the channel index that the solvers read, not the
anaconda.org API. The API lists uploaded files about an hour before the
index does, and a run started in between fails in every job. Wait in the
background until this prints all five platforms:

```bash
for s in linux-64 linux-aarch64 osx-64 osx-arm64 win-64; do
  curl -sL https://conda.anaconda.org/conda-forge/$s/current_repodata.json | python3 -I -c \
    "import sys,json; d=json.load(sys.stdin); p={**d.get('packages',{}),**d.get('packages.conda',{})}; \
     print('$s' if any(v['name']=='lfortran' and v['version']=='X.Y.Z' for v in p.values()) else '')"
done
```

If CI still fails to solve with `lfortran =X.Y.Z * does not exist (perhaps a
typo or a missing channel)`, it is too early, not a regression. Rerun the
failed jobs (`gh run rerun <id> --failed`) once the index lists the version.

```bash
gh repo fork lfortran/lfortran-conda-testing --clone=false
git clone https://github.com/$ME/lfortran-conda-testing .release/vX.Y.Z/conda-testing
cd .release/vX.Y.Z/conda-testing
git remote add upstream https://github.com/lfortran/lfortran-conda-testing
git fetch upstream && git switch -c lf<minor> upstream/main
```

1. In `.github/workflows/CI.yml`, set `env.LFORTRAN_VERSION: X.Y.Z`. Commit
   it as `Add version X.Y.Z`.
2. Re-enable earlier workarounds that are no longer needed. For each step
   that is commented out, or each workaround comment that links an lfortran
   issue or PR, check whether the fix is in `$PREV..vX.Y.Z`. If it is,
   restore the original step in its own commit.
3. Compare the third-party pins in `CI.yml` (`git checkout <tag or SHA>`
   lines and `test "$(git rev-parse HEAD)" = ...` checks) with the ones
   LFortran tests at `vX.Y.Z`, in `ci/test_third_party_codes.sh` and
   `ci/test_caffeine.sh`. A pin that lags LFortran's can fail for reasons
   already fixed upstream: 0.68.0's PRK coarray failure came from Caffeine
   0.8.2, which does not set `stat=` (lfortran#14114, fixed in Caffeine
   0.8.4, which LFortran already used). Moving a lagging pin to LFortran's
   commit is an environment fix. Make it when its job fails, in its own
   commit naming the LFortran commit that moved the pin.
4. Push (see the `workflow` scope note under **State**), then open the PR:
   `gh pr create --repo lfortran/lfortran-conda-testing --base main --head
   $ME:lf<minor> --title "Add version X.Y.Z" --body ""`.

The jobs are: LFortran integration, `--fast` integration, reference, and LSP
tests, run from the `vX.Y.Z` checkout against the conda package. Then
third-party codes (minpack, dftatom, fastGPT, stdlib, SNAP, POT3D, PRIMA,
LAPACK), fpm packages, do concurrent/GPU (Formal, Fiats, PRK), coarrays
(Caffeine), and Windows, each on macOS and Linux where listed. Wait for all
of them. For each failure, save the log and compare it per job: find the
most recent upstream `main` run in which that job passed (the latest `main`
run as a whole may be red for another job). Then classify:

- **Too early** (the solver cannot find `lfortran =X.Y.Z`): see above.
- **Intermittent** (a job that passed earlier in this PR, or on the previous
  attempt, fails with the same commit, package, and pins, often without a
  diagnostic): rerun the failed jobs once with `gh run rerun <id> --failed`.
  If the same failure comes back, treat it as real. If the rerun passes,
  search for an existing issue. Otherwise file one titled
  `Intermittent: <symptom> (X.Y.Z)`, with the failing job URL, the pinned
  commits and versions, the end of the log, and how to try to reproduce it.
  Do not add a workaround for it.

- **Environment or toolchain** (solver conflicts with the new package's
  dependencies, new Python requirements of LFortran's test runners, a
  compiler or preprocessor change in conda-forge): fix `CI.yml` or
  `environment.yml`. Past examples: adding `tomli`, relaxing pins,
  `PP="cpp -x c -traditional-cpp"`.
- **The third-party project changed** (repositories cloned without a pinned
  commit): pin the last commit that worked, or adapt the build step.
- **Packaging** (the installed layout differs from the build tree, or
  runtime files are missing): fix the feedstock with a patch and a new build
  number in a follow-up feedstock PR, then port it as in Step 6. Work around
  it here only until the new build is published.
- **LFortran bug** (it passed with `$PREV`, and the third-party code and the
  environment did not change; or it is one of LFortran's own tests, run from
  the `vX.Y.Z` checkout, failing against the package, even a test added in
  this release): reproduce it first with the released package itself, which
  needs no build:
  `pixi exec --spec "lfortran==X.Y.Z" --spec clang --channel conda-forge -- bash -c '...'`.
  The feedstock builds with its `llvm_version` (LLVM 21 for 0.68.0), not
  LLVM 11, and a bug can depend on it. #15968 (an undefined `__floatunsitf`
  under `--fast` on macOS arm64) appears only with LLVM 21 or newer, and
  passed with the Pixi `llvm11` build. Search open and closed lfortran
  issues. File a new issue only when no existing one covers it, with a
  minimal reproducer (repro-issue/create-mre) and the LLVM version it needs.
  Then use the narrowest workaround that keeps the rest of the coverage: a
  compiler flag, skipping one test, or commenting out one command or step.
  Put a comment next to it,
  `# Disabled for LFortran X.Y.Z: https://github.com/lfortran/lfortran/issues/<n>`,
  following the existing comments in `CI.yml`. Never disable a whole job to
  get past a single failure.
  - To skip one of LFortran's integration tests, note that
    `LFORTRAN_CTEST_EXCLUDE` only filters ctest after every test was built.
    A test that fails to compile or link stops the build. If `run_tests.py`
    at `vX.Y.Z` has `--exclude` (lfortran#15972), pass the registered test
    names to it. Otherwise delete just their `RUN(...)` lines, on the
    affected OS only, before calling `run_tests.py`:
    `sed -i.bak '/^RUN(NAME <name> /d' CMakeLists.txt`.

Commit each workaround separately and push. Merge once every check is
green: `gh pr merge <pr> --repo lfortran/lfortran-conda-testing --merge`. If
the merge is refused for the token's missing `workflow` scope, ask the user
to merge it in the browser, then confirm with `gh pr view <pr> --json
state,mergedAt`.

## Step 8 — Fix every regression on LFortran main

Use fix-issue for every LFortran regression issue from Step 7, and for
compiler bugs from Steps 4–5 that a patch does not already cover. Use its
batch mode if there are several. The goal is that the next release passes
conda-testing with the workarounds removed. Link the fix PRs from the issues
and from `state.md`. Report draft PRs that are still iterating as in
progress. Do not hold the release report for them.

Pass the reproduction environment to fix-issue. If the bug needs the
feedstock's LLVM, the fixer also builds the Pixi environment with that LLVM
(such as `llvm21`) in the same worktree, and shows the test failing before
the fix and passing after with it. It still runs the suites with `llvm11`. Quick runs
`--fast` only with LLVM 11, so such a test may never fail in Quick; the PR
must say so. The user may mark fix PRs ready or merge them; record a merged
PR as merged.

## Step 9 — Announcement and report

Print the announcement exactly in this form, ready to paste:

```text
X.Y.Z

https://github.com/lfortran/lfortran/releases/tag/vX.Y.Z
https://github.com/conda-forge/lfortran-feedstock/pull/<n>
https://github.com/lfortran/lfortran-conda-testing/pull/<m>
```

Then report:

- Release SHA, with links to its green main Quick and Exhaustive runs and to
  the tag runs.
- Tarball URL and sha256.
- Feedstock: patches removed and patches added (with the failure each
  fixes), and the merge time.
- Upstream ports: one LFortran PR per carried patch, with status.
- conda-testing: environment fixes, pins, re-enabled steps, and disabled
  steps with their issue links.
- Regressions: issue → fix PR → status.
- Intermittent failures: job, symptom, and the issue filed or linked.
- Anything still open or blocked, and what it waits on.

Never report the release as complete while a step is unfinished. Name the
step and what it waits on.
