# Repository Guidelines

This file is for LLM agents and new contributors to have a single point of
detailed reference how to contribute to the project.

## Required Agent Toolchain: Pixi

Use **Pixi exclusively** to provision project dependencies, build LFortran,
and run compilers and tests. Use the `llvm11` environment for native development
unless the task explicitly requires another Pixi environment. **All reference
tests and reference updates require LLVM 11** (`llvmdev ==11.1.0`).

Run from the assigned worktree's root, saving and inspecting logs:

```bash
CMAKE_BUILD_PARALLEL_LEVEL=8 pixi run -e llvm11 build > build.log 2>&1
CMAKE_BUILD_PARALLEL_LEVEL=8 pixi run -e llvm11 rebuild > build.log 2>&1
pixi run -e llvm11 start --version
pixi run -e llvm11 ctest -j8 > unit.log 2>&1
pixi run -e llvm11 tests -j8 > reference.log 2>&1
pixi run -e llvm11 integration_tests -j8 > integration.log 2>&1
```

**Use `build` once, then `rebuild` after edits.** `build` configures CMake,
which reruns `build0.sh` and rewrites every generated source (`asr.h`, `ast.h`,
the parser, ...) with a new timestamp even when its content is unchanged, so
Ninja then recompiles almost everything (a one-line edit can cost 15+ minutes).
`rebuild` (`cmake --build`) recompiles only what changed. Run `build` again
only for a new worktree, for new CMake options, or after editing a generator
input: `src/libasr/ASR.asdl`, `grammar/AST.asdl`,
`src/lfortran/parser/parser.yy`, `tokenizer.re`, `preprocessor.re`,
`src/libasr/wasm_instructions.txt`, the generator scripts themselves
(`asdl_cpp.py`, `wasm_instructions_visitor.py`,
`intrinsic_func_registry_util_gen.py`) or `src/server/generator/`. `rebuild`
does not regenerate these, so it would compile stale generated sources.

Use the allocated job budget instead of `8` when sharing resources. `pixi run`
installs the selected environment automatically. The compiler is
`build/llvm11/src/bin/lfortran` (`lfortran.exe` on Windows). For ad hoc commands
or reproducers, use `pixi run -e llvm11 start <arguments>` or
`pixi run -e llvm11 bash run.sh`; shell activation is not inherited by later
tool calls or subagents.

Do not install dependencies with `apt`, `apt-get`, Homebrew, Conda, mamba,
micromamba, pip, or other system/global installers. Do not substitute a host
compiler or invoke standalone CMake, Ninja, build scripts, or test runners as
an alternative to the Pixi tasks. Resolve missing project dependencies in
`pixi.toml` and the selected Pixi environment, pinning each package to an
exact version (zlib and zstd excepted). If Pixi or a required platform SDK is
unavailable, report it rather than attempting another installation method.
Git, Pixi, and the platform SDKs listed below are prerequisites.

This policy also applies when consulting older installation or CI recipes:
`pixi.toml` and its tasks own agent setup. Read-only review and issue triage do
not require provisioning a compiler.

## Automatic Issue-Fixing Workflow

When the user asks to fix LFortran GitHub issues, automatically load and follow
[fix-issue](.agents/skills/fix-issue/SKILL.md), whether the input is one issue,
a list, a search URL, or filters. The user does not need to name the skill or
repeat the workflow. A prompt such as "Fix all these high-priority derived
type issues: <GitHub search URL>" selects the full end-to-end workflow below.

Apply its defaults for subagents, isolated worktrees, Pixi/LLVM 11, fork PRs,
dependency grouping, additional bug reports, and commit authorship. Explicit
task limits such as "local only" or "do not push" take precedence. Requests to
triage, reproduce, review, plan, or discuss an example prompt select their
corresponding skills rather than starting issue fixes.

## Release Workflow

When the user asks to make, cut, or publish an LFortran release, load and
follow [make-release](.agents/skills/make-release/SKILL.md). Invoking it
authorizes the tag, the downstream PRs, and the fix PRs it describes; only
the green tip of `main` may be released.

## Merge Workflow

When the user asks to merge, land, or go through open PRs, or to judge
whether a PR can be merged, load and follow
[merge-pr](.agents/skills/merge-pr/SKILL.md). It judges the change itself
(complexity, locality, non-local interactions, placement, design impact)
and the merge gates, then merges or takes the specific next step (request,
rebase, adopt, consolidate, split, close, or escalate). Readiness and triage
requests stay read-only.

## Project Structure & Module Organization
- `src/`: core sources
  - `libasr/`: ASR + utilities, passes, verification, backends
  - `lfortran/`: parser, semantics, drivers
  - `runtime/`: Fortran runtime (built via CMake)
  - `server/`: language server
- `tests/`, `integration_tests/`: unit/E2E suites
- `doc/`: docs & manpages (site generated from here)
- `examples/`, `grammar/`, `cmake/`, `ci/`, `share/`: supporting assets
- `.agents/skills/`: agent skills (see "Agent Skills" below)

## Agent Skills

Reusable, agent-invocable procedures live in `.agents/skills/<name>/SKILL.md`,
following the [Agent Skills](https://agentskills.io/) open standard. Agents load
a skill on demand when the task matches its `description`.

`.agents/skills/` is the vendor-neutral location read by Codex, Copilot, and
Grok. Claude Code only discovers `.claude/skills/`, so `.claude/skills` is a
symlink to `../.agents/skills` — the same approach as the `CLAUDE.md` →
`AGENTS.md` symlink. **Skills are stored once, in `.agents/skills/`;** edit them
there and every agent picks up the change.

Available skills:

| Skill | Purpose |
| --- | --- |
| `classify-issue` | Triage issues with evidence-based, additive labels and a frequency-first High/Medium/Low priority for every issue |
| `repro-issue` | Turn a GitHub issue into a faithful Reproducible Example (RE) |
| `create-mre` | Reduce an RE or third-party failure to a Minimal Reproducible Example (MRE) |
| `fix-mre` | Fix the compiler bug behind an MRE and add an integration test |
| `pr-review` | Review LFortran PRs with architecture, correctness, and maintainer guidance |
| `fix-issue` | Fix one issue or a filtered batch in isolated worktrees and subagents, publish dependency-aware draft PRs from a fork early, review locally while CI runs, and iterate on review and CI |
| `merge-pr` | Decide whether PRs can be merged by judging the change itself (risk class from size, locality, non-local interactions, placement, design impact) and the gates (Quick green on a fresh head, tests, history, stacks), then merge or do the exact remedy: draft-marking notes, rebase, adopt/rewrite external PRs with credit, consolidate duplicates, split, close with a link, or escalate |
| `make-release` | Release the green tip of `main`: tag with generated notes, verify the tarball, update the conda-forge feedstock (patches upstreamed afterwards) and lfortran-conda-testing, and turn regressions into issues and fix PRs |

`classify-issue` distinguishes invalid-code diagnostics from valid-code bugs,
enhancements, new features, and maintenance or internal-correctness work.
Every classification also recommends exactly one High, Medium, or Low
priority by expected encounter frequency in ordinary Fortran programming,
with severity only secondary; High is reserved for frequent failures of valid
standard Fortran on the default LLVM path. It uses the live label catalog,
preserves existing labels, and asks about uncertain cases. GitHub changes
require a labeling request and stay within its requested scope;
recommendation and local-priority requests stay read-only.

### The reproduce → reduce → fix loop

The three skills chain into a pipeline, each consuming the previous one's output:

```
repro-issue  ──►  create-mre  ──►  fix-mre
 (RE: faithful)    (MRE: minimal)   (fix + integration test)
```

This is designed to be run **in a loop** to bring a third-party Fortran package
up on LFortran: build the package, take the first failure, reduce it, fix it,
then rebuild and repeat until the package compiles and its tests pass. Each
iteration should produce one focused PR — `AGENTS.md`'s "one bug = one MRE =
one PR" rule applies to every pass through the loop.

Reproducers are written to the **assigned worktree's root** by convention
(`run.sh`, `mre_*.f90`, `re_*.f90`) and are gitignored — they are scratch inputs
to `fix-mre`. The committed deliverable is always the integration test.

### Defaults for end-to-end issue fixing

A request such as "Fix all these issues: <GitHub search URL>" is sufficient;
select `fix-issue` automatically. Do not require the user to repeat these defaults:

- Resolve the complete issue selection, preserving all query filters and
  fetching every page. Record the selected issue numbers once; do not silently
  truncate the batch or keep expanding it as new issues appear.
- Use a fresh issue-worker subagent and a separate Git worktree/branch per
  issue, starting from the same fetched upstream `main`. Within each job,
  delegate reproduction, reduction, fixing, and review to fresh phase
  subagents as specified by `fix-issue`. Keep the caller's checkout untouched,
  including any uncommitted work.
- Run independent jobs concurrently within available CPU, memory, and disk
  capacity. Allow only one writer/build/test process per worktree; never share
  build directories, runtime module files, or scratch reproducers between jobs.
  Serialize operations on shared Git state, including worktree creation and
  fetches. Do not use `git stash` across workers; its stack is repository-wide.
- Send separate PRs for independent fixes. When issues share a root cause or
  genuinely depend on one another, consolidate them into **one PR with ordered,
  focused commits**, rather than a stack of PRs, unless the user requests a
  stack. Similar labels or edits to the same file alone are not dependencies.
  Each distinct bug still needs its own MRE, commit, and integration test;
  verify every included issue's original reproducer.
- Use Pixi and LLVM 11 for fresh fixing worktrees, as described below. Pass
  each subagent the exact worktree, build directory, compiler path, environment
  invocation, and resource budget; shell activation is not inherited.
- `fix-mre` verifies each fix with targeted checks (the reproducers, the MRE,
  and each new test run directly in normal, `--fast`, and `--std=f23` modes
  and with GFortran); after the last fix, the job runs the unit, integration,
  and reference suites once, in the default mode. Then publish a draft from
  the user's fork so Quick starts early, and review it locally with
  `pr-review` while CI runs; the review reruns the suites only if compiler
  source changed since they passed. Never push compiler-source changes that
  have not passed the local suites once. In a batch, at most four PRs run
  Quick at once. Iterate on CI failures, review findings, and human feedback
  until Quick is green and the review is clean before marking ready. Every
  push reruns Quick, so batch pushes: push CI fixes as soon as they pass
  locally; hold other changes until the current run finishes rather than
  cancelling a healthy run. Follow the commit-authorship policy below in every
  subagent, including CI fixers.
- Record every additional compiler bug discovered in any phase. Fix regressions
  introduced by the current changes in the same PR. For unrelated pre-existing
  bugs, verify against upstream `main`, search open and closed GitHub issues,
  and file only genuinely new reports. Deduplicate across the whole batch;
  link existing or newly filed issues from the affected PRs and final report.
  Do this even if the original fixing job is blocked or produces no PR.
- Continue independent jobs when one is blocked. Report every selected issue
  and its PR or explicit disposition; never present a partially processed batch
  as complete.

`fix-issue` owns orchestration and publication; `fix-mre` owns each compiler fix,
its regression test, and commit preparation. The detailed batch procedure lives
in `.agents/skills/fix-issue/references/batch.md`. These defaults apply to actual
fix requests, not to triage, plan-only requests, or quoted example prompts.

The reproduction and fix skills use the compiler from the assigned build:
`build/<pixi-environment>/src/bin/lfortran`. Pixi activation selects
that directory on `PATH`. Use the assigned Pixi environment and its build
consistently. Use Pixi's `gfortran`, matching the
integration-test label, for differential testing.
Issue classification requires GitHub access (authenticated `gh` or the GitHub
MCP tools; see "Git Remotes & Issues"), not a compiler build.

## Prerequisites
- **Required:** Git and [Pixi](https://pixi.sh/). The repository manifest
  supplies build/test dependencies and Linux C/C++ compilers. On macOS, install
  Xcode Command Line Tools; on Windows, use an initialized MSVC developer
  shell. Pixi supplies the cross-platform `shell` that runs the build scripts,
  so Bash is not required. Use Pixi inside WSL for Unix-only test runners.
  If a prerequisite is missing, report it; do not provision an alternate toolchain.

## Build, Test, and Development Commands

**Use only Pixi** for installation, building, and testing. Keep dependency
versions and build details in `pixi.toml` and the scripts it invokes, rather
than duplicating setup recipes in prompts or skills. These entry points stay
the same when the build system or dependencies change:

```bash
pixi run -e llvm11 build
pixi run -e llvm11 rebuild
pixi run -e llvm11 start --version
pixi run -e llvm11 ctest -j8 > unit.log 2>&1
pixi run -e llvm11 tests -j8 > reference.log 2>&1
pixi run -e llvm11 integration_tests -j8 > integration.log 2>&1
```

`pixi run` installs the selected environment automatically. Native tasks default
to `llvm11`. Use `-e <environment>` consistently to select another configuration:

```bash
pixi run -e llvm22 build
pixi run -e llvm22 start --version
pixi run -e llvm22 integration_tests -j8 > integration-llvm22.log 2>&1
```

Each named environment has its own `.pixi/envs/<environment>` dependencies and
`build/<environment>` CMake cache, objects, executable, and runtime modules.
Reference tests run in `build/<environment>/reference_tests` (via
`run_tests.py --build-dir`, which links to the test inputs in the source tree);
integration-test builds live under `build/<environment>/integration_tests`.
Tasks select that environment's compiler explicitly, even if an older in-source
binary exists. Do not run the legacy test entry points instead of these tasks
and assume `PATH` alone overrides their in-source defaults.

Configurations can coexist, including multiple configurations using the same
LLVM version. Shared source generation still runs during configuration, so
serialize builds/configuration in one worktree; use separate worktrees for
parallel fixing jobs. `pixi run -e llvm11 clean` cleans the selected CMake build
targets, not other configurations, source files, or Pixi environments. It does
not run `git clean`.

### Pixi toolchain for fixing worktrees

Use the repository's `pixi.toml` environment `llvm11`, which pins
`llvmdev ==11.1.0`. Reference outputs are LLVM-version-sensitive; `ci/test.sh`
runs the reference suite only with LLVM 11. Do not regenerate references with
a different LLVM version to make a test pass.

In a fresh worktree, create its ignored state directory and set
`CMAKE_BUILD_PARALLEL_LEVEL` to its allocated job budget. Then run from that
worktree's root (substitute the assigned `<id>`):

```bash
pixi run -e llvm11 build '-DCMAKE_CXX_FLAGS_DEBUG=-g -O2' '-DCMAKE_C_FLAGS_DEBUG=-g -O2' > .fix-issue/<id>/build.log 2>&1
pixi run -e llvm11 llvm-config --version
pixi run -e llvm11 start --version
```

The extra flags build a Debug compiler (assertions on) with `-O2`, like
Quick's LLVM 11 compiler, keeping `-g` for stack traces; they persist in the
CMake cache, so later `build` and `rebuild` runs keep them. While iterating,
rebuild with `pixi run -e llvm11 rebuild` (see above for when `build` is
needed instead) and rerun reproducers and new tests directly (`fix-mre` targeted
checks). Run the full suites below once, after the last fix, in the default
mode only, and inspect their saved logs. Budget `<jobs>` across active
workers instead of giving each worker all cores:

```bash
pixi run -e llvm11 bash run.sh > .fix-issue/<id>/mre.log 2>&1
pixi run -e llvm11 ctest -j<jobs> > .fix-issue/<id>/unit.log 2>&1
pixi run -e llvm11 tests -j<jobs> > .fix-issue/<id>/reference.log 2>&1
pixi run -e llvm11 integration_tests -j<jobs> > .fix-issue/<id>/integration.log 2>&1
```

Pixi sets `LFORTRAN_BUILD_DIR` to the absolute `build/<environment>` path and
prepends its `src/bin` to `PATH`. Pass the worktree, environment, build directory,
compiler path, and exact invocation to every fresh subagent; activation is not
inherited across tool calls. The `tests` task checks the built compiler's LLVM
version before running or updating references; committed references remain in
the source checkout, not in the scratch workspace.

Verify `command -v lfortran`, `lfortran --version`, `llvm-config --version`,
and `gfortran --version` in that same environment before testing. Checking only
`llvm-config` is insufficient: a stale binary may still link another LLVM.
Check the platform prerequisites above. Provision missing project dependencies
through Pixi, not global package installs. Do not silently substitute another
LLVM version, disable failing checks, or commit machine-specific workarounds.
Report unavailable toolchains or SDKs explicitly.

Honor an explicitly supplied Pixi environment and reuse its build directory.
Reference tests and updates still require the `llvm11` environment.
Do not copy CMake caches, generated sources, `.mod` files, or `.pixi` environments
between worktrees. Do not use `git clean -dfx` as automatic setup or recovery.

**IMPORTANT**: always redirect test output to a log file and then examine the
log file. Do NOT run tests using the style like `pixi run -e llvm11 tests | tail` because
if you need more output than the `tail` provides, you have to rerun them and
that is very expensive, the tests can run several minutes. Instead, run tests
only once, redirect to a log file and then examine the log file.

## Quick Smoke Test
- Default build: `pixi run -e llvm11 build` (LLVM enabled); after edits,
  `pixi run -e llvm11 rebuild`.
- AST/ASR: `pixi run -e llvm11 start --show-ast examples/expr2.f90`
- Compile and run: `pixi run -e llvm11 start examples/expr2.f90`.
- Keep an executable: `pixi run -e llvm11 start examples/expr2.f90 -o expr2`.

## Architecture & Scope
- AST (syntax) ↔ ASR (semantic, valid-only). See `doc/src/design.md`.
- Pipeline: parse → semantics → ASR passes → codegen (LLVM/C/C++/x86/WASM).
- Prefer `src/libasr/pass` and existing utils; avoid duplicate helpers/APIs.
- Type coercion and casting belong in AST→ASR (semantics). Insert explicit
  Cast nodes in ASR. The LLVM/codegen backend must never infer or fix types —
  it should only lower what ASR gives it. If codegen needs a type workaround,
  the bug is upstream.
- libasr is frontend-independent. Never reference `_lfortran` or any
  frontend-specific names in libasr code. Use enums or structured types,
  not string comparisons.

## Git Remotes & Issues
- Upstream: `lfortran/lfortran` on GitHub (canonical repo and issues).
- Fork workflow: fork the upstream lfortran/lfortran repository to your own
  username, then push PRs as branches into your fork and send a PR from there.
  Never push branches to upstream.
- GitHub access: the `gh` commands in this file and the skills name the
  required GitHub operation. Use `gh` where it works. In a cloud session, or
  whenever the session instructions recommend GitHub MCP tools over `gh`, use
  those tools instead; push with plain `git push` to the fork either way. If
  no available tool can perform a required operation, such as filing a bug,
  stop and report it to the user rather than continuing silently.

## Coding Style & Naming Conventions
- C/C++: C++17; follow the existing formatting in the file to be consistent;
  use 4 spaces for indentation
- Names: lower_snake_case files; concise CMake target names.
- No commented-out code.
- No new C/C++ macros. Use constexpr, templates, or inline functions.
- Error messages: lowercase, show explicit kinds (e.g., integer(4) vs integer(8)),
  never expose internal ASR node names to users.

## Testing Guidelines
- Full coverage required: every behavior change must come with tests that fail
  before your change and pass after. Before pushing a compiler change, run
  the unit, integration (default mode), and reference suites locally once.
  Quick checks covers `--fast`, `--std=f23`, other backends, and platforms,
  and is the merge gate.

### CI policy

- `Quick checks` is the normal PR gate and runs the same builds, tests and
  selections on PRs, main, release tags and manual runs. It runs full Linux
  LLVM/reference coverage and representative checks on every platform, plus
  shared compiler compatibility jobs. Keep Metal, CUDA-on-CPU and Caffeine-backed coarray
  capability checks in Quick. Exhaustive never runs on PRs and is not
  required before review or merge.
  Caffeine's own LFortran unit tests and all coarray capability tests always run.
  Only Linux GFortran/OpenCoarrays reference validation is source-change-aware:
  use the same input comparison on every event, validate conservatively when
  inputs cannot be determined, and retain full reference validation in Exhaustive.
  Data, support and unknown-path changes request reference validation by default;
  only explicit compiler-source and simple standalone-test cases may skip it.
  Do not infer arbitrary runtime file dependencies from a Fortran keyword list.
- Quick's LLVM 11 Debug compiler owns the full normal/fast and Fortran 2023
  suites; LLVM 21 Debug owns full separate-compilation and leak-detection suites.
  Every full Quick suite runs with assertions and per-pass ASR verification.
  The LLVM 11 compiler is a Debug build compiled with `-O2` (assertions and
  verification stay on, no `NDEBUG`), which makes its four full passes ~1.6x
  faster. The Linux LLVM 21 platform compiler is compiled with `-O2` the same
  way (in `.github/actions/build-platform`, also for Exhaustive's copy); the
  macOS, Windows and Linux LLVM 11 platform builds keep their defaults.
  Both full-suite compilers also use the platform C/C++ diagnostic/hardening
  flags (including `-Werror`) and `WITH_INTERNAL_ALLOC_CHECK=yes`.
  These modes are not just smoke selections.
  The native phase of `Build LFortran to WASM` is Quick's GCC Release
  (`NDEBUG`) build and also uses `-Werror`, matching Exhaustive's
  `Check Release build`.
  Exhaustive checks add missing configurations without replaying Quick.
  LLVM-WASM, no-LLVM and MLIR belong only to Quick, including on main.
  The LLVM 19 WASM compiler is also a Debug build compiled with `-O2`; its
  WASI and Emscripten suites run in two parallel jobs.
  Full Linux LLVM 11/21 Debug platform suites and macOS LLVM 11 normal/reference
  coverage belong to supplemental Exhaustive jobs, preserving the original
  main coverage without making Quick slower on main.
- Main runs Quick plus Exhaustive. Exhaustive is identical on main and on
  manual dispatch, including the third-party application catalog; only
  publishing and deployment are push-only.
- Third-party applications are **bug generators for integration tests**, not
  part of ordinary PR checks. They run on the latest `main` and in every
  requested Exhaustive run, including applications such as FIATS.
- A compiler failure found by an application must become a reduced, registered
  integration regression. Fix it promptly or revert the offending change,
  and verify the original application failure as well as the regression.
  Do not add whole applications to Quick or waive their failures.
- Run Exhaustive for a PR only when explicitly requested, by dispatching it on
  the PR branch in a fork (`gh workflow run Exhaustive-Checks-CI.yml --repo
  <fork-owner>/lfortran --ref <branch>`; see `doc/src/installation.md`). Never
  do it automatically based on files or compiler subsystems touched. Verify the
  tested SHA and result, and link the run from the PR.
- Quick and Exhaustive (full compiler matrix and application validation) on
  `main` are each coalesced: at most one run is in progress and one is
  pending. A running main run is never cancelled; a newer push replaces the
  pending run, so the latest `main` is always tested but intermediate commits
  may be skipped. To test a skipped commit, re-run its cancelled run
  (`gh run rerun <run-id>`); `workflow_dispatch` accepts only a branch or tag. Release-tag workflows keep compiler and packaging checks without
  repeating the application catalog.
- Release only a main commit whose own Quick and Exhaustive runs, including
  applications, are green (re-run them if they were skipped). Quick or extended
  PR checks alone do not qualify a release.
- Every CI job sets `timeout-minutes` (about twice its slowest normal run) so a
  hang fails fast instead of holding a runner for 6 hours.
- Compiler caches are saved only on `main` and restored everywhere; keep
  `save: ${{ github.ref == 'refs/heads/main' }}` on every cache step. Save
  each cache from one job only: Exhaustive restores Quick's platform caches
  but does not save them (`save-cache: 'false'`).
- `integration_tests/run_tests.py --smoke` selects the maintained feature set in
  `integration_tests/smoke_tests.cmake` before compilation. This is for secondary
  CI configurations, not a replacement for full local regression testing.
- The `main` ruleset requires all twelve Quick jobs directly (listed in
  `doc/src/installation.md`); there is no aggregate status job. Keep their
  job names stable, and update the ruleset in the same rollout when a
  required Quick job is renamed or added. Do not gate required jobs on
  repository variables: `vars` is not passed to PRs from forks.

See [CI coverage and policy](doc/src/installation.md#ci-coverage) for commands
and the distinction between capability tests and application validation.

### Test Placement Decision Tree
- If the test compiles and runs end-to-end → integration test (preferred).
- If the test checks a compile-time error → `tests/errors/continue_compilation_1.f90`
  (append at end to minimize diff).
- If the test cannot compile end-to-end yet → reference test in `tests/tests.toml`
  (promote to integration test once it compiles).
- Every new test file MUST be registered in `CMakeLists.txt` or `tests.toml`.
  An unregistered test is dead code.

### Integration Tests (`integration_tests/`)
- Purpose: build-and-run end-to-end programs across backends/configurations via
  CMake/CTest.
- Add a `.f90` program under `integration_tests/` and register it in
  `integration_tests/CMakeLists.txt` using the `RUN(...)` macro (labels like
  `gfortran`, `llvm`, `cpp`, etc.).
  - See `integration_tests/CMakeLists.txt` (search for `macro(RUN` and existing `RUN(NAME ...)` entries).
  - Avoid custom generation; place real sources in the tree and check them in.
  - Search for similar tests and use similar name convention (e.g., `intrinsic_name_NN.f90`, `derived_type_feature_NN.f90`)
- Prefer integration tests; all new tests should be integration tests.
- Ensure the new test passes locally, then run the full suite once:
  `pixi run -e llvm11 integration_tests -j16 > integration.log 2>&1`.
- Add checks for correct results inside the `.f90` file using `if (i /= 4) error stop`-style idioms.
- Always label new tests with at least `gfortran` (to ensure the code compiles with GFortran and does not rely on any LFortran-specific behavior) and `llvm` (to test with LFortran's default LLVM backend).
- When fixing a bug, add an integration test that reproduces the failure and now compiles/runs successfully.
- Use the Pixi integration task for additional CI modes and backends. Run
  from the assigned worktree's root; each command writes its own log:

  ```bash
  pixi run -e llvm11 integration_tests -b llvm -f -j8 > integration-fast.log 2>&1
  pixi run -e llvm11 integration_tests -b gfortran -j8 > gfortran.log 2>&1
  pixi run -e llvm11 integration_tests -b c cpp -j8 > c-cpp.log 2>&1
  pixi run -e llvm11 integration_tests -b llvm_omp -j8 > openmp.log 2>&1
  ```

  Other registered backends and modes use the same task with the corresponding
  runner arguments. If a backend needs additional dependencies, declare them
  in a Pixi environment; do not install them globally. Read CI scripts to
  identify coverage, then reproduce it through the Pixi tasks.
- For missing debug-info tools, inspect the selected Pixi environment and
  resolve its dependencies in `pixi.toml`, then rebuild using the Pixi task.

### Unit/Reference Tests (`tests/`)
- Use only when an integration test is not yet feasible (e.g., feature doesn’t compile end‑to‑end). Prefer integration tests for all new work.
- If possible, still add a test under `integration_tests/`, but only register `gfortran` (not `llvm`), then register this test in `tests/tests.toml` with the needed outputs (`ast`, `asr`, `llvm`, `run`, etc.). Use `.f90` or `.f` (fixed-form auto-handled). Only if that cannot be done, add a new test into `tests/`.
  - See `tests/tests.toml` for examples; reference outputs live under `tests/reference/`.
- Multi-file modules: set `extrafiles = "mod1.f90,mod2.f90"`.
- Run locally: `pixi run -e llvm11 tests -j16 > reference.log 2>&1` (use `-s` to debug).
- Update references only when outputs intentionally change, with LLVM 11:
  `pixi run -e llvm11 tests -t path/to/test -u -s > reference-update.log 2>&1`.
- Error messages: add to `tests/errors/continue_compilation_1.f90` and update references.
- If your integration test does not compile yet, temporarily validate the change by adding a reference test that checks AST/ASR construction (enable `asr = true` and/or `ast = true` in `tests/tests.toml`). Promote it to an integration test once end‑to‑end compilation succeeds.

### Local Troubleshooting
- Modfile version mismatch: if you see "Incompatible format: LFortran Modfile...",
  clean and recompile the selected configuration
  (`pixi run -e llvm11 clean && pixi run -e llvm11 build`).
  Clean only the assigned build directory and ensure its `src/bin` directory
  is first on `PATH` when running tests.

### Common Commands
- Run unit/reference/integration suites: `pixi run -e llvm11 ctest`, `pixi run -e llvm11 tests`,
  and `pixi run -e llvm11 integration_tests`, each redirected to its own log.
- Run a specific reference test: `pixi run -e llvm11 tests -t pattern -s > reference.log 2>&1`.
- Run a specific integration test:
  `pixi run -e llvm11 integration_tests -t pattern > integration.log 2>&1`.

## References
- Developer docs: `doc/src/installation.md` (Tests) and `doc/src/progress.md` (workflow).
- Online docs: https://docs.lfortran.org/en/installation/ (Tests: run, update, integration).
- CI examples: `.github/workflows/Quick-Checks-CI.yml` and `ci/test.sh`.

## Commit & Pull Request Guidelines
- Commits: small, single-topic, imperative (e.g., "fix: handle BOZ constants").
- Do not add `Co-authored-by:` or `Co-author:` lines to any agent-created
  commit, including bug fixes, CI follow-ups, and merges. Do not substitute
  `Generated-By:`, `Assisted-By:`, other AI-credit trailers, or free-form AI
  attribution. Preserve the user's configured human Git identity; never
  invent an identity or set an AI as author or committer.
  `.claude/settings.json` also disables Claude Code's automatic commit/PR
  attribution and session-link trailers; retain that configuration.
- **Why:** AI tools may help write code, but every commit and PR must be
  submitted by a human who has read, understood, and guarantees the change.
  An AI co-author stamp misrepresents that responsibility; automated review
  is not human sign-off. `check_ai_commit_authorship.py`, run by Quick Checks
  CI, enforces this by inspecting author, committer, and commit messages across
  the PR history. It rejects AI identities/attribution (not legitimate human
  co-authorship); omitting co-author lines entirely is the agent workflow rule.
  Inspect all new commits before pushing, not just the tip. The script's local
  comparison range is `origin/main..HEAD`; ensure that reflects the intended
  upstream base before relying on its result, without repointing user remotes.
- One bug = one MRE = one PR. Do not bundle unrelated fixes.
  - Exception: one issue may expose several bugs, or several selected issues
    may share a root cause or require dependent fixes. Use one comprehensive PR
    for that connected group, with one MRE, focused commit, and integration
    test per distinct bug. Verify and reference every issue it fixes.
- Never mix refactoring or formatting with bug fixes. Send those separately.
- Every fix PR must demonstrate: test fails on main, test passes on branch.
  If you cannot find such a test, the fix is not understood well enough.
- Keep PR history linear: never merge `main` into a PR branch. When a PR must
  be updated (base conflicts, or it needs a change that landed on `main`),
  rebase it onto `upstream/main` and push with `git push --force-with-lease`.
  Do not update a PR only to keep it current; every push reruns CI, and
  Quick on `main` catches integration breakage after merging. Address review feedback
  with new commits rather than rewriting commits reviewers have seen.
- PRs target `upstream/main`; reference issues (`fixes #123`), explain rationale.
- Include test evidence (commands + summary); ensure CI passes.
- Do not commit generated artifacts, large binaries, or local configs.
- Use Draft PRs while iterating; click “Ready for review” only when satisfied.
- Use plain Markdown in PR descriptions (no escaped `\n`). Keep it clean, minimal, and follow simple headings (Summary, Scope, Verification, Rationale).
- Before marking ready: ensure Quick checks is green and include local test evidence.
