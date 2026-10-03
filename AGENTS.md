# Repository Guidelines

This file is for LLM agents and new contributors to have a single point of
detailed reference how to contribute to the project.

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
| `classify-issue` | Triage issues with evidence-based, additive labels and optional frequency-first prioritization |
| `repro-issue` | Turn a GitHub issue into a faithful Reproducible Example (RE) |
| `create-mre` | Reduce an RE or third-party failure to a Minimal Reproducible Example (MRE) |
| `fix-mre` | Fix the compiler bug behind an MRE and add an integration test |
| `pr-review` | Review LFortran PRs with architecture, correctness, and maintainer guidance |
| `fix-issue` | Fix one issue or a filtered batch in isolated worktrees and subagents, publish dependency-aware PRs from a fork, and iterate on review and CI |

`classify-issue` distinguishes invalid-code diagnostics from valid-code bugs,
enhancements, new features, and maintenance or internal-correctness work. It
uses the live label catalog, preserves existing labels, and asks about
uncertain cases. GitHub changes require a labeling request; recommendation
and local-priority requests stay read-only.

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

A request such as "Fix all these issues using the fix-issue skill: <GitHub
search URL>" is sufficient. Do not require the user to repeat these defaults:

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
- Publish drafts from the user's fork, then iterate on `pr-review`, human
  feedback, and CI, including exhaustive CI, before marking ready. Follow the
  commit-authorship policy below in every subagent, including CI fixers.
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
`build/<pixi-environment>/src/bin/lfortran` by default. Pixi activation selects
that directory on `PATH`. Existing manual builds may instead use
`src/bin/lfortran` or `build/src/bin/lfortran`; honor an explicitly supplied
build rather than silently substituting one. Use `gfortran`, matching the
integration-test label, for differential testing.
Issue classification requires authenticated `gh`, not a compiler build.

## Prerequisites
- **Recommended:** Git and [Pixi](https://pixi.sh/). The repository manifest
  supplies build/test dependencies and Linux C/C++ compilers. On macOS, install
  Xcode Command Line Tools; on Windows, use an initialized MSVC developer shell
  with Git Bash available. The existing WSL and manual installation paths remain
  supported.
- For manual builds, the dependencies include:
- Tools: CMake (>=3.10), Ninja, Git, Python (>=3.8), GCC/Clang/MSVC.
- Generators: re2c, bison (needed for build0/codegen).
- Libraries: zlib; optional: LLVM dev, libunwind, RapidJSON, fmt, xeus/xeus-zmq, Pandoc.

## Build, Test, and Development Commands

**Start with Pixi** for installation, building, and testing. Keep dependency
versions and build details in `pixi.toml` and the scripts it invokes, rather
than duplicating setup recipes in prompts or skills. These entry points stay
the same when the build system or dependencies change:

```bash
pixi run build
pixi run start --version
pixi run ctest -j8 > unit.log 2>&1
pixi run tests -j8 > reference.log 2>&1
pixi run integration_tests -j8 > integration.log 2>&1
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
Reference-test scratch files live under `build/<environment>/reference-tests`;
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
pixi run -e llvm11 build > .fix-issue/<id>/build.log 2>&1
pixi run -e llvm11 llvm-config --version
pixi run -e llvm11 start --version
```

Use the same tasks for rebuilds and tests, and inspect their saved logs.
Budget `<jobs>` across active workers instead of giving each worker all cores:

```bash
pixi run -e llvm11 ctest -j<jobs> > .fix-issue/<id>/unit.log 2>&1
pixi run -e llvm11 tests -j<jobs> > .fix-issue/<id>/reference.log 2>&1
pixi run -e llvm11 integration_tests -j<jobs> > .fix-issue/<id>/integration.log 2>&1
pixi run -e llvm11 bash run.sh > .fix-issue/<id>/mre.log 2>&1
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

Honor an explicitly supplied environment or existing build for standalone work;
inspect its `CMakeCache.txt` and reuse its actual build directory rather than
creating a second one by assumption. Reference updates still require LLVM 11.
Do not copy CMake caches, generated sources, `.mod` files, or `.pixi` environments
between worktrees. Do not use `git clean -dfx` as automatic setup or recovery.

### Other supported build methods

Conda/micromamba, source tarballs, and direct CMake builds remain supported;
see `doc/src/installation.md`. For manual Git builds:

- `./build0.sh` generates sources. `./build1.sh` without arguments preserves
  the existing in-source workflow, with `CMakeCache.txt` at the root and
  `src/bin/lfortran` as the executable.
- `./build1.sh build/manual` configures an out-of-source Debug build. Additional
  arguments are passed to CMake, for example
  `./build1.sh build/release -DCMAKE_BUILD_TYPE=Release`.
- Direct CMake remains available:
  `cmake -S . -B build/manual -G Ninja -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=ON -DWITH_STACKTRACE=yes`,
  followed by `cmake --build build/manual -j`.
- Legacy test entry points remain available: `./run_tests.py` and
  `integration_tests/run_tests.py`. Both accept `--compiler-dir` for an explicit
  compiler; integration tests also accept `--build-dir` for isolated outputs.

**IMPORTANT**: always redirect test output to a log file and then examine the
log file. Do NOT run tests using the style like `./run_tests.py | tail` because
if you need more output than the `tail` provides, you have to rerun them and
that is very expensive, the tests can run several minutes. Instead, run tests
only once, redirect to a log file and then examine the log file.

## Quick Smoke Test
- Default build: `pixi run build` (LLVM enabled).
- AST/ASR: `pixi run start --show-ast examples/expr2.f90`
- Compile and run: `pixi run start examples/expr2.f90`.
- Keep an executable: `pixi run start examples/expr2.f90 -o expr2`.

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
  before your change and pass after. Do not merge without a full local pass of
  unit and integration suites.

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
- Ensure integration tests pass locally: `pixi run integration_tests -j16 > integration.log 2>&1`.
- Add checks for correct results inside the `.f90` file using `if (i /= 4) error stop`-style idioms.
- Always label new tests with at least `gfortran` (to ensure the code compiles with GFortran and does not rely on any LFortran-specific behavior) and `llvm` (to test with LFortran's default LLVM backend).
- When fixing a bug, add an integration test that reproduces the failure and now compiles/runs successfully.
- CI‑parity (recommended): run with the same env and scripts CI uses
  - Use micromamba with `ci/environment.yml` to match toolchain (LLVM, etc.).
  - Set env like CI and call the same helper scripts:
    - `export LFORTRAN_CMAKE_GENERATOR=Ninja`
    - `export ENABLE_RUNTIME_STACKTRACE=yes` (Linux/macOS)
    - Build: `bash ci/build.sh`
    - Quick integration run (LLVM):
      - `bash ci/test.sh` (runs a CMake+CTest LLVM pass and runner passes)
      - or: `cd integration_tests && ./run_tests.py -b llvm && ./run_tests.py -b llvm -f &> log`
  - GFortran pass: `cd integration_tests && ./run_tests.py -b gfortran &> log`
  - Other backends as in CI:
    - `./run_tests.py -b llvm2 llvm_rtlib llvm_nopragma &> log && ./run_tests.py -b llvm2 llvm_rtlib llvm_nopragma -f &> log`
    - `./run_tests.py -b cpp c c_nopragma &> log` and `-f`
    - `./run_tests.py -b wasm &> log` and `-f`
    - `./run_tests.py -b llvm_omp &> log` / `target_offload` / `fortran -j1`

- Minimal local (without micromamba):
  - Build: `cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=ON -DWITH_RUNTIME_STACKTRACE=yes`
  - Run: `cd integration_tests && ./run_tests.py -b llvm &> log && ./run_tests.py -b llvm -f &> log`
- If builds fail with messages about missing debug info:
  - Install LLVM tools so `llvm-dwarfdump` is available (e.g., `sudo pacman -S llvm`,
    `apt install llvm`, or `conda install -c conda-forge llvm-tools`).
  - Rebuild with runtime stacktraces if needed:
    `cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=ON -DWITH_RUNTIME_STACKTRACE=yes -DWITH_UNWIND=ON`
  - More details: `integration_tests/run_tests.py &> log` (CLI flags and supported backends).

### Unit/Reference Tests (`tests/`)
- Use only when an integration test is not yet feasible (e.g., feature doesn’t compile end‑to‑end). Prefer integration tests for all new work.
- If possible, still add a test under `integration_tests/`, but only register `gfortran` (not `llvm`), then register this test in `tests/tests.toml` with the needed outputs (`ast`, `asr`, `llvm`, `run`, etc.). Use `.f90` or `.f` (fixed-form auto-handled). Only if that cannot be done, add a new test into `tests/`.
  - See `tests/tests.toml` for examples; reference outputs live under `tests/reference/`.
- Multi-file modules: set `extrafiles = "mod1.f90,mod2.f90"`.
- Run locally: `pixi run tests -j16 > reference.log 2>&1` (use `-s` to debug).
- Update references only when outputs intentionally change, with LLVM 11:
  `pixi run tests -t path/to/test -u -s > reference-update.log 2>&1`.
- Error messages: add to `tests/errors/continue_compilation_1.f90` and update references.
- If your integration test does not compile yet, temporarily validate the change by adding a reference test that checks AST/ASR construction (enable `asr = true` and/or `ast = true` in `tests/tests.toml`). Promote it to an integration test once end‑to‑end compilation succeeds.

### Local Troubleshooting
- Modfile version mismatch: if you see "Incompatible format: LFortran Modfile...",
  clean and recompile the selected configuration
  (`pixi run -e llvm11 clean && pixi run -e llvm11 build`).
  Clean only the assigned build directory and ensure its `src/bin` directory
  is first on `PATH` when running tests.

### Common Commands
- Run unit/reference/integration suites: `pixi run ctest`, `pixi run tests`,
  and `pixi run integration_tests`, each redirected to its own log.
- Run a specific reference test: `pixi run tests -t pattern -s > reference.log 2>&1`.
- Run a specific integration test:
  `pixi run integration_tests -t pattern > integration.log 2>&1`.

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
- Once a PR is in review, merge upstream into it (do not rebase) —
  rebasing forces complete re-review.
- PRs target `upstream/main`; reference issues (`fixes #123`), explain rationale.
- Include test evidence (commands + summary); ensure CI passes.
- Do not commit generated artifacts, large binaries, or local configs.
- Use Draft PRs while iterating; click “Ready for review” only when satisfied.
- Use plain Markdown in PR descriptions (no escaped `\n`). Keep it clean, minimal, and follow simple headings (Summary, Scope, Verification, Rationale).
- Before marking ready: ensure all local tests pass (unit + integration) and include evidence.
