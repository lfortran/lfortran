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
| `fix-issue` | Orchestrate the whole loop for one issue in subagents: reproduce, reduce, fix, review locally, open a PR from a fork, and iterate until CI is green |

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

Reproducers are written to the repository root by convention (`run.sh`,
`mre_*.f90`, `re_*.f90`) and are gitignored — they are scratch inputs to
`fix-mre`. The committed deliverable is always the integration test.

`fix-issue` automates this loop for a single issue: its top-level agent
only orchestrates, and fresh subagents run `repro-issue`, then `create-mre`
and `fix-mre` repeatedly (one commit with its own integration test per bug)
until the original issue is fixed. It reviews the branch locally with
`pr-review` until it is clean, then opens a draft PR from the user's fork and
iterates on CI failures and review findings until the PR is ready for
review. Every push reruns the full Quick checks, so it batches pushes: CI
fixes are pushed as soon as they pass locally, while other changes wait
until the current CI run finishes rather than cancelling a healthy run.

The reproduction and fix skills assume `build/src/bin` is first on `PATH`
(so `lfortran` is the in-tree build) and that a reference compiler — `gfortran`,
matching the `gfortran` integration-test label — is available for differential
testing. Issue classification requires authenticated `gh`, not a compiler build.

## Prerequisites
- Tools: CMake (>=3.10), Ninja, Git, Python (>=3.8), GCC/Clang/MSVC.
- Generators: re2c, bison (needed for build0/codegen).
- Libraries: zlib; optional: LLVM dev, libunwind, RapidJSON, fmt, xeus/xeus-zmq, Pandoc.

## Build, Test, and Development Commands
- Typical dev config (Ninja + LLVM) is specified in `./build1.sh`:
  - `cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=ON -DWITH_STACKTRACE=yes`
  - `cmake --build build -j`
- Release build: `cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release -DWITH_LLVM=ON`
- Tests: `./run_tests.py &> log` (reference tests); `cd integration_tests && ./run_tests.py -j16 &> log` (integration tests)

**IMPORTANT**: always redirect test output to a log file and then examine the
log file. Do NOT run tests using the style like `./run_tests.py | tail` because
if you need more output than the `tail` provides, you have to rerun them and
that is very expensive, the tests can run several minutes. Instead, run tests
only once, redirect to a log file and then examine the log file.

## Quick Smoke Test
- We usually build with LLVM enabled (`-DWITH_LLVM=ON`).
- AST/ASR (no LLVM): `build/src/bin/lfortran --show-ast examples/expr2.f90`
- Run program (LLVM): `build/src/bin/lfortran examples/expr2.f90 && ./a.out`

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
  Both full-suite compilers also use the platform C/C++ diagnostic/hardening
  flags (including `-Werror`) and `WITH_INTERNAL_ALLOC_CHECK=yes`.
  These modes are not just smoke selections.
  Exhaustive checks add missing configurations without replaying Quick.
  LLVM-WASM, no-LLVM and MLIR belong only to Quick, including on main.
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
- Compiler caches are saved only on `main` and restored everywhere; keep
  `save: ${{ github.ref == 'refs/heads/main' }}` on every cache step.
- `integration_tests/run_tests.py --smoke` selects the maintained feature set in
  `integration_tests/smoke_tests.cmake` before compilation. This is for secondary
  CI configurations, not a replacement for full local regression testing.
- The `main` ruleset requires all eleven Quick jobs directly (listed in
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
- Ensure integration tests pass locally: `cd integration_tests && ./run_tests.py -j16 &> log`.
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
- Run locally: `./run_tests.py -j16 &> log` (use `-s` to debug).
- Update references only when outputs intentionally change: `./run_tests.py -t path/to/test -u -s`.
- Error messages: add to `tests/errors/continue_compilation_1.f90` and update references.
- If your integration test does not compile yet, temporarily validate the change by adding a reference test that checks AST/ASR construction (enable `asr = true` and/or `ast = true` in `tests/tests.toml`). Promote it to an integration test once end‑to‑end compilation succeeds.

### Local Troubleshooting
- Modfile version mismatch: if you see "Incompatible format: LFortran Modfile...",
  clean and recompile (`ninja clean && ninja`)
  Ensure the current `build/src/bin` is first on `PATH` when running tests.

### Common Commands
- Run all tests: `ctest` and `./run_tests.py -j16 &> log`
- Run a specific test: `./run_tests.py -t pattern -s &> log`

## References
- Developer docs: `doc/src/installation.md` (Tests) and `doc/src/progress.md` (workflow).
- Online docs: https://docs.lfortran.org/en/installation/ (Tests: run, update, integration).
- CI examples: `.github/workflows/Quick-Checks-CI.yml` and `ci/test.sh`.

## Commit & Pull Request Guidelines
- Commits: small, single-topic, imperative (e.g., "fix: handle BOZ constants").
- One bug = one MRE = one PR. Do not bundle unrelated fixes.
  - Exception: when fixing one issue requires several MREs (fixing one bug
    exposes the next failure in the same reported code), all of them may go
    in a single PR for that issue. Each bug still gets its own MRE, its own
    commit, and its own integration test. This is what the `fix-issue` skill does.
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
- Before marking ready: ensure all local tests pass (unit + integration) and include evidence.
