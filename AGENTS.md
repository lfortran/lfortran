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
      - or: `cd integration_tests && ./run_tests.py -b llvm && ./run_tests.py -b llvm -f -nf16 &> log`
  - GFortran pass: `cd integration_tests && ./run_tests.py -b gfortran &> log`
  - Other backends as in CI:
    - `./run_tests.py -b llvm2 llvm_rtlib llvm_nopragma &> log && ./run_tests.py -b llvm2 llvm_rtlib llvm_nopragma -f &> log`
    - `./run_tests.py -b cpp c c_nopragma &> log` and `-f`
    - `./run_tests.py -b wasm &> log` and `-f`
    - `./run_tests.py -b llvm_omp &> log` / `target_offload` / `fortran -j1`

- Minimal local (without micromamba):
  - Build: `cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=ON -DWITH_RUNTIME_STACKTRACE=yes`
  - Run: `cd integration_tests && ./run_tests.py -b llvm &> log && ./run_tests.py -b llvm -f -nf16 &> log`
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
