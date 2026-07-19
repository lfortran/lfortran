---
name: fix-mre
description: >
  Fix an LFortran bug given an MRE (Minimal Reproducible Example) produced by
  the create-mre skill. Reproduces the bug, diagnoses which compiler phase is
  at fault, fixes it in the LFortran source, adds an integration test, and
  runs the integration and reference test suites. Triggers: fix MRE, fix bug,
  fix lfortran, fix reproducer, implement, resolve, patch, bugfix.
---

# Fix MRE — Fix an LFortran Bug from a Minimal Reproducible Example

Given an MRE (produced by the `create-mre` skill or written by hand), reproduce
the LFortran bug, fix it in the compiler source, add an integration test, and
verify the full suites still pass.

## Prerequisites

- The LFortran repository is the current working directory.
- A configured build directory. `AGENTS.md` documents the standard dev config:
  ```bash
  cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=ON \
      -DWITH_STACKTRACE=yes
  ```
  `./build1.sh` does this for you.
- `build/src/bin` first on `PATH`, so `lfortran` resolves to the in-tree build
  you are about to modify. Verify with `which lfortran` before starting —
  testing against a stale system `lfortran` wastes an entire debug cycle.
- A **reference Fortran compiler** on `PATH` (`gfortran` preferred; it is what
  the `gfortran` integration-test label uses).

## Inputs to Gather

Ask the user for (if not already provided):

1. **The MRE** — the `.f90` file(s) and `run.sh` that reproduce the bug. By the
   `create-mre` convention these are in the repository root.

## Procedure

### Phase 0: Read Project Guidelines

Read `AGENTS.md` in the LFortran repository root to understand project
conventions, coding style, testing practices, and contribution guidelines.
Note especially the architecture rules — they determine where a fix belongs:

- Type coercion and casting belong in AST→ASR (semantics). The LLVM/codegen
  backend must never infer or fix types; it only lowers what ASR gives it.
  **If codegen appears to need a type workaround, the bug is upstream** — fix
  it in semantics instead.
- `libasr` is frontend-independent. Never reference `_lfortran` or other
  frontend-specific names there.
- No new C/C++ macros; use `constexpr`, templates, or inline functions.

Follow all instructions therein, but the instructions in this SKILL.md file
take precedence where they conflict.

### Phase 1: Reproduce the Bug

1. Read `run.sh` to understand the bug.
2. Run it to confirm the failure:
   ```bash
   bash run.sh
   ```
3. Note the **exact error message**, **error type** (compilation error, runtime
   crash, wrong output), and the **Fortran construct** involved.

Do not proceed until you have reproduced the failure locally. A fix for a bug
you have not observed is a guess.

### Phase 2: Diagnose the Root Cause

1. Analyze the error message to determine which compiler phase is failing:
   - **Parser** (`src/lfortran/parser/`): syntax errors, tokenizer failures
   - **Semantics** (`src/lfortran/semantics/`): type errors, symbol resolution
   - **ASR passes** (`src/libasr/pass/`): transformation errors
   - **Code generation** (`src/libasr/codegen/`): LLVM IR generation failures
2. Search the LFortran source for the error message text or error label to find
   the code that produces it:
   ```bash
   grep -rn "error text" src/
   ```
3. Inspect the intermediate representations to see where the tree first goes
   wrong — this is usually faster than reading code:
   ```bash
   lfortran --show-ast <mre_file>.f90
   lfortran --show-asr <mre_file>.f90
   ```
4. Understand the code path that leads to the failure. Read surrounding code to
   understand the intended behavior.
5. Identify the minimal fix needed. Fix the root cause, not the symptom.

### Phase 3: Implement the Fix

1. Make the code change in the LFortran source. Keep changes minimal and
   focused on the bug. Match the formatting of the file you are editing;
   do not reformat surrounding code.
2. Rebuild:
   ```bash
   cmake --build build -j
   ```
3. Re-run the MRE to verify the fix:
   ```bash
   bash run.sh
   ```
4. The MRE should now succeed (compile and/or run correctly) with `lfortran`.

If the fix doesn't work, iterate: re-diagnose, adjust, rebuild, and re-test.

### Phase 4: Add an Integration Test

The integration test — not the MRE — is the deliverable of this skill. The MRE
files stay untracked; the test is what gets committed.

1. Look at existing integration tests in `integration_tests/` to find similar
   tests (same Fortran construct, similar naming pattern).
2. Create a new `.f90` file in `integration_tests/` following the naming
   convention (e.g. `intrinsic_name_NN.f90`, `derived_type_feature_NN.f90`).
   Pick the next available number.
3. The test should be based on the MRE but written as a proper integration test:
   - Include runtime checks using `if (result /= expected) error stop` idioms.
   - Keep it minimal but cover the bug scenario.
4. Register the test in `integration_tests/CMakeLists.txt`:
   - Find the appropriate section (search for `macro(RUN` and existing
     `RUN(NAME ...)` entries).
   - Add a `RUN(NAME <test_name> LABELS gfortran llvm)` style entry.
   - Use at least the labels `gfortran` and `llvm`. An unregistered test is
     dead code.
5. Verify the test compiles with the reference compiler — this confirms the
   test is valid Fortran and does not depend on LFortran-specific behavior:
   ```bash
   gfortran -o /tmp/test_ref integration_tests/<test_name>.f90 && /tmp/test_ref
   rm -f /tmp/test_ref
   ```
6. Verify the test compiles and runs with `lfortran`:
   ```bash
   cd integration_tests
   ./run_tests.py -t <test_name>
   ```

**Confirm the test actually captures the bug.** `AGENTS.md` requires that every
fix PR demonstrate the test fails on `main` and passes on the branch. Verify
this explicitly — `git stash` your source change, rebuild, confirm the new test
fails, then restore and rebuild. If the test passes without your fix, it does
not cover the bug and the fix is not understood well enough.

### Phase 5: Run Integration Tests

Run the full integration test suite:

```bash
cd integration_tests
./run_tests.py -j16 &> log
tail -n30 log
```

**Always redirect test output to a log file and then examine it.** Do not pipe
to `tail` directly — if you need more output than `tail` shows you have to
rerun the whole suite, which takes several minutes.

- If all tests pass, proceed to Phase 6.
- If any test fails:
  1. Examine the log to identify which test failed and why.
  2. Determine if the failure is caused by your change (a regression) or a
     pre-existing issue.
  3. If your change caused it, fix the regression, rebuild, and re-run tests.
  4. Repeat until all integration tests pass.

### Phase 6: Run Reference Tests

Return to the LFortran root and run reference tests:

```bash
cd <lfortran-root>
./run_tests.py &> log
```

- If reference tests pass, proceed to Phase 7.
- If reference tests fail (expected when your fix changes compiler output):
  1. Update reference results:
     ```bash
     ./run_tests.py -u
     ```
  2. Review the changes with `git diff` to ensure all reference updates are
     correct and expected — they should all be consequences of your bug fix,
     not regressions.
  3. If any reference change looks wrong, investigate and fix before proceeding.

Never run `-u` blindly. An unreviewed reference update can silently bake a
regression into the expected output.

### Phase 7: Report

Summarize the work and let the user decide whether to commit. Do not commit or
push without being asked — `AGENTS.md` requires PRs to go to a fork, never
upstream, and the user may want to review the diff first.

Before staging anything, confirm the MRE scratch files (`mre_*.f90`,
`re_*.f90`, `run.sh`, `run_re.sh`) are not included — they are ignored by
`.gitignore` and are not part of the fix.

Print a summary:

```
Bug fixed!

Fix: <one-line description of what was changed>
Files modified:
  <list of changed source files>
Integration test: integration_tests/<test_name>.f90
  fails on main, passes on this branch — verified

Integration tests: pass
Reference tests:   pass  (<N> reference outputs updated, reviewed)

Not committed. Review the diff, then commit when ready.
```

If the user asks you to commit, follow the `AGENTS.md` commit conventions:
small single-topic commits, imperative mood, one bug = one MRE = one PR, and
never mix refactoring or formatting with a bug fix.

## Tips

- **Multiple errors**: The MRE may expose more than one bug. First fix the bug
  that the MRE demonstrates. If you discover additional issues that block
  compiling and running the newly added test, fix them as well — but keep
  unrelated fixes to separate PRs.
- **ASR passes**: Many bugs live in ASR passes (`src/libasr/pass/`). These
  transform the ASR tree and are a common source of codegen failures.
- **Semantic errors**: If the bug is "not yet implemented", the fix likely
  involves adding a new case in the semantics or codegen visitor.
- **Error message style**: Per `AGENTS.md`, messages are lowercase, show
  explicit kinds (e.g. `integer(4)` vs `integer(8)`), and never expose
  internal ASR node names to users.
- **Modfile issues**: If you see "Incompatible format: LFortran Modfile...",
  the module files are stale — rebuild from scratch (`ninja -C build clean &&
  cmake --build build -j`) and ensure `build/src/bin` is first on `PATH`.
- **Rebuild quickly**: `cmake --build build -j` only recompiles changed files.
  Use it frequently during iteration.
- **Test naming**: Look at nearby tests in `CMakeLists.txt` for naming
  conventions. Usually it's `<feature>_<number>`.
