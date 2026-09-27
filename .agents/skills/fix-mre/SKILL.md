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

- The **assigned LFortran worktree** is the current working directory. Do not
  switch branches or modify another worker's checkout.
- A configured build. **Default to `pixi run -e llvm11 build`**, which installs
  dependencies and builds `build/llvm11/src/bin/lfortran`. Reuse the exact
  environment/build supplied by `fix-issue` or the user, including supported
  manual builds. `AGENTS.md` owns setup details; do not duplicate dependency
  lists or CMake configuration recipes here.
- Put the assigned build's absolute `src/bin` directory first on `PATH` inside
  the selected environment. Verify `command -v lfortran`, `lfortran --version`,
  and `llvm-config --version` there; reference updates require LLVM 11.
- A **reference Fortran compiler** on `PATH` (`gfortran` preferred; it is what
  the `gfortran` integration-test label uses).

Run commands in that worktree and selected environment; parent shell activation
does not carry into fresh tools or subagents. The examples use the default
`llvm11` tasks and allocated `<jobs>`. For an explicitly supplied non-Pixi
build, use the equivalent commands in `AGENTS.md`, selecting the compiler and
test-output directories explicitly.

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
   pixi run -e llvm11 bash run.sh
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
   pixi run -e llvm11 start --show-ast <mre_file>.f90
   pixi run -e llvm11 start --show-asr <mre_file>.f90
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
   CMAKE_BUILD_PARALLEL_LEVEL=<jobs> pixi run -e llvm11 build > build.log 2>&1
   ```
3. Re-run the MRE to verify the fix:
   ```bash
   pixi run -e llvm11 bash run.sh
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
   pixi run -e llvm11 gfortran -o test_ref integration_tests/<test_name>.f90 && ./test_ref
   rm -f test_ref
   ```
   Run this in the assigned worktree; a shared `/tmp/test_ref` races with other
   issue workers.
6. Verify the test compiles and runs with `lfortran`:
   ```bash
   pixi run -e llvm11 integration_tests -t <test_name> -j<jobs> > targeted.log 2>&1
   ```

**Confirm the test actually captures the bug.** Record the pre-fix SHA and use
an isolated baseline worktree/build to verify the new test fails without the
fix and passes with it. Copy only the new test and its registration into that
baseline, not the fix. For the first independent fix the baseline is upstream
`main`; for a later dependent commit it is the branch immediately before that
commit. Coordinate worktree creation with the orchestrator. Do not stash, reset,
or switch the caller's or another worker's checkout: worktrees share Git refs
and the stash stack. If the test passes without the fix, it does not cover the
bug and the fix is not understood well enough.

### Phase 5: Run Unit and Integration Tests

Run the unit tests from the assigned build and the full integration test suite,
using the worker's resource budget:

```bash
pixi run -e llvm11 ctest -j<jobs> > unit.log 2>&1
pixi run -e llvm11 integration_tests -j<jobs> > integration.log 2>&1
tail -n30 integration.log
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
  4. Repeat until the changed code introduces no failures. Record pre-existing
     compiler bugs with their reproducer, baseline SHA, command, and output.
     Return them to the `fix-issue` orchestrator for batch-wide duplicate
     checking and issue filing; do not silently skip them or fix them here.

### Phase 6: Run Reference Tests

Return to the assigned worktree root and run reference tests with its LLVM 11
build. Do not regenerate LLVM-version differences using a newer toolchain:

```bash
cd <lfortran-root>
pixi run -e llvm11 tests -j<jobs> > reference.log 2>&1
```

- If reference tests pass, proceed to Phase 7.
- If reference tests fail:
  1. Distinguish intended output changes from regressions or environment
     mismatches. For intended changes only, update the affected tests:
     ```bash
     pixi run -e llvm11 tests -t <affected-test> -u -s > reference-update.log 2>&1
     ```
  2. Review the changes with `git diff` to ensure all reference updates are
     correct and expected — they should all be consequences of your bug fix,
     not regressions.
  3. If any reference change looks wrong, investigate and fix before proceeding.
  4. Rerun the reference suite without `-u` and inspect the saved log.

Never run `-u` blindly. An unreviewed reference update can silently bake a
regression into the expected output.

### Phase 7: Report

Summarize the work and, by default, let the user decide whether to commit.
An explicit user request or a `fix-issue` handoff can authorize a commit; report
its SHA instead of saying "Not committed" in that case. Push only if separately
authorized, and only to the user's fork.

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
  fails before this fix, passes on this branch — verified

Unit tests:        pass
Integration tests: pass
Reference tests:   pass  (<N> reference outputs updated, reviewed)
Additional bugs:  <evidence paths for the orchestrator, or none>

Not committed. Review the diff, then commit when ready.
```

When authorized to commit, follow `AGENTS.md`: one focused bug fix with its
integration test, imperative mood, and no unrelated refactoring or formatting.
Dependent fixes may share a PR under `fix-issue`, but retain separate commits.

**Do not add `Co-authored-by:` or `Co-author:` lines to any commit.** Do not use
AI author/committer identities, `Generated-By:` / `Assisted-By:` trailers, or
equivalent AI-credit stamps. Preserve the user's configured human identity.
As explained in `check_ai_commit_authorship.py`, AI may assist with code, but
the submitting human must read, understand, and guarantee the change; AI is
not an accountable co-author. Quick Checks CI inspects the whole PR history,
including author/committer metadata and messages. Inspect the commit you create
before handing it back; never claim automated review supplies human sign-off.

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
  the module files are stale — clean only the assigned build with
  `pixi run -e llvm11 clean`, then `pixi run -e llvm11 build`, and confirm
  the selected compiler path. Never clean another worker's build.
- **Rebuild quickly**: reuse `pixi run -e llvm11 build` during iteration.
- **Test naming**: Look at nearby tests in `CMakeLists.txt` for naming
  conventions. Usually it's `<feature>_<number>`.
