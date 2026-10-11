---
name: fix-mre
description: >
  Fix an LFortran bug given an MRE (Minimal Reproducible Example) produced by
  the create-mre skill. Reproduces the bug, diagnoses which compiler phase is
  at fault, fixes it in the LFortran source, adds an integration test, and
  verifies it with a short local gate, leaving the full suites to Quick CI.
  Triggers: fix MRE, fix bug, fix lfortran, fix reproducer, implement,
  resolve, patch, bugfix.
---

# Fix MRE — Fix an LFortran Bug from a Minimal Reproducible Example

Given an MRE (produced by the `create-mre` skill or written by hand), reproduce
the LFortran bug, fix it in the compiler source, add an integration test, and
verify it with the short local gate (Phase 5). Quick CI runs the full unit,
integration, and reference suites in all modes once the branch is pushed.

## Prerequisites

- The **assigned LFortran worktree** is the current working directory. Do not
  switch branches or modify another worker's checkout.
- A configured build. For a new build, **use
  `pixi run -e llvm11 build '-DCMAKE_CXX_FLAGS_DEBUG=-g -O2' '-DCMAKE_C_FLAGS_DEBUG=-g -O2'`**,
  which installs dependencies and builds `build/llvm11/src/bin/lfortran` like
  Quick's LLVM 11 compiler (Debug with assertions, compiled with `-O2`); the
  flags persist in the CMake cache for later `pixi run -e llvm11 build`
  rebuilds. Reuse the exact Pixi environment/build supplied by `fix-issue` or
  the user.
  `AGENTS.md` owns setup details; do not duplicate dependency
  lists or CMake configuration recipes here.
- Put the assigned build's absolute `src/bin` directory first on `PATH` inside
  the selected environment. Verify `command -v lfortran`, `lfortran --version`,
  and `llvm-config --version` there; reference updates require LLVM 11.
- Pixi's **reference Fortran compiler** on `PATH` (`gfortran`; it is what
  the `gfortran` integration-test label uses).

Run commands in that worktree and selected environment; parent shell activation
does not carry into fresh tools or subagents. The examples use the default
`llvm11` tasks and allocated `<jobs>`. Follow
[AGENTS.md](../../../AGENTS.md#required-agent-toolchain-pixi): use only Pixi
for dependencies, builds, compilers, and tests. Reference tests and updates
always use `llvm11`, even when the fix also needs another Pixi configuration.

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

Follow all instructions therein, including the required Pixi toolchain policy.

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
2. Before the first rebuild with the fix, write the integration test (Phase 4)
   and capture its fail-before log with the still-unfixed build (see
   **Confirm the test actually captures the bug**). Then rebuild:
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
5. Verify it with the local gate (Phase 5). Its GFortran run confirms the
   test is valid Fortran that does not depend on LFortran-specific behavior.

**Confirm the test actually captures the bug.** Run the new test through the
gate with a build that predates the fix and save the failing log, for example
`.fix-issue/<id>/fail_before_<test_name>.log`. Write the test before the first
rebuild with the fix (Phase 3), so the existing build supplies this evidence:
the worker's first build, made at the base SHA, for the first fix, or the
build before this commit's change for a later dependent commit. Only if that
evidence was not captured, use an isolated baseline worktree/build with the
new test and its registration copied in, never the fix. Coordinate worktree
creation with the orchestrator. Do not stash, reset, or switch the caller's or
another worker's checkout: worktrees share Git refs and the stash stack. If the
test passes without the fix, it does not cover the bug and the fix is not
understood well enough.

### Phase 5: Local Gate

The local gate takes minutes, not a full suite. Quick CI runs the full unit,
integration (all modes and backends), and reference suites after the branch is
pushed, so do not run `pixi run -e llvm11 ctest`, `tests`, or
`integration_tests` over the whole suite here. Do not use
`integration_tests -t` either: each call reconfigures the whole CMake suite.

1. Rerun the MRE (`pixi run -e llvm11 bash run.sh`) and any original
   reproducers supplied with it (`run_re.sh`).
2. Run each new or changed integration test directly with the built
   `lfortran`, mirroring its `RUN(...)` entry in
   `integration_tests/CMakeLists.txt`: in normal mode, with `--fast` (unless
   `NO_FAST`; add `--no-fast-math` for `NO_FAST_MATH`), and with `--std=f23`
   (unless `NO_STD_F23`), then with Pixi's `gfortran` (plus its
   `GFORTRAN_ARGS`). Pass its `EXTRA_ARGS` and `-I` for `INCLUDE_PATH`;
   compile its `EXTRAFILES` first, in order, with `-c` and link their objects;
   copy `COPY_TO_BIN` files into the scratch directory; a `FAIL` test must
   exit nonzero. Work in an ignored scratch directory, for example:
   ```bash
   mkdir -p .fix-issue/<id>/gate
   pixi run -e llvm11 bash -c 'set -ex; t=$PWD/integration_tests/<test_name>.f90
       cd .fix-issue/<id>/gate
       for m in "" --fast --std=f23; do lfortran $m "$t" -o a.out; ./a.out; done
       gfortran "$t" -o a.out; ./a.out' > .fix-issue/<id>/gate.log 2>&1
   ```
   Other backends in its labels are left to CI unless the fix targets them.
3. When the deliverable is a reference test (for example a diagnostic appended
   to `tests/errors/continue_compilation_1.f90`), create or update only that
   test's references with LLVM 11 and review the diff:
   ```bash
   pixi run -e llvm11 tests -t <test> -u -s > reference-update.log 2>&1
   ```
   Never run `-u` blindly; an unreviewed update can bake a regression into the
   expected output.

**Always redirect test output to a log file and then examine it.** Do not pipe
to `tail` directly; you may need more output than `tail` shows.

If a gate check fails, determine whether your change caused it. Fix
regressions, rebuild, and rerun the gate. Record pre-existing compiler bugs
with their reproducer, baseline SHA, command, and output. Return them to the
`fix-issue` orchestrator for batch-wide duplicate checking and issue filing;
do not silently skip them or fix them here.

### Phase 6: Report

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

Local gate:        MRE, reproducers, test (normal/--fast/--std=f23/gfortran) pass
Full suites:       left to Quick CI
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
