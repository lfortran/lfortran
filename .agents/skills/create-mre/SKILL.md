---
name: create-mre
description: >
  Create a Minimal Reproducible Example (MRE) from a third-party Fortran code
  failure in LFortran. Use this skill when third-party code fails to build or
  run with LFortran but works with other Fortran compilers (e.g. GFortran,
  Flang). Produces standalone .f90 file(s) and a run.sh script that
  reproduces the bug. Triggers: MRE, minimal reproducible example, reduce,
  reproduce, reproducer, third-party failure, compilation error, runtime
  failure, bug reduction.
---

# Create MRE — Minimal Reproducible Example

Reduce a third-party Fortran code failure into the smallest possible standalone
example that reproduces the LFortran bug. The MRE must compile and run correctly
with the reference compiler but fail with LFortran, exhibiting the same error as
the original third-party code.

## Prerequisites

Before starting, confirm you have:

- A built `lfortran` — `build/src/bin/lfortran` for a standard in-tree build
  (see `AGENTS.md`). Put it first on `PATH`, or set `$LFORTRAN` to its path.
- A **reference Fortran compiler** on `PATH`. `gfortran` is the project
  default — it is what `integration_tests` uses via the `gfortran` label, so
  prefer it. `flang` works too if that is what you have.
- Access to the failing third-party code and the exact error message

## Inputs to Gather

Ask the user for (if not already provided):

1. **The third-party project** — name, repo URL, or local path
2. **The exact error** — full error message or failure description from LFortran
3. **How to reproduce** — the build command, test command, or steps that trigger
   the failure
4. **Failure type** — compilation error, runtime crash, or wrong output
5. **The file(s) involved** — which source file(s) trigger the error

If the input is a GitHub issue rather than a third-party project, run the
`repro-issue` skill first — it produces an RE that this skill then reduces.

## Procedure

### Phase 0: Read Project Guidelines

Read `AGENTS.md` in the repository root for build commands and project
conventions. Follow it, but this SKILL.md takes precedence where they conflict.

### Phase 1: Understand the Failure

1. Reproduce the original failure with LFortran to capture the exact error
   message and exit code.
2. Identify the **error category**:
   - **Compilation error**: parser failure, semantic error, codegen crash
   - **Runtime failure**: crash, wrong output, hang
3. Note the specific error text — the MRE must trigger this same error.

### Phase 2: Isolate the Failing Construct

Use a **systematic isolation** approach first:

1. Read the error message carefully. Identify which Fortran construct is
   implicated (e.g. a specific intrinsic, derived type feature, array
   operation, module interaction, I/O statement).
2. Identify the minimal source file(s) that contain this construct.
3. If the failure involves modules, determine the minimum set of modules
   needed and their dependency order.

If you are unable to reproduce the error this way, proceed to the next phase.

### Phase 3: Reduce via Binary Search

Apply **binary-search reduction** to shrink the code:

1. Start with the isolated file(s) from Phase 2. If you failed to isolate in
   Phase 2, start with the whole third-party code.
2. Remove approximately half of the code that is NOT related to the failing
   construct (unused subroutines, unrelated variables, comments, etc.).
3. Test after each removal:
   - Does the reference compiler still compile/run it successfully?
   - Does `lfortran` still produce the **same error**?
4. If the error disappears, undo the last removal and try removing a smaller
   portion.
5. Repeat until no further code can be removed without losing the error.

**Guard against false reduction.** A reduction step is only valid if the
reference compiler still accepts the program. If you reduce until *both*
compilers fail, you have most likely produced invalid Fortran rather than
isolated an LFortran bug — back up to the last state where the reference
compiler succeeded.

### Phase 4: Minimize and Clean Up

1. Remove all unused:
   - `use` statements and modules
   - Variables and parameters
   - Subroutines and functions
   - Arguments and dummy parameters
2. Simplify compiler options:
   - Remove as many lfortran options as possible, while still reproducing the
     error
3. Simplify remaining code:
   - Replace complex expressions with simple literals where possible
   - Reduce array sizes to the smallest that still triggers the bug
   - Shorten identifier names for clarity (but keep them meaningful)
   - Remove all comments, empty lines, etc.
4. If multiple files are needed (modules), minimize the number of files.
   Prefer a single file if possible.

### Phase 5: Verify the MRE

Run the final verification:

```bash
# Must succeed with the reference compiler
gfortran -o test_ref <mre_file>.f90 && ./test_ref
echo "reference exit code: $?"

# Must fail with lfortran with the SAME error as the original
lfortran <mre_file>.f90
echo "lfortran exit code: $?"
```

For **runtime failures**, both compilers must compile successfully, but the
lfortran-compiled binary must produce wrong output or crash:

```bash
# Both compile
gfortran -o test_ref <mre_file>.f90
lfortran <mre_file>.f90 -o test_lfortran

# reference binary works
./test_ref

# lfortran binary fails
./test_lfortran
```

Confirm:
- [ ] The reference compiler compiles and runs successfully
- [ ] LFortran fails with the same (or closely related) error as the original
- [ ] No code can be removed without losing the bug
- [ ] All MRE files are in the repository root (`./`), not in any subdirectory

### Phase 6: Produce Output Files

**IMPORTANT: All output files MUST be created directly in the repository root
directory (`./`). Do NOT create a subdirectory (e.g. `mre_dir/`, `mre/`,
`output/`) for MRE files. Every file — `.f90` files, `run.sh` — goes in `./`.**

These are scratch artifacts, not deliverables. They are ignored via
`.gitignore` (see the `# repro-issue / create-mre scratch artifacts` block) so
they never end up in a fix PR. The deliverable is the integration test that
`fix-mre` adds under `integration_tests/`. Do not `git add -f` the MRE files.

#### 1. The MRE Fortran file(s)

Create the `.f90` file(s) directly in the repository root (`./`).
Name the file descriptively based on the bug, e.g.:
- `./mre_derived_type_alloc.f90`
- `./mre_intrinsic_reshape.f90`

If multiple files are needed (e.g. module dependencies), name them with a
common prefix and number them in compilation order:
- `./mre_mod1.f90` (module)
- `./mre_main.f90` (program)

#### 2. `./run.sh` — Reproduction script

Create an executable bash script **`./run.sh`** in the repository root
directory (the same directory as `CMakeLists.txt`). Replace any previous
`./run.sh` if it exists. Do NOT place it inside any subdirectory:

```bash
#!/usr/bin/env bash
set -e

# MRE for: <brief description of the bug>
# Original failure in: <third-party project name>
# Error type: <compilation error | runtime failure | wrong output>
#
# Expected: compiles and runs with the reference compiler
# Actual:   fails with lfortran — <brief error description>

LFORTRAN="${LFORTRAN:-lfortran}"
REF="${REF:-gfortran}"

echo "=== Verifying MRE ==="

echo ""
echo "--- $REF (should succeed) ---"
$REF -o test_ref <file(s)>
./test_ref
echo "reference: OK"

echo ""
echo "--- lfortran (should fail) ---"
echo "Expected error: <paste key part of the error message>"
$LFORTRAN <file(s)> 2>&1 || true
echo ""
echo "Done. If lfortran showed the expected error above, the MRE is valid."

# Cleanup
rm -f test_ref test_lfortran a.out *.mod
```

Make it executable:
```bash
chmod +x run.sh
```

For **runtime failures**, adjust `run.sh` to compile with both compilers and
run both binaries, comparing output.

### Phase 7: Summary

Print a summary for the user:

```
MRE created successfully!

Files (all in repository root ./):
  <list of .f90 files>
  ./run.sh

Bug: <one-line description>
Error type: <compilation | runtime | wrong output>
Original project: <name>

To reproduce:
  ./run.sh

The MRE compiles with the reference compiler but fails with lfortran.

Next step: run the `fix-mre` skill to fix the bug and add an integration test.
```

## Tips

- **Module files**: If lfortran fails during module compilation, the MRE may
  need separate files compiled in order. Reflect this in `run.sh`.
- **Compiler flags**: If the bug only manifests with certain flags (e.g.
  `--fast`), include those flags in `run.sh` and document them.
- **Multiple errors**: If the third-party code has multiple LFortran failures,
  create one MRE per distinct bug. Don't combine unrelated issues — `AGENTS.md`
  requires one bug = one MRE = one PR.
- **Fixed-form Fortran**: If the original code is fixed-form (`.f` or `.f77`),
  the MRE can use fixed-form too. Adjust file extensions accordingly.
- **Large codebases**: For very large projects, start by identifying which
  translation unit fails, then reduce that single unit first.
