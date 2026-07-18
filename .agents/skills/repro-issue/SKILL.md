---
name: repro-issue
description: >
  Create a Reproducible Example (RE) from a GitHub issue filed against
  lfortran/lfortran. Fetches the issue, extracts the Fortran code as-posted,
  minimally extends it if necessary so it can be compiled and run, and produces
  a standalone .f90 file (or files) plus a run_re.sh script that reproduces
  the bug with `lfortran` and shows that the same program compiles/runs with
  a reference compiler. The output is intentionally as close to the issue's
  original code as possible — reduction/minimization is the job of the
  create-mre skill, which consumes this skill's output. Triggers: repro issue,
  reproduce issue, RE, reproducible example, github issue, lfortran issue,
  issue reproducer.
---

# Repro Issue — Reproducible Example from a GitHub Issue

Turn a GitHub issue filed at `lfortran/lfortran` into a local **Reproducible
Example (RE)**. The RE is a standalone Fortran program that:

1. Uses the code from the issue **as closely as possible** — ideally
   byte-for-byte identical — with only the smallest additions needed to make it
   compile/run.
2. **Fails** when compiled/run with `lfortran` (matching the error reported in
   the issue).
3. **Succeeds** when compiled/run with the reference compiler (confirming the
   issue is an LFortran bug, not invalid Fortran).

An RE is **not** a minimized MRE. Do not shrink, rename, or rewrite the issue's
code. The downstream `create-mre` skill will consume this RE and reduce it.
The downstream `fix-mre` skill will then fix the bug using the MRE.

Pipeline: **repro-issue → create-mre → fix-mre**

## Prerequisites

Before starting, confirm you have:

- A built `lfortran` — `build/src/bin/lfortran` for a standard in-tree build
  (see `AGENTS.md`). Put it first on `PATH`, or set `$LFORTRAN` to its path.
- A **reference Fortran compiler** on `PATH`. `gfortran` is the project
  default — it is what `integration_tests` uses via the `gfortran` label, so
  prefer it. `flang` works too if that is what you have.
- `gh` CLI available on `PATH` and authenticated (`gh auth status`)
- Working directory is the lfortran repository root (same dir as
  `CMakeLists.txt`) — output files go here

## Inputs to Gather

Ask the user for (if not already provided):

1. **The issue** — a GitHub issue number (e.g. `1234`) or URL
   (e.g. `https://github.com/lfortran/lfortran/issues/1234`). Default repo
   is `lfortran/lfortran`.

## Procedure

### Phase 0: Read Project Guidelines

Read `AGENTS.md` in the repository root. It is the single point of reference
for build commands, test layout, and contribution rules. Follow it, but the
instructions in this SKILL.md take precedence where they conflict.

### Phase 1: Fetch the Issue

Use the `gh` CLI to fetch the issue body and title:

```bash
gh issue view <N> --repo lfortran/lfortran --json number,title,body,labels,url
```

Also fetch comments if the body alone is insufficient:

```bash
gh issue view <N> --repo lfortran/lfortran --comments
```

Extract:

- **Title** (used in the RE header comment)
- **URL** (used in the RE header comment)
- **Fortran code blocks** from the body (and comments, if body is incomplete)
- **Reported error / expected vs actual behavior**
- **Any reproduction command** the reporter used (e.g. specific `lfortran`
  flags). Record these verbatim — they must be preserved in `run_re.sh`.

### Phase 2: Extract the Code As-Posted

1. Copy the Fortran code out of the issue **verbatim**. Do not:
   - Rename identifiers
   - Reformat, reindent, or reflow the code
   - Remove comments or unused declarations
   - "Fix" anything in the user's code
2. If the issue contains multiple code blocks (e.g. a module plus a program),
   preserve each as its own file when they are clearly separate compilation
   units. Otherwise concatenate in the order they appear.
3. If the issue links to a gist, pastebin, or external repo file, fetch that
   content and treat it as the code block.

### Phase 3: Minimal Extension (Only If Required)

The goal is that the RE compiles/runs end-to-end. If — and only if — the code
as-posted cannot compile or run on its own, add the **smallest possible**
wrapper around it. Permitted additions, in order of preference:

1. Wrap free-standing statements in `program repro ... end program` if no
   program unit is present.
2. Add `implicit none` only if the original code already relies on it
   implicitly; otherwise leave implicit typing as the user wrote it.
3. Add `use <module>` lines only if the issue clearly depends on them.
4. Provide trivially obvious sample inputs (e.g. initialize an integer used
   uninitialized) only when needed to reach the failing construct.

Every addition you make must be marked with a comment:

```fortran
! [RE-ADDED] wrapper program so the snippet can be compiled standalone
```

If the issue cannot be reasonably reproduced without substantial invention
(e.g. it depends on a third-party library, or the code is a tiny fragment
with no clear surrounding context), **stop and report this to the user**
rather than guessing.

### Phase 4: Verify the RE

Run both compilers and confirm the RE fails with LFortran and passes with the
reference compiler.

For a **compilation error** reported in the issue:

```bash
# Must succeed with the reference compiler
gfortran -o test_ref <re_file>.f90 && ./test_ref
echo "reference exit code: $?"

# Must fail with lfortran — ideally with the same error message as the issue
lfortran <re_file>.f90
echo "lfortran exit code: $?"
```

For a **runtime failure** reported in the issue:

```bash
gfortran -o test_ref <re_file>.f90
lfortran <re_file>.f90 -o test_lfortran

./test_ref         # must succeed / produce expected output
./test_lfortran    # must crash or produce wrong output
```

Confirm:

- [ ] The reference compiler compiles and runs successfully
- [ ] LFortran fails with (or very close to) the error reported in the issue
- [ ] The code in the RE file(s) matches the issue's code as closely as possible
- [ ] Any additions are clearly marked with `! [RE-ADDED]` comments
- [ ] All RE files live in the repository root (`./`), not in any subdirectory

If lfortran's error does **not** match the issue's error, investigate:

- Is the compiler rebuilt on the relevant branch?
- Are you missing a flag the reporter used?
- Did the bug get fixed already (close the loop with the user before continuing)?

### Phase 5: Produce Output Files

**IMPORTANT: All output files MUST be created directly in the repository root
directory (`./`). Do NOT create a subdirectory. Every file — `.f90` files,
`run_re.sh` — goes in `./`.** This is the same convention `create-mre` uses,
so the next stage in the pipeline can pick them up without any path wiring.

These are scratch artifacts, not deliverables. They are ignored via
`.gitignore` (see the `# repro-issue / create-mre scratch artifacts` block) so
they never end up in a fix PR. Do not `git add -f` them.

#### 1. The RE Fortran file(s)

Name files with an `re_` prefix and a short descriptor derived from the issue
(include the issue number to keep things traceable), for example:

- `./re_issue_1234.f90`
- `./re_issue_1234_mod.f90` + `./re_issue_1234_main.f90` (if multiple units)

#### 2. `./run_re.sh` — Reproduction script

Create an executable bash script **`./run_re.sh`** in the repository root
directory. Replace any previous `./run_re.sh` if it exists. Do NOT place it
inside any subdirectory. Template:

```bash
#!/usr/bin/env bash
set -e

# RE for: <issue title>
# Issue:  <issue URL>
# Error type: <compilation error | runtime failure | wrong output>
#
# Expected: compiles and runs with the reference compiler
# Actual:   fails with lfortran — <brief error description from the issue>

LFORTRAN="${LFORTRAN:-lfortran}"
REF="${REF:-gfortran}"

echo "=== Verifying RE for issue <N> ==="

echo ""
echo "--- $REF (should succeed) ---"
$REF -o test_ref <file(s)>
./test_ref
echo "reference: OK"

echo ""
echo "--- lfortran (should fail) ---"
echo "Expected error: <paste key part of the error message from the issue>"
$LFORTRAN <lfortran-flags-from-issue> <file(s)> 2>&1 || true
echo ""
echo "Done. If lfortran showed the expected error above, the RE is valid."

# Cleanup
rm -f test_ref test_lfortran a.out *.mod
```

Make it executable:

```bash
chmod +x run_re.sh
```

For **runtime failures**, adjust `run_re.sh` to compile with both compilers
and run both binaries, comparing output.

Preserve any `lfortran` flags mentioned in the issue verbatim (e.g.
`--fast`, `--realloc-lhs`, backend selection). The RE must reproduce under
the exact invocation the reporter used.

### Phase 6: Summary

Print a summary for the user:

```
RE created successfully!

Files (all in repository root ./):
  <list of re_*.f90 files>
  ./run_re.sh

Issue:      #<N> — <title>
URL:        <issue URL>
Error type: <compilation | runtime | wrong output>

To reproduce:
  ./run_re.sh

The RE compiles with the reference compiler but fails with lfortran,
matching the issue.

Next step: run the `create-mre` skill on this RE to reduce it to a minimal
reproducible example, then `fix-mre` to fix the bug.
```

## Tips

- **Stay faithful to the issue**: the whole point of the RE stage is that it
  mirrors the reported code. Any reduction, renaming, or simplification belongs
  in `create-mre`, not here.
- **Multiple code blocks**: if the issue shows both a module and a program,
  keep them as separate files; this mirrors how a real project is structured
  and is friendlier to `create-mre`.
- **Compiler flags**: some bugs only manifest with specific `lfortran` flags
  (`--fast`, `--realloc-lhs`, `--openmp`, backend flags). Extract these from
  the issue text/code fences and bake them into `run_re.sh`.
- **Fixed-form Fortran**: if the issue uses fixed-form (`.f`/`.f77`), keep
  that format and extension in the RE.
- **Reporter's own reduction**: if the issue already contains a minimal
  snippet (common for well-written issues), your RE may be identical to the
  MRE. That's fine — the `create-mre` stage will be a no-op or near no-op.
- **Unreproducible issues**: if you cannot reproduce the failure locally,
  stop and report back with diagnostics (compiler version, flags tried,
  actual vs reported error). Do not fabricate a reproduction.
- **Multiple bugs in one issue**: if an issue mentions several distinct
  failures, create one RE per distinct failure (e.g. `re_issue_1234_a.f90`,
  `re_issue_1234_b.f90`) and one `run_re.sh` that exercises each in turn,
  clearly separated.
