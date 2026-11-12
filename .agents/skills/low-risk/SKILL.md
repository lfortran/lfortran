---
name: low-risk
description: >
  Judge whether an LFortran pull request is low risk by reading its diff, and
  if it is, add the "low risk" label so maintainers can review and merge it
  quickly. Use it when a PR to lfortran/lfortran is opened or gets a new push,
  as the last step of a pr-review, and whenever asked which PRs are low risk.
  Low-risk kinds: a PR that only adds an integration test; a fix that only
  registers something already supported in an existing table; a tiny
  one-function fix that turns a compile-time crash into working code without
  changing any program that already compiled; a fix that only lower-cases a
  Fortran name, like a sibling construct already does; a small fix that wraps
  an existing step in an `if` check the code base already uses widely for the
  same decision; and a fix that adds one missing case to a switch whose
  default asserts, built like its neighbour cases. Each fix kind comes with a
  new integration test. The skill only adds the label: it does not mark a
  draft ready and does not turn on auto-merge.
compatibility: Requires gh or GitHub MCP tools; adding a label needs triage access to lfortran/lfortran.
---

# Low-risk LFortran PRs

A low-risk PR is one whose diff is small and fits one of the kinds below, so a
maintainer can check it quickly. Such a PR gets the `low risk` label. The label
is only a hint for reviewers: it does not mark a draft ready, does not merge
the PR and does not turn on auto-merge. The PR still follows the normal rules
in `AGENTS.md` (draft while iterating, ready only when Quick is green and the
review is clean, merged by a maintainer).

**Read the diff and judge it yourself.** Do not match line patterns with a
script. Understand what the change does and decide whether it fits a kind.

## When to run it

- When a PR to `lfortran/lfortran` is opened or gets a new push.
- As the last step of a [pr-review](../pr-review/SKILL.md). A low-risk verdict
  does not replace the review: a PR with blocker or rework findings is not low
  risk.
- When asked which PRs are low risk.

## How to judge a PR

1. Read the PR and its whole diff:
   ```bash
   gh pr view <PR> --repo lfortran/lfortran --json title,body,files,labels,isDraft,state
   gh pr diff <PR> --repo lfortran/lfortran
   ```
2. If it changes compiler code, read that code on upstream `main` around the
   change: what uses the changed table or function, and whether the new case
   really goes through code that already works for its neighbours.
3. Decide: do ALL the changes fit ONE kind below? When in doubt, it is not low
   risk.
4. Act:
   - **Low risk:** add the label.
     ```bash
     gh pr edit <PR> --repo lfortran/lfortran --add-label "low risk"
     ```
   - **Has the label but is no longer low risk** (a later push added other
     changes): remove the label and say why.
     ```bash
     gh pr edit <PR> --repo lfortran/lfortran --remove-label "low risk"
     ```
   - **Not low risk:** do nothing.
   - **No triage access:** do not try other ways to label; report the verdict
     and the kind instead.
5. Report the verdict, the kind, and a one-line reason.

## Low-risk kinds

| Kind | What fits | What does not fit |
|---|---|---|
| `new_integration_test` | The PR only adds a new integration test (and any files it needs) and registers it with a `RUN(...)` line in `integration_tests/CMakeLists.txt`. Example: #15670. | Any change to compiler code, CI, build scripts, docs, existing tests or reference outputs. |
| `register_entry` | The fix only registers something that was missed: a new entry in an existing table (a map, list or registry) that fits the table's current form, for something the compiler already supports in general; the code that reads the table needs no change. Plus a new integration test for it. Example: #15679 added `{ AST::boolopType::NEqv, "~neqv" }` to `boolop2str`, next to `.eqv.`, so a defined `operator(.neqv.)` is found like `.eqv.` already was. | A new table; changed or removed entries; an entry that needs new handling code anywhere else to work; an entry whose support is missing or only partial; any other compiler change in the same PR. |
| `small_ice_fix` | A tiny compiler fix that turns a compile-time crash (ICE, "not implemented", assertion) into working code, and leaves every program that already compiled unchanged. All the points in "Checks for `small_ice_fix`" below must hold. Example: #16941 (#16892): `SelectCaseVisitor::visit_Select` now calls the base walker on the case bodies before lowering the Select, so nested SELECT CASE of any depth is lowered in one walk; the second whole-unit walk, a workaround, is removed. +4 -3 lines in one function of one ASR pass. | Wrong-result fixes (old output changes); new or changed error messages; any reference-output update; changes in more than one function, or in more than one file of compiler code; a new helper, class, flag or table; backend code that works around a type or ASR problem (the bug is upstream); a crash fix that only skips or hides the case (early return, `if (!x) return`) instead of handling it. |
| `name_case_fix` | A fix for a name that the source spells with upper-case letters but that the compiler stores or looks up as written, while every other lookup uses lower case. The fix only lower-cases that name with the existing `to_lower` (or the `s2c(al, to_lower(...))` form) where it enters the symbol table or ASR node, the same way a sibling construct already does, and uses the lowered name in place of the raw one. All the points in "Checks for `name_case_fix`" below must hold. Example: #17358 (#17227): `visit_SelectType` and `visit_SelectRank` lower-case the association name, as `visit_AssociateBlock` already did; +8 -5 lines in one file. | Any other logic in the same PR (new branches, new lookups, changed scoping); lower-casing something that must keep its case (strings, `bind(c, name=...)`, file names, external link names); changes in more than one file of compiler code; a new helper; any reference-output update. |
| `common_guard_fix` | A small fix that wraps an existing step in an `if` that the code base already uses for the same decision in many places, so the step runs only for the cases it was written for. In LLVM codegen this is usually a well-known type check before a load or a descriptor step (for example `LLVM::is_llvm_pointer`, `ASRUtils::is_array`, `is_a<Pointer_t>` / `is_a<Allocatable_t>`, `ASRUtils::is_character`). All the points in "Checks for `common_guard_fix`" below must hold. Example: #17349 (#14335): in `visit_PointerAssociated`, the descriptor load now runs only `if (LLVM::is_llvm_pointer(*tgt_type))` (a check used about 180 times in `asr_to_llvm.cpp`), so an assumed-shape target no longer asserts. | A check that is new, rare, or written for this PR; a guard that skips or hides the case (early return, `if (!x) return`) instead of handling it; a new `else` with new logic; changes in more than one function; any reference-output update; any other compiler change in the same PR. |
| `missing_switch_case` | A fix that adds one missing `case` to a `switch` whose `default` asserts or says "not implemented", so a kind that crashed now works, and the case is built like its neighbours in the same switch. All the points in "Checks for `missing_switch_case`" below must hold. Example: the `set_allocation_size` part of #17345 (#17270): a new `Unpack` case allocates the temporary with `size(mask, i)` per dimension, like the CSHIFT, EOSHIFT and TRANSPOSE cases next to it. #17345 itself does not fit, because it also adds `Unpack` to `intrinsic_array_result_len`, which changes the length taken on the fixed-size path that already compiled. | Any other compiler change in the same PR, even a related table entry, if it can change programs that already compiled; a case with new logic that no sibling has; a case that only skips or hides the kind (empty case, `break`, early return); more than one new case or function; any reference-output update. |

### Checks for `small_ice_fix`

Every point must hold. When one is unclear, it is not low risk.

1. **Tiny and local.** At most about 10 changed lines of compiler code, all in
   one function (or one function plus the line that calls it), in one file.
   Tests do not count.
2. **Reuses what exists.** The fix uses a pattern or helper the code base
   already has, used the same way elsewhere: for example calling the base
   visitor to walk children first, or an existing `ASRUtils` helper. No new
   logic of its own.
3. **Removing is fine.** Dropping a workaround that the fix makes useless (like
   a repeated walk) is allowed, and is a good sign, as long as the PR body says
   why it is now dead.
4. **Only adds support.** On main the case failed at compile time. Programs
   that compiled before take the same path and get the same ASR and IR. A fix
   that changes what an already-compiling program produces is not this kind.
5. **No reference change.** Any `tests/reference/` file in the diff rules it
   out.
6. **Real test.** A new integration test, registered with at least `gfortran`
   and `llvm`, that crashes on main and passes on the branch. It covers more
   than the one reported shape (for #16941: deeper nesting, DO / IF / DO WHILE
   between levels, CASE DEFAULT, character selectors).
7. **Local suites pass.** The PR shows that the local unit, integration and
   reference suites pass. Judge again after each new push.

To check point 4, read the changed function on upstream `main` and ask: for
input that already worked, does the new code do exactly what the old code did?
For #16941, yes: a Select with no nested Select gains only a walk over bodies
that contain nothing to lower.

### Checks for `name_case_fix`

Every point must hold. When one is unclear, it is not low risk.

1. **Only lower-casing.** Every compiler change is either a `to_lower` of the
   raw name or a swap of the raw name for the lowered one. Nothing else. It may
   touch a few functions in one file when each gets the same change (#17358:
   SELECT TYPE and SELECT RANK).
2. **A sibling already does it.** Some other construct already lower-cases the
   same kind of name (for #17358: `visit_AssociateBlock`). The PR body names it.
3. **Lower-case code is unchanged.** `to_lower` does nothing to a lower-case
   name, so a program that spells the name in lower case gets the same ASR and
   IR. Only programs that used upper-case letters change, and those failed (or
   were wrong) before.
4. **The name is a Fortran identifier.** Fortran names are case-insensitive, so
   lower-casing is always right. Not for anything whose case matters at run
   time or link time.
5. **No reference change.** Any `tests/reference/` file in the diff rules it
   out.
6. **Real test.** A new integration test, registered with at least `gfortran`
   and `llvm`, that fails on main and passes on the branch. It spells the name
   in mixed case at the definition and in several different spellings at the
   uses, for every construct the fix touches.
7. **Local suites pass.** The PR shows that the local unit, integration and
   reference suites pass. Judge again after each new push.

### Checks for `common_guard_fix`

Every point must hold. When one is unclear, it is not low risk.

1. **Only a guard.** The compiler change is an `if (<check>)` around code that
   is already there (plus a short comment). The guarded code itself is not
   changed.
2. **The check is common.** The same check is used for the same kind of
   decision in many other places in that phase (grep it). The PR body names it.
3. **Old cases keep their path.** Every case that compiled before still meets
   the guard and runs the same code. Only cases that crashed before skip the
   guarded step.
4. **Tiny and local.** One function in one file of compiler code, about 10
   changed lines or fewer. Tests do not count.
5. **No reference change.** Any `tests/reference/` file in the diff rules it
   out.
6. **Real test.** A new integration test, registered with at least `gfortran`
   and `llvm`, that fails on main and passes on the branch, and covers several
   types or shapes that reach the guard.

### Checks for `missing_switch_case`

Every point must hold. When one is unclear, it is not low risk.

1. **Only a new case.** The compiler change is one new `case` in a `switch` (or
   one new branch in an if-chain over the same kinds) whose `default` asserts,
   throws "not implemented" or reports an internal error. Nothing outside the
   new case changes.
2. **Only the failing path changes.** On main the new kind reached that
   `default` and crashed. Every kind that worked before still takes its old
   case, so programs that compiled before get the same ASR and IR.
3. **Built like its neighbours.** The body follows a sibling case in the same
   switch, with the same helpers and the same shape of code (for #17345's
   `Unpack`: the CSHIFT, EOSHIFT and TRANSPOSE cases). The PR body names that
   sibling. Only the parts that differ for the new kind differ, and those
   follow the standard (for UNPACK: the result has the shape of MASK).
4. **Small and local.** One case in one function in one file, about 25 changed
   lines or fewer. No new helper, class or flag.
5. **No reference change.** Any `tests/reference/` file in the diff rules it
   out.
6. **Real test.** A new integration test, registered with at least `gfortran`
   and `llvm`, that crashes on main and passes on the branch, and covers
   several types, ranks and shapes that reach the new case (zero size, non-1
   lower bounds where they matter).
7. **Local suites pass.** The PR shows that the local unit, integration and
   reference suites pass. Judge again after each new push.

### Checks for `new_integration_test` and `register_entry`

- `new_integration_test`: the new test passes on upstream `main` in the
  default, `--fast` and `--std=f23` modes and with GFortran, and its name is
  not already used in `integration_tests/`. The PR body says that main already
  gives the right result and names the PR that fixed it.
- `register_entry`: the new test fails on main and passes on the branch, the
  `fix-mre` targeted checks pass, and the local unit, integration and
  reference suites pass.

## Adding a kind

The list of kinds is meant to grow. Add a kind only when a maintainer agrees
that it is low risk, in a PR to this skill: add a row to the table (what fits,
what does not, an example PR) and, if needed, its checks under the table. Do
not widen a kind, or label a PR because it "looks safe", without that.
