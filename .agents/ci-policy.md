# Agent CI policy

Detailed CI requirements for [Repository Guidelines](../AGENTS.md).
Read this policy before changing CI workflows, coverage, caches, required
checks, or release validation.

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
  faster. The Linux LLVM 11 and LLVM 21 platform compilers are compiled with
  `-O2` the same way (in `.github/actions/build-platform`, also for Exhaustive's
  copies, cache key suffix `-o2`); the macOS and Windows platform builds keep
  their defaults. Quick runs the full CUDA-on-CPU suite on Linux LLVM 21 only
  and a smoke check on Linux LLVM 11 and macOS. Quick's macOS job runs the
  Metal smoke check; Exhaustive's macOS platform job runs the full Metal suite.
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

See [CI coverage and policy](../doc/src/installation.md#ci-coverage) for commands
and the distinction between capability tests and application validation.

