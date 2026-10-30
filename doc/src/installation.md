# Installation

Use Pixi for development on Linux, macOS and Windows.

## Pixi (recommended)

Use [Pixi](https://pixi.sh/) to install the repository's build dependencies and
build LFortran. Install Pixi following its platform instructions, and install
Git. On macOS, install Xcode Command Line Tools (`xcode-select --install`).
On Windows, install Visual Studio's C++ build tools and run Pixi
from an initialized MSVC developer environment (the x64 Native Tools Command
Prompt). Bash is not required: Pixi installs the cross-platform
[`shell`](https://github.com/prefix-dev/shell), which runs the build scripts
natively on every platform. Use Pixi inside WSL for the Unix-only integration
runner.
Pixi supplies the Linux C/C++ compilers, Clang for native LLVM executable
linking, and LLVM's OpenMP runtime (`libomp`), so host Clang is not required.
On macOS, the environment selects the Xcode C/C++ compilers; the C backend
honors this selection for both compilation and linking (see
[Selecting the C Compiler](usage.md#selecting-the-c-compiler)).

```bash
git clone https://github.com/lfortran/lfortran.git
cd lfortran
pixi run -e llvm11 build > build.log 2>&1
pixi run -e llvm11 start --version
```

There is no separate dependency-install or source-generation step:
`pixi run build` installs the environment and invokes the repository's build
scripts. The default native environment is `llvm11`, which matches the reference
test suite. Its executable is `build/llvm11/src/bin/lfortran`; run it through
`pixi run start`, or use `pixi shell -e llvm11` to put it on your `PATH`.
On Windows the executable has the `.exe` extension. The native Windows task
uses Release mode with compiler and runtime stacktraces disabled.

Arguments after `build` are passed to CMake and override the task's defaults.
For example, on Linux or macOS, build Debug with `-O2`:

```bash
pixi run -e llvm11 build -DCMAKE_BUILD_TYPE=Debug '-DCMAKE_CXX_FLAGS_DEBUG=-g -O2' '-DCMAKE_C_FLAGS_DEBUG=-g -O2' > build.log 2>&1
```

After editing sources, rebuild incrementally:

```bash
pixi run -e llvm11 rebuild > build.log 2>&1
```

`rebuild` runs `cmake --build` and recompiles only what changed. `build`
reconfigures CMake, which reruns `build0.sh`: it regenerates `asr.h`, `ast.h`,
the parser and the other generated sources with new timestamps, so almost
everything is recompiled. Use `build` for the first build, for new CMake
options, and after editing a generator input (`src/libasr/ASR.asdl`,
`grammar/AST.asdl`, `src/lfortran/parser/parser.yy`, the `.re` tokenizers,
`src/libasr/wasm_instructions.txt`, the generator scripts or
`src/server/generator/`); `rebuild` would compile stale generated sources.

Select another environment explicitly to keep multiple configurations:

```bash
pixi run -e llvm22 build
pixi run -e llvm22 start --version
```

Dependencies live in `.pixi/envs/<environment>` and CMake builds in
`build/<environment>`. Configuration names, not just LLVM versions, determine
the directory, so feature combinations using the same LLVM can coexist too.
Build configurations one at a time within a checkout because source generation
is shared; use separate Git worktrees for parallel development.

Run tests from the repository root and inspect the saved logs:

```bash
pixi run -e llvm11 ctest -j8 > unit.log 2>&1
pixi run -e llvm11 tests -j8 > reference.log 2>&1
pixi run -e llvm11 integration_tests -j8 > integration.log 2>&1
pixi run -e llvm11 integration_tests -b gfortran -j8 > gfortran.log 2>&1
```

The `llvm11` Pixi environment also provisions Kokkos for generated C++ tests
and sets `LFORTRAN_KOKKOS_DIR` to that environment's prefix.

The `gfortran` backend honors `FC` when set, otherwise it uses `gfortran`.
On macOS ARM, the `llvm11` environment sets `FC="gfortran -B/usr/bin/"` to
select Xcode's assembler and linker. Recent GFortran emits M1/LSE instructions
that the LLVM 11 environment's bundled Clang assembler rejects by default.
This tool selection does not pass GFortran-specific flags to LFortran or
change the LLVM version used for reference outputs.

Native test environments include Kokkos 4.4.01 for the C++ backend. Inside
the selected Pixi environment, set `LFORTRAN_KOKKOS_DIR="$CONDA_PREFIX"`.
For an uninstalled development build, also set
`LFORTRAN_RUNTIME_LIBRARY_HEADER_DIR="$LFORTRAN_BUILD_DIR/src/libasr/runtime"`.
The packaged Kokkos enables OpenMP, so C++ compilation must enable OpenMP too.
On macOS, Xcode Clang accepts `-Xpreprocessor -fopenmp`, with
`-I"$CONDA_PREFIX/include"` and `-L"$CONDA_PREFIX/lib" -lomp`.
The C++ driver's `--openmp` currently passes `-fopenmp`, which Xcode Clang
rejects; to validate generated C++ on that platform, emit it with
`--backend=cpp --show-cpp` and compile/link it explicitly with these flags,
the runtime header directory, and `-lkokkoscontainers -lkokkoscore`.
Keep `-Wl,-rpath,"$CONDA_PREFIX/lib"` when linking against the Pixi libraries.

The integration task uses Make from the selected environment. The runner is
currently intended for Unix-like shells; use WSL
for that suite on Windows. CTest uses the selected native build. Append `-t
<pattern>` to `tests` or `integration_tests` for a focused run. Reference tests
require a compiler built with LLVM 11, which the task checks before running or
updating any test. They run in
`build/<environment>/reference_tests`, which links to the test inputs in the
source tree, so outputs, module files and executables stay in that build
directory and several builds can be tested at once. The directory is emptied at
the start of every run. They update the source
`tests/reference` only when explicitly given `-u`. Review every reference
update.

Use `pixi run -e <environment> clean` to clean that configuration's CMake build
targets without deleting other builds, local changes, or installed dependencies.
Set `CMAKE_BUILD_PARALLEL_LEVEL` to limit build parallelism. Keep the same Pixi
entry points as dependencies/build details evolve in `pixi.toml` and the scripts,
rather than maintaining a separate list of packages or CMake flags.

Use the Pixi workflow for builds and tests; it supplies the declared
project dependencies without separate system/global package installation.
AI contributors must follow the
[required Pixi toolchain policy](https://github.com/lfortran/lfortran/blob/main/AGENTS.md#required-agent-toolchain-pixi).
Optional alternatives for packaging or existing non-Pixi installations are
collected [at the end of this guide](#optional-installation-alternatives).

## Tests

#### Run unit and reference tests

Run from the repository root after building, and inspect the saved logs:

```bash
pixi run -e llvm11 ctest -j8 > unit.log 2>&1
pixi run -e llvm11 tests -j8 > reference.log 2>&1
```

Reference tests and updates require LLVM 11 (`llvmdev ==11.1.0`). The Pixi
`tests` task checks the selected compiler before running or updating references.

#### Update test references

Update only the tests whose outputs intentionally changed, using LLVM 11,
and review the resulting diff:

```bash
pixi run -e llvm11 tests -t <pattern> -u -s > reference-update.log 2>&1
git diff -- tests/reference
```

#### Run integration tests

```bash
pixi run -e llvm11 integration_tests -j8 > integration.log 2>&1
pixi run -e llvm11 integration_tests -t <pattern> -j8 > targeted.log 2>&1
pixi run -e llvm11 integration_tests -b gfortran -j8 > gfortran.log 2>&1
```

Additional backends and modes use the same Pixi task, for example `-b c`,
`-b llvm_omp`, or `-f` for fast mode. Use `-e <environment>` consistently for
another Pixi build; reference tests and updates always use `llvm11`.

#### Speed up integration tests on macOS

Integration tests run slowly because Apple checks the hash of each executable online before running.

You can turn off that feature in the Privacy tab of the Security and Privacy item of System Preferences > Developer Tools > Terminal.app > "allow the apps below to run software locally that does not meet the system's security
policy."

#### CI coverage

Pull requests normally run only **Quick checks**. Quick uses the same
builds, test suites and selection rules on PRs, main pushes, release tags and
manual runs. Publishing steps remain push-only. Main runs Quick plus
Exhaustive. Exhaustive adds configurations and broader suites, never another
invocation of Quick, and runs identically on main and on manual dispatch.
Exhaustive never runs on PRs.

The shared native compiler workflow has two explicit coverage roles:

| Role | Caller | Native LLVM matrix |
| --- | --- | --- |
| `quick` | Quick on every event | Linux 7/11/23 |
| `exhaustive` | Exhaustive on every event | Linux 7/8/10/11/15/17/18/19/21/22/23 and macOS 22 |

Full LLVM-WASM, no-LLVM and MLIR suites belong directly to Quick on every event.
They are not declared in Exhaustive or its shared compiler workflow, so there
are no duplicate or skipped Exhaustive copies of these jobs.

The full LLVM-WASM suite runs in two parallel jobs that build the same
compiler: `Test LLVM 19 WASM` runs the WASI backend (`llvm_wasm`, wasmtime)
and the unit tests, and `Test LLVM 19 WASM Emscripten` runs the Emscripten
backend (`llvm_wasm_emcc`, Node). The Emscripten pass alone takes about as
long as building the compiler plus the WASI pass, because every `emcc` link
runs Emscripten's JavaScript compiler (about 0.3 s per test). Only the WASI
job saves the shared compiler cache. Like Quick's LLVM 11 compiler, this
compiler is a Debug build compiled with `-O2`, which halves the time each
test spends in LFortran.

Quick distributes the full CPU modes over two existing builds rather than
serializing them in one long job:

| Compiler | Complete regression modes |
| --- | --- |
| Linux/LLVM 11 Debug, compiled with `-O2` | Normal, `--fast`, Fortran 2023 normal/fast, full references, small LLVM variants, submodules and single invocation |
| Linux/LLVM 21 Debug, compiled with `-O2` | Separate compilation, submodules with separate compilation, and leak detection |

Every registered LLVM test runs in each of these modes on its designated
compiler. Both are Debug builds, so every full Quick suite runs with
assertions and per-pass ASR verification. The LLVM 11 compiler runs four full
passes, so it is compiled with `-O2` (`CMAKE_CXX_FLAGS_DEBUG`); it stays a
Debug build, so `WITH_LFORTRAN_ASSERT` stays on and nothing defines `NDEBUG`.
This makes each pass about 1.6 times faster. The Linux/LLVM 21 platform
compiler is compiled with `-O2` the same way: `.github/actions/build-platform`
adds `-O2` to `CFLAGS`/`CXXFLAGS` for that platform only, so Exhaustive's
Linux/LLVM 21 platform job builds the same compiler and restores the same
cache (key suffix `-o2`). The macOS, Windows and Linux/LLVM 11 platform builds
keep their defaults, so Quick still tests a `-O0` compiler.
Both also use the platform C/C++
diagnostic and standard-library hardening flags, including `-Werror`, and
`WITH_INTERNAL_ALLOC_CHECK=yes`. Splitting the modes across two
jobs reduces the critical path without sampling those suites or adding
another dependent job/queue.

The Linux/LLVM 11 platform build retains full reference coverage, platform smoke tests,
and the full GFortran, C/C++, Fortran, direct-WASM, OpenMP and CUDA-on-CPU
backend suites. Its GFortran reference step installs conda-forge GFortran
15.3 or newer within the 15.x series in the active environment and selects it
explicitly through `FC`. This avoids the saved deferred-length character
regression in GCC 15.2 ([GCC PR120713](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=120713))
without skipping tests or changing other jobs, including Caffeine's separate
OpenCoarrays reference toolchain.
Linux/LLVM 21 Debug also runs normal/fast smoke coverage, and
Linux/LLVM 7/23 Release provide additional smoke coverage. macOS/LLVM 11 keeps
platform smoke coverage and
the full Metal and CUDA-on-CPU suites. Windows keeps its native Release
build and supported compile/link/run checks. Caffeine/coarrays run on the
LLVM 11 Debug compatibility compiler. The standalone compiler-to-WASM build is also retained;
its native phase is Quick's Linux GCC Release build, so it uses `-Werror`
with the LSP and runtime-stacktrace options of Exhaustive's `Check Release
build`. This catches warnings that only appear with `NDEBUG`, such as a
variable used only in an assertion, before they break main.

Exhaustive runs the full compatibility suites on every LLVM version except
11 and 19, which run the application catalog instead, as does macOS LLVM 22;
every compatibility job runs Caffeine/coarrays. Three supplemental platform builds also run full normal/fast suites on Linux
LLVM 11/21 Debug and full normal/reference suites on macOS LLVM 11 Debug.
These retain the full platform coverage that used to run only in main's Quick.
Quick and Exhaustive share `.github/actions/build-platform` so these compiler
configurations cannot drift. The supplemental jobs do not rerun Quick's GPU,
alternate-backend or descriptor-mode suites; Linux references stay in Quick.

All native compatibility profiles enable runtime-stacktrace support, including
the LLVM 11/19 and macOS application compilers. Caffeine removes LFortran
`-g` from its defaults and GASNet linker flags; its `--enable-debug` build does
not require disabling runtime stacktraces. Actual LFortran `-g` links invoke
`llvm-dwarfdump` and `dwarf_convert.py` (also `dsymutil` on macOS); ordinary
non-`-g` links do not. The LLVM packages supply these debug tools. A separate
application-compiler probe verifies the generated runtime-support define,
executes the tools and checks both ordinary and `-g` links before the catalog.
Missing/broken tools or failed links must fail the job, not disable support.

On Linux, runtime-stacktrace support uses the compiler's `<unwind.h>` interface.
It does not itself enable CMake's separate `WITH_LIBUNWIND` option. LLVM >=12
requires that library independently, and the workflow retains its explicit
`libunwind` installation (also kept in the existing Quick LLVM 11 environment).
The LLVM 11 application compiler does not need an additional `libunwind`
installation merely to enable runtime stacktraces. Application validation must
exercise the runtime-enabled Release LLVM 11/19 and macOS LLVM 22 profiles;
success with the former disabled-runtime flags is not evidence for this change.

The distinct Kokkos/out-of-source and custom-install configurations run
full suites. Standalone C++ builds, documentation/kernel tests, the
Docker build/tests, JupyterLite and source packaging remain additional checks.

PRs do not run the Exhaustive workflow at all. For a rare, explicitly
requested extended check of a PR, dispatch it in a fork (see below).

##### Job timeouts

Every job sets `timeout-minutes`, about twice its slowest normal run (for
example 60 minutes for the Windows platform build, 150 for macOS). Without
it, a hung job holds a runner for GitHub's 6-hour default; under the
organization's 20 concurrent-job limit, one hang in a test that runs on every
PR can block CI for hours. When a job's normal duration grows, raise its
timeout in the same change.

##### Compiler caches

Every C/C++ build runs through ccache or sccache
(`hendrikmuhs/ccache-action`), including both halves of the WASM build, which
use ccache as the CMake compiler launcher for the native compiler and for
`em++`. The action keeps the cache in the workspace (`.ccache` or
`.sccache`), so a build that runs `git clean` must exclude it. Caches are **saved only on
`main`** (`save: ${{ github.ref == 'refs/heads/main' }}`) and restored
everywhere. A cache saved for a PR or tag can only be restored by that same
ref, and the repository's 10 GB cache limit evicts the least recently used
caches first, so PR caches would push out the `main` caches that every run
starts from. The `Cleanup caches by a branch` workflow also deletes a PR's
caches when it closes.

Keep compiler inputs identical between commits. The version comes from
`git describe` and changes with every commit, so it must not appear in build
paths: `ci/build.sh` unpacks the versioned source tarball into the fixed
`lfortran-src/` directory before building.

The action appends a timestamp to every saved key, so each save on `main` adds
a new copy. After every Quick or Exhaustive run on `main`,
`Prune-Main-Caches-CI.yml` (`ci/prune_main_caches.py`) deletes all but the
newest copy of each key, keeping the total under the limit. This leaves room
for a 1.5 GB ccache per platform build (`max-size: 1500M` in
`.github/actions/build-platform`); the action's 500 MB default is smaller than
one Debug build, so ccache evicted objects it still needed. Quick's `Build`
and Exhaustive's `platform` jobs build the same platform compilers with the
same cache keys, so only Quick saves them (`save-cache: 'false'` in
Exhaustive). When both saved, each `main` push added a second copy of every
platform cache before the prune ran, which pushed the total over 10 GB and
evicted other jobs' caches.

##### Required checks

The `main` ruleset requires these twelve Quick checks directly, bound to the
GitHub Actions app (app ID `15368`). There is no aggregate status job.

```text
LFortran CI (OS=macos-latest, LLVM=11)
LFortran CI (OS=ubuntu-latest, LLVM=11)
LFortran CI (OS=ubuntu-latest, LLVM=21)
LFortran CI (OS=windows-2025, LLVM=11)
Build LFortran to WASM
Compiler compatibility / Test LLVM 7 (ubuntu-latest)
Compiler compatibility / Test LLVM 11 (ubuntu-latest)
Compiler compatibility / Test LLVM 23 (ubuntu-latest)
Compiler compatibility / Test LLVM 19 WASM (ubuntu-latest)
Compiler compatibility / Test LLVM 19 WASM Emscripten (ubuntu-latest)
Compiler compatibility / Test without LLVM Backend
Compiler compatibility / Test MLIR backend
```

Keep these job names stable. When a required Quick job is renamed or added,
update the ruleset in the same rollout; a required check that is never
reported leaves PRs blocked. A [conditionally skipped job reports success and
does not block merging even when
required](https://docs.github.com/en/actions/how-tos/write-workflows/choose-when-workflows-run/control-jobs-with-conditions),
so required jobs must not be skipped by conditions. Repository variables
(`vars`) are not passed to workflows triggered by PRs from forks, so required
jobs must not depend on them either. Exhaustive uses the distinct
`Extended compiler checks` prefix, so an optional Exhaustive result cannot
substitute for a required Quick result.

**Third-party applications generate bugs for the integration suite; they are
not part of ordinary PR checks.** The application catalog runs in every
Exhaustive run on `main` (coalesced, so always on the latest main), where it
both finds coverage gaps and demonstrates compatibility
with real applications, and in every explicitly requested Exhaustive run.
There is no automatic exception for changes to serialization, finalization,
I/O or GPU lowering.

Caffeine is different: it supplies the coarray runtime backend. Building it,
running its own LFortran-compiled unit tests and running every registered coarray
capability test remain part of Quick, just as
Metal and CUDA-on-CPU integration tests validate particular backends and
platforms. Toolchain/runtime dependencies are not the application catalog.

`ci/test_caffeine.sh` uses Caffeine and its generated `run-fpm.sh` wrapper,
which selects LFortran and the GASNet runner. Unit tests use four images;
the PRIF smoke test and integration tests keep their existing image settings.
The missing-tool installer uses the same `fpm=0.12.0` pin as the application
harness. A failed installed tool or unit test is an error, not a reason to
skip coverage or reinstall speculatively.

Only the Linux **GFortran/OpenCoarrays reference validation** is source-dependent
in Quick. The shared workflow supplies `LFORTRAN_COARRAY_BASE` (the PR base SHA
or push's previous SHA) and `LFORTRAN_COARRAY_HEAD` (the actual checkout SHA).
`ci/coarray_tests.py` shares the harness's manifest parser and compares registered
primary and `EXTRAFILES` sources, including edits, additions, renames and deletions.
Changed coarray registrations, harness/environment inputs or relevant CMake
dependencies also request reference validation. Other changed paths default to
reference validation, including data files anywhere in the repository, unregistered
sources, support files and unknown configuration. This does not depend on finding
literal file names or particular I/O statements in the Fortran sources.
Only regular compiler implementation files under `src/` with the explicit suffixes
in `COMPILER_SUFFIXES`, and simple standalone non-coarray `.f90` programs with
literal `RUN(NAME ... LABELS ...)` registrations, can skip this fallback. Both
versions of a changed file must qualify; additions/deletions check the existing
version. Module/procedure sources, preprocessing, continuations and more complex
registrations are intentionally conservative, even when unrelated.
Unrelated simple registrations in the manifest retain their fast path.
The same comparison rule applies on every event; there is no reduced PR-only
LFortran selection.

When those inputs are demonstrably unchanged, neither shared-workflow setup nor
the Caffeine script installs OpenMPI/OpenCoarrays for Quick, and no `caf`/`cafrun`
checks run. Caffeine uses GASNet's SMP conduit, not MPI. Missing or inconsistent
history (including manual runs, new refs or unavailable push bases), dirty
checkouts and unresolved source dependencies log **conservative reference
validation**, never an unexplained skip. Even the narrow source-change fast path
requires ordinary free-form `.f90` coarray sources and known source options;
fixed-form/preprocessed/other-language support and unknown options always request
reference validation. The additional source guard keeps quoted text and trailing
comments rather than guessing where a Fortran comment starts. File I/O (including
`INQUIRE`), foreign bindings, split tokens and continued character literals request
reference validation; only explicit standard-output `write(*, ...)` is exempt.
These guards can request extra work, but are not a general Fortran dependency
parser and never exempt data or unknown changed paths.
Invalid test registrations fail explicitly.
Standalone/default and Exhaustive invocations always request full Linux reference
validation. macOS retains its existing no-OpenCoarrays behavior; Caffeine unit,
smoke and all LFortran integration tests still run.
The OpenMPI availability probe uses `mpifort --showme:version`, which checks the
wrapper without invoking its configured build-time compiler. OpenCoarrays'
CMake build still selects GFortran and checks that it can compile and link MPI;
a missing or broken reference compiler remains an error.

When an application finds a compiler bug, reduce the failure to a registered
integration regression in the relevant modes, fix the compiler, and verify
the original application failure. Promptly fix or revert a regression on main.
The lasting protection for future PRs is the integration test, not adding the
whole application to Quick. Finding such a gap on main is an accepted trade-off,
not a reason to silently ignore the failing application check.

Exhaustive on main runs the full LLVM matrix, full platform suites, application,
documentation, packaging and JupyterLite checks. Main pushes share one
concurrency group per workflow (Quick and Exhaustive), so for each workflow at
most one main run is in progress and one is pending. A running main run is
never cancelled; a newer push replaces the pending run. The latest main is
therefore always tested, but when several pushes land while a run is in
progress, the intermediate commits are not tested individually. Their changes
are covered by the next run. To test a skipped commit, re-run its cancelled
run (`gh run rerun <run-id>`), which tests exactly that commit; it rejoins the
main concurrency group and waits behind the run in progress.
`workflow_dispatch` accepts only a branch or tag, not a commit.

**Releases require green main, including application validation.** The commit
selected for release must have its own green Quick and Exhaustive runs on
main; re-run them if that commit was skipped by coalescing. A green Quick PR or
extended compiler run is not a substitute. Release-tag workflows still run
compiler, documentation and packaging checks; they do not repeat the application
catalog already validated on main.

##### Running Exhaustive for a PR

Exhaustive does not run on PRs. Run it only for rare, explicitly requested
extended coverage, for example a particular major refactor. It is not a normal
condition for marking a PR ready, and automation must not request it based on
the subsystem being changed.

`workflow_dispatch` accepts only a branch or tag, and PR branches live in
forks, so dispatch the workflow in the fork that holds the branch:

```bash
gh workflow run Exhaustive-Checks-CI.yml --repo <fork-owner>/lfortran --ref <branch>
gh run list --repo <fork-owner>/lfortran --workflow Exhaustive-Checks-CI.yml \
    --branch <branch> --event workflow_dispatch --limit 1
gh run watch <run-id> --repo <fork-owner>/lfortran
```

For someone else's PR, push its head to a branch in your own fork first:

```bash
gh pr checkout <PR> --repo lfortran/lfortran
git push <your-fork-remote> HEAD:exhaustive-pr-<PR>
gh workflow run Exhaustive-Checks-CI.yml --repo <your-login>/lfortran --ref exhaustive-pr-<PR>
```

Notes:

- Forks have GitHub Actions workflows disabled until enabled once in the
  fork's **Actions** tab.
- The run uses the fork owner's runners, so it does not compete with
  `lfortran/lfortran` CI.
- The result does not appear among the upstream PR's checks. Post the run URL
  and the tested head SHA in the PR, and rerun after new pushes if needed.
- Exhaustive never invokes Quick, and a green Exhaustive result does not imply
  a green Quick result. If the PR's current revision has no successful Quick
  run, dispatch `Quick-Checks-CI.yml` the same way.
- Manual runs do not publish or deploy.

To run the representative integration subset locally from the repository root:

```bash
pixi run -e llvm11 integration_tests -b llvm --smoke > smoke.log 2>&1
pixi run -e llvm11 integration_tests -b llvm --smoke -f > smoke-fast.log 2>&1
```

The explicit list lives in `integration_tests/smoke_tests.cmake`. Selection
happens before targets and configure-time compiler commands are created,
including WASM and implicit-interface tests. Backend support labels and
normal/fast/standard flags still apply. An empty selected backend is an error.
Use the full suite for primary regression coverage; add representative tests
to the list when introducing a new feature or platform-sensitive path.

To leave out specific tests, pass their registered names (the `NAME` of
`RUN` or `COMPILE`) to `--exclude`:

```bash
pixi run -e llvm11 integration_tests -b llvm -f --exclude real128_intrinsic_math_01 real128_intrinsic_math_02 > integration-fast.log 2>&1
```

Excluded tests are removed at configure time, like the smoke selection, so
they are neither built nor run, in every mode (`-f`, `--std=f23`, ...). This
is for working around a test that does not compile in a downstream
environment, such as an installed package. An unregistered name is an error.
The `LFORTRAN_CTEST_EXCLUDE` environment variable (a regular expression) only
skips running tests that were already built.

## Optional installation alternatives

Pixi above is the recommended workflow for local development and testing.
The following recipes are optional for packaging, prebuilt binaries, or
maintaining an existing non-Pixi installation. AI agents must use the Pixi
workflow and must not use these recipes to install project dependencies.

### Binaries

Prebuilt LFortran binaries are also available using Conda.
Install Conda for example by installing the
[Miniconda](https://conda.io/en/latest/miniconda.html) installation by following instructions there for your platform.
Then create a new environment (you can choose any name, here we chose `lf`) and
activate it:
```bash
conda create -n lf
conda activate lf
```
Then install LFortran by:
```bash
conda install lfortran -c conda-forge
```
Now the `lf` environment has the `lfortran` compiler available, you can start the
interactive prompt by executing `lfortran`, or see the command line options using
`lfortran -h`.

#### Note about Conda Installation

When installing LFortran using Conda, multiple copies of the `lfortran`
executable may be present in different locations (for example, in the package
cache). Only the executable inside the active Conda environment should be used.

After activating a conda environment, the correct executable is typically located at:
`$CONDA_PREFIX/bin/lfortran`

To verify which executable is being used, activate a conda environment and run:
`which lfortran`

Other copies located in package directories may not run correctly and can be ignored.

The Jupyter kernel is automatically installed by the above command, so after installing Jupyter itself:
```bash
conda install jupyter -c conda-forge
```
You can create a Fortran based Jupyter notebook by executing:
```bash
jupyter notebook
```
and selecting `New->Fortran`.


### Build From a Source Tarball

Source tarballs remain a supported alternative, especially for packaging
(Spack, Conda, Debian, etc.) or builds with minimal dependencies. They include
all generated sources.

The source tarball of LFortran depends on:

* Python
* cmake
* LLVM 10-19
* zstd-static
* zlib

First we have to install dependencies, for example using Conda:
```bash
conda create -n lf python cmake llvmdev zstd-static zlib
conda activate lf
```

On a Linux system, we additionally need to install `libunwind`:
```bash
conda install libunwind
```

Then download a tarball from
[https://lfortran.org/download/](https://lfortran.org/download/),
e.g.:
```bash
wget https://github.com/lfortran/lfortran/releases/download/v0.42.0/lfortran-0.42.0.tar.gz
tar xzf lfortran-0.42.0.tar.gz
cd lfortran-0.42.0
```
And build:
```bash
cmake -DWITH_LLVM=yes -DCMAKE_INSTALL_PREFIX=`pwd`/inst .
make -j8
make install
```
This will install `lfortran` into `inst/bin`.  It assumes that c++ and cc are available, which on Linux
are typically the GNU C++/C compilers.

### Build From Git

We assume you have C++ compilers installed, as well as `git` and `wget`.
In Ubuntu, you can also install `binutils-dev` for stacktraces.

If you do not have Conda installed, you can do so on Linux (and similarly on
other platforms):
```bash
wget --no-check-certificate https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O miniconda.sh
bash miniconda.sh -b -p $HOME/conda_root
export PATH="$HOME/conda_root/bin:$PATH"
```
Clone the LFortran git repository:
```
git clone https://github.com/lfortran/lfortran.git
cd lfortran
```
Then prepare the environment:
```bash
conda env create -f environment_linux.yml
conda activate lf
```
Generate files that are needed for the build (this step depends on `re2c`, `bison` and `python`):
```bash
./build0.sh
```
Now you can use our script `./build1.sh` to build in Debug mode:
```bash
./build1.sh
```

and can use `ninja` to rebuild.

To do a clean rebuild, you can use:
```bash
# NOTE: the below git command deletes all untracked files
git clean -dfx  # reset repository to a clean state by removing artifacts generated during the build process
./build0.sh
./build1.sh
```

Run an interactive prompt:
```bash
./src/bin/lfortran
```

See [how to run tests](#tests) to make sure all tests pass

### Build from Git on Windows with Visual Studio

Install Visual Studio (MSVC), for example the version 2022, you can download the
Community version for free from: https://visualstudio.microsoft.com/downloads/.

Install miniforge using the Windows installer from https://github.com/conda-forge/miniforge.

Launch the Miniforge Prompt from the Desktop.

In the shell, initialize the MSVC compiler using:

```
call "C:\Program Files\Microsoft Visual Studio\2022\Community\Common7\Tools\VsDevCmd" -arch=x64
```

You can optionally test that MSVC works by:
```
cl /?
link /?
```
Both commands must print help (several pages).

Now you can download and build LFortran:
```
git clone https://github.com/lfortran/lfortran.git
cd lfortran
conda env create -f environment_win.yml
conda activate lf
build0.bat
build1.bat
```

If everything compiled, then you can use LFortran as follows:
```
inst\bin\lfortran examples/expr2.f90
expr2.exe
inst\bin\lfortran
```
And so on.

Note: LFortran currently uses the MSVC's linker program (`link`), which is only
available when the MSVC bat script above is ran. If you forget to activate it,
LFortran's linking will fail.

Note: the miniforge shell seems to be running some version of `git-bash`
(although it is `cmd.exe`), which has some unix-like filesystem mounted in
`/usr` and several commands available such as `ls`, `which`, `git`, `vim`.  For
this reason the Conda build `environment_win.yml` contains everything needed,
including `git`.

### Build from Git on Windows with WSL
* In windows search "turn windows features on or off".

* Tick Windows subsystem for Linux.

* Press OK and restart computer.

* Go to Microsoft store and download Ubuntu (20.04 or 22.04 or 24.04), and launch it.

* Now setup LFortran by running the following commands.

  ```bash
  wget  https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh -O miniconda.sh
  bash miniconda.sh -b -p $HOME/conda_root
  echo "export PATH=$HOME/conda_root/bin:$PATH" >> ~/.bashrc
  ```

* After that restart the Ubuntu terminal.

* Now clone the LFortran git repository (you should clone it inside a linux owned directory like `~` or  any of its sub-directories).

  ```bash
  cd ~
  git clone https://github.com/lfortran/lfortran.git
  cd lfortran
  ```

* Run the following

  ```bash
  conda env create -f environment_linux.yml
  conda init bash
  ```

* Restart Ubuntu terminal again

  ```bash
  conda activate lf
  sudo apt update
  sudo apt-get install build-essential
  sudo apt-get install zlib1g-dev libzstd-dev
  sudo apt install clang
  ```

* Run the following commands

  ```bash
  conda activate lf
  ./build0.sh
  cmake -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=yes -DCMAKE_INSTALL_PREFIX=`pwd`/inst .
  make -j8
  ```

* If everything compiles, you can use LFortran as follows

  ```bash
  ./src/bin/lfortran ./examples/expr2.f90
  ./expr2.out
  ```

* Run an interactive prompt

  ```bash
  ./src/bin/lfortran
  ```

See [how to run tests](#tests) to make sure all tests pass

### Enabling the Jupyter Kernel

A build with LLVM has a Jupyter kernel (`lfortran kernel -f FILE`), LFortran's
own implementation of the protocol, which needs no other library; its
kernelspec is installed with LFortran. To use the Xeus kernel instead, install
the following Conda packages also:
```
conda install xeus=6.0.0 xeus-zmq=4.0.0 nlohmann_json
```
and enable the kernel by `-DWITH_XEUS=yes` and install into `$CONDA_PREFIX`. For
example:
```
cmake \
    -DCMAKE_BUILD_TYPE=Debug \
    -DWITH_LLVM=yes \
    -DWITH_XEUS=yes \
    -DCMAKE_PREFIX_PATH="$CONDA_PREFIX" \
    -DCMAKE_INSTALL_PREFIX="$CONDA_PREFIX" \
    .
cmake --build . -j4 --target install
```
To use it, install Jupyter (`conda install jupyter`) and test that the LFortran
kernel was found:
```
jupyter kernelspec list --json
```
Then launch a Jupyter notebook as follows:
```
jupyter notebook
```
Click `New->Fortran`. To launch a terminal jupyter LFortran console:
```
jupyter console --kernel=fortran
```


### Build From Git with Nix

There's a provided Nix shell for making a consistent build environment with the exact same dependency versions across users.

#### Using the Environment

Enter the development environment:
```bash
nix develop ./ci/nix
```

To change the compilation environment from `gcc` (default) to `clang`:
```bash
nix develop ./ci/nix#clangOnly
```

Depending on your system configuration, you might have to run `nix develop` with the following extra nix features explicitly enabled:
```bash
nix --extra-experimental-features "flakes nix-command" develop ./ci/nix
```

#### Building the Code

The build steps are the same as when building from git:
```bash
./build0.sh
./build1.sh
```

As of 2025-11-10, the environment passes the CI tests, provided you tell it where to install the Jupyter kernel:
```bash
LFORTRAN_CMAKE_GENERATOR=Ninja CONDA_PREFIX=$(pwd) JUPYTER_PATH=$(pwd)/share/jupyter bash ci/build.sh
```
(take note that the Nix shell does not use conda)

Give the same `JUPYTER_PATH` when running `jupyter notebook` / `jupyter lab` to use the same jupyter kernel.

### Note About Dependencies

End users (and distributions) are encouraged to use the tarball
from [https://lfortran.org/download/](https://lfortran.org/download/),
which only depends on LLVM, CMake and a C++ compiler.

The tarball is generated automatically by our CI (continuous integration) and
contains some autogenerated files: the parser, the AST and ASR nodes, which is generated by an ASDL
translator (requires Python).

The instructions from git are to be used when developing LFortran itself.

### Note for users who do not use Conda

Following are the dependencies necessary for installing this
repository in development mode,

- [Bison - 3.5.1](https://ftp.gnu.org/gnu/bison/bison-3.5.1.tar.xz)
- [LLVM - 11.0.1](https://github.com/llvm/llvm-project/releases/download/llvmorg-11.0.1/llvm-11.0.1.src.tar.xz)
- [re2c - 2.0.3](https://re2c.org/install/install.html)
- [binutils - 2.31.90](ftp://sourceware.org/pub/binutils/snapshots/binutils-2.31.90.tar.xz) - Make sure that you should enable the required options related to this dependency to build the dynamic libraries (the ones ending with `.so`).

### Stacktraces

LFortran can print stacktraces when there is an unhandled exception, as well as
on any compiler error with the `--show-stacktrace` option. This is very helpful
for developing the compiler itself to see where in LFortran the problem is. The
stacktrace support is turned off by default, to enable it,
compile LFortran with the `-DWITH_STACKTRACE=yes` cmake option after installing
the prerequisites on each platform per the instructions below.

#### LLVM
In all platforms having LLVM, stacktraces can be shown with LLVM, so no
additional prerequisites are required. If LLVM is not available, you can use
the following instructions, depending on your platform.

#### Ubuntu

In Ubuntu, `apt install binutils-dev`.

#### macOS

If you use the default Clang compiler on macOS, then the stacktraces should
just work on both Intel and M1 based macOS (the CMake build system
automatically invokes the `dsymtuil` tool and our Python scripts to store the
debug information, see `src/bin/CMakeLists.txt` for more details). If it does
not work, please report a bug.

If you do not like the default way, an alternative is to use bintutils. For
that, first install
[Spack](https://spack.io/), then:
```bash
spack install binutils
spack find -p binutils
```
The last command will show a full path to the installed `binutils` package. Add
this path to your shell config file, e.g.:
```bash
export CMAKE_PREFIX_PATH_LFORTRAN=/Users/ondrej/repos/spack/opt/spack/darwin-catalina-broadwell/apple-clang-11.0.0/binutils-2.36.1-wy6osfm6bp2323g3jpv2sjuttthwx3gd
```
and compile LFortran with the
`-DCMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH_LFORTRAN;$CONDA_PREFIX"` cmake option.
The `$CONDA_PREFIX` is there if you install some other dependencies (such as
`llvm`) using Conda, otherwise you can remove it.
