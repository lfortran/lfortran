# Installation

All the instructions below work on Linux, macOS and Windows.

## Binaries

The recommended way to install LFortran is using Conda.
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

### Note about Conda Installation

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


## Build From a Source Tarball

This method is the recommended method if you just want to install LFortran, either yourself or in a package manager (Spack, Conda, Debian, etc.). The source tarball has all the generated files included and has minimal dependencies.

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

## Build From Git

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

See [how to run tests](#Tests) to make sure all tests pass

## Build from Git on Windows with Visual Studio

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

## Build from Git on Windows with WSL
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

See [how to run tests](#Tests) to make sure all tests pass

## Enabling the Jupyter Kernel

To install the Jupyter kernel, install the following Conda packages also:
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


## Build From Git with Nix

There's a provided Nix shell for making a consistent build environment with the exact same dependency versions across users.

### Using the Environment

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

### Building the Code

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

## Note About Dependencies

End users (and distributions) are encouraged to use the tarball
from [https://lfortran.org/download/](https://lfortran.org/download/),
which only depends on LLVM, CMake and a C++ compiler.

The tarball is generated automatically by our CI (continuous integration) and
contains some autogenerated files: the parser, the AST and ASR nodes, which is generated by an ASDL
translator (requires Python).

The instructions from git are to be used when developing LFortran itself.

## Note for users who do not use Conda

Following are the dependencies necessary for installing this
repository in development mode,

- [Bison - 3.5.1](https://ftp.gnu.org/gnu/bison/bison-3.5.1.tar.xz)
- [LLVM - 11.0.1](https://github.com/llvm/llvm-project/releases/download/llvmorg-11.0.1/llvm-11.0.1.src.tar.xz)
- [re2c - 2.0.3](https://re2c.org/install/install.html)
- [binutils - 2.31.90](ftp://sourceware.org/pub/binutils/snapshots/binutils-2.31.90.tar.xz) - Make sure that you should enable the required options related to this dependency to build the dynamic libraries (the ones ending with `.so`).

## Stacktraces

LFortran can print stacktraces when there is an unhandled exception, as well as
on any compiler error with the `--show-stacktrace` option. This is very helpful
for developing the compiler itself to see where in LFortran the problem is. The
stacktrace support is turned off by default, to enable it,
compile LFortran with the `-DWITH_STACKTRACE=yes` cmake option after installing
the prerequisites on each platform per the instructions below.

### LLVM
In all platforms having LLVM, stacktraces can be shown with LLVM, so no
additional prerequisites are required. If LLVM is not available, you can use
the following instructions, depending on your platform.

### Ubuntu

In Ubuntu, `apt install binutils-dev`.

### macOS

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


## Tests

#### Run tests:

```bash
ctest
./run_tests.py
```

#### Update test references:

```bash
./run_tests.py -u
```

#### Run integration tests

```bash
cd integration_tests
./run_tests.py
```

#### Speed up integration tests on macOS

Integration tests run slowly because Apple checks the hash of each executable online before running.

You can turn off that feature in the Privacy tab of the Security and Privacy item of System Preferences > Developer Tools > Terminal.app > "allow the apps below to run software locally that does not meet the system's security
policy."

#### CI coverage

Pull requests normally run only **Quick checks**. Quick uses the same
builds, test suites and selection rules on PRs, main pushes, release tags and manual
runs. Publishing steps remain push-only. Main runs Quick plus Exhaustive.
Exhaustive adds configurations and broader suites, never another invocation
of Quick, and runs identically on main, on labeled PRs and on manual dispatch.

The shared native compiler workflow has two explicit coverage roles:

| Role | Caller | Native LLVM matrix |
| --- | --- | --- |
| `quick` | Quick on every event | Linux 7/11/23 |
| `exhaustive` | Exhaustive on every event | Linux 7/8/10/11/15/17/18/19/21/22/23 and macOS 22 |

Full LLVM-WASM, no-LLVM and MLIR suites belong directly to Quick on every event.
They are not declared in Exhaustive or its shared compiler workflow, so there
are no duplicate or skipped Exhaustive copies of these jobs.

Quick distributes the full CPU modes over two existing builds rather than
serializing them in one long job:

| Compiler | Complete regression modes |
| --- | --- |
| Linux/LLVM 11 Debug | Normal, `--fast`, Fortran 2023 normal/fast, full references, small LLVM variants, submodules and single invocation |
| Linux/LLVM 21 Debug | Separate compilation, submodules with separate compilation, and leak detection |

Every registered LLVM test runs in each of these modes on its designated
compiler. Both are Debug builds, so every full Quick suite runs with
assertions and per-pass ASR verification. Both also use the platform C/C++
diagnostic and standard-library hardening flags, including `-Werror`, and
`WITH_INTERNAL_ALLOC_CHECK=yes`. Splitting the modes across two
jobs reduces the critical path without sampling those suites or adding
another dependent job/queue.

The Linux/LLVM 11 platform build retains full reference coverage, platform smoke tests,
and the full GFortran, C/C++, Fortran, direct-WASM, OpenMP and CUDA-on-CPU
backend suites. Linux/LLVM 21 Debug also runs normal/fast smoke coverage, and
Linux/LLVM 7/23 Release provide additional smoke coverage. macOS/LLVM 11 keeps
platform smoke coverage and
the full Metal and CUDA-on-CPU suites. Windows keeps its native Release
build and supported compile/link/run checks. Caffeine/coarrays run on the
LLVM 11 Debug compatibility compiler. The standalone compiler-to-WASM build is also retained.

Exhaustive runs the full compatibility suites on every LLVM version except
11 and 19, which run the application catalog instead, as does macOS LLVM 22;
every compatibility job runs Caffeine/coarrays. Three supplemental platform builds also run full normal/fast suites on Linux
LLVM 11/21 Debug and full normal/reference suites on macOS LLVM 11 Debug.
These retain the full platform coverage that used to run only in main's Quick.
Quick and Exhaustive share `.github/actions/build-platform` so these compiler
configurations cannot drift. The supplemental jobs do not rerun Quick's GPU,
alternate-backend or descriptor-mode suites; Linux references stay in Quick.

All native compatibility profiles enable runtime-stacktrace support, including
the LLVM 11/19 and macOS application compilers. Caffeine 0.8.2 removes LFortran
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

Ordinary PRs run only the small gate of the standalone Exhaustive workflow;
its compiler jobs require an explicit request.

##### Required-check rollout

By default, the existing protected `Build LFortran to WASM and Upload` status
still aggregates every Quick job. This is safe with the existing branch
protection, but its tiny final job can wait for a runner after all real work
has finished. Changing its runner size cannot bypass account-wide concurrency
limits.

To eliminate that final runner job, first deploy this workflow version with
the legacy gate still enabled. A repository administrator can then migrate
to direct required checks. **Do not enable the variable before updating
protection.** Keep the four existing platform requirements and add the seven
compatibility/backend requirements below, retaining the expected GitHub Actions
app binding (currently app ID `15368`). All eleven real-work contexts must
remain required, in addition to the legacy aggregate during the transition:

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
Compiler compatibility / Test without LLVM Backend
Compiler compatibility / Test MLIR backend
```

Verify those requirements and their app binding on a fresh PR run **before**
setting the repository Actions variable `LFORTRAN_DIRECT_REQUIRED_CHECKS`
to `true`. Then verify another fresh PR run with all eleven contexts still
required. Job names stay stable; the legacy summary is skipped without a runner.
Only after verifying direct protection may an administrator remove the old
`Build LFortran to WASM and Upload` requirement. Do not remove any of the four
platform requirements: they own backend, GPU, reference and full descriptor-mode
coverage that the compatibility jobs do not replace. Exhaustive uses the
distinct `Extended compiler checks` prefix, so an optional Exhaustive result
cannot substitute for a required Quick result. Existing PRs may need their
checks refreshed after a protection change; a manual-dispatch run alone is
not evidence that a PR's required checks are satisfied.

A [conditionally skipped job reports success and does not block merging even
when required](https://docs.github.com/en/actions/how-tos/write-workflows/choose-when-workflows-run/control-jobs-with-conditions).
Thus the old aggregate requirement may remain while its job is skipped, but
then it provides **no protection** for failed dependencies. This differs from a
missing check or a [whole workflow skipped by branch/path/commit filtering,
whose required checks remain pending](https://docs.github.com/en/pull-requests/how-tos/merge-and-close-pull-requests/troubleshooting-required-status-checks).
Do not rely on a skipped summary to validate the migration.

For rollback, clear the variable **but keep all direct requirements in place**.
Restore the legacy aggregate requirement, with its GitHub Actions app binding,
if it was removed.
Changing a variable does not replace completed checks: an old direct-mode
summary is still skipped, even if a compatibility job failed.
Drain outstanding direct-mode runs, then rerun Quick for every active PR's
current revision. Verify that the protected status comes from an executed,
successful `quick_status` aggregate, not an old skipped result, before
optionally removing the seven newly added direct requirements. Keep the four
platform requirements throughout rollback as well. Leaving all eleven direct
requirements in place is safe and adds no runner work.

Without this explicit migration, the workflow retains its safe aggregate
default; code alone cannot remove its queue while preserving the old settings.

**Third-party applications generate bugs for the integration suite; they are
not part of ordinary PR checks.** The application catalog runs on every push
to `main`, where it both finds coverage gaps and demonstrates compatibility
with real applications, and in every explicitly requested Exhaustive run.
There is no automatic exception for changes to serialization, finalization,
I/O or GPU lowering.

Caffeine is different: it supplies the coarray runtime backend. Building it,
running its own LFortran-compiled unit tests and running every registered coarray
capability test remain part of Quick, just as
Metal and CUDA-on-CPU integration tests validate particular backends and
platforms. Toolchain/runtime dependencies are not the application catalog.

`ci/test_caffeine.sh` uses Caffeine 0.8.2 and its generated `run-fpm.sh` wrapper,
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
dependencies also request reference validation. Unrelated integration registrations
and compiler-only changes do not. The same comparison rule applies on every event;
there is no reduced PR-only LFortran selection.

When those inputs are demonstrably unchanged, neither shared-workflow setup nor
the Caffeine script installs OpenMPI/OpenCoarrays for Quick, and no `caf`/`cafrun`
checks run. Caffeine uses GASNet's SMP conduit, not MPI. Missing or inconsistent
history (including manual runs, new refs or unavailable push bases), dirty
checkouts and unresolved source dependencies log **conservative reference
validation**, never an unexplained skip. The source dependency guard keeps quoted
text and trailing comments rather than guessing where a Fortran comment starts.
Split tokens and continued character literals request full reference validation;
it does not attempt to parse arbitrary Fortran to prove dependencies unchanged.
Invalid test registrations fail explicitly.
Standalone/default and Exhaustive invocations always request full Linux reference
validation. macOS retains its existing no-OpenCoarrays behavior; Caffeine unit,
smoke and all LFortran integration tests still run.

When an application finds a compiler bug, reduce the failure to a registered
integration regression in the relevant modes, fix the compiler, and verify
the original application failure. Promptly fix or revert a regression on main.
The lasting protection for future PRs is the integration test, not adding the
whole application to Quick. Finding such a gap on main is an accepted trade-off,
not a reason to silently ignore the failing application check.

Every main push keeps the full LLVM matrix, full platform suites, application,
documentation, packaging and JupyterLite checks. Main runs are not automatically
cancelled or rotated. Maintainers may cancel older runs manually when runners
are saturated, keeping the latest run.

**Releases require green main, including application validation.** The commit
selected for release must have passed the full main CI. A green Quick PR or
extended compiler run is not a substitute. Release-tag workflows still run
compiler, documentation and packaging checks; they do not repeat the application
catalog already validated on main.

Use `Tests::Run-Exhaustive` only for rare, explicitly requested extended compiler
coverage, for example a particular major refactor. It is not a normal condition
for marking a PR ready, and automation must not apply it based on the subsystem
being changed. Add it with:

```bash
gh pr edit <PR> --repo lfortran/lfortran --add-label Tests::Run-Exhaustive
```

The label controller reruns the current PR revision's Exhaustive workflow,
whose gate reads the live labels. Subsequent pushes run extended checks while
the label remains present. Unrelated label changes do not replace the result.
GitHub cannot rerun workflows older than 30 days; push a new commit or close
and reopen an older PR before requesting these checks.

Alternatively, explicitly dispatch checks on your fork. Run Quick as well if
the same revision does not already have a successful Quick result:

```bash
gh workflow run Quick-Checks-CI.yml --repo <fork-owner>/lfortran --ref <branch>
gh workflow run Exhaustive-Checks-CI.yml --repo <fork-owner>/lfortran --ref <branch>
gh run list --repo <fork-owner>/lfortran --branch <branch> --event workflow_dispatch
gh run watch <run-id> --repo <fork-owner>/lfortran
```

Check both workflows' results and head SHAs. Labeled and manually dispatched
Exhaustive runs are purely supplemental: neither invokes Quick. A green
Exhaustive result alone does not imply a green Quick result. Manual runs do
not publish or deploy. Manual fork runs need not
appear among the upstream PR's checks.

To run the representative integration subset locally:

```bash
cd integration_tests
./run_tests.py -b llvm --smoke > smoke.log 2>&1
./run_tests.py -b llvm --smoke -f > smoke-fast.log 2>&1
```

The explicit list lives in `integration_tests/smoke_tests.cmake`. Selection
happens before targets and configure-time compiler commands are created,
including WASM and implicit-interface tests. Backend support labels and
normal/fast/standard flags still apply. An empty selected backend is an error.
Use the full suite for primary regression coverage; add representative tests
to the list when introducing a new feature or platform-sensitive path.
