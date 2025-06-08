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
```
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
conda install xeus=5.1.0 xeus-zmq=3.0.0 nlohmann_json
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

One of the ways to ensure exact environment and dependencies is with `nix`. This will ensure that system dependencies do not interfere with the development environment. If you want, you can report bugs in a `nix-shell` environment to make it easier for others to reproduce.

### With Root

We start by getting `nix`. The following multi-user installation will work on any machine with a Linux distribution, MacOS or Windows (via WSL):
```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```
### Without Root

If you would like to not provide `nix` with root access to your machine, on Linux distributions we can use [nix-portable](https://github.com/DavHau/nix-portable).
```bash
wget https://github.com/DavHau/nix-portable/releases/download/v003/nix-portable
```
Now just prepend all `nix-shell` commands with `NP_RUNTIME=bwrap ./nix-portable `. So:
```bash
# Do not
nix-shell --run "bash"
# Do
NP_RUNTIME=bwrap ./nix-portable nix-shell --run "bash"
```

### Development

Now we can enter the development environment:
```bash
nix-shell --run "bash" --cores 4 -j4 --pure ci/shell.nix
```
The `--pure` flag ensures no system dependencies are used in the environment.

The build steps are the same as with the `ci`:
```bash
./build0.sh
./build1.sh
```

To change the compilation environment from `gcc` (default) to `clang` we can use `--argstr`:
```bash
nix-shell --run "bash" --cores 4 -j4 --pure ci/shell.nix --argstr clangOnly "yes"
```

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
```
spack install binutils
spack find -p binutils
```
The last command will show a full path to the installed `binutils` package. Add
this path to your shell config file, e.g.:
```
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
