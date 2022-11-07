# Installation

All the instructions below work on Linux, macOS and Windows. The building process consists of two
steps: first we build the compiler, second we build the runtime (written in Fortran). If you want
to build a cross compiler you only need to enable support for the target architecture
(e.g. `-DWITH_TARGET_AARCH64`) on the first step and pass a CMake toolchain file on the second step
(we will provide them). See the cross compilation section for more instructions.

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

First we have to install dependencies, for example using Conda:
```bash
conda create -n lf python cmake llvmdev
conda activate lf
```
Then download a tarball from
[https://lfortran.org/download/](https://lfortran.org/download/),
e.g.:
```bash
wget https://lfortran.github.io/tarballs/dev/lfortran-0.9.0.tar.gz
tar xzf lfortran-0.9.0.tar.gz
cd lfortran-0.9.0
```
And build:
```
# First we build the compiler
cmake -DWITH_LLVM=yes -DCMAKE_INSTALL_PREFIX=`pwd`/inst .
make -j8
make install

# Now we build the runtime providing the compiler we just build
cmake -DCMAKE_Fortran_COMPILER=`pwd`/inst/bin/lfortran -DWITH_RUNTIME_LIBRARY=Yes
make -j8
make install
```
This will install the `lfortran` into the `inst/bin`.

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
Then prepare the environment:
```bash
conda create -n lf -c conda-forge llvmdev=11.0.1 bison=3.4 re2c python cmake make toml
conda activate lf
```
Clone the LFortran git repository:
```
git clone https://github.com/lfortran/lfortran.git
cd lfortran
```
Generate files that are needed for the build (this step depends on `re2c`, `bison` and `python`):
```bash
./build0.sh
```
Now the process is the same as installing from the source tarball. For example to build in Debug mode:
```
# First we build the compiler
cmake -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=yes -DCMAKE_INSTALL_PREFIX=`pwd`/inst .
make -j8
make install

# Now we build the runtime providing the compiler we just build
cmake -DCMAKE_Fortran_COMPILER=`pwd`/inst/bin/lfortran -DWITH_RUNTIME_LIBRARY=Yes
make -j8
make install
```

Run tests:
```bash
ctest
./run_tests.py
```
Run an interactive prompt:
```bash
./src/bin/lfortran
```

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
a.out
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
* Go to Microsoft store and download Ubuntu 20.04, and launch it.
* Run the following commands.

```bash
wget  https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh -O miniconda.sh
bash miniconda.sh -b -p $HOME/conda_root
export PATH="$HOME/conda_root/bin:$PATH"
```
* Now do the following to configure the path
```bash
sudo nano .bashrc
```
* Then go to the bottom of the file and paste the following
```bash
export PATH="$HOME/conda_root/bin:$PATH"
```
* Then press ctrl + O (save), Enter (confirm), ctrl + X (exit)
* After that restart Ubuntu
* Run the following
```bash
conda create -n lf -c conda-forge llvmdev=11.0.1 bison=3.4 re2c python cmake make toml
conda init bash
```
* Restart Ubuntu again
```bash
conda activate lf
sudo apt update
sudo apt-get install build-essential
sudo apt-get install zlib1g-dev
sudo apt install clang
```
* You can change the directory to a Windows location using `cd /mnt/[drive letter]/[windows location]`.
* e.g. `cd mnt/c/Users/name/source/repos/`

* Now clone the LFortran git repository
```bash
git clone https://github.com/lfortran/lfortran.git
cd lfortran
```

* Run the following commands
```bash
# First we build the compiler 
conda activate lf
./build0.sh
cmake -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=yes -DCMAKE_INSTALL_PREFIX=`pwd`/inst .\
make -j8
make install

# Now we build the runtime providing the compiler we just build
cmake -DCMAKE_Fortran_COMPILER=`pwd`/inst/bin/lfortran -DWITH_RUNTIME_LIBRARY=Yes
make -j8
make install
```

* If everything compiles, you can use LFortran as follows
```bash
./src/bin/lfortran ./examples/expr2.f90
./a.out
```

* Run an interactive prompt
```bash
./src/bin/lfortran
```

* Run tests
```bash
ctest
./run_tests.py
```

## Enabling the Jupyter Kernel

To install the Jupyter kernel, install the following Conda packages also:
```
conda install xeus xtl nlohmann_json cppzmq
```
and enable the kernel by `-DWITH_XEUS=yes` and install into `$CONDA_PREFIX`. For
example:
```
# First we build the compiler
cmake \
    -DCMAKE_BUILD_TYPE=Debug \
    -DWITH_LLVM=yes \
    -DWITH_XEUS=yes \
    -DCMAKE_PREFIX_PATH="$CONDA_PREFIX" \
    -DCMAKE_INSTALL_PREFIX="$CONDA_PREFIX" \
    .
cmake --build . -j4 --target install

# Now we build the runtime providing the compiler we just build
cmake -DCMAKE_Fortran_COMPILER=lfortran -DWITH_RUNTIME_LIBRARY=Yes
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

## Cross compilation
LFortran can work as a cross compiler. The instructions here are for the platforms known to work, other platforms might work too, specially if they are not too different (e.g. compiling to iOS from an arm64 Mac instead of an Intel one).

Cross compilation can be complicated but LFortran makes it relatively simple. The main things you need to know are: 

The "host" system is where you are running the compiler. The "target" system is where the generated binaries will be ran. For example if you are cross compiling from macOS to iOS, macOS is the host system and iOS is the target.

LFortran can only cross compile platform independent code: Your software shouldn't rely on hardcoded platform specific paths like `/usr/lib/x86_64-linux-gnu`, which is a system directory specific to Linux x86_64 and won't be available on Linux arm64 systems. Similarly on iOS, Apple disables the `system()` syscall to run commands on a shell.

LFortran can only cross compile Fortran code: If your application depends on auxiliary C or C++ code you will also need to cross compile that and make it available to LFortran during compilation.

### Linux x86\_64 -> arm64
These steps use the gcc cross compilation toolchain but clang should work too. To install gcc-aarch64-linux-gnu on Debian based systems use:

```bash
sudo apt install gcc make gcc-aarch64-linux-gnu binutils-aarch64-linux-gnu g++-aarch64-linux-gnu
```

First we need to build lfortran as usual.

```
conda create -n lf-cross -c conda-forge llvmdev=11.0.1 bison=3.4 re2c python cmake make toml
conda activate lf-cross

# First we build the compiler
./build0.sh
mkdir build && cd build

cmake -GNinja -DCMAKE_BUILD_TYPE=Debug \
              -DWITH_LLVM=yes \
              -DWITH_TARGET_AARCH64=Yes \
              -DCMAKE_INSTALL_PREFIX=`pwd`/../inst ..

ninja install

# Now we build the runtime providing the compiler we just build
cmake -GNinaj -DCMAKE_Fortran_COMPILER=../inst/bin/lfortran \
              -DWITH_RUNTIME_LIBRARY=Yes

ninja install
```

We now have a working cross compiler but we still need to build the runtime for the target system. You will need the cmake [toolchain](https://gist.github.com/meow464/00bd0b89567e9c621846b3b3a74b589e) file.

```bash
mkdir build_aarch64 && cd build_aarch6

cmake -GNinja \
      -DCMAKE_TOOLCHAIN_FILE=./aarch64-linux-gnu.toolchain.cmake\
      -DCMAKE_BUILD_TYPE=Debug\
      -DWITH_LLVM=yes\
      -DCMAKE_INSTALL_PREFIX="$(pwd)/../inst_aarch64"\
      -DCMAKE_Fortran_COMPILER="$(pwd)/../inst/bin/lfortran"\
      -DWITH_ZLIB=NO\
      -DWITH_RUNTIME_LIBRARY_ONLY=YES\
      ..
	  
ninja install
```

Now we need to move our cross compiler and cross compiled runtime together:
```bash
cp  inst/bin/lfortran inst_aarch64/bin/lfortran
```

You can now cross compile Fortran binaries with:

```bash
LFORTRAN_CC=/usr/bin/aarch64-linux-gnu-gcc lfortran --static --link-with-gcc --target arm64-linux
```

`LFORTRAN_CC` should point to the gcc cross compiler, the path might vary from distro to distro. You don't need to use `--static` but then you will have to copy `liblfortran_runtime.so` to the appropriate location on the target system.

### macOS x86\_64 -> iOS arm64
You need to install Xcode from the App Store and `xcode-select --install`.

First we need to build lfortran as usual.

```bash
conda create -n lf-cross -c conda-forge llvmdev=11.0.1 bison=3.4 re2c python cmake make toml
conda activate lf-cross
# First we build the compiler
./build0.sh
mkdir build && cd build

cmake -GNinja -DCMAKE_BUILD_TYPE=Debug \
              -DWITH_LLVM=yes \
              -DWITH_TARGET_AARCH64=Yes \
              -DCMAKE_INSTALL_PREFIX=`pwd`/../inst .. 

ninja install

# Now we build the runtime providing the compiler we just build
cmake -DCMAKE_Fortran_COMPILER=../inst/bin/lfortran \
      -DWITH_RUNTIME_LIBRARY=Yes ..

ninja install
```

We now have a working cross compiler but we still need to build the runtime for the target system. You will need the cmake [toolchain](https://github.com/leetal/ios-cmake) file.

```bash
mkdir build_aarch64 && cd build_aarch64

cmake -GNinja \
      -DCMAKE_BUILD_TYPE=Debug \
      -DWITH_LLVM=yes \
      -DCMAKE_INSTALL_PREFIX=`pwd`/../iphone_inst \
      -DWITH_ZLIB=No \
      -DWITH_RUNTIME_LIBRARY_ONLY=Yes \
      -DCMAKE_Fortran_COMPILER=../inst/bin/lfortran \
      -DCMAKE_TOOLCHAIN_FILE=../ios.toolchain.cmake \
      -DPLATFORM=OS64 \
      -DENABLE_BITCODE=No ..

ninja install
```

Now we need to move our cross compiler and cross compiled runtime together:
```bash
cp inst/bin/lfortran iphone_inst/bin/lfortran
``` 

You can now cross compile Fortran binaries with:
```bash
SDKROOT=$(xcrun --sdk iphoneos --show-sdk-path) ./iphone_inst/bin/lfortran --target arm64-apple-ios --generate-object-code -c my_library.f90 -o my_library.o
```

Now you only need to copy (drag and drop) `iphone_inst/share/lfortran/lib/liblfortran_runtime_static.a` and `my_library.o` to your Xcode project.

Note that we specified `-c` in the command line to disable linking. Otherwise lfortran would try to make an executable file, and not a library, and that would conflict with the `main()` function in the iOS app.

#### Troubleshooting
If you receive the error `xcrun: error: SDK "iphoneos" cannot be located`, just run `sudo xcode-select --switch /Applications/Xcode.app`.

If Xcode gives an error related to bitcode not being present on the lfrotran generated binary, simply disable it for the entire app. Apple [deprecated embedded bitcode](https://developer.apple.com/documentation/xcode-release-notes/xcode-14-release-notes) as of Xcode 14
