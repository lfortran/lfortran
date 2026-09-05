# LFortran User Guide

## About LFortran

LFortran's goal is an implementation of the latest Fortran standard (currently F2018) with
some added extensions.  It works on Linux, macOS, most BSDs and on Windows.

The LFortran compiler consists of the following components:

* The LFortran compiler itself.
* A runtime library supporting language elements and intrinsic functions.
* Several module definitions which define the external environment and
  supply particular details about the computer running the program.
* The capability to read additional module definitions which define
  the interfaces to external libraries such as OpenMPI.
* Generation of executable code with the LLVM compiler infrastructure.
* Generation of C, C++, WebAssembly or Julia code.
* Interactive usage via LLVM JIT
* Source file formatting (`lfortran fmt`).

## Standards

The LFortran compiler is currently in the alpha stage and is actively under development. It is designed to support the following Fortran standards and modes:

Supported Standards:

* ISO Fortran 2023 (`--std=f23`): Partial support for the latest Fortran 2023 features. Use this option to enable conformance to Fortran 2023 where supported.
  Conditional expressions (10.1.2.3), written `( cond ? a : b )`, are supported and are accepted in every mode. They are executable expressions only: 10.1.11 and 10.1.12 do not list a conditional expression among the primaries a specification expression or a constant expression may contain, so it is rejected in a `KIND=`, a `LEN=`, an array bound and an initialization expression; use `merge` there.
  Conditional arguments (15.5.1, 15.5.2.3) are supported too. In an actual argument position the same syntax selects the actual argument itself, so a consequent that is a variable is passed by reference and stays definable, and a consequent of `.NIL.` leaves an optional dummy argument not present: `call sub( ( x<5 ? x : y ), ( have_edge ? edge : .NIL. ) )`.
* Legacy Mode (`--std=legacy`): Allows compilation of older Fortran codebases that use non-standard or deprecated syntax.
* LFortran Mode (`--std=lf`): Default mode focusing on modern Fortran features, including experimental extensions beyond the current standards.

## Extensions

The extensions are currently in development and are planned to include:

* Global Scope (statements outside of a program block).
* Interactive Fortran (in alpha).
* Jupyter integration.
* Support for GPUs and other accelerators.
* Type inference (`:=` operator and `--infer` mode).

### Type Inference with `:=`

**Experimental, subject to change.**

The `:=` operator declares a new variable whose type is inferred from the
right-hand side. It is closely inspired by Go's short variable declaration
(`:=`), where it is the dominant way to declare local variables. The
rationale is the same: when the right-hand side already communicates the
type, repeating it on the left is redundant noise. `:=` makes declarations
visible at a glance without requiring the programmer to spell out types
that the compiler can deduce.

`:=` is not standard Fortran syntax. It is an LFortran-specific extension,
and the compiler emits a warning on each use to make this clear. The
`--infer` flag suppresses the warning.

```fortran
x := 42              ! integer
y := 3.14d0          ! real(8)
z := (1.0d0, 2.0d0)  ! complex(8)
vals := [1, 2, 3]    ! integer, dimension(3)
p := point_t(0.0, 0.0)  ! type(point_t)
```

#### Rules

`:=` always declares a new variable. It never assigns to an existing one.

```fortran
x := 5
x = 10     ! ok: plain assignment to existing variable
x := 10    ! error: x is already declared in this scope
```

```fortran
integer :: x
x := 5     ! error: x is already declared
```

Inner blocks may shadow outer variables:

```fortran
x := 5
block
    x := "hello"   ! ok: new x in inner scope
end block
! outer x is still integer 5
```

Supported right-hand sides: integer, real, complex, logical, character
scalars; fixed-size arrays when dimensions are compile-time constants;
derived types via structure constructors or typed expressions.
The inferred kind matches the expression (`3.14d0` produces `real(8)`).

#### Guidance

Use `:=` when the type is evident from the right-hand side:

```fortran
cfg := load_config("input.nml")
origin := point_t(0.0, 0.0)
coeffs := [1.0d0, 0.5d0, 0.25d0]
```

Use explicit declarations when the type matters for clarity or safety:

```fortran
real(dp) :: result
result = compute(data)    ! reader sees the type; catches signature changes

integer :: i, n           ! simple counters are clearer with explicit types
```

#### `--infer` mode

`--infer` enables a second, more permissive form of inference: the first
plain `=` to an undeclared name declares it, like a scripting language.
This is intended for interactive and notebook use.

| Feature | Syntax | Declares | Intended for |
|---------|--------|----------|--------------|
| `:=` | `x := expr` | Always | Production code |
| `--infer` | `x = expr` | On first use | Interactive / notebooks |

`:=` works independently of `--infer`. When both are active, either form
declares, but `:=` makes the intent explicit.

## Interactive Compiler

LFortran supports an interactive mode; just run the `lfortran` command
to start it.

## Invoking LFortran

The LFortran compiler supports numerous command-line flags to select
compilation options, output options, link options and so on.

### Compiler information

* `--print-targets`, Print the registered CPU targets
* `--version`, Show the current version

### Source code format

* `--fixed-form`, Parse the file assuming Fortran 66 format (6 spaces)
* `--fixed-form-infer`, Use heuristics to infer if a file is in fixed form

### Source code processing

* `--cpp`, Enable C preprocessing
* `-E`, Preprocess only; do not compile, assemble or link

### Other inputs

* `-D <macro>=<value>` Define a macro (or 1 if <value> omitted)
* `-I <value>`, Include path for `include` statements
* `-L <value>`, Library path for shared libraries
* `-l <value>`, Link library naming a linkable shared library

### Compiler feature selections

* `--fast`, Optimize for best performance on the host CPU (disable strict standard compliance). This implies native CPU selection unless `--march`, `--mcpu`, `--mtune`, or `--target` is specified.
* `--implicit-argument-casting`, Allow implicit argument casting
* `--implicit-interface`, Allow implicit interface
* `--implicit-typing`, Allow implicit typing
* `--openmp`, Enable OpenMP
* `--print-leading-space`, Print leading white space if format is unspecified
* `--realloc-lhs-arrays`, Reallocate left hand side automatically for arrays
* `--disable-realloc-lhs-arrays`, Disables reallocating left hand side automatically for arrays
* `--target <value>`, Generate code for the given target
* `--march <value>`, Select the instruction-set architecture. Use `native` for the host ISA.
* `--mcpu <value>`, Select both the CPU instruction set and scheduling model. Use `native` for the host CPU.
* `--mtune <value>`, Select the CPU scheduling model without changing the instruction set. Use `native` for the host CPU.
* `--backend` flag is used to specify the target backend for code generation in LFortran. The supported backends are:
  
  - `llvm`: The most advanced and default backend, used for generating LLVM IR or machine code through LLVM.
  - `wasm`: For generating `Webassembly` via our custom `wasm` backend.
  - `c`: For generating C code.
  - `cpp`: For generating C++ code (requires the Kokkos library).
  - `x86`: For generating x86 machine code directly (without LLVM).
  - `fortran`: For generating Fortran code.
  - `julia`: For generating Julia code.

### Compiler text outputs

* `--error-format <value>`, Control how errors are produced (human, short)
* `--no-error-banner`, Turn off error banner
* `--no-warnings`, Turn off all warnings
* `-S`, Emit assembly, do not assemble or link
* `--time-report`, Show compilation time report
* `-v`, Be more verbose

### Compiler binary outputs

* `-c`, Compile and assemble, do not link
* `--separate-compilation`, Generate object code into .o files
* `-J <value>`, Where to save mod files
* `-o <value>`, Specify the file to place the compiler's output into
* `--static`, Create a static executable

### Compiler debugging

A number of command-line options select various text outputs useful
for debugging the compiler.  See `lfortran --help` for a list.

## Examples

The following commands and code demonstrate basic operation of the compiler.

```
lfortran helloworld.f90
Hello World!

lfortran -o hw helloworld.f90
./hw
Hello World!

cat helloworld.f90
program hello_world
    implicit none
    write (*, *) 'Hello World!'
end program hello_world

```

Here is a simple example with a module:

```
lfortran -c varray.f90
lfortran usev.f90
 sum is    7.20000000e+01

cat varray.f90
module varray
    integer :: nsize
end module varray

cat usev.f90
program usev
    use varray
    real, allocatable, dimension(:) :: A
    integer :: i
    nsize = 8
    allocate(A(nsize))
    do i = 1, nsize
        A(i) = 2.0*i
    end do
    print *, " sum is ", (A(1)+A(nsize))*nsize/2.0
    deallocate(A)
end program usev

```


The compile command for the module requires `-c` to avoid automatic running
of the code.


## Formatting Fortran source files

The `lfortran` compiler will automatically format source files with the `fmt`
option.  You can select auto-indent for modules, and in-place update of
the Fortran source file with the `-i` option (use with caution!).

```
lfortran fmt varray.f90
module varray
integer :: nsize
real, allocatable, dimension(:) :: A
end module varray
```

Or add spaces and indentation as follows:

```
lfortran fmt --spaces 4 --indent-unit varray.f90
module varray
    integer :: nsize
    real, allocatable, dimension(:) :: A
end module varray
```

## Selecting the C Compiler (Link Driver)

Producing an executable (for example `lfortran hw.f90` or
`lfortran -o hw hw.o`) requires a C compiler driver, because the LLVM
backend invokes one at run time to link the generated object files against
the LFortran runtime library. Any standard C compiler driver works — there
is no clang-specific coupling, the driver only supplies the platform glue
(startup object files, the C library, default library search paths).

The `--linker`/`LFORTRAN_LINKER` selection described here only affects
the final link step of the LLVM backend. Separately, the C and C++
backends (`--backend=c`, `--backend=cpp`) compile the generated C/C++
source with the driver from the `LFORTRAN_CC` environment variable
(default `cc`).

If no driver is selected with `--linker` or `LFORTRAN_LINKER`, LFortran
uses a fixed per-platform default driver (`clang` on macOS, `gcc` on
Windows MinGW targets, `cc` on Linux and other Unix systems, where it is
normally provided by the system gcc). LFortran deliberately does not
search `$PATH` for a driver on every invocation and does not consult the
standard `CC` environment variable — build tools and CMake often export
`CC` values (toolchain-internal paths, extra flags, cross compilers) that
belong to their own context and are not valid link drivers for LFortran.
The selected name is resolved by the shell when the link command runs;
if it does not exist, LFortran reports that the linker command was not
found and how to select a different one.

A non-standard directory containing the driver can be selected with
`--linker-path` or `LFORTRAN_LINKER_PATH`; that directory is verified
up front, so a misplaced `--linker-path` fails with an explicit,
actionable error message instead of a shell error:

```
lfortran hw.f90                      # platform default driver
lfortran hw.f90 --linker=gcc         # use gcc
export LFORTRAN_LINKER=gcc
lfortran hw.f90                      # same via environment variable
export LFORTRAN_LINKER="ccache clang"
lfortran hw.f90                      # wrappers and extra arguments work
export LFORTRAN_LINKER_PATH=/usr/local/bin
export LFORTRAN_LINKER=gcc-13
lfortran hw.f90                      # use /usr/local/bin/gcc-13
```

Only the final link step needs a C compiler driver: interactive mode,
the `--show-*` text outputs and compiling to object files (`-c`) do not
need one; WASM targets either need no driver or use user-provided
toolchains (`EMSDK_PATH`, `WASI_SDK_PATH`). On Windows the default link
path uses MSVC's `link` instead.

The one exception to "any driver works" is the Metal GPU backend
(`--gpu=metal`): it compiles its Objective-C runtime with the same
driver, and only clang can compile Objective-C, so that backend requires
clang.

## Debug Line Information

When compiling with `-g`, LFortran additionally generates side files with
line information used to print source line numbers in runtime stacktraces.
This step uses the external LLVM tools `llvm-dwarfdump` (and `dsymutil` on
macOS, which ships with the Xcode command line tools) plus Python. This
step is optional: if any of these tools are missing or the step otherwise
fails, LFortran prints a warning and continues — the executable is still
built with the DWARF debug information emitted by LLVM, only the line
numbers in runtime stacktraces are missing. Install the LLVM tools (for
example `conda install llvm-tools`) to enable them.

## Differences from other compilers

GNU, Intel and LLVM Fortran use "standard" Fortran carriage control where the
first character of each output line controls a conceptual "line printer".  A
space " " means
single-space, a zero "0" means double-space and one "1" means form-feed
before printing.  This is obsolete and LFortran omits this unless
`--print-leading-space` is selected at compile-time.

LFortran does not currently expose arbitrary Clang options such as `-flto`.
Use `--fast` and the target-selection options above for native optimization.

GNU extension declarations `real*8 xvalue` are accepted but deprecated. This
was never standard-conforming Fortran; it is an old IBM extension that predates Fortran-77.
