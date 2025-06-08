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

* ISO Fortran 2023 (Provisional, `--std=f23`): Partial support for the latest Fortran 2023 features. Use this option to enable conformance to Fortran 2023 where supported.
* Legacy Mode (`--std=legacy`): Allows compilation of older Fortran codebases that use non-standard or deprecated syntax.
* LFortran Mode (`--std=lf`): Default mode focusing on modern Fortran features, including experimental extensions beyond the current standards.

## Extensions

The extensions are currently in development and are planned to include:

* Global Scope (statements outside of a program block).
* Interactive Fortran (in alpha).
* Jupyter integration.
* Support for GPUs and other accelerators

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

* `--fast`, Best performance (disable strict standard compliance)
* `--implicit-argument-casting`, Allow implicit argument casting
* `--implicit-interface`, Allow implicit interface
* `--implicit-typing`, Allow implicit typing
* `--openmp`, Enable OpenMP
* `--print-leading-space`, Print leading white space if format is unspecified
* `--realloc-lhs`, Reallocate left hand side automatically
* `--target <value>`, Generate code for the given target
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
* `--generate-object-code`, Generate object code into .o files
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

## Selecting the C Compiler

By default LFortran uses the `clang` compiler.  On some systems
the compiler has a version number or spelling difference.  The compiler
can be changed with the `LFORTRAN_CC` symbol:

```
unset LFORTRAN_CC
lfortran hw.f90
Hello World!

export LFORTRAN_CC=gcc
lfortran hw.f90
Hello World!

export LFORTRAN_CC=clang-14
lfortran hw.f90
sh: clang-14: not found
...(further error messages)...
```

## Differences from other compilers

GNU, Intel and LLVM Fortran use "standard" Fortran carriage control where the
first character of each output line controls a conceptual "line printer".  A
space " " means
single-space, a zero "0" means double-space and one "1" means form-feed
before printing.  This is obsolete and LFortran omits this unless
`--print-leading-space` is selected at compile-time.

There is currently no way to specify detailed compiler options to
Clang such as `-O3` or `-flto` (optimization and link-time optimization).

GNU extension declarations `real*8 xvalue` are accepted but deprecated.  This
is valid Fortran-77 but not Fortran-2018.


