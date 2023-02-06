#!/usr/bin/env xonsh

$RAISE_SUBPROC_ERROR = True
trace on

import platform
$IS_MAC = platform.system() == "Darwin"
$IS_WIN = platform.system() == "Windows"

# Run some simple compilation tests, works everywhere:
src/bin/lfortran --version
# Compile and link separately
src/bin/lfortran -c examples/expr2.f90 -o expr2.o
src/bin/lfortran -o expr2 expr2.o
./expr2

# Compile C and Fortran
src/bin/lfortran -c integration_tests/modules_15b.f90 -o modules_15b.o
src/bin/lfortran -c integration_tests/modules_15.f90 -o modules_15.o

if $IS_WIN:
    cl /MD /c integration_tests/modules_15c.c /Fomodules_15c.o
elif $IS_MAC: # macOS
    clang -c integration_tests/modules_15c.c -o modules_15c.o
else:
    # Linux
    gcc -c integration_tests/modules_15c.c -o modules_15c.o

src/bin/lfortran modules_15.o modules_15b.o modules_15c.o -o modules_15
./modules_15


# Compile and link in one step
src/bin/lfortran integration_tests/intrinsics_04s.f90 -o intrinsics_04s
./intrinsics_04s

src/bin/lfortran integration_tests/intrinsics_04.f90 -o intrinsics_04
./intrinsics_04


# Run all tests (does not work on Windows yet):
cmake --version
if not $IS_WIN:
    ./run_tests.py -s

    cd integration_tests
    mkdir build-lfortran-llvm
    cd build-lfortran-llvm
    $FC="../../src/bin/lfortran"
    cmake -DLFORTRAN_BACKEND=llvm ..
    make
    ctest -L llvm
    cd ..

    ./run_tests.py -b llvm2
    ./run_tests.py -b llvm_rtlib
