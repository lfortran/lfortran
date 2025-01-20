#!/usr/bin/env shell
# This is a cross-platform `shell` script.

set -ex

echo "Running SHELL"

# Run some simple compilation tests, works everywhere:
src/bin/lfortran --version
# Compile and link separately
src/bin/lfortran -c examples/expr2.f90 -o expr2.o
src/bin/lfortran -o expr2 expr2.o
./expr2

# Compile and link in one step
src/bin/lfortran integration_tests/intrinsics_04s.f90 -o intrinsics_04s
./intrinsics_04s

src/bin/lfortran integration_tests/intrinsics_04.f90 -o intrinsics_04
./intrinsics_04

# Run all tests (does not work on Windows yet):
cmake --version
if [[ $WIN != "1" ]]; then
    ./run_tests.py

    cd integration_tests
    mkdir build-lfortran-llvm
    cd build-lfortran-llvm
    FC="../../src/bin/lfortran" cmake -DLFORTRAN_BACKEND=llvm -DCURRENT_BINARY_DIR=. ..
    make
    ctest -L llvm
    cd ..

    ./run_tests.py -b llvm2 llvm_rtlib llvm_nopragma
    ./run_tests.py -b llvm2 llvm_rtlib llvm_nopragma -f
fi
