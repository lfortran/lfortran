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

# Compile C and Fortran
src/bin/lfortran -c integration_tests/modules_15b.f90 -o modules_15b.o
src/bin/lfortran -c integration_tests/modules_15.f90 -o modules_15.o

if [[ $WIN == "1" ]]; then # Windows
    cl /MD /c integration_tests/modules_15c.c /Fomodules_15c.o
elif [[ $MACOS == "1" ]]; then # macOS
    clang -c integration_tests/modules_15c.c -o modules_15c.o
else # Linux
    gcc -c integration_tests/modules_15c.c -o modules_15c.o
fi

src/bin/lfortran modules_15.o modules_15b.o modules_15c.o -o modules_15
./modules_15


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

    ./run_tests.py -b llvm llvm2 llvm_rtlib llvm_nopragma
    ./run_tests.py -b llvm2 llvm_rtlib llvm_nopragma -f
    ./run_tests.py -b llvm -f -nf16
    cd ..

    pip install src/server/tests tests/server
    # NOTE: `--full-trace` tends to print excessively long stack traces. Please
    # re-enable it if needed:
    # pytest -vv --showlocals --full-trace --capture=no --timeout=5 tests/server
#    pytest -vv --showlocals --capture=no --timeout=5 tests/server
    pytest tests/server/tests/test_lsp.py
    pytest tests/server/tests/test_lsp.py -s
fi
