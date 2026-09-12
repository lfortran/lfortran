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
    # using debugging option i.e. `-x` causes incorrect assignment
    set +x
    if [[ $MACOS == "1" ]]; then
        # macOS ARM64 runners have 3 cores; higher parallelism overwhelms them
        NPROC=3
    else
        # this works fine on Linux
        NPROC=$(nproc)
    fi
    # we turn on the debugging again
    set -x
    echo "NPROC: ${NPROC}"

    if [[ $LFORTRAN_LLVM_VERSION == "11" ]]; then
        ./run_tests.py
    fi

    cd integration_tests
    # Check that CMake can detect and drive lfortran as a Fortran compiler
    # without any help. run_tests.py below bypasses this detection with
    # -DCMAKE_Fortran_COMPILER_WORKS=1, so this configure step is the only
    # place we exercise it. Build and run just the two CMake-driven tests:
    # building the whole suite here duplicates `run_tests.py -b llvm` below.
    mkdir build-lfortran-llvm
    cd build-lfortran-llvm
    FC="../../src/bin/lfortran" cmake -DLFORTRAN_BACKEND=llvm -DCURRENT_BINARY_DIR=. ..
    make -j${NPROC} program_cmake_01 program_cmake_02
    ctest -j${NPROC} -R program_cmake
    cd ..

    ./run_tests.py -b llvm llvm2 llvm_rtlib llvm_nopragma llvm_integer_8 llvmImplicit -j${NPROC}
    if [[ $MACOS != "1" ]]; then
        ./run_tests.py -b llvm -sc -j${NPROC}
        ./run_tests.py -b llvm2 llvm_rtlib llvm_nopragma llvm_integer_8 -f -j${NPROC}
    fi
    if [[ $LFORTRAN_LLVM_VERSION == "11" ]]; then
        if [[ $MACOS != "1" ]]; then
            ./run_tests.py -b llvm llvmImplicit -f -nf16 -j${NPROC}
        fi
    else
        if [[ $MACOS != "1" ]]; then
            ./run_tests.py -b llvm llvmImplicit -f -j${NPROC}
        fi
    fi
    ./run_tests.py -b llvm_submodule -j${NPROC}
    if [[ $MACOS != "1" ]]; then
        ./run_tests.py -b llvm_submodule -sc -j${NPROC}
    fi
    # Leak detection is a compile-flag sweep over the whole suite; it is not
    # platform specific, so run it on Linux only (a full pass costs ~7x more
    # on the 3-core macOS runners).
    if [[ $MACOS != "1" ]]; then
        ./run_tests.py -b llvm --detect-leaks -j${NPROC}
    fi
    cd ..

    pip install src/server/tests tests/server
    # NOTE: `--full-trace` tends to print excessively long stack traces. Please
    # re-enable it if needed:
    # pytest -vv --showlocals --full-trace --capture=no --timeout=5 tests/server
#    pytest -vv --showlocals --capture=no --timeout=5 tests/server
fi
