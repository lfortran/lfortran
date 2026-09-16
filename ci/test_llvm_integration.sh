#!/usr/bin/env bash
# LLVM-backend integration tests for Exhaustive CI.
#
# Used on LLVM versions that do not run the third-party suite. Covers the
# LLVM option combinations from Quick checks (ci/test.sh) plus separate
# compilation and leak detection (those two live in Exhaustive only).
set -ex

NPROC=${NPROC:-$(nproc)}
echo "NPROC: ${NPROC}"
echo "LFORTRAN_LLVM_VERSION: ${LFORTRAN_LLVM_VERSION:-unset}"

# LLVM 8 compiles these, but the generated code is numerically wrong for
# real(16)/real128 (gpu_metal_291 error-stops "real16"; real128_compare_01
# error-stops 1). Keep compiling them; skip running them on LLVM 8.
if [[ "${LFORTRAN_LLVM_VERSION}" == "8" ]]; then
    export LFORTRAN_CTEST_EXCLUDE='gpu_metal_291|real128_compare_01'
    echo "LFORTRAN_CTEST_EXCLUDE=${LFORTRAN_CTEST_EXCLUDE}"
fi

cd integration_tests
./run_tests.py -b llvm llvm2 llvm_rtlib llvm_nopragma llvm_integer_8 llvmImplicit -j"${NPROC}"
./run_tests.py -b llvm -sc -j"${NPROC}"
./run_tests.py -b llvm2 llvm_rtlib llvm_nopragma llvm_integer_8 -f -j"${NPROC}"
# LLVM < 17: still run --fast, but skip tests marked NOFAST_TILL_LLVM16.
if [[ "${LFORTRAN_LLVM_VERSION}" -lt 17 ]]; then
    ./run_tests.py -b llvm llvmImplicit -f -nf16 -j"${NPROC}"
else
    ./run_tests.py -b llvm llvmImplicit -f -j"${NPROC}"
fi
./run_tests.py -b llvm_submodule -j"${NPROC}"
./run_tests.py -b llvm_submodule -sc -j"${NPROC}"
./run_tests.py -b llvm --detect-leaks -j"${NPROC}"
