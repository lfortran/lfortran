#!/usr/bin/env bash
# LLVM-backend integration tests for Exhaustive CI.
#
# Used on LLVM versions that do not run the third-party suite. Covers the
# LLVM option combinations from Quick checks (ci/test.sh) plus separate
# compilation and leak detection (those two live in Exhaustive only).
set -ex

NPROC=${NPROC:-$(nproc)}
echo "NPROC: ${NPROC}"

cd integration_tests
./run_tests.py -b llvm llvm2 llvm_rtlib llvm_nopragma llvm_integer_8 llvmImplicit -j"${NPROC}"
./run_tests.py -b llvm -sc -j"${NPROC}"
./run_tests.py -b llvm2 llvm_rtlib llvm_nopragma llvm_integer_8 -f -j"${NPROC}"
./run_tests.py -b llvm llvmImplicit -f -j"${NPROC}"
./run_tests.py -b llvm_submodule -j"${NPROC}"
./run_tests.py -b llvm_submodule -sc -j"${NPROC}"
./run_tests.py -b llvm --detect-leaks -j"${NPROC}"
