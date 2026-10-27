#!/usr/bin/env bash
# LLVM-backend integration tests for full and PR compatibility CI.
#
# Quick splits full CPU modes across its existing LLVM 11 (--core) and
# LLVM 21 (--options) builds. Exhaustive adds full platform coverage.
set -ex

if [[ $# -gt 1 ]]; then
    echo "usage: $0 [--core|--options|--platform]" >&2
    exit 2
fi
MODE=${1:-}
case "$MODE" in
    ""|--core|--options|--platform) ;;
    *) echo "usage: $0 [--core|--options|--platform]" >&2; exit 2 ;;
esac
if [[ -n "$MODE" ]]; then
    export LFORTRAN_TEST_SUITE=full
fi

NPROC=${NPROC:-$(nproc)}
echo "NPROC: ${NPROC}"
echo "LFORTRAN_LLVM_VERSION: ${LFORTRAN_LLVM_VERSION:-unset}"

# LLVM 7 and 8 compile these, but the generated code is numerically wrong
# for real(16)/real128 (gpu_metal_291 error-stops "real16"; real128_compare_01
# error-stops 1; real128_literal_02 error-stops 2; template_deferred_const_05
# and implicit_typing_13 error-stop; real_star_01 and the real128_arith,
# real128_intrinsic math, sqrt and real tests, real128_int_to_real_02/03 and
# real128_cast_01/04 error-stop too): their x86-64 backend passes fp128
# libcall arguments (e.g. __lttf2, __trunctfdf2) in general purpose instead of SSE
# registers. Keep compiling them; skip running them on LLVM 7 and 8.
if [[ "${LFORTRAN_LLVM_VERSION}" == "7" || "${LFORTRAN_LLVM_VERSION}" == "8" ]]; then
    LFORTRAN_CTEST_EXCLUDE='gpu_metal_291|real128_compare_01|real128_literal_02|template_deferred_const_05|implicit_typing_13'
    LFORTRAN_CTEST_EXCLUDE+='|real_star_01|real128_arith_11|real128_arith_12|real128_intrinsic_math_01'
    LFORTRAN_CTEST_EXCLUDE+='|real128_intrinsic_math_02|real128_intrinsic_math_03|real128_intrinsic_real_02|real128_intrinsic_sqrt_01'
    LFORTRAN_CTEST_EXCLUDE+='|real128_int_to_real_02|real128_int_to_real_03|real128_cast_01|real128_cast_04'
    export LFORTRAN_CTEST_EXCLUDE
    echo "LFORTRAN_CTEST_EXCLUDE=${LFORTRAN_CTEST_EXCLUDE}"
fi

cd integration_tests
selection=()
if [[ "${LFORTRAN_TEST_SUITE:-full}" == "smoke" ]]; then
    selection=(--smoke)
fi
if [[ "$MODE" != "--options" ]]; then
    ./run_tests.py -b llvm llvm2 llvm_rtlib llvm_nopragma llvm_integer_8 llvmImplicit -j"${NPROC}" "${selection[@]}"
    if [[ "$MODE" != "--platform" || "${MACOS:-0}" != "1" ]]; then
        ./run_tests.py -b llvm2 llvm_rtlib llvm_nopragma llvm_integer_8 -f -j"${NPROC}" "${selection[@]}"
        ./run_tests.py -b llvm llvmImplicit -f -j"${NPROC}" "${selection[@]}"
    fi
    ./run_tests.py -b llvm_submodule -j"${NPROC}" "${selection[@]}"
fi
if [[ "$MODE" != "--core" && "$MODE" != "--platform" ]]; then
    ./run_tests.py -b llvm -sc -j"${NPROC}" "${selection[@]}"
    ./run_tests.py -b llvm_submodule -sc -j"${NPROC}" "${selection[@]}"
    ./run_tests.py -b llvm --detect-leaks -j"${NPROC}" "${selection[@]}"
fi
if [[ "$MODE" == "--core" ]]; then
    ./run_tests.py -b llvm --std=f23 -j"${NPROC}"
    ./run_tests.py -b llvm -f --std=f23 -j"${NPROC}"
    ./run_tests.py -b llvm_single_invocation -j"${NPROC}"
fi
