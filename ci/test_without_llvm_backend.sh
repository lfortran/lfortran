#!/usr/bin/env shell
# This is a cross-platform `shell` script.
#
# Build LFortran without LLVM and test it, as Quick's "Test without LLVM
# Backend" job does:
#
#     pixi run -e nollvm shell ci/test_without_llvm_backend.sh
#
# Pixi sets LFORTRAN_BUILD_DIR; without it, this builds in the source tree.

set -ex

build_dir="${LFORTRAN_BUILD_DIR:-.}"

shell build1.sh "$build_dir" -DWITH_LLVM=no -DWITH_STACKTRACE=no \
    -DWITH_RUNTIME_STACKTRACE=no -DWITH_LSP=yes -DWITH_JSON=yes

ctest --test-dir "$build_dir" --output-on-failure

if [ -n "${LFORTRAN_BUILD_DIR:-}" ]; then
    python run_tests.py --no-llvm --skip-run-with-dbg \
        --compiler-dir "$build_dir/src/bin" \
        --build-dir "$build_dir/reference_tests"
else
    python run_tests.py --no-llvm --skip-run-with-dbg
fi
