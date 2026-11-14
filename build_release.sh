#!/usr/bin/env bash

# Production configuration example: the compiler and runtime options a release
# package is built with, in Release mode, out of the source tree.
#
# The Pixi task 'build-release' runs this with the Prefix shell as
# 'build_release.sh [cmake_args...]'. Builds without Pixi run it with Bash.
# Keep to syntax that both support, and do not use backticks: the Prefix shell
# does not expand them.

set -e
set -x

root="${PIXI_PROJECT_ROOT:-$(pwd)}"
build_dir="$root/build/release"
# Build another source tree, such as an unpacked source tarball.
source_dir="${LFORTRAN_SOURCE_DIR:-.}"

# GCC gets warnings and unrolling on top of CMake's Release flags; MSVC needs
# its own optimized Release flags and rejects -Wextra/-O3/-funroll-loops.
if [ -n "$WINDIR" ]; then
    release_flags="/MD /O2 /Ob2 /DNDEBUG"
else
    release_flags="-Wall -Wextra -O3 -funroll-loops -DNDEBUG"
fi

# Later arguments win, so the caller can override any of these.
cmake \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_CXX_FLAGS_RELEASE="$release_flags" \
    -DWITH_LLVM=yes \
    -DWITH_LSP=yes \
    -DLFORTRAN_BUILD_ALL=yes \
    -DWITH_STACKTRACE=no \
    -DWITH_RUNTIME_STACKTRACE=yes \
    -DUSE_DYNAMIC_ZSTD=no \
    -DCMAKE_PREFIX_PATH="$CONDA_PREFIX" \
    -DCMAKE_INSTALL_PREFIX="$build_dir/inst" \
    -DCMAKE_INSTALL_LIBDIR=share/lfortran/lib \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=yes \
    -G Ninja \
    -S "$source_dir" -B "$build_dir" \
    "$@"
cmake --build "$build_dir" --target install
