#!/usr/bin/env shell

set -ex

echo "Running SHELL"

echo "CONDA_PREFIX=$CONDA_PREFIX"
llvm-config --components

shell ci/source_tarball.sh
source_dir=lfortran-src
build_dir=$source_dir/test-bld

# Note: we have to build in Release mode on Windows, because `llvmdev` is
# compiled in Release mode and we get link failures if we mix and match build
# modes:
if [[ $WIN == "1" ]]; then # Windows
    BUILD_TYPE="Release"
else # Linux or macOS
    BUILD_TYPE="Debug"
fi

cmake -G$LFORTRAN_CMAKE_GENERATOR -DCMAKE_VERBOSE_MAKEFILE=ON -DWITH_LSP=yes -DWITH_LLVM=yes -DWITH_XEUS=yes -DCMAKE_PREFIX_PATH=$CONDA_PREFIX -DCMAKE_INSTALL_PREFIX=$CONDA_PREFIX -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DWITH_RUNTIME_STACKTRACE=$ENABLE_RUNTIME_STACKTRACE -DWITH_INTERNAL_ALLOC_CHECK=yes -S "$source_dir" -B "$build_dir"
shell ci/check_build.sh "$build_dir"
