#!/usr/bin/env xonsh

$RAISE_SUBPROC_ERROR = True
trace on

./build0.sh

mkdir build
cd build

# build lfortran

cmake -GNinja -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=yes -DWITH_TARGET_AARCH64=Yes -DCMAKE_PREFIX_PATH=$CONDA_PREFIX -DCMAKE_INSTALL_PREFIX=../inst ..

ninja install

cmake -DCMAKE_Fortran_COMPILER=../inst/bin/lfortran -DWITH_RUNTIME_LIBRARY=Yes ..

ninja install

# build the cross compilation runtime
cd ..
mkdir build_aarch64 && cd build_aarch64

cmake -GNinja -DCMAKE_TOOLCHAIN_FILE=../cmake/toolchains/aarch64-linux-gnu.toolchain.cmake -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=yes -DCMAKE_INSTALL_PREFIX=../install_aarch64 -DCMAKE_Fortran_COMPILER=../install/bin/lfortran -DWITH_ZLIB=NO -DWITH_RUNTIME_LIBRARY_ONLY=YES ..

ninja install
