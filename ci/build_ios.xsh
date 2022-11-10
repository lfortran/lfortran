#!/usr/bin/env xonsh

$RAISE_SUBPROC_ERROR = True
trace on

git clone https://github.com/leetal/ios-cmake -b 4.3.0

./build0.sh

mkdir build
cd build

# build lfortran

cmake -GNinja -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=yes -DWITH_TARGET_AARCH64=Yes -DCMAKE_INSTALL_PREFIX=../inst ..

ninja install

cmake -DCMAKE_Fortran_COMPILER=../inst/bin/lfortran -DWITH_RUNTIME_LIBRARY=Yes ..

ninja install

# build the cross compilation runtime
mkdir build_aarch64 && cd build_aarch64

cmake -GNinja -DCMAKE_BUILD_TYPE=Debug -DWITH_LLVM=yes -DCMAKE_INSTALL_PREFIX=../iphone_inst -DWITH_ZLIB=No -DWITH_RUNTIME_LIBRARY_ONLY=Yes -DCMAKE_Fortran_COMPILER=../inst/bin/lfortran -DCMAKE_TOOLCHAIN_FILE=../ios-cmake/ios.toolchain.cmake -DPLATFORM=OS64x -DENABLE_BITCODE=No ..

ninja install

