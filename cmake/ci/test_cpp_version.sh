#!/usr/bin/env shell
# This is a cross-platform `shell` script.

set -ex

# TODO: this file has been ported from a xonsh file, hence
# we need to re-check the below FIXME
# FIXME: Using the below flag generates many warnings.
# os.environ['CXXFLAGS'] = "-Werror"

echo "CONDA_PREFIX=$CONDA_PREFIX"

./build0.sh
cmake -DCMAKE_PREFIX_PATH=$CONDA_PREFIX -DCMAKE_INSTALL_PREFIX=$PWD -DCMAKE_BUILD_TYPE=Debug -DWITH_JSON=yes -DWITH_BENCHMARKS=yes .
cmake --build . --target install -j16
ctest --output-on-failure

./src/bin/parse


echo "Testing libasr - CPP"

mkdir build
cd build
cp -r ../src/libasr .
cd libasr
cmake -DCMAKE_PREFIX_PATH=$CONDA_PREFIX -DCMAKE_INSTALL_PREFIX=$PWD -DCMAKE_BUILD_TYPE=Debug .
cmake --build . -j16
