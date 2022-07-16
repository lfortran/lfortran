#!/usr/bin/env xonsh

$RAISE_SUBPROC_ERROR = True
trace on

import os
os.environ['CXXFLAGS'] = "-Werror"

echo "CONDA_PREFIX=$CONDA_PREFIX"

./build0.sh
cmake -DCMAKE_PREFIX_PATH=$CONDA_PREFIX -DCMAKE_INSTALL_PREFIX=$PWD -DCMAKE_BUILD_TYPE=Debug -DWITH_JSON=yes .
cmake --build . --target install -j16
ctest --output-on-failure

./src/bin/parse


echo "Testing libasr - CPP"

mkdir build
cd build
mv ../src/libasr .
cd libasr
cmake -DCMAKE_PREFIX_PATH=$CONDA_PREFIX -DCMAKE_INSTALL_PREFIX=$PWD -DCMAKE_BUILD_TYPE=Debug .
cmake --build . -j16
