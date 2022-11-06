#!/usr/bin/env xonsh

$RAISE_SUBPROC_ERROR = True
trace on

import os
os.environ['CXXFLAGS'] = "-Werror"

./build0.sh
cmake -DCMAKE_BUILD_TYPE=Debug -DWITH_RUNTIME_LIBRARY=No .
make -j16
cmake -DCMAKE_Fortran_COMPILER=src/bin/lfortran -DWITH_RUNTIME_LIBRARY=Yes .
make -j16

ctest --output-on-failure

./run_tests.py --no-llvm
