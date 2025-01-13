#!/usr/bin/env xonsh

$RAISE_SUBPROC_ERROR = True
trace on

import os
# FIXME: Using the below flag generates many warnings.
# os.environ['CXXFLAGS'] = "-Werror"

./build0.sh
cmake -DWITH_LSP=yes -DWITH_JSON=yes -DCMAKE_BUILD_TYPE=Debug .
make -j16

ctest --output-on-failure

./run_tests.py --no-llvm --skip-run-with-dbg
