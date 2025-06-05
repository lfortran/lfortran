#!/usr/bin/env shell
# This is a cross-platform `shell` script.

set -ex

# TODO: this file has been ported from a xonsh file, hence
# we need to re-check the below FIXME
# FIXME: Using the below flag generates many warnings.
# os.environ['CXXFLAGS'] = "-Werror"

./build0.sh
cmake -DWITH_LSP=yes -DWITH_JSON=yes -DCMAKE_BUILD_TYPE=Debug .
make -j16

ctest --output-on-failure

./run_tests.py --no-llvm --skip-run-with-dbg
