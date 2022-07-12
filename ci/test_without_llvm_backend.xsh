#!/usr/bin/env xonsh

$RAISE_SUBPROC_ERROR = True
trace on

tar xzf dist/lfortran-*.tar.gz
cd lfortran-*
CXXFLAGS="-Werror" cmake -DCMAKE_BUILD_TYPE=Debug .
make

ctest --output-on-failure

cp src/bin/lfortran ../src/bin/
cp src/bin/cpptranslate ../src/bin/
cp src/runtime/*.mod ../src/runtime/
cd ..

./run_tests.py --no-llvm
