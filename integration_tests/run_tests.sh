#!/bin/bash

set -ex

rm -rf b1 b2

# Append "-j4" or "-j" to run in parallel
jn=$1

export PATH="$(pwd)/../src/bin:$PATH"

mkdir b1
cd b1
FC=lfortran cmake -DLFORTRAN_BACKEND=llvm ..
make $jn
ctest $jn --output-on-failure
cd ..

mkdir b2
cd b2
FC=lfortran cmake -DLFORTRAN_BACKEND=cpp ..
make $jn
ctest $jn --output-on-failure
