#!/bin/bash

set -ex

rm -rf b1 b2 b3

# Append "-j4" or "-j" to run in parallel
jn=$1

export PATH="$(pwd)/../src/bin:$PATH"

mkdir b1
cd b1
FC=lfortran cmake -DLFORTRAN_BACKEND=llvm ..
make $jn
ctest $jn --output-on-failure
cd ..

if [[ ! -z "${LFORTRAN_KOKKOS_DIR}" ]]
then
    mkdir b2
    cd b2
    FC=lfortran cmake -DLFORTRAN_BACKEND=cpp ..
    make $jn
    ctest $jn --output-on-failure
    cd ..
fi

mkdir b3
cd b3
FC=lfortran cmake -DLFORTRAN_BACKEND=wasm -DCURRENT_BINARY_DIR=. ..
make $jn
ctest $jn --output-on-failure
