#!/bin/sh
# You can run this script as follows from the current directory:
# ./test_cmake_integration.sh $(pwd)/../../bin $(pwd)

set -e
set -x

lfortran="$1/lfortran"
cmakedir="$2/test_cmake_integration"

test -x "${lfortran}"
test -d "${cmakedir}"

cwd=$PWD
builddir=_cmake_integration_build

rm -rf $builddir
mkdir $builddir
cd $builddir
cmake ${cmakedir} -DCMAKE_Fortran_COMPILER=${lfortran}
cmake --build .
cd ..
rm -rf $builddir
