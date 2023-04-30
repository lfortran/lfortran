#!/usr/bin/env bash
#
# Compile the latest master and a given PR in Release mode from scratch.
#
# Example:
#
# CC=gcc-10 CXX=g++-10 ./bench.sh 448
# bench/main/src/bin/parse
# bench/pr/src/bin/parse
# bench/main/src/bin/parse2
# bench/pr/src/bin/parse2

set -ex

PR=$1
if [[ $PR == "" ]]; then
    echo "Specify the PR number as an argument"
    exit 1
fi

echo "Benchmarking the latest master against the PR !$1"

rm -rf bench
mkdir bench
cd bench

git clone https://github.com/lfortran/lfortran
cd lfortran

mkdir main
cd main
#cmake -DWITH_FMT=yes -DCMAKE_CXX_FLAGS_RELEASE="-Wall -Wextra -O3 -funroll-loops -DNDEBUG" ../lfortran
cmake -DWITH_FMT=yes ../lfortran
make -j
cd ..

cd lfortran
git clean -dfx
git fetch origin pull/$PR/head:pr-origin-$PR
git checkout pr-origin-$PR

mkdir pr
cd pr
#cmake -DWITH_FMT=yes -DCMAKE_CXX_FLAGS_RELEASE="-Wall -Wextra -O3 -funroll-loops -DNDEBUG" ../lfortran
cmake -DWITH_FMT=yes ../lfortran
make -j
cd ..
