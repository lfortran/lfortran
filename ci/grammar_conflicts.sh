#!/usr/bin/env bash

set -ex

patch -p1 < ci/parser.yy.patch
sed -i '.bak' '/^%expect-rr/d' src/lfortran/parser/parser.yy
sed -i '.bak' 's/^%expect .*/%expect 0/' src/lfortran/parser/parser.yy
sed -i '.bak' '/^%glr-parser/d' src/lfortran/parser/parser.yy
(cd src/lfortran/parser && bison -Wall -d -r all parser.yy)

echo "Grammar is LALR(1), no conflicts"
