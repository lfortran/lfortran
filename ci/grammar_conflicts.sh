#!/usr/bin/env bash

set -ex

patch -p1 < ci/parser.yy.patch
(cd src/lfortran/parser && bison -Wall -d -r all parser.yy)

echo "Grammar is LALR(1), no conflicts"
