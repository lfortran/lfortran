#!/usr/bin/env bash
# Steps 1-2, which need `lfortran` on PATH (run with `pixi run -e llvm11`):
# measure the accuracy of legendre_poly and check that LFortran translates it
# to exactly the checked-in Legendre/Generated.lean, on which the proof is built.
set -ex
cd "$(dirname "$0")"

mkdir -p .lake/accuracy
(cd .lake/accuracy && lfortran ../../legendre.f90 ../../legendre_accuracy.f90)

lfortran --show-lean legendre.f90 > .lake/Generated.lean
diff -u Legendre/Generated.lean .lake/Generated.lean
