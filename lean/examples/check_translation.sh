#!/usr/bin/env bash
# Steps 1-2, which need `lfortran` on PATH (run with `pixi run -e llvm11`):
# measure the accuracy of legendre_poly and check that LFortran translates
# legendre.f90, sin_kernels.f90 and horner.f90 to exactly the checked-in
# */Generated.lean files, on which the proofs are built.
set -ex
cd "$(dirname "$0")"

mkdir -p .lake/accuracy
(cd .lake/accuracy && lfortran ../../legendre.f90 ../../legendre_accuracy.f90)

lfortran --show-lean legendre.f90 > .lake/Generated.lean
diff -u Legendre/Generated.lean .lake/Generated.lean

lfortran --show-lean sin_kernels.f90 > .lake/SinGenerated.lean
diff -u SinKernels/Generated.lean .lake/SinGenerated.lean

lfortran --show-lean horner.f90 > .lake/HornerGenerated.lean
diff -u Horner/Generated.lean .lake/HornerGenerated.lean
