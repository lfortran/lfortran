#!/usr/bin/env bash
# Driver for autodiff integration tests.
#
# Pipeline:
#   1. lfortran --show-llvm SRC > base.ll        (Fortran -> LLVM IR)
#   2. opt --load-pass-plugin=ENZYME_SO          (Enzyme transformation)
#          --passes=enzyme base.ll -S -o base.opt.ll
#   3. lfortran base.opt.ll -o base.out          (link via lfortran)
#   4. base.out                                  (run; .f90 contains error stop)
#
# Required env / args:
#   LFORTRAN          path to the lfortran binary
#   LLVMENZYME_PLUGIN path to the built LLVMEnzyme-<ver>.so plugin
#   OPT               path to llvm `opt` (default: opt)
#   $1                path to .f90 source
#
# Exit 0 on success, nonzero on any failure.

set -euo pipefail

src="${1:?source .f90 required as first argument}"
lfortran="${LFORTRAN:?LFORTRAN env var must point to lfortran binary}"
plugin="${LLVMENZYME_PLUGIN:?LLVMENZYME_PLUGIN env var must point to LLVMEnzyme-<ver>.so}"
opt="${OPT:-opt}"

work="$(mktemp -d -t lfortran-autodiff-XXXXXX)"
trap 'rm -rf "$work"' EXIT

base="$(basename "$src" .f90)"
ll="$work/$base.ll"
opt_ll="$work/$base.opt.ll"
exe="$work/$base.out"

echo "[autodiff] step 1: lfortran --show-llvm $src"
"$lfortran" --show-llvm "$src" > "$ll"

echo "[autodiff] step 2: opt --passes=enzyme (plugin: $plugin)"
"$opt" --load-pass-plugin="$plugin" --passes=enzyme "$ll" -S -o "$opt_ll"

echo "[autodiff] step 3: lfortran $opt_ll -o $exe"
"$lfortran" "$opt_ll" -o "$exe"

echo "[autodiff] step 4: run"
"$exe"
