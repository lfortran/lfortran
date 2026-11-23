#!/usr/bin/env bash
# End-to-end check of the Lean backend prototype. Run from the repository root:
#   pixi run -e llvm11 bash lean/run.sh
set -ex
cd "$(dirname "$0")"

# 1-2. Accuracy of legendre_poly and its translation to Lean
bash check_translation.sh

# 3-4. Check the spec, the generated code and the proof of legendre_correct
bash fetch_deps.sh
lake build --wfail
