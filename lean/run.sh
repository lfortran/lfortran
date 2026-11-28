#!/usr/bin/env bash
# End-to-end check of the Lean backend prototype. Run from the repository root:
#   pixi run -e llvm11 bash lean/run.sh
#
# runtime/   the Mathlib-free runtime that generated code imports
# proof/     the proof library (arithmetic models, Float is a VModel, ...)
# examples/  verified programs: Fortran sources, generated templates, proofs
set -ex
cd "$(dirname "$0")"

# 1-2. Accuracy of legendre_poly and its translation to Lean
bash examples/check_translation.sh

# 3-4. Check the specs, the generated code and the proofs
bash examples/fetch_deps.sh
(cd examples && lake build --wfail)

# 5. Run the same generated template at Int32/Float
(cd examples && lake env lean --run Legendre/Run.lean)

# 6. Test the proved Float runs: Lean, LFortran and GFortran agree bit for
#    bit, and every result is within its proved bound
python examples/check_float_runs.py
