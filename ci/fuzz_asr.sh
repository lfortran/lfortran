#!/usr/bin/env bash

set -euo pipefail

LFORTRAN=${LFORTRAN:-src/bin/lfortran}
ASR_FUZZ_CASES=${ASR_FUZZ_CASES:-1000}
ASR_FUZZ_SEED=${ASR_FUZZ_SEED:-0}
ASR_FUZZ_STRATEGY=${ASR_FUZZ_STRATEGY:-mixed}
ASR_FUZZ_GENERATOR=${ASR_FUZZ_GENERATOR:-all}
ASR_FUZZ_TIMEOUT=${ASR_FUZZ_TIMEOUT:-30}
ASR_FUZZ_ARTIFACTS=${ASR_FUZZ_ARTIFACTS:-asr-fuzz-artifacts}

python3 tests/asr/fuzz.py \
    --lfortran "$LFORTRAN" \
    --manifest tests/asr/fuzz_seeds.toml \
    --cases "$ASR_FUZZ_CASES" \
    --seed "$ASR_FUZZ_SEED" \
    --strategy "$ASR_FUZZ_STRATEGY" \
    --generator "$ASR_FUZZ_GENERATOR" \
    --timeout "$ASR_FUZZ_TIMEOUT" \
    --artifacts "$ASR_FUZZ_ARTIFACTS"
