#!/usr/bin/env bash
# Build gpu_cuda_scratch_order.cu against the real CUDA runtime and run its
# three sequences (scratch order, discarded output, aliases) on the local
# NVIDIA GPU. The host-only counterpart is the test_gpu_cuda_api_state unit
# test, which runs the same sequences against the CPU emulation of the CUDA
# runtime API.
#
# Usage: src/tests/gpu_cuda_scratch_order.sh [build-dir]
# NVCC selects the compiler (default: nvcc on PATH, else
# /usr/local/cuda/bin/nvcc) and CUDA_ARCH the architecture (default: native).
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
out="${1:-$(mktemp -d)}"
mkdir -p "$out"
nvcc="${NVCC:-$(command -v nvcc || echo /usr/local/cuda/bin/nvcc)}"

"$nvcc" -std=c++17 -arch="${CUDA_ARCH:-native}" \
    -o "$out/gpu_cuda_scratch_order" \
    "$here/gpu_cuda_scratch_order.cu" \
    "$here/../libasr/runtime/lfortran_gpu_cuda.c"

for mode in "" discard aliases; do
    echo "gpu_cuda_scratch_order ${mode:-scratch}"
    timeout 60 "$out/gpu_cuda_scratch_order" $mode
done
echo "gpu_cuda_scratch_order: all three sequences passed"
