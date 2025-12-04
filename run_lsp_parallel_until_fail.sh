#!/usr/bin/env bash
set -euo pipefail

attempt=0
while true; do
    attempt=$((attempt + 1))
    echo "=== Attempt ${attempt} ==="
    if ! pixi r lsp_tests_parallel; then
        echo "Run failed on attempt ${attempt}"
        exit 1
    fi
done
