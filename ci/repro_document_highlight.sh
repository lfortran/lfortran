#!/usr/bin/env bash
#
# Stress runs the flaky document-highlight test in parallel mode until it fails.
# Usage:
#   ci/repro_document_highlight.sh            # loop forever
#   ci/repro_document_highlight.sh 100        # stop after 100 successful runs

set -euo pipefail

trap 'echo "Interrupted"; exit 130' INT TERM

limit="${1:-0}"
attempt=0

while true; do
    attempt=$((attempt + 1))
    echo "=== Document highlight attempt ${attempt} ==="

    if ! pixi run -e test -- pytest -vv --showlocals \
        --execution-strategy="parallel" \
        tests/server/tests/test_features.py::test_document_highlight
    then
        echo "Document highlight test failed on attempt ${attempt}"
        exit 1
    fi

    if [[ "${limit}" != "0" && "${attempt}" -ge "${limit}" ]]; then
        echo "Completed ${attempt} attempts without failure"
        exit 0
    fi
done
