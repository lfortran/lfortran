#!/usr/bin/env bash
set -euo pipefail

# Loop over integration tests that have NO_DETECT_LEAK but no USER_LEAK comment,
# and invoke Copilot to investigate/fix each one using the fix-leak skill.

CMAKEFILE="integration_tests/CMakeLists.txt"

while true; do
    # Find the next test with NO_DETECT_LEAK and no USER_LEAK
    line=$(grep 'NO_DETECT_LEAK' "$CMAKEFILE" \
        | grep -v 'USER_LEAK' \
        | grep -v 'macro\|set(' \
        | grep -v 'if.*DETECT_LEAK' \
        | head -n1 || true)

    if [[ -z "$line" ]]; then
        echo "=== All NO_DETECT_LEAK tests have been processed. ==="
        break
    fi

    # Extract the test name from the RUN(NAME <name> ...) line
    test_name=$(echo "$line" | sed -E 's/.*RUN\(NAME ([^ ]+) .*/\1/')

    echo "============================================"
    echo "Processing: $test_name"
    echo "============================================"

    copilot --share transcript.md --autopilot --yolo --model claude-opus-4.6 \
        -p "Use the fix-leak skill to investigate and fix the memory leak in the integration test: ${test_name}"

    echo ""
    echo "Finished: $test_name"
    echo "============================================"
    echo ""
done

echo "Done."
