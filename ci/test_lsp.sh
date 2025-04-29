#!/usr/bin/env shell
# This is a cross-platform `shell` script.

set -ex

echo "Running SHELL"

src/bin/lfortran --version

cmake --version
if [[ $WIN != "1" ]]; then
    pip install src/server/tests tests/server
    # NOTE: If you want to print all the messages, even on success, then disable
    # `--capture` as follows:
    # --------------------------------------------------------------------------
    # pytest -vv --showlocals --capture=no --timeout=10 tests/server
    timeout -k 10 60s pytest -vv --showlocals --timeout=10 --execution-strategy="concurrent" tests/server
    timeout -k 10 60s pytest -vv --showlocals --timeout=10 --execution-strategy="parallel" tests/server
fi
