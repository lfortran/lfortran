#!/usr/bin/env shell
# This is a cross-platform `shell` script.

set -ex

echo "Running SHELL"

src/bin/lfortran --version

cmake --version
if [[ $WIN != "1" ]]; then
    pip install src/server/tests tests/server
    pytest -vv --showlocals --capture=no --timeout=10 tests/server
fi
