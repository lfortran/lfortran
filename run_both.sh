#!/usr/bin/env bash

# Builds both LLVM 11 and 20 versions of LFortran and runs integration tests
# for both.

set -ex

LF11=$HOME/.pixi/envs/lf/bin
LF20=$HOME/.pixi/envs/lf/bin

(cd b11 && $LF11/ninja)
(cd b20 && $LF20/ninja)

(cd integration_tests && PATH="$(pwd)/../b11/src/bin:$LF11:$PATH" ./run_tests.py -j16)
(cd integration_tests && PATH="$(pwd)/../b20/src/bin:$LF20:$PATH" ./run_tests.py -j16)
