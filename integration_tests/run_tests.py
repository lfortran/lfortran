#!/usr/bin/env python

import argparse
import subprocess as sp
import os

# Initialization
NO_OF_THREADS = 8 # default no of threads is 8
SUPPORTED_BACKENDS = ['llvm', 'llvm2', 'llvm_rtlib', 'cpp', 'x86', 'wasm', 'gfortran', 'llvmImplicit']
BASE_DIR = os.path.dirname(os.path.realpath(__file__))
LFORTRAN_PATH = f"{BASE_DIR}/../src/bin:$PATH"

def run_cmd(cmd, cwd=None):
    print(f"+ {cmd}")
    process = sp.run(cmd, shell=True, cwd=cwd)
    if process.returncode != 0:
        print("Command failed.")
        exit(1)

def run_test(backend):
    run_cmd(f"mkdir {BASE_DIR}/test-{backend}")
    cwd=f"{BASE_DIR}/test-{backend}"
    common=f" -DCURRENT_BINARY_DIR={BASE_DIR}/test-{backend} -S {BASE_DIR} -B {BASE_DIR}/test-{backend}"
    if backend == "gfortran":
        run_cmd(f"FC=gfortran cmake" + common,
                cwd=cwd)
    elif backend == "cpp":
        run_cmd(f"FC=lfortran FFLAGS=\"--openmp\" cmake -DLFORTRAN_BACKEND={backend}" + common,
                cwd=cwd)
    else:
        run_cmd(f"FC=lfortran cmake -DLFORTRAN_BACKEND={backend}" + common,
                cwd=cwd)
    run_cmd(f"make -j{NO_OF_THREADS}", cwd=cwd)
    run_cmd(f"ctest -j{NO_OF_THREADS} --output-on-failure", cwd=cwd)


def test_backend(backend):
    if backend not in SUPPORTED_BACKENDS:
        print(f"Unsupported Backend: {backend}\n")
        return
    run_test(backend)

def get_args():
    parser = argparse.ArgumentParser(description="LFortran Integration Test Suite")
    parser.add_argument("-j", "-n", "--no_of_threads", type=int,
                help="Parallel testing on given number of threads")
    parser.add_argument("-b", "--backends", nargs="*", default=["llvm"], type=str,
                help="Test the requested backends (%s)" % \
                        ", ".join(SUPPORTED_BACKENDS))
    return parser.parse_args()

def main():
    args = get_args()

    # Setup
    global NO_OF_THREADS
    os.environ["PATH"] += os.pathsep + LFORTRAN_PATH
    # delete previously created directories (if any)
    for backend in SUPPORTED_BACKENDS:
        run_cmd(f"rm -rf {BASE_DIR}/test-{backend}")

    NO_OF_THREADS = args.NO_OF_THREADS or NO_OF_THREADS
    for backend in args.backends:
        test_backend(backend)

if __name__ == "__main__":
    main()
