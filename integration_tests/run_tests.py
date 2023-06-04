#!/usr/bin/env python

import argparse
import subprocess as sp
import os

# Initialization
no_of_threads = 8 # default no of threads is 8
supported_backends = ['llvm', 'llvm2', 'llvm_rtlib', 'cpp', 'x86', 'wasm', 'gfortran', 'llvmImplicit']
base_dir = os.path.dirname(os.path.realpath(__file__))
lfortran_path = f"{base_dir}/../src/bin:$PATH"

def run_cmd(cmd, cwd=None):
    print(f"+ {cmd}")
    process = sp.run(cmd, shell=True, cwd=cwd)
    if process.returncode != 0:
        print("Command failed.")
        exit(1)

def run_test(backend):
    run_cmd(f"mkdir {base_dir}/test-{backend}")
    cwd=f"{base_dir}/test-{backend}"
    common=f" -DCURRENT_BINARY_DIR={base_dir}/test-{backend} -S {base_dir} -B {base_dir}/test-{backend}"
    if backend == "gfortran":
        run_cmd(f"FC=gfortran cmake" + common,
                cwd=cwd)
    elif backend == "cpp":
        run_cmd(f"FC=lfortran FFLAGS=\"--openmp\" cmake -DLFORTRAN_BACKEND={backend}" + common,
                cwd=cwd)
    else:
        run_cmd(f"FC=lfortran cmake -DLFORTRAN_BACKEND={backend}" + common,
                cwd=cwd)
    run_cmd(f"make -j{no_of_threads}", cwd=cwd)
    run_cmd(f"ctest -j{no_of_threads} --output-on-failure", cwd=cwd)


def test_backend(backend):
    if backend not in supported_backends:
        print(f"Unsupported Backend: {backend}\n")
        return
    run_test(backend)

def get_args():
    parser = argparse.ArgumentParser(description="LFortran Integration Test Suite")
    parser.add_argument("-j", "-n", "--no_of_threads", type=int,
                help="Parallel testing on given number of threads")
    parser.add_argument("-b", "--backends", nargs="*", default=["llvm"], type=str,
                help="Test the requested backends (%s)" % \
                        ", ".join(supported_backends))
    return parser.parse_args()

def main():
    args = get_args()

    # Setup
    global no_of_threads
    os.environ["PATH"] += os.pathsep + lfortran_path
    # delete previously created directories (if any)
    for backend in supported_backends:
        run_cmd(f"rm -rf {base_dir}/test-{backend}")

    no_of_threads = args.no_of_threads or no_of_threads
    for backend in args.backends:
        test_backend(backend)

if __name__ == "__main__":
    main()
