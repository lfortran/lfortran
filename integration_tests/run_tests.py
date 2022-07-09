#!/usr/bin/env python

import argparse
import subprocess as sp
import os

# Initialization
no_of_threads = 8 # default no of threads is 8
supported_backends = ['llvm', 'cpp', 'x86', 'wasm']
base_dir = os.path.dirname(os.path.realpath(__file__))
lfortran_path = f"{base_dir}/../src/bin:$PATH"

def run_test(backend):
    run_command = f"""mkdir {base_dir}/test-{backend}
    cd {base_dir}/test-{backend}
    FC=lfortran cmake -DLFORTRAN_BACKEND={backend} -DCURRENT_BINARY_DIR={base_dir}/test-{backend} -S {base_dir} -B {base_dir}/test-{backend}
    make -j{no_of_threads}
    ctest -j{no_of_threads} --output-on-failure"""
    process = sp.run(run_command, shell=True)
    if process.returncode: exit()


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
                help="Test the requested backends")
    return parser.parse_args()

def main():
    # Setup
    global no_of_threads
    os.environ["PATH"] += os.pathsep + lfortran_path
    # delete previously created directories (if any)
    for backend in supported_backends:
        os.system(f"rm -rf {base_dir}/test-{backend}")

    args = get_args()
    no_of_threads = args.no_of_threads or no_of_threads
    for backend in args.backends:
        test_backend(backend)

if __name__ == "__main__":
    main()
