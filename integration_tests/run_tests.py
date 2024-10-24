#!/usr/bin/env python

import argparse
import subprocess as sp
import os

# Initialization
NO_OF_THREADS = 8 # default no of threads is 8
SUPPORTED_BACKENDS = ['llvm', 'llvm2', 'llvm_rtlib', 'c', 'cpp', 'x86', 'wasm',
                      'gfortran', 'llvmImplicit', 'llvmStackArray', 'fortran',
                      'c_nopragma', 'llvm_nopragma', 'llvm_wasm', 'llvm_wasm_emcc',
                      'llvm_omp', 'mlir']
BASE_DIR = os.path.dirname(os.path.realpath(__file__))
LFORTRAN_PATH = f"{BASE_DIR}/../src/bin:$PATH"

fast_tests = "no"

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
        run_cmd(f"FC=lfortran FFLAGS=\"--openmp\" cmake -DLFORTRAN_BACKEND={backend} -DFAST={fast_tests} -DEXPERIMENTAL_SIMPLIFIER={experimental_simplifier}" + common,
                cwd=cwd)
    elif backend == "fortran":
        run_cmd(f"FC=lfortran cmake -DLFORTRAN_BACKEND={backend} "
            f"-DFAST={fast_tests} -DCMAKE_Fortran_FLAGS=\"-fPIC\" -DEXPERIMENTAL_SIMPLIFIER={experimental_simplifier}" + common,
                cwd=cwd)
    else:
        run_cmd(f"FC=lfortran cmake -DLFORTRAN_BACKEND={backend} -DFAST={fast_tests} -DEXPERIMENTAL_SIMPLIFIER={experimental_simplifier}" + common,
                cwd=cwd)
    run_cmd(f"make -j{NO_OF_THREADS}", cwd=cwd)
    run_cmd(f"ctest -j{NO_OF_THREADS} --output-on-failure", cwd=cwd)


def test_backend(backend):
    if backend not in SUPPORTED_BACKENDS:
        print(f"Unsupported Backend: {backend}\n")
        return
    run_test(backend)

def check_module_names():
    from glob import glob
    import re
    mod = re.compile(r"(module|MODULE)[ ]+(.*)", re.IGNORECASE)
    files = glob("*.f90")
    module_names = []
    file_names = []
    for file in files:
        f = open(file).read()
        s = mod.search(f)
        if s:
            module_names.append(s.group(2).lower())
            file_names.append(file)
    for i in range(len(module_names)):
        name = module_names[i]
        if name in module_names[i+1:]:
            print("FAIL: Found a duplicate module name")
            print("Name:", name)
            print("Filename:", file_names[i])
            raise Exception("Duplicate module names")
    print("OK: All module names are unique")

def get_args():
    parser = argparse.ArgumentParser(description="LFortran Integration Test Suite")
    parser.add_argument("-j", "-n", "--no_of_threads", type=int,
                help="Parallel testing on given number of threads")
    parser.add_argument("-b", "--backends", nargs="*", default=["llvm"], type=str,
                help="Test the requested backends (%s)" % \
                        ", ".join(SUPPORTED_BACKENDS))
    parser.add_argument("-f", "--fast", action='store_true',
                help="Run supported tests with --fast")
    parser.add_argument("-m", action='store_true',
                help="Check that all module names are unique")
    parser.add_argument("--experimental-simplifier",
                        action='store_true', help="Use simplifier ASR pass")
    return parser.parse_args()

def main():
    args = get_args()

    if args.m:
        check_module_names()
        return

    # Setup
    global NO_OF_THREADS, fast_tests, experimental_simplifier
    os.environ["PATH"] += os.pathsep + LFORTRAN_PATH
    # delete previously created directories (if any)
    for backend in SUPPORTED_BACKENDS:
        run_cmd(f"rm -rf {BASE_DIR}/test-{backend}")

    NO_OF_THREADS = args.no_of_threads or NO_OF_THREADS
    fast_tests = "yes" if args.fast else "no"
    experimental_simplifier = "yes" if args.experimental_simplifier else "no"
    for backend in args.backends:
        test_backend(backend)

if __name__ == "__main__":
    main()
