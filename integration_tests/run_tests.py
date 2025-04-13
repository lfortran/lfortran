#!/usr/bin/env python

import argparse
import subprocess as sp
import os

# Initialization
NO_OF_THREADS = 8 # default no of threads is 8
SUPPORTED_BACKENDS = ['llvm', 'llvm2', 'llvm_rtlib', 'c', 'cpp', 'x86', 'wasm',
                      'gfortran', 'llvmImplicit', 'llvmStackArray', 'fortran',
                      'c_nopragma', 'llvm_nopragma', 'llvm_wasm', 'llvm_wasm_emcc',
                      'llvm_omp', 'mlir', 'mlir_omp', 'mlir_llvm_omp', 'llvm_sc']
SUPPORTED_STANDARDS = ['lf', 'f23', 'legacy']
BASE_DIR = os.path.dirname(os.path.realpath(__file__))
LFORTRAN_PATH = f"{BASE_DIR}/../src/bin:$PATH"

fast_tests = "no"
nofast_llvm16 = "no"

def run_cmd(cmd, cwd=None):
    print(f"+ {cmd}")
    process = sp.run(cmd, shell=True, cwd=cwd)
    if process.returncode != 0:
        print("Command failed.")
        exit(1)

def run_test(backend, std):
    run_cmd(f"mkdir {BASE_DIR}/test-{backend}")
    if std == "f23":
        std_string = "-DSTD_F23=yes"
    elif std == "legacy":
        std_string = "-DSTD_LEGACY=yes"
    elif std == "lf":
        std_string = ""
    else:
        raise Exception("Unsupported standard")

    cwd=f"{BASE_DIR}/test-{backend}"
    common=f" -DCURRENT_BINARY_DIR={BASE_DIR}/test-{backend} -S {BASE_DIR} -B {BASE_DIR}/test-{backend}"
    if backend == "gfortran":
        run_cmd(f"FC=gfortran cmake" + common,
                cwd=cwd)
    elif backend == "cpp":
        run_cmd(f"FC=lfortran FFLAGS=\"--openmp\" cmake -DLFORTRAN_BACKEND={backend} -DFAST={fast_tests} -DNOFAST_LLVM16={nofast_llvm16} {std_string}" + common,
                cwd=cwd)
    elif backend == "fortran":
        run_cmd(f"FC=lfortran cmake -DLFORTRAN_BACKEND={backend} "
            f"-DFAST={fast_tests} -DNOFAST_LLVM16={nofast_llvm16} -DCMAKE_Fortran_FLAGS=\"-fPIC\" {std_string}" + common,
                cwd=cwd)
    else:
        run_cmd(f"FC=lfortran cmake -DLFORTRAN_BACKEND={backend} -DFAST={fast_tests} {std_string} -DNOFAST_LLVM16={nofast_llvm16} " + common,
                cwd=cwd)
    run_cmd(f"make -j{NO_OF_THREADS}", cwd=cwd)
    run_cmd(f"ctest -j{NO_OF_THREADS} --output-on-failure", cwd=cwd)


def test_backend(backend, std):
    if backend not in SUPPORTED_BACKENDS:
        raise Exception(f"Unsupported Backend: {backend}\n")
    if std not in SUPPORTED_STANDARDS:
        raise Exception(f"Unsupported Backend: {std}\n")

    run_test(backend, std)

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
    parser.add_argument("--std", type=str, default="lf",
                help="Run tests with the requested Fortran standard: ".join(SUPPORTED_STANDARDS))
    parser.add_argument("-f", "--fast", action='store_true',
                help="Run supported tests with --fast")
    parser.add_argument("-nf16", "--no_fast_till_llvm16", action='store_true',
                help="Don't run unsupported tests with --fast when LLVM < 17")
    parser.add_argument("-m", action='store_true',
                help="Check that all module names are unique")
    return parser.parse_args()

def main():
    args = get_args()

    if args.m:
        check_module_names()
        return

    # Setup
    global NO_OF_THREADS, fast_tests, std_f23_tests, nofast_llvm16
    os.environ["PATH"] += os.pathsep + LFORTRAN_PATH
    # Set environment variable for testing
    os.environ["LFORTRAN_TEST_ENV_VAR"] = "STATUS OK!"
    # delete previously created directories (if any)
    for backend in SUPPORTED_BACKENDS:
        run_cmd(f"rm -rf {BASE_DIR}/test-{backend}")

    NO_OF_THREADS = args.no_of_threads or NO_OF_THREADS
    fast_tests = "yes" if args.fast else "no"
    nofast_llvm16 = "yes" if args.no_fast_till_llvm16 else "no"
    for backend in args.backends:
        test_backend(backend, args.std)

if __name__ == "__main__":
    main()
