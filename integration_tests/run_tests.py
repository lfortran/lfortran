#!/usr/bin/env python

import argparse
import subprocess as sp
import os

# Initialization
NO_OF_THREADS = 8 # default no of threads is 8
SUPPORTED_BACKENDS = ['llvm', 'llvm2', 'llvm_rtlib', 'c', 'cpp', 'x86', 'wasm',
                      'gfortran', 'llvmImplicit', 'llvmStackArray', 'llvm_integer_8',
                      'llvm_infer', 'fortran', 'c_nopragma', 'llvm_nopragma',
                      'llvm_wasm', 'llvm_wasm_emcc', 'llvm_omp', 'llvm_submodule',
                      'mlir', 'mlir_omp', 'mlir_llvm_omp', 'llvm_goc',
                      'target_offload', 'llvm_single_invocation']
SUPPORTED_STANDARDS = ['lf', 'f23', 'legacy']
BASE_DIR = os.path.dirname(os.path.realpath(__file__))
LFORTRAN_PATH = f"{BASE_DIR}/../src/bin:$PATH"

fast_tests = "no"
nofast_llvm16 = "no"
separate_compilation = "no"
use_ninja = False
user_specified_threads = False

def run_cmd(cmd, cwd=None):
    print(f"+ {cmd}")
    process = sp.run(cmd, shell=True, cwd=cwd)
    if process.returncode != 0:
        print("Command failed.")
        exit(1)

def run_test(backend, std, test_pattern=None):
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

    # Conditionally use Ninja or Make (default)
    if use_ninja:
        # Use Ninja generator for faster builds
        # Add flags to skip Fortran compiler detection issues with CMake 3.29+
        # Set CMAKE_Fortran_PREPROCESS_SOURCE which is required by Ninja but missing for lfortran
        generator_flags = ("-G Ninja -DCMAKE_Fortran_COMPILER_WORKS=1 -DCMAKE_Fortran_COMPILER_FORCED=1 "
                          "-DCMAKE_Fortran_PREPROCESS_SOURCE=\"<CMAKE_Fortran_COMPILER> <DEFINES> <INCLUDES> <FLAGS> "
                          "-E <SOURCE> -o <PREPROCESSED_SOURCE>\"")
    else:
        # Use default Make generator
        generator_flags = ""

    common=f" {generator_flags} -DCURRENT_BINARY_DIR={BASE_DIR}/test-{backend} -S {BASE_DIR} -B {BASE_DIR}/test-{backend}"
    if backend == "gfortran":
        run_cmd(f"FC=gfortran cmake" + common,
                cwd=cwd)
    elif backend == "cpp":
        run_cmd(f"FC=lfortran FFLAGS=\"--openmp\" cmake -DLFORTRAN_BACKEND={backend} -DFAST={fast_tests} "
                f"-DLLVM_GOC={separate_compilation} -DNOFAST_LLVM16={nofast_llvm16} {std_string}" + common,
                cwd=cwd)
    elif backend == "fortran":
        run_cmd(f"FC=lfortran cmake -DLFORTRAN_BACKEND={backend} "
            f"-DFAST={fast_tests} -DLLVM_GOC={separate_compilation} -DNOFAST_LLVM16={nofast_llvm16} "
            f"-DCMAKE_Fortran_FLAGS=\"-fPIC\" {std_string}" + common,
                cwd=cwd)
    else:
        run_cmd(f"FC=lfortran cmake -DLFORTRAN_BACKEND={backend} -DFAST={fast_tests} "
                f"-DLLVM_GOC={separate_compilation} {std_string} -DNOFAST_LLVM16={nofast_llvm16} " + common,
                cwd=cwd)

    # If a test pattern is provided, find matching tests and build only those
    if test_pattern:
        # Query ctest to find which tests match the pattern
        result = sp.run(f"ctest -N -R {test_pattern}", shell=True, cwd=cwd,
                       stdout=sp.PIPE, stderr=sp.PIPE, text=True)
        if result.returncode != 0:
            print("Failed to query tests with ctest")
            exit(1)

        # Parse the output to extract test names
        # Output format: "  Test #123: test_name"
        import re
        test_names = []
        for line in result.stdout.split('\n'):
            match = re.match(r'\s+Test\s+#\d+:\s+(\S+)', line)
            if match:
                test_names.append(match.group(1))

        if not test_names:
            print(f"No tests match pattern: {test_pattern}")
            exit(1)

        print(f"Building {len(test_names)} test(s): {', '.join(test_names)}")
        # Build only the matching test targets
        build_cmd = "ninja" if use_ninja else "make"
        # Ninja uses all cores by default, so only specify -j if user provided it
        # Make needs -j specified, so use default or user-provided value
        if use_ninja and not user_specified_threads:
            # User didn't specify -j, let ninja use all cores
            j_flag = ""
        else:
            j_flag = f" -j{NO_OF_THREADS}"
        for test_name in test_names:
            run_cmd(f"{build_cmd}{j_flag} {test_name}", cwd=cwd)
    else:
        # Build all tests
        build_cmd = "ninja" if use_ninja else "make"
        if use_ninja and not user_specified_threads:
            j_flag = ""
        else:
            j_flag = f" -j{NO_OF_THREADS}"
        run_cmd(f"{build_cmd}{j_flag}", cwd=cwd)

    # Build ctest command with optional test pattern filter
    ctest_cmd = f"ctest -j{NO_OF_THREADS} --output-on-failure"
    if test_pattern:
        ctest_cmd += f" -R {test_pattern}"
    run_cmd(ctest_cmd, cwd=cwd)


def test_backend(backend, std, test_pattern=None):
    if backend not in SUPPORTED_BACKENDS:
        raise Exception(f"Unsupported Backend: {backend}\n")
    if std not in SUPPORTED_STANDARDS:
        raise Exception(f"Unsupported Backend: {std}\n")

    run_test(backend, std, test_pattern)

def check_module_names():
    from glob import glob
    import re
    mod = re.compile(
        r'(?im)^\s*(?:module|submodule)\b\s*(?:\([^)]+\))?\s*(?!\bprocedure\b)(\w+)'
    )
    files = glob("*.f90")
    module_names = []
    file_names = []
    for file in files:
        with open(file) as f:
            content = f.read()
        s = mod.search(content)
        if s:
            module_names.append(s.group(1).lower())
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
    parser.add_argument("-sc", "--separate_compilation", action='store_true',
                help="Run tests with --separate-compilation")
    parser.add_argument("-nf16", "--no_fast_till_llvm16", action='store_true',
                help="Don't run unsupported tests with --fast when LLVM < 17")
    parser.add_argument("-t", "--test", type=str,
                help="Run specific tests matching pattern (regex)")
    parser.add_argument("--ninja", action='store_true',
                help="Use Ninja build system instead of Make (faster builds)")
    parser.add_argument("-m", action='store_true',
                help="Check that all module names are unique")
    return parser.parse_args()

def main():
    args = get_args()

    if args.m:
        check_module_names()
        return

    # Setup
    global NO_OF_THREADS, fast_tests, std_f23_tests, nofast_llvm16, separate_compilation, use_ninja, user_specified_threads
    os.environ["PATH"] += os.pathsep + LFORTRAN_PATH
    # Set environment variable for testing
    os.environ["LFORTRAN_TEST_ENV_VAR"] = "STATUS OK!"
    # delete previously created directories (if any)
    for backend in SUPPORTED_BACKENDS:
        run_cmd(f"rm -rf {BASE_DIR}/test-{backend}")

    if args.no_of_threads:
        NO_OF_THREADS = args.no_of_threads
        user_specified_threads = True
    fast_tests = "yes" if args.fast else "no"
    nofast_llvm16 = "yes" if args.no_fast_till_llvm16 else "no"
    separate_compilation = "yes" if args.separate_compilation else "no"
    use_ninja = args.ninja
    test_pattern = args.test
    for backend in args.backends:
        test_backend(backend, args.std, test_pattern)

if __name__ == "__main__":
    main()
