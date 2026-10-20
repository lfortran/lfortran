import argparse
from pathlib import Path
import re
import subprocess
import sys


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--backend", choices=("cpu", "metal"), required=True)
    parser.add_argument("--cc", required=True)
    parser.add_argument("--sysroot", default="",
                        help="macOS SDK path or name (default: macosx)")
    parser.add_argument("--workdir", type=Path, required=True)
    args = parser.parse_args()
    here = Path(__file__).resolve().parent
    repo = here.parents[2]
    runtime = repo / "src/libasr/runtime"
    workdir = args.workdir.resolve()
    workdir.mkdir(parents=True, exist_ok=True)
    binary = workdir / "test_gpu_launch_geometry"
    command = [args.cc, "-O2", "-I" + str(runtime)]
    if sys.platform == "darwin":
        # Direct compiler calls do not inherit CMake's macOS SDK selection.
        sysroot = args.sysroot or "macosx"
        if not Path(sysroot).is_absolute():
            sysroot = subprocess.check_output(
                ["xcrun", "--sdk", sysroot, "--show-sdk-path"], text=True).strip()
        command += ["-isysroot", sysroot]
    if args.backend == "metal":
        command += ["-x", "objective-c", "-fobjc-arc",
                    "-framework", "Foundation", "-framework", "Metal"]
        implementation = runtime / "lfortran_gpu_metal.m"
    else:
        implementation = runtime / "lfortran_gpu_cpu.c"
    command += [str(here / "test_gpu_launch_geometry.c"),
                str(implementation), "-o", str(binary)]
    print("compile:", command, flush=True)
    subprocess.run(command, cwd=workdir, check=True)
    run = [str(binary)]
    if args.backend == "metal":
        run = ["bash", str(repo / "examples/nbody/gpu_run.sh"),
               "env", "MTL_DEBUG_LAYER=1", str(binary)]

    failures = 0
    cases = [("positive", ["1"] * 6, 0, "launch returned: 7"),
             ("empty-body", ["empty-body"], 0, "launch returned: 0")]
    for dimension in range(6):
        kind = "grid" if dimension < 3 else "block"
        axis = dimension % 3
        for value in (0, -1):
            geometry = ["1"] * 6
            geometry[dimension] = str(value)
            diagnostic = (f"lfortran_gpu_launch: {kind}[{axis}] must be "
                          f"positive (got {value})")
            cases.append((f"{kind}[{axis}]={value}", geometry, 1, diagnostic))
    for name, geometry, status, expected in cases:
        result = subprocess.run(run + geometry, cwd=workdir, text=True,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, timeout=90)
        print(f"{name}: exit {result.returncode}\n{result.stdout}", flush=True)
        if result.returncode != status or expected not in result.stdout:
            failures += 1
            print(f"FAIL: expected exit {status} and {expected!r}", flush=True)

    # Native CUDA is not emulated here. Its entry point must call the same
    # validator before any handle checks, uploads or residency changes.
    for name in ("lfortran_gpu_cpu.c", "lfortran_gpu_cuda.cu",
                 "lfortran_gpu_metal.m"):
        source = (runtime / name).read_text()
        first_statement = (r"void lfortran_gpu_launch\([^)]*\)\s*\{\s*"
                           r"lfortran_gpu_validate_launch\(grid, block\);")
        valid = re.search(first_statement, source) is not None
        print(f"{name}: validation-first source invariant: {valid}", flush=True)
        failures += not valid
    print(f"{len(cases)} runtime cases; {failures} failures", flush=True)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
