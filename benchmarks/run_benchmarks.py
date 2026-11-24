#!/usr/bin/env python3
"""
Measure compile time and runtime of the benchmark kernels in this directory
with LFortran and a reference compiler (GFortran by default).

Each kernel checks its own result and stops with an error if it is wrong.
Every timing is the best of --repeat runs.

With --compare, the times are also compared against an earlier --json run,
and the script fails if any of them got slower by more than --threshold.
"""

import argparse
import json
import os
import shlex
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

BENCH_DIR = Path(__file__).resolve().parent

DEFAULT_FLAGS = {
    "lfortran": "--fast",
    "gfortran": "-O3 -march=native -ffast-math -funroll-loops",
}


def best_time(cmd, cwd, repeat):
    best = None
    for _ in range(repeat):
        t0 = time.perf_counter()
        r = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True)
        dt = time.perf_counter() - t0
        if r.returncode != 0:
            raise RuntimeError("command failed: %s\n%s%s"
                % (shlex.join(cmd), r.stdout, r.stderr))
        best = dt if best is None else min(best, dt)
    return best


def bench(kernel, compiler, exe, flags, repeat):
    # Compile in a fresh directory so .mod files of one compiler are never
    # picked up by the other
    with tempfile.TemporaryDirectory() as tmp:
        compile_cmd = [exe, *shlex.split(flags), str(kernel), "-o", "a.out"]
        compile_time = best_time(compile_cmd, tmp, repeat)
        run_time = best_time(["./a.out"], tmp, repeat)
    return {"kernel": kernel.stem, "compiler": compiler,
            "compile_time": compile_time, "run_time": run_time}


def compare(results, baseline, threshold, min_diff):
    """Print the change against baseline and return the number of
    regressions: times slower by more than threshold percent and min_diff
    seconds. The absolute floor keeps noise in short runs from failing."""
    base = {(r["kernel"], r["compiler"]): r for r in baseline["results"]}
    print("\n%-16s %-9s %-8s %10s %10s %8s" % ("kernel", "compiler",
        "time", "baseline", "current", "change"))
    regressions = 0
    for r in results:
        b = base.get((r["kernel"], r["compiler"]))
        if b is None:
            continue
        for what in ("compile", "run"):
            old, new = b[what + "_time"], r[what + "_time"]
            change = 100 * (new - old) / old
            slower = change > threshold and new - old > min_diff
            regressions += slower
            print("%-16s %-9s %-8s %9.3fs %9.3fs %+7.1f%%%s" % (r["kernel"],
                r["compiler"], what, old, new, change,
                "  REGRESSION" if slower else ""))
    return regressions


def main():
    parser = argparse.ArgumentParser(description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("kernels", nargs="*",
        help="kernel names to run (default: all *.f90 in this directory)")
    parser.add_argument("--lfortran", default="lfortran",
        help="LFortran executable (default: %(default)s)")
    parser.add_argument("--gfortran", default="gfortran",
        help="reference compiler executable (default: %(default)s)")
    parser.add_argument("--lfortran-flags", default=DEFAULT_FLAGS["lfortran"],
        help="default: %(default)s")
    parser.add_argument("--gfortran-flags", default=DEFAULT_FLAGS["gfortran"],
        help="default: %(default)s")
    parser.add_argument("--no-gfortran", action="store_true",
        help="only run LFortran")
    parser.add_argument("-r", "--repeat", type=int, default=3,
        help="runs per measurement, the best is kept (default: %(default)s)")
    parser.add_argument("--json", metavar="FILE",
        help="also write the results to FILE as JSON")
    parser.add_argument("--compare", metavar="FILE",
        help="compare against the results in FILE, written by --json")
    parser.add_argument("--threshold", type=float, default=10,
        help="percent slowdown that counts as a regression "
            "(default: %(default)s)")
    parser.add_argument("--min-diff", type=float, default=0.02,
        help="seconds of slowdown below which a change is ignored as noise "
            "(default: %(default)s)")
    args = parser.parse_args()

    if args.kernels:
        kernels = [BENCH_DIR / (Path(k).stem + ".f90") for k in args.kernels]
    else:
        kernels = sorted(BENCH_DIR.glob("*.f90"))
    compilers = [("lfortran", args.lfortran, args.lfortran_flags)]
    if not args.no_gfortran:
        compilers.append(("gfortran", args.gfortran, args.gfortran_flags))
    # Kernels are compiled in temporary directories, so relative paths
    # must be resolved first
    for i, (name, exe, flags) in enumerate(compilers):
        path = shutil.which(exe)
        if path is None:
            sys.exit("%s executable not found: %s" % (name, exe))
        compilers[i] = (name, os.path.abspath(path), flags)

    results = []
    for kernel in kernels:
        for name, exe, flags in compilers:
            print("%s with %s ..." % (kernel.stem, name), file=sys.stderr)
            results.append(bench(kernel, name, exe, flags, args.repeat))

    by_key = {(r["kernel"], r["compiler"]): r for r in results}
    names = [c[0] for c in compilers]
    header = "%-16s" % "kernel"
    for what in ("compile", "run"):
        for name in names:
            header += " %12s" % ("%s %s" % (what, name[:2]))
        if len(names) == 2:
            header += " %8s" % "lf/gf"
    print(header)
    for kernel in kernels:
        row = "%-16s" % kernel.stem
        for what in ("compile_time", "run_time"):
            times = [by_key[(kernel.stem, name)][what] for name in names]
            row += "".join(" %11.3fs" % t for t in times)
            if len(times) == 2:
                row += " %8.2f" % (times[0] / times[1])
        print(row)

    if args.json:
        with open(args.json, "w") as f:
            json.dump({"compilers": {n: {"exe": e, "flags": fl}
                for n, e, fl in compilers}, "results": results}, f, indent=2)

    if args.compare:
        with open(args.compare) as f:
            baseline = json.load(f)
        regressions = compare(results, baseline, args.threshold, args.min_diff)
        if regressions:
            sys.exit("%d time(s) regressed by more than %g%%"
                % (regressions, args.threshold))


if __name__ == "__main__":
    main()
