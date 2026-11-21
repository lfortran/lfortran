#!/usr/bin/env python3
"""Benchmark reallocation on assignment (realloc-lhs) across compilers.

Builds realloc_lhs.f90, linked with the separately compiled
realloc_lhs_ext.f90, once per configuration, runs every kernel best-of-N,
checks that the checksums agree across configurations and prints a
GitHub-flavored Markdown table. See README.md in this directory.

A configuration is "label=compiler flags...", for example

    "main=/path/to/main/lfortran --fast --realloc-lhs-arrays"

The first configuration is the baseline of the percentages. Uses only the
Python 3 standard library.
"""

import argparse
import os
import platform
import shlex
import shutil
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
SOURCE = os.path.join(HERE, "realloc_lhs.f90")
# Compiled separately (with -c) and linked, so that the compiler of SOURCE
# cannot inline its procedures or see what they do.
EXTRA_SOURCES = [os.path.join(HERE, "realloc_lhs_ext.f90")]

# (kernel, n, reps, requires_realloc). reps is chosen so that one run takes
# roughly 0.1-0.3 s. Kernels that require reallocation on assignment are
# shown as n/a in configurations that disable it.
CASES = [
    ("add", 3, 50_000_000, False),
    ("add", 1000, 2_000_000, False),
    ("self", 3, 50_000_000, False),
    ("self", 1000, 2_000_000, False),
    ("copy", 3, 50_000_000, False),
    ("copy", 1000, 4_000_000, False),
    ("ctor", 3, 30_000_000, False),
    ("func", 3, 20_000_000, False),
    ("func", 1000, 1_000_000, False),
    ("allocfn", 3, 10_000_000, False),
    ("allocfn", 1000, 200_000, False),
    ("section", 8, 1_000_000, False),
    ("section", 1000, 200_000, False),
    ("matmul", 3, 5_000_000, False),
    ("matmul", 64, 5_000, False),
    ("add2d", 3, 10_000_000, False),
    ("add2d", 100, 150_000, False),
    ("pack", 3, 10_000_000, False),
    ("pack", 1000, 200_000, False),
    ("comp", 3, 50_000_000, False),
    ("comp", 1000, 2_000_000, False),
    ("dtexpr", 3, 50_000_000, False),
    ("dtexpr", 1000, 2_000_000, False),
    ("inout", 3, 50_000_000, False),
    ("inout", 1000, 1_000_000, False),
    ("stencil", 16, 10_000_000, False),
    ("stencil", 1000, 1_000_000, False),
    ("append", 16, 10_000_000, True),
    ("append", 1000, 1_000_000, True),
    ("local", 3, 10_000_000, True),
    ("local", 1000, 1_000_000, True),
    ("resize", 8, 10_000_000, True),
    ("resize", 1000, 1_000_000, True),
    # Kernels whose shape check the compiler cannot remove or move out of
    # the loop at compile time (see README.md).
    ("xadd", 3, 30_000_000, False),
    ("xadd", 1000, 2_000_000, False),
    ("xadd2d", 3, 20_000_000, False),
    ("xadd3d", 3, 10_000_000, False),
    ("xchain", 3, 20_000_000, False),
    ("xchain", 1000, 1_000_000, False),
    ("xchainalloc", 3, 10_000_000, False),
    ("xchainlocal", 3, 10_000_000, True),
    ("opaque", 3, 50_000_000, False),
    ("exit", 3, 50_000_000, False),
    ("target", 3, 50_000_000, False),
    ("modvar", 3, 50_000_000, False),
    ("internal", 3, 50_000_000, False),
    ("saved", 3, 30_000_000, False),
    ("pairs", 16, 200_000, False),
    ("pairsc", 16, 200_000, False),
    ("concur", 64, 1_000_000, False),
    ("jagged", 3, 3_000_000, False),
    ("jagged", 1000, 20_000, False),
    ("while", 3, 50_000_000, False),
    ("cond", 3, 50_000_000, False),
    ("chain", 3, 20_000_000, False),
    ("chain", 1000, 1_000_000, False),
    ("datadep", 3, 50_000_000, False),
    ("alternate", 3, 10_000_000, True),
    ("ctorv", 3, 30_000_000, False),
    ("charcond", 3, 20_000_000, False),
    ("ptrcond", 3, 50_000_000, False),
    ("exit2d", 3, 10_000_000, False),
    ("intabs", 3, 30_000_000, False),
]


def inner_iterations(kernel, n):
    """The assignments of one repetition of a kernel that runs several,
    so that the table shows the time per assignment."""
    if kernel in ("pairs", "pairsc"):
        return n * n
    if kernel == "concur":
        return n
    if kernel == "jagged":
        return 16
    return 1

# Flags that disable reallocation on assignment (LFortran, GFortran/Flang,
# Intel, NVIDIA). A configuration with one of them skips kernels that
# require reallocation.
NO_REALLOC_FLAGS = (
    "--disable-realloc-lhs-arrays",
    "--no-realloc-lhs-arrays",
    "-fno-realloc-lhs",
    "-nostandard-realloc-lhs",
    "norealloc_lhs",
    "-Mnorealloc",
)
REALLOC_FLAGS = (
    "--realloc-lhs-arrays",
    "-frealloc-lhs",
    "-standard-realloc-lhs",
    "realloc_lhs",
    "-Mrealloc",
)


class Config:
    def __init__(self, spec):
        label, sep, command = spec.partition("=")
        if not sep or not label.strip() or not command.strip():
            raise SystemExit(
                f"error: configuration {spec!r} is not 'label=compiler flags'")
        self.label = label.strip()
        self.command = shlex.split(command)
        self.no_realloc = any(t in NO_REALLOC_FLAGS for t in self.command)
        self.explicit = self.no_realloc or any(
            t in REALLOC_FLAGS for t in self.command)
        self.exes = []
        self.version = ""

    def spec(self):
        return " ".join(shlex.quote(t) for t in self.command)


def default_configs():
    configs = []
    if shutil.which("lfortran"):
        configs += [
            "lfortran-no-realloc=lfortran --fast --disable-realloc-lhs-arrays",
            "lfortran-realloc=lfortran --fast --realloc-lhs-arrays",
        ]
    if shutil.which("gfortran"):
        configs += [
            "gfortran-no-realloc=gfortran -O3 -march=native -fno-realloc-lhs",
            "gfortran-realloc=gfortran -O3 -march=native -frealloc-lhs",
        ]
    if not configs:
        raise SystemExit("error: neither lfortran nor gfortran is on PATH; "
                         "pass configurations explicitly")
    return configs


def compiler_version(command):
    try:
        p = subprocess.run([command[0], "--version"], capture_output=True,
                           text=True, timeout=60)
    except (OSError, subprocess.TimeoutExpired):
        return ""
    lines = [l.strip() for l in p.stdout.splitlines() if l.strip()]
    return lines[0] if lines else ""


def padded_source(placement, path):
    """Write to `path` a copy of SOURCE with a procedure of `placement`
    blocks of code before the kernels, which moves them to other addresses,
    and return `path`. The driver calls it for a kernel name no run uses, so
    that it is kept."""
    with open(SOURCE) as f:
        text = f.read()
    # Opaque calls, which the compiler cannot merge, about 8 bytes each.
    body = "call ext_tick(k)\n" * (5 * placement)
    padding = ("\ncontains\n\nsubroutine aaa_placement_padding(k)\n"
               "integer, intent(inout) :: k\n" + body +
               "end subroutine\n")
    text = text.replace("\ncontains\n", padding, 1)
    case = ('case ("aaa_placement_padding")\n'
            '    call aaa_placement_padding(n)\ncase default\n')
    text = text.replace("case default\n", case, 1)
    with open(path, "w") as f:
        f.write(text)
    return path


def build(config, build_dir, index, placements):
    # Each configuration and placement gets its own directory, since the
    # compilers write the module file into the current directory.
    config.exes = []
    for placement in range(placements):
        d = os.path.join(build_dir, f"{index}", f"p{placement}")
        os.makedirs(d, exist_ok=True)
        exe = os.path.join(d, "realloc_lhs")
        source = SOURCE if placement == 0 else padded_source(
            placement, os.path.join(d, "realloc_lhs.f90"))
        objects = []
        commands = []
        for src in EXTRA_SOURCES:
            obj = os.path.join(
                d, os.path.splitext(os.path.basename(src))[0] + ".o")
            objects.append(obj)
            commands.append((config.command + ["-c", src, "-o", obj], obj))
        commands.append((config.command + [source] + objects + ["-o", exe],
                         exe))
        for cmd, output in commands:
            print(f"[build] {config.label}: "
                  f"{' '.join(shlex.quote(t) for t in cmd)}", file=sys.stderr)
            p = subprocess.run(cmd, cwd=d, capture_output=True, text=True)
            if p.returncode != 0 or not os.path.exists(output):
                sys.stderr.write(p.stdout + p.stderr)
                raise SystemExit(f"error: building configuration "
                                 f"{config.label!r} failed")
        config.exes.append(exe)
    config.version = compiler_version(config.command)


def run_once(exe, pin, kernel, n, reps):
    """Return (seconds, checksum) or an error string."""
    cmd = pin + [exe, kernel, str(n), str(reps)]
    try:
        p = subprocess.run(cmd, capture_output=True, text=True, timeout=600)
    except subprocess.TimeoutExpired:
        return "timeout"
    if p.returncode != 0:
        msg = [l.strip() for l in (p.stderr + p.stdout).splitlines()
               if l.strip()]
        return f"exit code {p.returncode}: {' / '.join(msg[:2])}"
    fields = p.stdout.split()
    if len(fields) != 5 or fields[0] != kernel:
        return f"unexpected output: {p.stdout.strip()!r}"
    return float(fields[3]), float(fields[4])


def fmt_ns(x):
    if x < 10:
        return f"{x:.2f}"
    if x < 100:
        return f"{x:.1f}"
    return f"{x:.0f}"


def fmt_pct(x, base):
    return f"{100.0 * (x / base - 1.0):+.1f}%"


def pick_cpu(requested):
    if requested is not None:
        return requested
    cpus = sorted(os.sched_getaffinity(0)) if hasattr(
        os, "sched_getaffinity") else []
    # The last allowed CPU: CPU 0 usually serves more interrupts.
    return str(cpus[-1]) if cpus else None


def main():
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument(
        "configs", nargs="*", metavar="LABEL=COMPILER FLAGS",
        help="configurations; default: lfortran --fast with "
             "--disable-realloc-lhs-arrays and --realloc-lhs-arrays, and "
             "gfortran -O3 -march=native with -fno-realloc-lhs and -frealloc-lhs "
             "(each only if found on PATH)")
    ap.add_argument("-r", "--repeat", type=int, default=5,
                    help="runs per kernel and configuration; the best one "
                         "is reported (default: 5)")
    ap.add_argument("-k", "--kernels", default="",
                    help="comma-separated kernels to run, either NAME or "
                         "NAME:N (default: all)")
    ap.add_argument("--scale", type=float, default=1.0,
                    help="multiply every repetition count (e.g. 0.1 for a "
                         "quick run; default: 1)")
    ap.add_argument("--cpu", default=None,
                    help="CPU to pin the runs to with taskset (default: the "
                         "last CPU this process may use)")
    ap.add_argument("--no-pin", action="store_true",
                    help="do not pin the runs with taskset")
    ap.add_argument("--placements", type=int, default=1,
                    help="build every configuration this many times, with "
                         "the kernels at different addresses (padding "
                         "code before them), and report the best time over "
                         "all of them, so that a lucky or unlucky placement "
                         "of a loop does not decide a comparison (default: "
                         "1)")
    ap.add_argument("--rtol", type=float, default=1e-9,
                    help="relative tolerance of the checksum comparison "
                         "(default: 1e-9)")
    ap.add_argument("--build-dir", default=None,
                    help="directory for the executables (default: a "
                         "temporary directory that is removed afterwards)")
    ap.add_argument("-o", "--output", default=None,
                    help="also write the Markdown table to this file")
    ap.add_argument("--list", action="store_true",
                    help="list the kernels and exit")
    args = ap.parse_args()

    if args.list:
        for kernel, n, reps, req in CASES:
            inner = inner_iterations(kernel, n)
            print(f"{kernel}:{n} reps={reps}"
                  + (f" x {inner} assignments" if inner > 1 else "")
                  + (" (requires realloc)" if req else ""))
        return 0

    cases = CASES
    if args.kernels:
        wanted = [k.strip() for k in args.kernels.split(",") if k.strip()]
        cases = [c for c in CASES
                 if c[0] in wanted or f"{c[0]}:{c[1]}" in wanted]
        known = {c[0] for c in CASES} | {f"{c[0]}:{c[1]}" for c in CASES}
        unknown = [k for k in wanted if k not in known]
        if unknown:
            raise SystemExit(f"error: unknown kernels: {', '.join(unknown)}")
    cases = [(k, n, max(1, int(reps * args.scale)), req)
             for k, n, reps, req in cases]

    configs = [Config(s) for s in (args.configs or default_configs())]
    labels = [c.label for c in configs]
    if len(set(labels)) != len(labels):
        raise SystemExit("error: configuration labels must be unique")
    for c in configs:
        if not c.explicit:
            print(f"warning: configuration {c.label!r} passes no explicit "
                  "realloc-lhs flag; its results depend on the compiler's "
                  "default", file=sys.stderr)

    pin = []
    cpu = None
    if not args.no_pin and shutil.which("taskset"):
        cpu = pick_cpu(args.cpu)
        if cpu is not None:
            pin = ["taskset", "-c", cpu]
    if not pin:
        print("warning: runs are not pinned to a CPU", file=sys.stderr)

    tmp = None
    build_dir = args.build_dir
    if build_dir is None:
        tmp = tempfile.mkdtemp(prefix="realloc_lhs_bench_")
        build_dir = tmp
    else:
        build_dir = os.path.abspath(build_dir)
    try:
        for i, c in enumerate(configs):
            build(c, build_dir, i, args.placements)

        # results[case index][config index] = best seconds, or None (n/a),
        # or an error string
        results = []
        checksums = []
        start = time.time()
        for ci, (kernel, n, reps, req) in enumerate(cases):
            best = [None] * len(configs)
            sums = [None] * len(configs)
            for _ in range(args.repeat):
                # Interleave the configurations, so that a slow phase of a
                # shared machine affects all of them alike.
                for j, c in enumerate(configs):
                    if req and c.no_realloc:
                        continue
                    for exe in c.exes:
                        if isinstance(best[j], str):
                            break
                        r = run_once(exe, pin, kernel, n, reps)
                        if isinstance(r, str):
                            best[j] = r
                            break
                        t, s = r
                        if sums[j] is not None and s != sums[j]:
                            best[j] = f"checksum changed between runs: " \
                                      f"{sums[j]!r} vs {s!r}"
                            break
                        sums[j] = s
                        if best[j] is None or t < best[j]:
                            best[j] = t
            results.append(best)
            checksums.append(sums)
            per = reps * inner_iterations(kernel, n)
            shown = "  ".join(
                f"{c.label}=" + (fmt_ns(b * 1e9 / per)
                                 if isinstance(b, float) else
                                 ("n/a" if b is None else "FAIL"))
                for c, b in zip(configs, best))
            print(f"[{ci + 1}/{len(cases)}] {kernel}:{n}  {shown}  "
                  f"({time.time() - start:.0f} s)", file=sys.stderr)
    finally:
        if tmp is not None:
            shutil.rmtree(tmp, ignore_errors=True)

    # Verify the checksums across configurations.
    errors = []
    for (kernel, n, reps, req), best, sums in zip(cases, results, checksums):
        for c, b in zip(configs, best):
            if isinstance(b, str):
                errors.append(f"{kernel}:{n} {c.label}: {b}")
        ref = None
        for c, s in zip(configs, sums):
            if s is None:
                continue
            if ref is None:
                ref = (c.label, s)
                continue
            tol = args.rtol * max(abs(ref[1]), abs(s), 1e-300)
            if abs(s - ref[1]) > tol:
                errors.append(
                    f"{kernel}:{n}: checksum mismatch: {ref[0]}={ref[1]!r} "
                    f"{c.label}={s!r}")

    # The Markdown table.
    out = []
    out.append("| kernel | n | " + " | ".join(labels) + " |")
    out.append("| --- | ---: | " + " | ".join("---:" for _ in labels) + " |")
    for (kernel, n, reps, req), best in zip(cases, results):
        cells = []
        per = reps * inner_iterations(kernel, n)
        base = best[0] * 1e9 / per if isinstance(best[0], float) else None
        for j, b in enumerate(best):
            if b is None:
                cells.append("n/a")
            elif isinstance(b, str):
                cells.append("**FAIL**")
            else:
                ns = b * 1e9 / per
                cell = fmt_ns(ns)
                if j > 0 and base is not None and base > 0:
                    cell += f" ({fmt_pct(ns, base)})"
                cells.append(cell)
        name = kernel + (" *" if req else "")
        out.append(f"| {name} | {n} | " + " | ".join(cells) + " |")
    out.append("")
    out.append(f"Nanoseconds per iteration (per assignment for pairs, "
               f"pairsc, concur and jagged), best of {args.repeat}"
               + (f" runs of each of {args.placements} placements"
                  if args.placements > 1 else "")
               + (f", pinned to CPU {cpu}" if pin else ", not pinned")
               + f"; percentages relative to {labels[0]}. "
               "Kernels marked * require reallocation on assignment and are "
               "n/a without it.")
    out.append("")
    for c in configs:
        version = f" ({c.version})" if c.version else ""
        out.append(f"- {c.label}: `{c.spec()}`{version}")
    out.append(f"- machine: {platform.machine()}, {platform.system()} "
               f"{platform.release()}, {os.cpu_count()} CPUs")
    table = "\n".join(out) + "\n"

    print(table, end="")
    if args.output:
        with open(args.output, "w") as f:
            f.write(table)

    if errors:
        print("\nERROR: the benchmark failed:", file=sys.stderr)
        for e in errors:
            print(f"  {e}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
