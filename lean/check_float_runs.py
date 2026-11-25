#!/usr/bin/env python3
"""Check the Float runs that the Lean proofs are about, by testing.

The theorems `legendre_float_spec`, `kernel_sin_spec` and `kernel_fast_spec`
are about the generated Lean templates run at
`LFortran.machine` (Int32, Float). Lean checks those proofs; this script
checks the two links that the proofs take on trust:

1. Lean's compiled `Float` operations implement `Float.Model`, and an
   LFortran- or GFortran-compiled executable computes the same IEEE
   operations as the Lean run. The script runs the freshly generated Lean
   templates (`lfortran --show-lean`, as in check_translation.sh), an
   LFortran build and a GFortran build of the same Fortran on the same
   inputs and requires bit-identical results.
2. The proved bounds themselves: every result in the specification's domain
   is compared with a high-precision reference (exact rational arithmetic for
   Legendre, 90-digit `decimal` for `sin`). A violation would mean a wrong
   specification or a broken trusted link, since Lean has checked the proof.

Run from the repository root with lfortran on PATH:

    pixi run -e llvm11 python lean/check_float_runs.py
"""

import argparse
import decimal
import math
import os
import random
import struct
import subprocess
import sys
import tempfile
from decimal import Decimal
from fractions import Fraction
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from run_integration_tests import TOOLCHAIN, prepare_runtime  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
LEAN = ROOT / "lean"

DRIVER = """
def parseLine (s : String) : Option (Int32 × Float) :=
  match ((s.trimAscii.copy).splitOn " ").filter (· ≠ "") with
  | [n, b] => do
    let n ← n.toInt?
    let b ← b.toInt?
    some (Int32.ofInt n, Float.ofBits (UInt64.ofInt b))
  | _ => none

def bitsInt (x : Float) : Int :=
  let b := x.toBits.toNat
  if b ≥ 2 ^ 63 then (b : Int) - 2 ^ 64 else b

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  if line.isEmpty then return
  match parseLine line with
  | some (n, x) =>
    let p := legendre_mod.legendre_poly (K := LFortran.machine) n x
    let s := sin_kernels.kernel_sin (K := LFortran.machine) x
    let c := sin_kernels.kernel_fast (K := LFortran.machine) x
    IO.println s!"{bitsInt p} {bitsInt s} {bitsInt c}"
  | none => pure ()
  loop h

def main : IO Unit := do
  let h ← IO.getStdin
  let _ ← h.getLine
  loop h
"""


def to_bits(x):
    return struct.unpack("<q", struct.pack("<d", x))[0]


def from_bits(b):
    return struct.unpack("<d", struct.pack("<q", b))[0]


def is_nan_bits(b):
    return math.isnan(from_bits(b))


def inputs(count, seed):
    rng = random.Random(seed)
    pi_d = math.pi
    xs = [0.0, -0.0, 5e-324, -5e-324, 2.0 ** -1022, -(2.0 ** -1022), 1e-310,
          1.0, -1.0, 0.5, -0.5, pi_d / 2, -pi_d / 2, pi_d, -pi_d,
          2.0 ** 30, -(2.0 ** 30), 1.5707963267948966, -1.5707963267948966,
          6e9, -6e9]
    for k in list(range(-40, 41)) + [10 ** 6, -10 ** 6, 123456789, 1909859317, -1909859317]:
        x = (k + 0.5) * pi_d
        xs += [x, math.nextafter(x, math.inf), math.nextafter(x, -math.inf)]
    for e in range(-60, 31):
        xs += [2.0 ** e, -(2.0 ** e)]
    while len(xs) < count:
        r = rng.random()
        if r < 0.3:
            x = rng.uniform(-1, 1)
        elif r < 0.6:
            x = rng.uniform(-pi_d / 2, pi_d / 2)
        elif r < 0.85:
            x = rng.choice([-1, 1]) * 2.0 ** rng.uniform(-30, 30)
        elif r < 0.95:
            x = rng.choice([-1, 1]) * rng.uniform(0, 2.0 ** 30)
        else:
            x = rng.choice([-1, 1]) * rng.uniform(0, 6e9)
        xs.append(x)
    return [(rng.randint(0, 100), to_bits(x)) for x in xs]


def build_lean_driver(work, lfortran):
    gen = subprocess.run([lfortran, "--show-lean", str(LEAN / "legendre.f90")],
                         capture_output=True, text=True, check=True).stdout
    gen_sin = subprocess.run([lfortran, "--show-lean", str(LEAN / "sin_kernels.f90")],
                             capture_output=True, text=True, check=True).stdout
    body = gen_sin.split("open LFortran\n", 1)[1]
    src = gen + "\n" + body + "\n" + DRIVER
    path = work / "FloatRunsCheck.lean"
    path.write_text(src)
    return path


def run_lean(path, runtime, data):
    env = os.environ.copy()
    env["LEAN_PATH"] = str(runtime)
    out = subprocess.run(["elan", "run", TOOLCHAIN, "lean", "--run", str(path)],
                         input=data, capture_output=True, text=True, env=env)
    if out.returncode != 0 or ".lean:" in out.stdout[:2000]:
        sys.exit(f"the Lean driver {path} failed:\n{out.stdout[:3000]}{out.stderr[:3000]}")
    return out.stdout


def build_fortran(work, compiler, name):
    exe = work / name
    srcs = [str(LEAN / "legendre.f90"), str(LEAN / "sin_kernels.f90"),
            str(LEAN / "float_runs_check.f90")]
    if compiler == "gfortran":
        cmd = ["gfortran", "-O0", "-ffp-contract=off", "-J", str(work)] + srcs + ["-o", str(exe)]
    else:
        cmd = [compiler] + srcs + ["-o", str(exe)]
    subprocess.run(cmd, check=True, cwd=work)
    return exe


def parse(out):
    return [tuple(int(t) for t in line.split()) for line in out.splitlines() if line.strip()]


# High-precision references --------------------------------------------------

decimal.getcontext().prec = 90


def dec_pi():
    """Pi to the current precision (decimal module recipe)."""
    decimal.getcontext().prec += 2
    three = Decimal(3)
    lasts, t, s, n, na, d, da = 0, three, 3, 1, 0, 0, 24
    while s != lasts:
        lasts = s
        n, na = n + na, na + 8
        d, da = d + da, da + 32
        t = (t * n) / d
        s += t
    decimal.getcontext().prec -= 2
    return +s


PI = dec_pi()


def dec_sin(x):
    """sin of an exact Decimal, with range reduction by 2 pi for |x| > pi
    only, so that tiny arguments keep their full relative precision."""
    if abs(x) > PI:
        two_pi = 2 * PI
        x = x - two_pi * (x / two_pi).to_integral_value(rounding=decimal.ROUND_FLOOR)
        if x > PI:
            x -= two_pi
    term, s, i = x, x, 1
    while True:
        term = -term * x * x / ((2 * i) * (2 * i + 1))
        if abs(term) < Decimal(10) ** -85:
            break
        s += term
        i += 1
    return s


def legendre_exact(n, x):
    a, b = Fraction(1), x
    if n == 0:
        return a
    for k in range(2, n + 1):
        a, b = b, ((2 * k - 1) * x * b - (k - 1) * a) / k
    return b


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--count", type=int, default=20000)
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--lfortran", default="lfortran")
    args = ap.parse_args()

    data_in = inputs(args.count, args.seed)
    data = f"{len(data_in)}\n" + "".join(f"{n} {b}\n" for n, b in data_in)
    work = Path(tempfile.mkdtemp(prefix="lfortran-floatruns-"))
    runtime = prepare_runtime(TOOLCHAIN)
    lean_out = parse(run_lean(build_lean_driver(work, args.lfortran), runtime, data))
    results = {"lean": lean_out}
    for compiler, name in [(args.lfortran, "lfortran_exe"), ("gfortran", "gfortran_exe")]:
        exe = build_fortran(work, compiler, name)
        out = subprocess.run([str(exe)], input=data, capture_output=True, text=True,
                             check=True).stdout
        results[compiler if compiler == "gfortran" else "lfortran"] = parse(out)

    failures = 0
    n_in = len(data_in)
    for key, rows in results.items():
        if len(rows) != n_in:
            print(f"{key}: {len(rows)} results for {n_in} inputs")
            failures += 1
    if failures:
        return 1

    # 1. bit-identical runs (NaNs compare equal regardless of payload)
    mismatches = 0
    for i, (n, b) in enumerate(data_in):
        for col, what in enumerate(["legendre_poly", "kernel_sin", "kernel_fast"]):
            # int() of |x/pi| >= 2^31 overflows (undefined in Fortran)
            if what == "kernel_fast" and abs(from_bits(b)) > 6e9:
                continue
            vals = {k: rows[i][col] for k, rows in results.items()}
            if len({("nan" if is_nan_bits(v) else v) for v in vals.values()}) != 1:
                mismatches += 1
                if mismatches <= 10:
                    print(f"mismatch {what}(n={n}, x={from_bits(b)!r}): "
                          + ", ".join(f"{k}={from_bits(v)!r}" for k, v in vals.items()))
    compared = sum(3 - (abs(from_bits(b)) > 6e9) for _, b in data_in)
    print(f"bit-identical: {compared - mismatches} of {compared} results "
          "(lean, lfortran, gfortran)")

    # 2. proved bounds against high-precision references
    worst = {"legendre_poly": 0.0, "kernel_sin": 0.0, "kernel_fast": 0.0}
    checked = {k: 0 for k in worst}
    violations = 0
    for i, (n, b) in enumerate(data_in):
        x = from_bits(b)
        p, s, c = (from_bits(v) for v in results["lean"][i])
        checks = []
        if abs(x) <= 1:
            exact = legendre_exact(n, Fraction(x))
            checks.append(("legendre_poly", abs(Fraction(p) - exact) if math.isfinite(p) else None,
                           Fraction(1, 10 ** 11)))
        xd = Decimal(x)
        if abs(xd) <= PI / 2:
            err = abs(Decimal(s) - dec_sin(xd)) if math.isfinite(s) else None
            checks.append(("kernel_sin", err,
                           Decimal(115) / Decimal(10) ** 17 * abs(xd) + Decimal(2) ** -1075))
        if abs(x) <= 6e9:
            err = abs(Decimal(c) - dec_sin(xd)) if math.isfinite(c) else None
            checks.append(("kernel_fast", err, Decimal(22) / Decimal(10) ** 16))
        for name, err, bound in checks:
            checked[name] += 1
            if err is None or err > bound:
                violations += 1
                print(f"bound violated: {name}(n={n}, x={x!r}): error {err}, bound {bound}")
                continue
            worst[name] = max(worst[name], float(err / bound) if bound else 0.0)
    for name in worst:
        print(f"{name}: {checked[name]} results within the proved bound, "
              f"largest error / bound = {worst[name]:.3g}")
    return 1 if mismatches or violations else 0


if __name__ == "__main__":
    sys.exit(main())
