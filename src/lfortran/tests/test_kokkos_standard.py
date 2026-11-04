"""Check the C++ standard the driver passes to the C++ compiler."""

import argparse
import os
from pathlib import Path
import subprocess


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--workdir", required=True)
    args = parser.parse_args()
    workdir = Path(args.workdir).resolve()
    workdir.mkdir(parents=True, exist_ok=True)
    source = workdir / "main.f90"
    source.write_text("end\n")
    # An empty Kokkos prefix: compilation fails, but only after the driver
    # has printed the C++ compiler command with the selected standard.
    include = workdir / "include"
    include.mkdir(exist_ok=True)
    failures = []
    for options, expected in (([], "c++20"),
                              (["--cpp-std=c++20"], "c++20"),
                              (["--cpp-std=c++17"], "c++17")):
        result = run(args.lfortran, source, workdir, options)
        if result.returncode == 0 or f"-std={expected} " not in result.stdout:
            failures.append(f"{options}: expected -std={expected}\n{result.stdout}")
    result = run(args.lfortran, source, workdir, ["--cpp-std=c++14"])
    if result.returncode == 0 or "-std=" in result.stdout:
        failures.append(f"--cpp-std=c++14 must be rejected\n{result.stdout}")
    assert not failures, "\n".join(failures)
    print("C++ standard selection checks passed")


def run(lfortran, source, workdir, options):
    env = dict(os.environ, LFORTRAN_KOKKOS_DIR=str(workdir), TMPDIR=str(workdir))
    return subprocess.run(
        [lfortran, "--backend=cpp", "--verbose", *options, "-c", str(source),
         "-o", str(workdir / "main.o")],
        env=env, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        check=False,
    )


if __name__ == "__main__":
    main()
