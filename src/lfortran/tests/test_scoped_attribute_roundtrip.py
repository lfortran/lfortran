#!/usr/bin/env python3
"""Regenerate the scoped-attribute kernels, preserving pending operations."""

import argparse
from pathlib import Path
import subprocess


def run(command, cwd):
    print("+", " ".join(map(str, command)), flush=True)
    return subprocess.run(command, cwd=cwd, check=True, text=True,
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True, type=Path)
    parser.add_argument("--workdir", required=True, type=Path)
    args = parser.parse_args()
    compiler = str(args.lfortran.resolve())
    sources = Path(__file__).resolve().parents[3] / "integration_tests"
    for name in ("noalias_dummy_06", "dead_array_store_03"):
        # Regenerate just the module that owns the kernels. The pending
        # operation must remain separately compiled and invisible to them.
        source = (sources / (name + ".f90")).read_text()
        marker = "\nprogram " + name + "\n"
        kernels, driver = source.split(marker)
        for mode, flags in (("default", []), ("fast", ["--fast"]),
                            ("f23", ["--std=f23"])):
            cwd = (args.workdir / name / mode).resolve()
            cwd.mkdir(parents=True, exist_ok=True)
            (cwd / "kernels.f90").write_text(kernels)
            (cwd / "driver.f90").write_text(marker + driver)
            regenerated = run([compiler, "--show-fortran", "kernels.f90"], cwd)
            (cwd / "regenerated.f90").write_text(regenerated)
            run([compiler, *flags, "--verify-all-passes", "-c",
                 str(sources / (name + "_pending.f90")), "-o", "pending.o"], cwd)
            run([compiler, *flags, "--verify-all-passes", "-c",
                 "regenerated.f90", "-o", "kernels.o"], cwd)
            run([compiler, *flags, "--verify-all-passes", "driver.f90",
                 "pending.o", "kernels.o", "-o", "check"], cwd)
            print(run([str(cwd / "check")], cwd), end="")
            print("PASS", name, mode, flush=True)


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as error:
        print(error.stdout, end="")
        raise
