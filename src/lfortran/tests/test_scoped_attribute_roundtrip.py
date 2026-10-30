#!/usr/bin/env python3
"""Regenerate the scoped-attribute kernels, preserving pending operations."""

import argparse
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import subprocess


def run(command, cwd, log):
    log.append("+ " + " ".join(map(str, command)) + "\n")
    return subprocess.run(command, cwd=cwd, check=True, text=True,
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout


def check(compiler, sources, workdir, name, mode, flags):
    """Compiles and runs `name` in `mode`; returns what it did, and the
    error that stopped it, if any."""
    log = []
    try:
        # Regenerate just the module that owns the kernels. The pending
        # operation must remain separately compiled and invisible to them.
        source = (sources / (name + ".f90")).read_text()
        marker = "\nprogram " + name + "\n"
        kernels, driver = source.split(marker)
        cwd = (workdir / name / mode).resolve()
        cwd.mkdir(parents=True, exist_ok=True)
        (cwd / "kernels.f90").write_text(kernels)
        (cwd / "driver.f90").write_text(marker + driver)
        regenerated = run([compiler, "--show-fortran", "kernels.f90"], cwd, log)
        (cwd / "regenerated.f90").write_text(regenerated)
        run([compiler, *flags, "--verify-all-passes", "-c",
             str(sources / (name + "_pending.f90")), "-o", "pending.o"], cwd, log)
        run([compiler, *flags, "--verify-all-passes", "-c",
             "regenerated.f90", "-o", "kernels.o"], cwd, log)
        run([compiler, *flags, "--verify-all-passes", "driver.f90",
             "pending.o", "kernels.o", "-o", "check"], cwd, log)
        log.append(run([str(cwd / "check")], cwd, log))
        log.append("PASS %s %s\n" % (name, mode))
        return log, None
    except subprocess.CalledProcessError as error:
        log.append(error.stdout)
        return log, error


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True, type=Path)
    parser.add_argument("--workdir", required=True, type=Path)
    args = parser.parse_args()
    compiler = str(args.lfortran.resolve())
    sources = Path(__file__).resolve().parents[3] / "integration_tests"
    jobs = [(name, mode, flags)
            for name in ("noalias_dummy_06", "dead_array_store_03")
            for mode, flags in (("default", []), ("fast", ["--fast"]),
                                ("f23", ["--std=f23"]))]
    # The jobs are independent: run them concurrently.
    with ThreadPoolExecutor(max_workers=len(jobs)) as pool:
        results = list(pool.map(lambda job: check(compiler, sources,
            args.workdir, *job), jobs))
    for log, error in results:
        print("".join(log), end="", flush=True)
    for _, error in results:
        if error:
            raise error


if __name__ == "__main__":
    main()
