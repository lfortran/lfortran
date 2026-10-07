"""Inject failure at each hidden allocation, including component initialization."""

import argparse
import os
from pathlib import Path
import re
import shutil
import subprocess


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--cc", required=True)
    parser.add_argument("--sysroot")
    parser.add_argument("--work-dir", required=True)
    parser.add_argument("--fast", action="store_true")
    args = parser.parse_args()
    sources = Path(__file__).resolve().parent
    compiler = str(Path(args.lfortran).resolve())
    work = Path(args.work_dir).resolve() / f"trait-allocation-failure-{os.getpid()}"
    work.mkdir(parents=True)
    environment = os.environ.copy()
    environment.update(TMPDIR=str(work), TMP=str(work), TEMP=str(work))
    environment.pop("LFORTRAN_TEST_FAIL_AT", None)
    environment.pop("LFORTRAN_TEST_FAILURE_MODE", None)
    flags = ["--verify-all-passes", "--no-color"]
    if args.fast:
        flags.append("--fast")
    cc_flags = ["-isysroot", args.sysroot] if args.sysroot else []

    def run(command, env=environment):
        result = subprocess.run(list(map(str, command)), cwd=work, env=env,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        return result.returncode, result.stdout

    try:
        native = work / "allocator.o"
        for command in (
                [args.cc, *cc_flags, "-c", sources / "traits_runtime_owning_failure_01.c", "-o", native],
                [compiler, *flags, "-c", sources / "traits_runtime_owning_failure_01.f90",
                 "-o", work / "program.o"],
                [compiler, work / "program.o", native, "-o", work / "program"]):
            status, output = run(command)
            assert status == 0, output
        status, output = run([work / "program"])
        assert status == 0, output
        match = re.search(r"owning allocations:\s*(\d+)", output)
        assert match, output
        count = int(match.group(1))
        for point in range(1, count + 1):
            status, output = run([work / "program"],
                                {**environment, "LFORTRAN_TEST_FAIL_AT": str(point)})
            assert status == 1 and "allocation failed" in output, (point, status, output)
        for mode, message in (
                (1, "cannot copy an unallocated concrete runtime trait source"),
                (2, "cannot allocate an already allocated runtime trait object"),
                (3, "cannot deallocate an unallocated runtime trait object"),
                (4, "cannot borrow a disassociated runtime trait pointer"),
                (5, "cannot borrow an unallocated runtime trait object")):
            status, output = run([work / "program"],
                                {**environment, "LFORTRAN_TEST_FAILURE_MODE": str(mode)})
            assert status == 1 and message in output, (mode, status, output)
        print(f"all {count} allocation failures and five invalid states terminate cleanly")
    finally:
        shutil.rmtree(work)


if __name__ == "__main__":
    main()
