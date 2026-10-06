"""Check diagnostics for every value use of an unallocated trait result."""

import argparse
import os
from pathlib import Path
import subprocess
import time


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--work-dir", required=True)
    parser.add_argument("--fast", action="store_true")
    args = parser.parse_args()
    compiler = Path(args.lfortran).resolve()
    source = Path(__file__).with_suffix(".f90")
    work = Path(args.work_dir).resolve() / f"trait-result-states-{os.getpid()}-{time.time_ns()}"
    work.mkdir(parents=True)
    environment = {**os.environ, "TMPDIR": str(work), "TMP": str(work), "TEMP": str(work)}
    flags = ["--no-color", "--verify-all-passes"]
    if args.fast:
        flags.append("--fast")

    def run(name, command, expected_status, message=""):
        command = list(map(str, command))
        print("+", " ".join(command), flush=True)
        log = work / (name + ".log")
        with log.open("w") as output:
            process = subprocess.run(command, cwd=work, env=environment,
                                     stdout=output, stderr=subprocess.STDOUT)
        text = log.read_text()
        assert process.returncode == expected_status and message in text, (
            command, process.returncode, text, str(log))

    executable = work / "program"
    run("compile", [compiler, *flags, source, "-o", executable], 0)
    run("allocated", [executable], 0)
    for count, use in enumerate(("borrow", "assignment", "source"), 1):
        run(use, [executable, *([use] * count)], 1,
            "cannot borrow an unallocated runtime trait object")
    print("allocated results and three unallocated-result diagnostics: passed")


if __name__ == "__main__":
    main()
