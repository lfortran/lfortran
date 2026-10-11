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
    parser.add_argument("--inspection", action="store_true")
    args = parser.parse_args()
    compiler = Path(args.lfortran).resolve()
    source = (Path(__file__).with_name("traits_runtime_inspection_state_01.f90")
              if args.inspection else Path(__file__).with_suffix(".f90"))
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
        assert "invalid default reached" not in text and "invalid guard reached" not in text

    executable = work / "program"
    run("compile", [compiler, *flags, source, "-o", executable], 0)
    run("allocated", [executable], 0)
    cases = (("owner-default", "pointer-default", "result-default", "owner-no-match", "pointer-no-match")
             if args.inspection else ("borrow", "assignment", "source"))
    for count, use in enumerate(cases, 1):
        message = ("cannot borrow a disassociated runtime trait pointer"
                   if args.inspection and count in (2, 5)
                   else "cannot borrow an unallocated runtime trait object")
        run(use, [executable, *([use] * count)], 1, message)
    print(f"allocated value and {len(cases)} invalid-state diagnostics: passed")


if __name__ == "__main__":
    main()
