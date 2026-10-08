"""Compile the unchanged paper programs and check both interactive choices."""

import argparse
from decimal import Decimal
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import time


SOURCE_HASHES = {
    "functional1": "1d2edfe92ea41958e0671fefccad774b1c9ae519078825071156c349199b609f",
    "functional2": "05d98eb6f43b20cd9f53f610301c6fecd96677babe6662f504c7093dd5c5a7e6",
}


def main():
    parser = argparse.ArgumentParser()
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--lfortran", type=Path)
    mode.add_argument("--executable", type=Path)
    parser.add_argument("--source", type=Path)
    parser.add_argument("--work-dir", type=Path, default=Path.cwd())
    parser.add_argument("--fast", action="store_true")
    args = parser.parse_args()
    work = args.work_dir.resolve() / f"paper-functional-{os.getpid()}-{time.time_ns()}"
    work.mkdir(parents=True)
    environment = {**os.environ, "TMPDIR": str(work), "TMP": str(work), "TEMP": str(work)}
    records = []

    def run(name, command, cwd, stdin=None):
        command = list(map(str, command))
        log = work / f"{name}.log"
        with log.open("w") as output:
            result = subprocess.run(command, cwd=cwd, env=environment,
                                    input=stdin, text=True, timeout=120,
                                    stdout=output, stderr=subprocess.STDOUT)
        records.append({"name": name, "command": command, "cwd": str(cwd),
                        "stdin": stdin, "returncode": result.returncode, "log": str(log)})
        (work / "commands.json").write_text(json.dumps(records, indent=2) + "\n")
        text = log.read_text()
        if result.returncode:
            raise RuntimeError(f"{name} failed ({result.returncode}):\n{text}")
        return text

    sources = ([args.source.resolve()] if args.source else
               [Path(__file__).resolve().parent / "traits_paper_functional" / f"{name}.f90"
                for name in SOURCE_HASHES])
    if args.executable and len(sources) != 1:
        parser.error("--executable requires --source")
    for source in sources:
        name = source.stem
        if hashlib.sha256(source.read_bytes()).hexdigest() != SOURCE_HASHES[name]:
            raise RuntimeError(f"the authoritative {source.name} fixture was changed")
        private = work / name
        private.mkdir()
        if args.lfortran:
            modules = private / "modules"
            modules.mkdir()
            executable = private / name
            flags = ["--no-color", "--verify-all-passes"]
            if args.fast:
                flags.append("--fast")
            run(name + "-compile", [args.lfortran.resolve(), *flags, "-J", modules,
                                    source, "-o", executable], private)
        else:
            executable = args.executable.resolve()
        for key in (1, 2):
            output = run(f"{name}-key{key}", [executable], private, f"{key}\n")
            match = re.search(
                r"Choose an averaging method:\s*([+-]?\d+)\s+([+-]?\d+\.\d+)\s*\Z",
                output)
            if not match or int(match[1]) != 3 or Decimal(match[2]) != Decimal("3.0"):
                raise RuntimeError(f"{name}, choice {key}: expected 3 and 3.0:\n{output}")
            print(f"{name}, choice {key}: integer 3, real 3.0", flush=True)


if __name__ == "__main__":
    main()
