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
    "functional1": "51a5c11e8b77624dd7cd653c0f1ded7fccee860b22e8d3668b3f2c09008eca25",
    "functional2": "808b71c5393ee9dfc19e192cbe7eb689dedd5a9f25fecd0e4018b30ec0cbbeb1",
}
INLINE_HASHES = {
    "inline_25.f90": "53af21a4684d3995fe2d5d14a412f13b183cfa48801f2e0170a963bf76ea6cc8",
    "inline_28.f90": "787574712d4127623f9912f2173397e20d7b5865ddfcfd27cdf01cb01d993fee",
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

    def compile_source(source, private):
        modules = private / "modules"
        modules.mkdir()
        executable = private / source.stem
        flags = ["--no-color", "--verify-all-passes"]
        if args.fast:
            flags.append("--fast")
        run(source.stem + "-compile",
            [args.lfortran.resolve(), *flags, "-I", source.parent, "-J", modules,
             source, "-o", executable], private)
        return executable

    source_dir = Path(__file__).resolve().parent
    sources = ([args.source.resolve()] if args.source else
               [source_dir / "traits_paper_functional" / f"{name}.f90"
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
            executable = compile_source(source, private)
        else:
            executable = args.executable.resolve()
        for key in (1, 2):
            output = run(f"{name}-key{key}", [executable], private, f"{key}\n")
            match = re.search(
                r"Choose an averaging method:\s*([+-]?\d+)\s+([+-]?\d+\.\d+)\s*"
                r"(?:-+ Memory Leak Report -+\s+"
                r"-+\s+NO LEAKS FOUND\s*)?\Z",
                output)
            if not match or int(match[1]) != 3 or Decimal(match[2]) != Decimal("3.0"):
                raise RuntimeError(f"{name}, choice {key}: expected 3 and 3.0:\n{output}")
            print(f"{name}, choice {key}: integer 3, real 3.0", flush=True)

    if args.lfortran and not args.source:
        for name, expected in INLINE_HASHES.items():
            source = source_dir / "traits_paper_functional" / name
            if hashlib.sha256(source.read_bytes()).hexdigest() != expected:
                raise RuntimeError(f"the authoritative {name} fragment was changed")
        source = source_dir / "traits_paper_manual_values.f90"
        private = work / source.stem
        private.mkdir()
        executable = compile_source(source, private)
        run(source.stem, [executable], private)
        print("inline-25/inline-28: real64 pointer and real32 ASSOCIATE sums 15, 20",
              flush=True)


if __name__ == "__main__":
    main()
