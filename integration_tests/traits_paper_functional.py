"""Compile the paper programs and check their documented results.

Averaging programs are checked for both interactive choices; overloading
runs once. Standard-Fortran oracles of the paper programs are checked
like the programs they model.
"""

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
    "mixed": "6f9515b4230468ee7f179395713a2b90ed1759e91ce26dd69c884c9115c0516a",
    "static": "7c07edea1d8a5c75b39e682f864e814f1e106ff7bfd31baf45ff0abaf560dfbf",
    "overloading": "6aa5224df68bd983ad7c99cecb1e76d5dffe4479ff72c010d38eb58e51589e47",
}
SOURCE_DIRECTORIES = {
    "mixed": "traits_paper_dynamic",
    "static": "traits_paper_static",
    "overloading": "traits_paper_protocols",
}
# Explicit provider imports: the fixture is the original paper file plus one
# USE line after the anchor line. The original, which contains the provider
# module in the same file, must be rejected with a diagnostic naming the trait.
PROVIDER_IMPORTS = {
    "overloading": ("9697a242ce03a441460b340660b52ade97357fdf2bb94389286ae977bd070e92",
                    b"   use my_type,          only: MyType\n", b"   use real_type\n",
                    ("ireducible", "iprintable")),
}
ORACLE_HASHES = {
    "static_oracle": "8dd669518a6849e7d2c72cea4e2ce55be053f24b3f035a0ed119053b751d994b",
    "overloading_oracle": "5f361d44de40fa5381e418af774821ffac900de3ec54bde5136e02a369b3a958",
}
INLINE_HASHES = {
    "inline_25.f90": "53af21a4684d3995fe2d5d14a412f13b183cfa48801f2e0170a963bf76ea6cc8",
    "inline_28.f90": "787574712d4127623f9912f2173397e20d7b5865ddfcfd27cdf01cb01d993fee",
}
LEAK_REPORT = r"(?:-+ Memory Leak Report -+\s+-+\s+NO LEAKS FOUND\s*)?"


def check_overloading(output):
    if not re.fullmatch(r"I am:    13824\nI am:  13824\.0\n" + LEAK_REPORT, output):
        raise RuntimeError(f"overloading: expected I am: 13824 and 13824.0:\n{output}")
    return "I am: 13824 (MyType) and 13824.0 (real)"


OUTPUT_CHECKS = {"overloading": check_overloading}


def example_of(name):
    return name[:-len("_oracle")] if name in ORACLE_HASHES else name


def check_provider_import(name, content):
    original_hash, anchor, line, _ = PROVIDER_IMPORTS[name]
    original = content.replace(anchor + line, anchor, 1)
    if (content.count(line) != 1 or original.count(anchor) != 1
            or hashlib.sha256(original).hexdigest() != original_hash):
        raise RuntimeError(f"{name} must be the original paper file plus one provider USE")
    return original


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

    def run(name, command, cwd, stdin=None, expect_failure=False):
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
        if expect_failure:
            if not result.returncode or any(marker in text for marker in
                                            ("ASR verify", "Traceback", "Assertion")):
                raise RuntimeError(f"{name} did not fail with a diagnostic:\n{text}")
        elif result.returncode:
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
               [source_dir / SOURCE_DIRECTORIES.get(name, "traits_paper_functional") /
                f"{name}.f90" for name in SOURCE_HASHES])
    if args.executable and len(sources) != 1:
        parser.error("--executable requires --source")
    for source in sources:
        name = source.stem
        content = source.read_bytes()
        expected_hash = ORACLE_HASHES[name] if name in ORACLE_HASHES else SOURCE_HASHES[name]
        if hashlib.sha256(content).hexdigest() != expected_hash:
            raise RuntimeError(f"the authoritative {source.name} fixture was changed")
        private = work / name
        private.mkdir()
        if name in PROVIDER_IMPORTS:
            original = check_provider_import(name, content)
            if args.lfortran:
                traits = PROVIDER_IMPORTS[name][3]
                missing = private / f"{name}_without_provider_use.f90"
                missing.write_bytes(original)
                text = run(f"{name}-without-provider-use",
                           [args.lfortran.resolve(), "--no-color", "--semantics-only",
                            missing], private, expect_failure=True)
                if not any(trait in text.lower() for trait in traits):
                    raise RuntimeError(f"{name}: the missing provider USE must be "
                                       f"diagnosed for {traits}:\n{text}")
                print(f"{name}: the original without its provider USE is rejected",
                      flush=True)
        if args.lfortran:
            executable = compile_source(source, private)
        else:
            executable = args.executable.resolve()
        check = OUTPUT_CHECKS.get(example_of(name))
        if check:
            output = run(f"{name}-run", [executable], private)
            print(f"{name}: {check(output)}", flush=True)
            continue
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
