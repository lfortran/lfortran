"""Check the paper's core inline Fortran listings.

traits_paper_inline/provenance.json accounts for the 35 core inline listings
of traits.tex; inline-36, the Outlook schematic, is excluded. A present
fixture must reproduce its listing, byte for byte or after reverting the
adaptations recorded with it, and only a ready listing may have a fixture.
Each ready listing is checked in a private directory: a declaration is
compiled to verified ASR and an object and never run, a program is run and
must print exactly its documented values. inline-25 and inline-28 remain
accepted by traits_paper_manual_values.f90. A blocked listing is reported
with its blocker and not compiled.
"""

import argparse
from decimal import Decimal, InvalidOperation
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import time


HERE = Path(__file__).resolve().parent
FIXTURES = HERE / "traits_paper_inline"
IDS = ["inline-%02d" % n for n in range(1, 36)]
LEAK_REPORT = re.compile(r"\s*-+ Memory Leak Report -+\s+-+\s+NO LEAKS FOUND\s*\Z")


def sha256(data):
    return hashlib.sha256(data).hexdigest()


def check_listing(case):
    """Check that the fixture of a listing reproduces the paper's text."""
    fixture = FIXTURES / case["fixture"]
    content = fixture.read_bytes()
    if sha256(content) != case["sha256"]:
        raise RuntimeError(f"{case['id']}: the fixture {case['fixture']} was changed")
    for adaptation in reversed(case.get("adaptations", [])):
        replacement = adaptation["fixture"].encode()
        if content.count(replacement) != 1:
            raise RuntimeError(f"{case['id']}: the {adaptation['kind']} adaptation "
                               f"must occur exactly once in {case['fixture']}")
        content = content.replace(replacement, adaptation["listing"].encode())
    if sha256(content) != case["listing_sha256"]:
        raise RuntimeError(f"{case['id']}: {case['fixture']} without its recorded "
                           "adaptations is not the paper's listing")


def same_values(expected, output):
    """Compare printed lines by words, numbers by value (list-directed output)."""
    match = LEAK_REPORT.search(output)
    if match:
        output = output[:match.start()]
    lines = [line.split() for line in output.splitlines() if line.strip()]
    if len(lines) != len(expected):
        return False
    for words, expected_words in zip(lines, (line.split() for line in expected)):
        if len(words) != len(expected_words):
            return False
        for word, expected_word in zip(words, expected_words):
            if word == expected_word:
                continue
            try:
                value = Decimal(word.replace("D", "E").replace("d", "e"))
                if value != Decimal(expected_word):
                    return False
            except InvalidOperation:
                return False
    return True


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True, type=Path)
    parser.add_argument("--work-dir", type=Path, default=Path.cwd())
    parser.add_argument("--fast", action="store_true")
    parser.add_argument("--case", action="append", default=[],
                        help="check only this listing, e.g. inline-07")
    args = parser.parse_args()
    cases = json.loads((FIXTURES / "provenance.json").read_text())["listings"]
    if [case["id"] for case in cases] != IDS:
        raise RuntimeError("provenance.json must list inline-01 to inline-35 once, in order")
    unknown = set(args.case) - set(IDS)
    if unknown:
        parser.error(f"unknown listings: {sorted(unknown)}")
    compiler = args.lfortran.resolve()
    work = args.work_dir.resolve() / f"paper-inline-{os.getpid()}-{time.time_ns()}"
    work.mkdir(parents=True)
    environment = {**os.environ, "TMPDIR": str(work), "TMP": str(work), "TEMP": str(work)}
    flags = ["--no-color", "--verify-all-passes"]
    if args.fast:
        flags.append("--fast")
    records = []

    def run(name, command, cwd):
        command = list(map(str, command))
        log = work / f"{name}.log"
        with log.open("w") as output:
            result = subprocess.run(command, cwd=cwd, env=environment, text=True,
                                    stdout=output, stderr=subprocess.STDOUT, timeout=120)
        records.append({"name": name, "command": command, "cwd": str(cwd),
                        "returncode": result.returncode, "log": str(log)})
        (work / "commands.json").write_text(json.dumps(records, indent=2) + "\n")
        text = log.read_text()
        if result.returncode:
            raise RuntimeError(f"{name} failed ({result.returncode}):\n{text}")
        return text

    def check_gate(case):
        private = work / case["id"]
        modules = private / "modules"
        modules.mkdir(parents=True)
        wrapper = HERE / case["wrapper"]
        common = [compiler, *flags, "-I", HERE, "-J", modules]
        if case["gate"] == "declaration":
            run(case["id"] + "-asr", [*common, "--show-asr", wrapper], private)
            obj = private / (wrapper.stem + ".o")
            run(case["id"] + "-object", [*common, "-c", wrapper, "-o", obj], private)
            if not obj.is_file() or not obj.stat().st_size:
                raise RuntimeError(f"{case['id']}: no object was produced")
            return "verified ASR and object; no execution"
        executable = private / wrapper.stem
        run(case["id"] + "-compile", [*common, wrapper, "-o", executable], private)
        output = run(case["id"] + "-run", [executable], private)
        expected = case["expected"].get("stdout", [])
        if not same_values(expected, output):
            raise RuntimeError(f"{case['id']}: expected {expected or 'no output'}:\n{output}")
        return "printed " + "; ".join(expected) if expected else case["expected"]["checks"]

    failures = []
    counts = {"passed": 0, "existing": 0, "not run": 0}
    for case in cases:
        if args.case and case["id"] not in args.case:
            continue
        try:
            if case["status"] == "existing":
                check_listing(case)
                counts["existing"] += 1
                print(f"{case['id']}: accepted by {case['accepted_by']}", flush=True)
            elif case["status"] == "ready":
                check_listing(case)
                message = check_gate(case)
                counts["passed"] += 1
                print(f"{case['id']}: {message}", flush=True)
            else:
                present = [name for name in (FIXTURES / case["fixture"], HERE / case["wrapper"])
                           if name.exists()]
                if present:
                    raise RuntimeError(f"{case['id']} is {case['status']} but has "
                                       f"{[str(name) for name in present]}")
                counts["not run"] += 1
                print(f"{case['id']}: not run ({case['status']}): {case['blocked_by']}",
                      flush=True)
        except (RuntimeError, OSError, subprocess.TimeoutExpired) as error:
            message = str(error)
            if not message.startswith(case["id"]):
                message = f"{case['id']}: {message}"
            failures.append(message)
            print(f"{case['id']}: FAILED", flush=True)
    print(", ".join(f"{count} {name}" for name, count in counts.items()), flush=True)
    if failures:
        raise RuntimeError("\n\n".join(failures))


if __name__ == "__main__":
    main()
