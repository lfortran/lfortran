"""Compile the paper programs and check their documented results.

Averaging programs are checked for both interactive choices; overloading and
vector run once. Standard-Fortran oracles of the paper programs are checked
like the programs they model.

A fixture is its paper program plus the adaptations that its provenance.json
records: an explicit provider USE (TD-07) and the explicit receivers of TD-36.
Reverting them must reproduce the paper program's SHA-256.
"""

import argparse
from decimal import Decimal
import hashlib
import json
import math
import os
from pathlib import Path
import re
import subprocess
import time


# The acceptance fixtures: each paper program with its recorded adaptations.
SOURCE_HASHES = {
    "functional1": "51a5c11e8b77624dd7cd653c0f1ded7fccee860b22e8d3668b3f2c09008eca25",
    "functional2": "808b71c5393ee9dfc19e192cbe7eb689dedd5a9f25fecd0e4018b30ec0cbbeb1",
    "mixed": "878bdb0ee08ee2df281863e98a3d83b11310287f785cf74f1f54c0622c48877d",
    "static": "fcf1939d232780a5cec5a9b727af724f5398e22bf546fbc64ce36b506474f78b",
    "overloading": "ef9f4eba9d8933ecb64c4c0da5c58c60680c7d529bcff4d585d7d9eec4ff35e4",
    "vector": "ee8558469dc85a8a6f7779d4c96baaaf7dbb9949d4ab3d750444e36b0fa8bce3",
    "ocp": "713cdbb4bc26b37c40df7bd2df55aa7e5eff504eba38f07a8049a80e88a2cd0f",
}
# The paper programs, as the companion repository has them.
PAPER_HASHES = {
    "functional1": "51a5c11e8b77624dd7cd653c0f1ded7fccee860b22e8d3668b3f2c09008eca25",
    "functional2": "808b71c5393ee9dfc19e192cbe7eb689dedd5a9f25fecd0e4018b30ec0cbbeb1",
    "mixed": "6f9515b4230468ee7f179395713a2b90ed1759e91ce26dd69c884c9115c0516a",
    "static": "7c07edea1d8a5c75b39e682f864e814f1e106ff7bfd31baf45ff0abaf560dfbf",
    "overloading": "9697a242ce03a441460b340660b52ade97357fdf2bb94389286ae977bd070e92",
    "vector": "88d11188ce85ed6c5c4067344eb2a432f39ec742730584310a2576143d671b40",
    "ocp": "7852f86195a9d44721baf930929f8704b49750d10b8dd255744d4d1e1dba7c4c",
}
SOURCE_DIRECTORIES = {
    "mixed": "traits_paper_dynamic",
    "static": "traits_paper_static",
    "overloading": "traits_paper_protocols",
    "vector": "traits_paper_associated",
    "ocp": "traits_paper_protocols",
}
# Explicit provider imports: the fixture adds one USE line after the anchor
# line. Without it, the program must be rejected with a diagnostic naming the
# trait, also when the provider module is in the same file.
PROVIDER_IMPORTS = {
    "overloading": (PAPER_HASHES["overloading"],
                    b"   use my_type,          only: MyType\n", b"   use real_type\n",
                    ("ireducible", "iprintable")),
    "ocp": (PAPER_HASHES["ocp"],
            b"   use averager_library, only: Averager\n", b"   use intrinsics\n",
            ("inumeric",)),
}
ORACLE_HASHES = {
    "static_oracle": "8dd669518a6849e7d2c72cea4e2ce55be053f24b3f035a0ed119053b751d994b",
    "overloading_oracle": "5f361d44de40fa5381e418af774821ffac900de3ec54bde5136e02a369b3a958",
    "vector_oracle": "ce00fc818858df104299695f019b64040d01c925fdf2e862a7c318a697376c2a",
    "ocp_oracle": "9bf12d3f6d8e7490faae568e15071218cfcbb07a1ada5a7963eaedbbb51c4d10",
}
# Standard semantics a program needs beyond the default options: vector
# appends with `elements = [elements, item]`, which reallocates the
# allocatable on assignment (Fortran 2003), LFortran's --realloc-lhs-arrays.
COMPILE_FLAGS = {"vector": ["--realloc-lhs-arrays"]}
INLINE_HASHES = {
    "inline_25.f90": "53af21a4684d3995fe2d5d14a412f13b183cfa48801f2e0170a963bf76ea6cc8",
    "inline_28.f90": "787574712d4127623f9912f2173397e20d7b5865ddfcfd27cdf01cb01d993fee",
}
# With --detect-leaks the runtime prints its report after a blank line.
LEAK_REPORT = r"(?:\n?-+ Memory Leak Report -+\s+-+\s+NO LEAKS FOUND\s*)?"


def check_overloading(output):
    if not re.fullmatch(r"I am:    13824\nI am:  13824\.0\n" + LEAK_REPORT, output):
        raise RuntimeError(f"overloading: expected I am: 13824 and 13824.0:\n{output}")
    return "I am: 13824 (MyType) and 13824.0 (real)"


def example_of(name):
    """The paper program a fixture or its standard-Fortran oracle models."""
    return name[:-len("_oracle")] if name in ORACLE_HASHES else name


def adaptations(name):
    """The adaptations of a fixture, in the order applied to its paper
    program, as (paper bytes, fixture bytes) pairs: the explicit provider USE,
    then the TD-36 receiver migration that its provenance.json records."""
    pairs = []
    if name in PROVIDER_IMPORTS:
        _, anchor, line, _ = PROVIDER_IMPORTS[name]
        pairs.append((anchor, anchor + line))
    directory = Path(__file__).resolve().parent / SOURCE_DIRECTORIES.get(
        name, "traits_paper_functional")
    provenance = json.loads((directory / "provenance.json").read_text())
    record = next((fixture for fixture in provenance["fixtures"]
                   if fixture["file"] == f"{name}.f90"), None)
    if (record is None or record["sha256"] != SOURCE_HASHES[name]
            or record.get("original_sha256", record["sha256"]) != PAPER_HASHES[name]):
        raise RuntimeError(f"{directory.name}/provenance.json does not record {name}.f90 "
                           "with its pinned fixture and paper hashes")
    for replacement in record.get("receiver_migration", {}).get("replacements", []):
        pairs.append((replacement["paper"].encode(), replacement["fixture"].encode()))
    return pairs


def revert(text, pairs, partial=False):
    """Undo adaptations, the last one first. The fixture text of each must
    occur exactly once; a piece of a fixture (partial) may also lack it."""
    for paper_text, fixture_text in reversed(pairs):
        count = text.count(fixture_text)
        if count == 0 and partial:
            continue
        if count != 1:
            raise RuntimeError(f"an adaptation occurs {count} times instead of once: "
                               f"{fixture_text.decode()!r}")
        text = text.replace(fixture_text, paper_text)
    return text


def paper_original(name, content):
    """The paper program of a fixture: its recorded adaptations reverted."""
    original = revert(content, adaptations(name))
    if hashlib.sha256(original).hexdigest() != PAPER_HASHES[name]:
        raise RuntimeError(f"{name} must be its paper program plus the adaptations "
                           "that its provenance.json records")
    return original


def without_provider_import(name, content):
    """The fixture without its explicit provider USE."""
    _, anchor, line, _ = PROVIDER_IMPORTS[name]
    if content.count(line) != 1 or content.count(anchor + line) != 1:
        raise RuntimeError(f"{name} must add one provider USE after its anchor line")
    return content.replace(anchor + line, anchor, 1)


def list_directed(records, count, convert):
    """Read count list-directed values, also as r*c repeats, from records."""
    values = []
    while records and len(values) < count:
        for token in re.split(r"[\s,]+", records.pop(0).strip()):
            if token:
                repeat, star, constant = token.rpartition("*")
                values += [convert(constant)] * (int(repeat) if star else 1)
    return values


def check_vector(output):
    """The three list-directed rows of vector: real(8) 0 1.5 2.2, logical
    T F T and the character(len=5) values 'John ', 'Mary ', 'Anne ', which
    list-directed output writes without separators."""
    records = re.sub(LEAK_REPORT + r"\Z", "", output).splitlines()
    try:
        reals = list_directed(records, 3, lambda t: float(t.upper().replace("D", "E")))
        logicals = list_directed(records, 3, {"T": True, "F": False}.__getitem__)
    except (KeyError, ValueError):
        reals = logicals = []
    # A processor may start each record with a blank. Each value keeps its
    # five characters, the trailing blank of the last one included.
    text = "".join(record[1:] if record.startswith(" ") else record for record in records)
    if (len(reals) != 3 or logicals != [True, False, True]
            or not all(math.isclose(value, expected, rel_tol=0, abs_tol=1e-12)
                       for value, expected in zip(reals, (0.0, 1.5, 2.2)))
            or not text.startswith("John Mary Anne ")
            or text[len("John Mary Anne "):].strip(" ")):
        raise RuntimeError("vector: expected rows 0 1.5 2.2, T F T and "
                           f"'John Mary Anne ':\n{output}")
    return "real(8) 0 1.5 2.2, logical T F T, character(len=5) John Mary Anne"


OUTPUT_CHECKS = {"overloading": check_overloading, "vector": check_vector}


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
        flags = ["--no-color", "--verify-all-passes",
                 *COMPILE_FLAGS.get(example_of(source.stem), [])]
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
        if name not in ORACLE_HASHES:
            paper_original(name, content)
        private = work / name
        private.mkdir()
        if name in PROVIDER_IMPORTS:
            without_use = without_provider_import(name, content)
            if args.lfortran:
                traits = PROVIDER_IMPORTS[name][3]
                missing = private / f"{name}_without_provider_use.f90"
                missing.write_bytes(without_use)
                text = run(f"{name}-without-provider-use",
                           [args.lfortran.resolve(), "--no-color", "--semantics-only",
                            missing], private, expect_failure=True)
                if not any(trait in text.lower() for trait in traits):
                    raise RuntimeError(f"{name}: the missing provider USE must be "
                                       f"diagnosed for {traits}:\n{text}")
                print(f"{name}: the fixture without its provider USE is rejected",
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
                + LEAK_REPORT + r"\Z",
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
