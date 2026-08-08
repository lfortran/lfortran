#!/usr/bin/env python3

import argparse
import dataclasses
import hashlib
import json
import pathlib
import random
import re
import subprocess
import sys
import tempfile

import toml


@dataclasses.dataclass(frozen=True)
class Mutation:
    start: int
    end: int
    old: str
    new: str
    description: str
    lane: str


@dataclasses.dataclass
class CommandResult:
    command: list
    returncode: int
    stdout: str
    stderr: str
    timed_out: bool = False


@dataclasses.dataclass
class OracleResult:
    accepted: bool
    outcome: str
    phase: str
    commands: list


INTEGER_CONSTANT = re.compile(
    r"\(IntegerConstant\s+:n\s+(-?\d+)"
)
LOGICAL_CONSTANT = re.compile(
    r"\(LogicalConstant\s+:value\s+(true|false)"
)
KIND_FIELD = re.compile(
    r"\((?:Integer|UnsignedInteger|Real|Complex|Logical)"
    r"\s+:kind\s+(-?\d+)"
)
INVALID_BOOLEAN_FIELD = re.compile(
    r":(realloc_lhs|move_allocation)\s+(true|false)"
)
DECLARATION_BOOLEAN_FIELD = re.compile(
    r":(value_attr|target_attr|contiguous_attr|"
    r"is_volatile|is_protected)\s+(true|false)"
)
PASS_FAILURE = re.compile(
    r"ASR_FUZZ_FAILURE phase=pass pass=([^\s]+)"
)


def sha256(text):
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def run(command, timeout):
    try:
        completed = subprocess.run(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=timeout,
            check=False,
        )
        return CommandResult(
            command=command,
            returncode=completed.returncode,
            stdout=completed.stdout,
            stderr=completed.stderr,
        )
    except subprocess.TimeoutExpired as error:
        return CommandResult(
            command=command,
            returncode=-1,
            stdout=error.stdout or "",
            stderr=error.stderr or "",
            timed_out=True,
        )


def replacement_mutations(match, values, description, lane):
    old = match.group(1)
    start, end = match.span(1)
    return [
        Mutation(start, end, old, value, description, lane)
        for value in values
        if value != old
    ]


def discover_mutations(text):
    mutations = []
    for match in INTEGER_CONSTANT.finditer(text):
        value = int(match.group(1))
        replacements = [
            str(value + 1),
            str(value - 1),
            "0",
            "1",
            "-1",
        ]
        mutations.extend(replacement_mutations(
            match, replacements, "integer constant", "valid"))

    for match in LOGICAL_CONSTANT.finditer(text):
        replacement = "false" if match.group(1) == "true" else "true"
        mutations.extend(replacement_mutations(
            match, [replacement], "logical constant", "valid"))

    for match in KIND_FIELD.finditer(text):
        mutations.extend(replacement_mutations(
            match, ["1", "2", "4", "8"], "type kind", "mixed"))

    for pattern, lane in (
            (INVALID_BOOLEAN_FIELD, "invalid"),
            (DECLARATION_BOOLEAN_FIELD, "valid")):
        for match in pattern.finditer(text):
            old = match.group(2)
            start, end = match.span(2)
            replacement = "false" if old == "true" else "true"
            mutations.append(Mutation(
                start,
                end,
                old,
                replacement,
                f"boolean field {match.group(1)}",
                lane,
            ))

    unique = {}
    for mutation in mutations:
        key = (
            mutation.start,
            mutation.end,
            mutation.new,
            mutation.description,
        )
        unique[key] = mutation
    return sorted(
        unique.values(),
        key=lambda item: (
            item.start,
            item.end,
            item.new,
            item.description,
        ),
    )


def apply_mutation(text, mutation):
    if text[mutation.start:mutation.end] != mutation.old:
        raise RuntimeError(
            "mutation no longer matches its source ASR: "
            f"expected {mutation.old!r}"
        )
    return text[:mutation.start] + mutation.new + text[mutation.end:]


def generate_initial_asr(lfortran, source, timeout):
    result = run([
        str(lfortran),
        str(source),
        "--show-asr",
        "--clojure",
        "--no-color",
        "--no-indent",
    ], timeout)
    if result.returncode != 0:
        raise RuntimeError(
            f"could not generate initial ASR from {source}\n"
            f"{result.stderr}"
        )
    return result.stdout.strip() + "\n"


def classify_failure(result, fallback_phase):
    if result.timed_out:
        return f"{fallback_phase}:timeout"
    if result.returncode < 0:
        return f"{fallback_phase}:signal:{-result.returncode}"
    pass_failure = PASS_FAILURE.search(result.stderr)
    if pass_failure:
        return f"pass:{pass_failure.group(1)}"
    if "code generation error:" in result.stderr:
        return "llvm"
    if "Internal Compiler Error:" in result.stderr:
        return f"{fallback_phase}:ice"
    return fallback_phase


def run_oracle(lfortran, candidate, timeout):
    commands = []
    with tempfile.TemporaryDirectory(prefix="lfortran-asr-fuzz-") as temp_dir:
        temp_path = pathlib.Path(temp_dir)
        fixture = temp_path / "candidate.asr"
        fixture.write_text(candidate, encoding="utf-8")

        initial = run([
            str(lfortran),
            str(fixture),
            "--verify-asr",
            "--error-format=short",
            "--no-error-banner",
            "--no-color",
        ], timeout)
        commands.append(initial)
        if initial.timed_out or initial.returncode < 0:
            return OracleResult(
                False,
                "failure",
                classify_failure(initial, "initial-verify"),
                commands,
            )
        if initial.returncode == 1:
            if "ASR verify pass error" in initial.stderr:
                return OracleResult(
                    True, "verify", "initial-verify", commands)
            return OracleResult(
                False, "failure", "initial-verify", commands)
        if initial.returncode != 0:
            return OracleResult(
                False,
                "failure",
                classify_failure(initial, "asr-parser"),
                commands,
            )

        object_file = temp_path / "candidate.o"
        compiled_object = run([
            str(lfortran),
            str(fixture),
            "--verify-all-passes",
            "--no-error-banner",
            "--no-color",
            "-c",
            "-o",
            str(object_file),
        ], timeout)
        commands.append(compiled_object)
        if compiled_object.returncode != 0:
            return OracleResult(
                False,
                "failure",
                classify_failure(compiled_object, "object"),
                commands,
            )
        if not object_file.is_file() or object_file.stat().st_size == 0:
            return OracleResult(
                False, "failure", "object:missing", commands)

        executable = temp_path / "candidate"
        linked = run([
            str(lfortran),
            str(fixture),
            "--verify-all-passes",
            "--no-error-banner",
            "--no-color",
            "-o",
            str(executable),
        ], timeout)
        commands.append(linked)
        if linked.returncode != 0:
            return OracleResult(
                False,
                "failure",
                classify_failure(linked, "link"),
                commands,
            )
        if not executable.is_file() or executable.stat().st_size == 0:
            return OracleResult(
                False, "failure", "link:missing", commands)
        return OracleResult(True, "compile", "link", commands)


def serialize_command(result):
    return {
        "command": [str(arg) for arg in result.command],
        "returncode": result.returncode,
        "stdout": result.stdout,
        "stderr": result.stderr,
        "timed_out": result.timed_out,
    }


def persist_failure(artifacts, case_index, candidate, metadata, result):
    digest = sha256(candidate)[:12]
    stem = f"failure-{case_index:06d}-{digest}"
    artifacts.mkdir(parents=True, exist_ok=True)
    asr_path = artifacts / f"{stem}.asr"
    json_path = artifacts / f"{stem}.json"
    asr_path.write_text(candidate, encoding="utf-8")
    metadata.update({
        "artifact": asr_path.name,
        "candidate_sha256": sha256(candidate),
        "outcome": result.outcome,
        "phase": result.phase,
        "commands": [
            serialize_command(command) for command in result.commands
        ],
    })
    json_path.write_text(
        json.dumps(metadata, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return asr_path


def load_sources(manifest, explicit_sources):
    if explicit_sources:
        return [path.resolve() for path in explicit_sources]
    manifest = manifest.resolve()
    root = manifest.parent
    return [
        (root / item["filename"]).resolve()
        for item in toml.load(manifest)["seed"]
    ]


def replay(lfortran, metadata_path, timeout):
    metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
    candidate = metadata_path.parent / metadata["artifact"]
    text = candidate.read_text(encoding="utf-8")
    result = run_oracle(lfortran, text, timeout)
    print(
        f"replay outcome={result.outcome} phase={result.phase} "
        f"accepted={result.accepted}"
    )
    return 0 if not result.accepted else 1


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True, type=pathlib.Path)
    parser.add_argument(
        "--manifest",
        type=pathlib.Path,
        default=pathlib.Path(__file__).with_name("fuzz_seeds.toml"),
    )
    parser.add_argument(
        "--source", action="append", type=pathlib.Path, default=[])
    parser.add_argument("--seed", type=int, default=0)
    parser.add_argument("--cases", type=int, default=100)
    parser.add_argument(
        "--strategy",
        choices=["valid", "invalid", "mixed"],
        default="mixed",
    )
    parser.add_argument("--timeout", type=int, default=30)
    parser.add_argument(
        "--artifacts",
        type=pathlib.Path,
        default=pathlib.Path("asr-fuzz-artifacts"),
    )
    parser.add_argument("--replay", type=pathlib.Path)
    args = parser.parse_args()

    lfortran = args.lfortran.resolve()
    if args.replay:
        return replay(lfortran, args.replay.resolve(), args.timeout)

    sources = load_sources(args.manifest, args.source)
    seeds = []
    for source in sources:
        text = generate_initial_asr(lfortran, source, args.timeout)
        mutations = discover_mutations(text)
        eligible = [
            mutation for mutation in mutations
            if args.strategy == "mixed" or
            mutation.lane == args.strategy
        ]
        if eligible:
            seeds.append((source, text, eligible))
    if not seeds:
        print("no mutable ASR seeds were generated", file=sys.stderr)
        return 2

    rng = random.Random(args.seed)
    counts = {"compile": 0, "verify": 0, "failure": 0}
    failures = []
    for case_index in range(args.cases):
        source, initial, mutations = rng.choice(seeds)
        mutation = rng.choice(mutations)
        candidate = apply_mutation(initial, mutation)
        result = run_oracle(lfortran, candidate, args.timeout)
        counts[result.outcome] += 1
        print(
            f"case={case_index} source={source.name} "
            f"mutation={mutation.description!r} "
            f"{mutation.old}->{mutation.new} "
            f"outcome={result.outcome} phase={result.phase}"
        )
        if not result.accepted:
            metadata = {
                "format_version": 1,
                "random_seed": args.seed,
                "case_index": case_index,
                "source": str(source),
                "source_sha256": hashlib.sha256(
                    source.read_bytes()).hexdigest(),
                "initial_asr_sha256": sha256(initial),
                "mutation": dataclasses.asdict(mutation),
            }
            artifact = persist_failure(
                args.artifacts, case_index, candidate, metadata, result)
            failures.append(artifact)

    print(
        f"summary compile={counts['compile']} verify={counts['verify']} "
        f"failure={counts['failure']}"
    )
    if failures:
        print("unexpected failures:", file=sys.stderr)
        for failure in failures:
            print(f"  {failure}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
