#!/usr/bin/env python3

import argparse
import dataclasses
import json
import pathlib
import sys

import edn
import fuzz


@dataclasses.dataclass(frozen=True)
class Reduction:
    path: tuple
    action: str
    index: int = -1
    end: int = -1
    replacement: str = ""

    def describe(self):
        location = ".".join(str(index) for index in self.path) or "root"
        if self.action == "remove":
            return f"remove {location}[{self.index}:{self.end}]"
        if self.action == "replace":
            return f"replace {location} with {self.replacement}"
        return f"{self.action} {location}"


def reduction_candidates(root):
    reductions = []
    for path, node in edn.walk(root):
        if node.kind == "vector" and node.children:
            reductions.append(Reduction(
                path, "remove", 0, len(node.children)))
            for index in range(len(node.children)):
                reductions.append(Reduction(
                    path, "remove", index, index + 1))
        elif node.kind == "map" and node.children:
            pair_count = len(node.children) // 2
            for index in range(pair_count):
                reductions.append(Reduction(
                    path, "remove", 2 * index, 2 * index + 2))
        elif node.kind == "list":
            for field, value_index in edn.named_fields(node):
                value = node.children[value_index]
                if value.kind in {"list", "vector", "map", "tag"} or (
                        value.kind == "atom" and
                        value.value not in {"nil", "false", "0", "1"}):
                    reductions.append(Reduction(
                        path + (value_index,), "replace",
                        replacement="nil"))
        elif edn.is_number(node) and node.value not in {"0", "1", "-1"}:
            for replacement in ("0", "1", "-1"):
                reductions.append(Reduction(
                    path, "replace", replacement=replacement))
    return reductions


def apply_reduction(root, reduction):
    candidate = root.clone()
    target = edn.at_path(candidate, reduction.path)
    if reduction.action == "remove":
        del target.children[reduction.index:reduction.end]
    elif reduction.action == "replace":
        replacement = edn.parse(reduction.replacement)
        if not reduction.path:
            candidate = replacement
        else:
            parent = edn.at_path(candidate, reduction.path[:-1])
            parent.children[reduction.path[-1]] = replacement
    else:
        raise ValueError(f"unknown reduction action {reduction.action}")
    return candidate


def failure_signature(result):
    stderr = "\n".join(
        command.stderr for command in result.commands if command.stderr)
    # A verifier rejection is a successful outcome for the fuzzer, but it is
    # still worth reducing: a rejection is what a committed `tests/asr/verify`
    # fixture pins, and the rule it exercises is named by its diagnostic code.
    if result.outcome == "verify":
        codes = fuzz.VERIFY_CODE.findall(stderr)
        return f"verify:{codes[0] if codes else ''}"
    if "LCOMPILERS_ASSERT failed:" in stderr:
        first = stderr.split("LCOMPILERS_ASSERT failed:", 1)[1].splitlines()[0]
        return f"{result.phase}:assert:{first.strip()}"
    if "Internal Compiler Error:" in stderr:
        tail = stderr.splitlines()[-1] if stderr.splitlines() else ""
        return f"{result.phase}:ice:{tail}"
    return result.phase


def interesting(lfortran, text, timeout, expected_signature):
    result = fuzz.run_oracle(lfortran, text, timeout)
    return failure_signature(result) == expected_signature, result


def reduce_failure(lfortran, text, timeout, max_attempts):
    initial = fuzz.run_oracle(lfortran, text, timeout)
    if initial.outcome == "compile":
        raise RuntimeError(
            "input is accepted and compiled, so there is nothing to reduce")
    expected_signature = failure_signature(initial)
    root = edn.parse(text)
    attempts = 0
    accepted = 0
    history = []
    progress = True
    while progress and attempts < max_attempts:
        progress = False
        for reduction in reduction_candidates(root):
            if attempts >= max_attempts:
                break
            attempts += 1
            candidate_root = apply_reduction(root, reduction)
            candidate = edn.render(candidate_root) + "\n"
            keeps_failure, result = interesting(
                lfortran, candidate, timeout, expected_signature)
            if keeps_failure:
                root = candidate_root
                accepted += 1
                history.append({
                    "reduction": reduction.describe(),
                    "size": len(candidate),
                    "phase": result.phase,
                })
                print(
                    f"accepted reduction={reduction.describe()!r} "
                    f"size={len(candidate)}"
                )
                progress = True
                break
    reduced = edn.render_indented(edn.strip_member_names(root)) + "\n"
    return reduced, {
        "attempts": attempts,
        "accepted_reductions": accepted,
        "original_size": len(text),
        "reduced_size": len(reduced),
        "signature": expected_signature,
        "history": history,
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True, type=pathlib.Path)
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--metadata", type=pathlib.Path)
    # A hand-written or generated document is reduced the same way a fuzzer
    # artifact is, which is what turns one into a committed fixture.
    group.add_argument("--input", type=pathlib.Path)
    parser.add_argument("--timeout", type=int, default=30)
    parser.add_argument("--max-attempts", type=int, default=1000)
    parser.add_argument("--output", type=pathlib.Path)
    args = parser.parse_args()

    if args.metadata:
        metadata_path = args.metadata.resolve()
        metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
        input_path = metadata_path.parent / metadata["artifact"]
    else:
        input_path = args.input.resolve()
    text = input_path.read_text(encoding="utf-8")
    try:
        reduced, reduction = reduce_failure(
            args.lfortran.resolve(), text, args.timeout, args.max_attempts)
    except (edn.ParseError, OSError, RuntimeError) as error:
        print(str(error), file=sys.stderr)
        return 1

    output = args.output or input_path.with_suffix(".min.asr")
    output.write_text(reduced, encoding="utf-8")
    reduction["input"] = input_path.name
    reduction["output"] = output.name
    reduction_path = output.with_suffix(".json")
    reduction_path.write_text(
        json.dumps(reduction, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    print(
        f"reduced {len(text)} -> {len(reduced)} bytes "
        f"in {reduction['attempts']} attempts"
    )
    print(output)
    return 0


if __name__ == "__main__":
    sys.exit(main())
