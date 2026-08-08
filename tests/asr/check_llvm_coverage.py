#!/usr/bin/env python3

import argparse
import pathlib
import re
import sys

import toml


def load_asdl(asdl_path):
    sys.path.insert(0, str(asdl_path.parent))
    import asdl

    module = asdl.parse(str(asdl_path))
    constructors = []
    for definition in module.dfns:
        if not isinstance(definition.value, asdl.Sum):
            continue
        for constructor in definition.value.types:
            if constructor.fields:
                constructors.append(
                    (definition.name, constructor.name))
    return constructors


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--asdl", required=True, type=pathlib.Path)
    parser.add_argument("--llvm", required=True, type=pathlib.Path)
    parser.add_argument("--manifest", required=True, type=pathlib.Path)
    args = parser.parse_args()

    constructors = load_asdl(args.asdl.resolve())
    llvm_source = args.llvm.read_text(encoding="utf-8")
    direct = set(re.findall(
        r"\bvisit_([A-Za-z0-9_]+)\s*\(", llvm_source))
    exceptions = toml.load(args.manifest.resolve())["constructors"]

    missing = []
    for base, constructor in constructors:
        key = f"{base}.{constructor}"
        if constructor not in direct and key not in exceptions:
            missing.append(key)

    constructor_keys = {
        f"{base}.{constructor}" for base, constructor in constructors}
    stale = sorted(
        key for key in exceptions
        if key not in constructor_keys or key.split(".", 1)[1] in direct
    )
    invalid = sorted(
        key for key, classification in exceptions.items()
        if not (
            classification in {"metadata", "non-executable"} or
            classification.startswith("helper:") or
            classification.startswith("lowered:")
        )
    )

    if missing:
        print("LLVM coverage is missing ASR constructors:", file=sys.stderr)
        for key in sorted(missing):
            print(f"  {key}", file=sys.stderr)
    if stale:
        print("LLVM coverage has stale exceptions:", file=sys.stderr)
        for key in stale:
            print(f"  {key}", file=sys.stderr)
    if invalid:
        print("LLVM coverage has invalid classifications:", file=sys.stderr)
        for key in invalid:
            print(f"  {key}: {exceptions[key]}", file=sys.stderr)
    if missing or stale or invalid:
        return 1

    print(
        f"{len(constructors)} ASR constructors classified: "
        f"{len(constructors) - len(exceptions)} direct LLVM visitors, "
        f"{len(exceptions)} explicit exceptions"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
