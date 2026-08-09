#!/usr/bin/env python3
"""Check that printed ASR text is readable by a third-party EDN reader.

The ASR text format claims to be an EDN data subset. That claim is only worth
making if something enforces it, so this script prints ASR for a set of inputs
and reads every document back with `edn_format`, which knows nothing about
LFortran. It checks two separate things:

- the document is valid UTF-8, the encoding the format is defined in;
- the document parses as EDN once handlers for the `#asr/*` tags are
  registered.

`edn_format` is required in CI and optional elsewhere, so a developer without
it still gets a working `ctest` run.
"""

import argparse
import os
import pathlib
import subprocess
import sys

TAGS = ("asr/bytes", "asr/float64", "asr/real128", "asr/loc")


def load_reader():
    try:
        import edn_format
    except ImportError:
        message = ("edn_format is not installed; install it with "
                   "`conda install -c conda-forge edn_format`")
        if os.environ.get("CI"):
            print("error: " + message, file=sys.stderr)
            sys.exit(1)
        print("SKIP: " + message)
        sys.exit(0)

    class Opaque(edn_format.TaggedElement):
        def __init__(self, name, value):
            self.name = name
            self.value = value

        def __str__(self):
            return "#%s %s" % (self.name, self.value)

    for tag in TAGS:
        edn_format.add_tag(tag, lambda value, _tag=tag: Opaque(_tag, value))
    return edn_format


def print_asr(lfortran, source, positional):
    command = [str(lfortran), str(source), "--show-asr", "--clojure",
               "--no-color"]
    if source.suffix == ".asr":
        command.append("--from-asr")
    if positional:
        command.append("--no-member-names")
    result = subprocess.run(command, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, check=False)
    if result.returncode != 0:
        raise RuntimeError("%s failed:\n%s" % (
            " ".join(command), result.stderr.decode("utf-8", "replace")))
    return result.stdout


def check(reader, name, raw):
    try:
        text = raw.decode("utf-8")
    except UnicodeDecodeError as error:
        raise RuntimeError("%s: output is not valid UTF-8: %s"
                           % (name, error)) from error
    try:
        reader.loads(text)
    except Exception as error:
        raise RuntimeError("%s: output is not readable as EDN: %s: %s"
                           % (name, type(error).__name__, error)) from error


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True, type=pathlib.Path)
    parser.add_argument("--input", action="append", default=[],
                        type=pathlib.Path,
                        help="Fortran or ASR text file to print and read back")
    args = parser.parse_args()

    reader = load_reader()
    # Only the Fortran inputs are discovered, because they are kept next to
    # this script for exactly this purpose. ASR fixtures are passed explicitly:
    # some of them are deliberately invalid and are not meant to be printed.
    inputs = args.input
    if not inputs:
        root = pathlib.Path(__file__).resolve().parent
        inputs = sorted(root.glob("*.f90"))

    failures = []
    for source in inputs:
        for positional in (False, True):
            form = "positional" if positional else "named"
            name = "%s [%s]" % (source.name, form)
            try:
                check(reader, name, print_asr(args.lfortran, source,
                                              positional))
                print("PASS %s" % name)
            except RuntimeError as error:
                failures.append(str(error))
                print("FAIL %s" % name)

    if failures:
        print("\n".join(failures), file=sys.stderr)
        return 1
    print("%d ASR text documents read back as EDN" % (2 * len(inputs)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
