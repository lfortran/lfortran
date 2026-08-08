#!/usr/bin/env python3

import argparse
import pathlib
import re
import subprocess
import sys
import tempfile

import toml


VERIFY_RE = re.compile(
    r"^(.*):(\d+)-(\d+):(\d+)-(\d+): "
    r"ASR verify pass error \[([^\]]+)\]: (.*)$"
)


def run(command, timeout):
    try:
        return subprocess.run(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=timeout,
            check=False,
        )
    except subprocess.TimeoutExpired as error:
        rendered = " ".join(str(arg) for arg in command)
        raise RuntimeError(
            f"command timed out after {timeout}s: {rendered}"
        ) from error


def anchor_span(path, anchor, occurrence=1):
    matches = []
    for line_number, line in enumerate(
            path.read_text(encoding="utf-8").splitlines(), start=1):
        for match in re.finditer(rf"\b{re.escape(anchor)}\b", line):
            matches.append((
                line_number,
                match.start() + 1,
                match.end(),
            ))
    if occurrence < 1 or len(matches) < occurrence:
        raise RuntimeError(
            f"{path}: expected anchor {anchor!r} occurrence {occurrence}, "
            f"found {len(matches)} total"
        )
    return matches[occurrence - 1]


def verify_initial_asr(lfortran, fixture, timeout):
    return run([
        str(lfortran),
        str(fixture),
        "--verify-asr",
        "--error-format=short",
        "--no-error-banner",
        "--no-color",
    ], timeout)


def check_compile(lfortran, fixture, timeout):
    initial = verify_initial_asr(lfortran, fixture, timeout)
    if initial.returncode != 0:
        raise RuntimeError(
            f"{fixture}: initial ASR verification unexpectedly failed\n"
            f"{initial.stderr}"
        )
    with tempfile.TemporaryDirectory(prefix="lfortran-asr-") as temp_dir:
        temp_path = pathlib.Path(temp_dir)
        object_output = temp_path / "fixture.o"
        assembled = run([
            str(lfortran),
            str(fixture),
            "--no-error-banner",
            "--no-color",
            "--verify-all-passes",
            "-c",
            "-o",
            str(object_output),
        ], timeout)
        if assembled.returncode != 0:
            raise RuntimeError(
                f"{fixture}: verified ASR did not produce an object file\n"
                f"{assembled.stderr}"
            )
        if not object_output.is_file() or object_output.stat().st_size == 0:
            raise RuntimeError(
                f"{fixture}: compiler succeeded without producing an object"
            )

        output = temp_path / "a.out"
        compiled = run([
            str(lfortran),
            str(fixture),
            "--no-error-banner",
            "--no-color",
            "--verify-all-passes",
            "-o",
            str(output),
        ], timeout)
        if compiled.returncode != 0:
            raise RuntimeError(
                f"{fixture}: verified ASR did not produce an executable\n"
                f"{compiled.stderr}"
            )
        if not output.is_file() or output.stat().st_size == 0:
            raise RuntimeError(
                f"{fixture}: compiler succeeded without producing a binary"
            )


def check_verify(lfortran, fixture, test, timeout):
    verified = verify_initial_asr(lfortran, fixture, timeout)
    if verified.returncode != 1:
        raise RuntimeError(
            f"{fixture}: expected initial verifier rejection, "
            f"got exit code {verified.returncode}\n{verified.stderr}"
        )
    lines = [line for line in verified.stderr.splitlines() if line]
    if len(lines) != 1:
        raise RuntimeError(
            f"{fixture}: expected one short verifier diagnostic, got:\n"
            f"{verified.stderr}"
        )
    match = VERIFY_RE.match(lines[0])
    if match is None:
        raise RuntimeError(
            f"{fixture}: malformed verifier diagnostic:\n{lines[0]}"
        )

    _, first_line, last_line, first_col, last_col, code, message = \
        match.groups()
    expected_line, expected_first_col, expected_last_col = anchor_span(
        fixture, test["anchor"], test.get("anchor_occurrence", 1))
    actual_span = (
        int(first_line),
        int(last_line),
        int(first_col),
        int(last_col),
    )
    expected_span = (
        expected_line,
        expected_line,
        expected_first_col,
        expected_last_col,
    )
    if code != test["diagnostic"]:
        raise RuntimeError(
            f"{fixture}: expected diagnostic {test['diagnostic']!r}, "
            f"got {code!r}"
        )
    if message != test["message"]:
        raise RuntimeError(
            f"{fixture}: expected message {test['message']!r}, "
            f"got {message!r}"
        )
    if actual_span != expected_span:
        raise RuntimeError(
            f"{fixture}: expected span {expected_span}, got {actual_span}"
        )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True, type=pathlib.Path)
    parser.add_argument("--manifest", required=True, type=pathlib.Path)
    parser.add_argument("--timeout", type=int, default=30)
    parser.add_argument(
        "--skip-compile", action="store_true",
        help="skip fixtures that must link an executable, for platforms "
             "where the toolchain cannot link one")
    args = parser.parse_args()

    manifest = args.manifest.resolve()
    root = manifest.parent
    tests = toml.load(manifest)["test"]
    failures = []
    skipped = 0
    for test in tests:
        fixture = (root / test["filename"]).resolve()
        if args.skip_compile and test["expect"] == "compile":
            skipped += 1
            print(f"SKIP {test['expect']:7} {test['filename']}")
            continue
        try:
            if test["expect"] == "compile":
                check_compile(args.lfortran, fixture, args.timeout)
            elif test["expect"] == "verify":
                check_verify(args.lfortran, fixture, test, args.timeout)
            else:
                raise RuntimeError(
                    f"{fixture}: unknown expectation {test['expect']!r}"
                )
            print(f"PASS {test['expect']:7} {test['filename']}")
        except (OSError, RuntimeError) as error:
            failures.append(str(error))
            print(f"FAIL {test['expect']:7} {test['filename']}")

    if failures:
        print("\n".join(failures), file=sys.stderr)
        return 1
    print(f"{len(tests) - skipped} ASR corpus tests passed, {skipped} skipped")
    return 0


if __name__ == "__main__":
    sys.exit(main())
