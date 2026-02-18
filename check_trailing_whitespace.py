#!/usr/bin/env python3
"""
Check (and optionally fix) trailing whitespace in .cpp, .c, .h, and .py files.

Usage:
    python check_trailing_whitespace.py          # check only (non-zero exit on failure)
    python check_trailing_whitespace.py --fix    # strip trailing whitespace in-place
"""

import argparse
import os
import sys
import subprocess


EXTENSIONS = {".cpp", ".c", ".h", ".py"}


def get_tracked_files():
    """Return the list of git-tracked files with the target extensions."""
    result = subprocess.run(
        ["git", "ls-files"],
        stdout=subprocess.PIPE,
        text=True,
        check=True,
    )
    files = []
    for path in result.stdout.splitlines():
        _, ext = os.path.splitext(path)
        if ext in EXTENSIONS and os.path.isfile(path):
            files.append(path)
    return sorted(files)


def check_file(filepath):
    """Return a list of (line_number, line_text) with trailing whitespace."""
    violations = []
    with open(filepath, "r", encoding="utf-8", errors="replace") as f:
        for lineno, line in enumerate(f, start=1):
            stripped = line.rstrip("\n").rstrip("\r")
            if stripped != stripped.rstrip():
                violations.append((lineno, stripped))
    return violations


def fix_file(filepath):
    """Strip trailing whitespace from every line in the file, in-place."""
    with open(filepath, "r", encoding="utf-8", errors="replace") as f:
        lines = f.readlines()
    with open(filepath, "w", encoding="utf-8", newline="") as f:
        for line in lines:
            f.write(line.rstrip() + "\n")


def main():
    parser = argparse.ArgumentParser(
        description="Check or fix trailing whitespace in source files."
    )
    parser.add_argument(
        "--fix",
        action="store_true",
        help="Strip trailing whitespace from files in-place.",
    )
    args = parser.parse_args()

    files = get_tracked_files()

    if args.fix:
        fixed = []
        for filepath in files:
            violations = check_file(filepath)
            if violations:
                fix_file(filepath)
                fixed.append(filepath)
        if fixed:
            print(f"Fixed trailing whitespace in {len(fixed)} file(s):")
            for f in fixed:
                print(f"  {f}")
        else:
            print("No trailing whitespace found.")
        return

    # --- Check mode (default) ---
    total_violations = 0
    for filepath in files:
        violations = check_file(filepath)
        for lineno, line in violations:
            print(f"{filepath}:{lineno}: trailing whitespace")
            total_violations += 1

    if total_violations:
        print(
            f"\nFAIL: {total_violations} line(s) with trailing whitespace found."
        )
        sys.exit(1)
    else:
        print("OK: no trailing whitespace found.")


if __name__ == "__main__":
    main()
