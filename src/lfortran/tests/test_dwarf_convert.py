#!/usr/bin/env python3
"""Checks src/libasr/dwarf_convert.py on small `llvm-dwarfdump --debug-line`
listings.

An optimized build can place one sequence of a compile unit, such as a `.cold`
function, after the code of later compile units. The converter must then
order whole sequences by address, keeping each end_sequence row, and must
still reject overlapping sequences and rows that go backwards.
"""

import argparse
import os
import pathlib
import struct
import subprocess
import sys

HEADER = """lfortran:\tfile format Mach-O arm64

.debug_line contents:
"""

ROWS_HEADER = """
Address            Line   Column File   ISA Discriminator Flags
------------------ ------ ------ ------ --- ------------- -------------
"""


def table(offset, name, rows):
    text = (f"debug_line[0x{offset:08x}]\n"
            "Line table prologue:\n"
            "         version: 5\n"
            'include_directories[  0] = "/work"\n'
            "file_names[  0]:\n"
            f'           name: "{name}"\n'
            "      dir_index: 0\n")
    text += ROWS_HEADER
    for address, line, flags in rows:
        text += f"0x{address:016x} {line:6d}      0      0   0             0  {flags}\n"
    return text


def convert(work_dir, converter, name, tables):
    raw = os.path.join(work_dir, name + ".txt")
    with open(raw, "w") as f:
        f.write(HEADER + "\n".join(tables))
    lines_txt = os.path.join(work_dir, name + "_lines.txt")
    lines_dat = os.path.join(work_dir, name + "_lines.dat")
    result = subprocess.run([sys.executable, converter, raw, lines_txt, lines_dat],
        cwd=work_dir, capture_output=True, text=True)
    if result.returncode != 0:
        return result, None, None
    with open(lines_txt) as f:
        text = f.read().splitlines()
    with open(lines_dat, "rb") as f:
        data = f.read()
    rows = [struct.unpack_from("3Q", data, i) for i in range(0, len(data), 24)]
    return result, text, rows


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--converter", required=True)
    parser.add_argument("--work-dir", required=True)
    args = parser.parse_args()
    args.converter = os.path.abspath(args.converter)
    os.makedirs(args.work_dir, exist_ok=True)
    failures = []

    def check(condition, message):
        if not condition:
            failures.append(message)

    # The second sequence of the first table lies after the second table.
    first = table(0, "/work/a.f90", [
        (0x1000, 1, "is_stmt"),
        (0x1010, 2, "is_stmt"),
        (0x1020, 2, "end_sequence"),
        (0x3000, 9, "is_stmt"),
        (0x3008, 9, "is_stmt end_sequence"),
    ])
    second = table(0x100, "/work/b.f90", [
        (0x2000, 5, "is_stmt"),
        (0x2004, 0, ""),
        (0x2010, 6, "end_sequence"),
    ])
    result, text, rows = convert(args.work_dir, args.converter, "cold", [first, second])
    check(result.returncode == 0, "out-of-order sequences rejected:\n" + result.stderr)
    if rows is not None:
        names = [str(pathlib.Path(name).resolve()) for name in ("/work/a.f90", "/work/b.f90")]
        check(text == ["2"] + names + ["7"], f"unexpected file list {text}")
        check(rows == [(0x1000, 1, 0), (0x1010, 2, 0), (0x1020, 2, 0),
                       (0x2000, 5, 1), (0x2010, 6, 1),
                       (0x3000, 9, 0), (0x3008, 9, 0)],
            f"unexpected rows {rows}")

    # A sequence that starts inside another one cannot be ordered.
    overlapping = table(0x100, "/work/b.f90", [
        (0x1008, 5, "is_stmt"),
        (0x1018, 6, "end_sequence"),
    ])
    result, _, _ = convert(args.work_dir, args.converter, "overlap", [first, overlapping])
    check(result.returncode != 0 and "AssertionError" in result.stderr,
        "overlapping sequences accepted")

    backwards = table(0, "/work/a.f90", [
        (0x1010, 1, "is_stmt"),
        (0x1008, 2, "is_stmt"),
        (0x1020, 2, "end_sequence"),
    ])
    result, _, _ = convert(args.work_dir, args.converter, "backwards", [backwards])
    check(result.returncode != 0 and "AssertionError" in result.stderr,
        "a decreasing address inside a sequence accepted")

    unterminated = table(0, "/work/a.f90", [
        (0x1000, 1, "is_stmt"),
        (0x1010, 2, "end_sequence"),
        (0x1020, 3, "is_stmt"),
    ])
    result, _, _ = convert(args.work_dir, args.converter, "unterminated", [unterminated])
    check(result.returncode != 0 and "end_sequence" in result.stderr,
        "a row after the last end_sequence accepted")

    for message in failures:
        print("FAIL:", message)
    if failures:
        sys.exit(1)
    print("dwarf_convert: all checks passed")


if __name__ == "__main__":
    main()
