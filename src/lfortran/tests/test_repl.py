#!/usr/bin/env python3
"""Drive the interactive prompt (`lfortran` with no files) through stdin."""

import argparse
import os
import subprocess
import sys
import tempfile

ENTER = b"\r"
CTRL_D = b"\x04"


def run_repl(lfortran, cwd, cells):
    keys = b"".join(cell.encode() + ENTER for cell in cells) + CTRL_D
    return subprocess.run([lfortran, "--no-color"], input=keys, cwd=cwd,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=300)


def check_keeps_files_in_cwd(lfortran):
    # The prompt must not write the submitted text to a file in the user's
    # directory, here one named `input` (#13189).
    sentinel = b"KEEP THIS FILE\n"
    with tempfile.TemporaryDirectory() as cwd:
        path = os.path.join(cwd, "input")
        with open(path, "wb") as f:
            f.write(sentinel)
        # The incomplete cells make the prompt quote them in a diagnostic,
        # the unterminated string already when it checks for completeness.
        r = run_repl(lfortran, cwd, ["integer :: trial_seed = 41",
            "trial_seed + 1", "integer :: x = ", 'x = "abc', "trial_seed"])
        out = r.stdout.decode(errors="replace")
        if r.returncode != 0 or "Exiting." not in out:
            sys.exit("the prompt did not exit cleanly (code %d):\n%s"
                % (r.returncode, out))
        if "File not found." in out:
            sys.exit("a diagnostic did not quote the cell:\n%s" % out)
        for expected in ["42", "41", "token not recognized", 'x = "abc']:
            if expected not in out:
                sys.exit("expected %r in the prompt output:\n%s"
                    % (expected, out))
        with open(path, "rb") as f:
            if f.read() != sentinel:
                sys.exit("the prompt overwrote `input` in its directory")
        if sorted(os.listdir(cwd)) != ["input"]:
            sys.exit("the prompt left files in its directory: %s"
                % sorted(os.listdir(cwd)))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    args = parser.parse_args()
    check_keeps_files_in_cwd(os.path.abspath(args.lfortran))
    print("ok")


if __name__ == "__main__":
    main()
