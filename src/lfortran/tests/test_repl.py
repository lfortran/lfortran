#!/usr/bin/env python3
"""Drive the interactive prompt (`lfortran` with no files) through stdin."""

import argparse
import os
import re
import subprocess
import sys
import tempfile
import threading

ENTER = b"\r"
CTRL_D = b"\x04"


def run_repl(lfortran, cwd, cells, timeout=60, max_output=1 << 20):
    # A prompt that never finishes can print without end, so its output is
    # capped and it is killed after `timeout` seconds.
    keys = b"".join(cell.encode() + ENTER for cell in cells) + CTRL_D
    p = subprocess.Popen([lfortran, "--no-color"], cwd=cwd,
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    timer = threading.Timer(timeout, p.kill)
    timer.start()
    try:
        p.stdin.write(keys)
        p.stdin.close()
        out = p.stdout.read(max_output)
        if len(out) >= max_output:
            p.kill()
            sys.exit("the prompt printed more than %d bytes" % max_output)
        p.wait()
    finally:
        timer.cancel()
        p.stdout.close()
    return p.returncode, out.decode(errors="replace")


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
        code, out = run_repl(lfortran, cwd, ["integer :: trial_seed = 41",
            "trial_seed + 1", "integer :: x = ", 'x = "abc', "trial_seed"])
        if code != 0 or "Exiting." not in out:
            sys.exit("the prompt did not exit cleanly (code %d):\n%s"
                % (code, out))
        # The prompt echoes every keystroke, so the results are matched as
        # whole lines and the cell as a line quoted by a diagnostic.
        for expected in [r"^42\r?$", r"^41\r?$", r'^1 \| x = "abc\r?$']:
            if not re.search(expected, out, re.M):
                sys.exit("expected %r in the prompt output:\n%s"
                    % (expected, out))
        if "File not found." in out or sentinel.decode().strip() in out:
            sys.exit("a diagnostic did not quote the cell:\n%s" % out)
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
