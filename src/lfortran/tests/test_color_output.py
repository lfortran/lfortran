#!/usr/bin/env python3
"""Check when LFortran colors its diagnostics and AST/ASR output (#17829).

By default (`--color=auto`) the output is colored only when it goes to a
terminal and NO_COLOR (https://no-color.org) is not set to a non-empty value.
`--color=always` and `--color=never` override that, and `--no-color` is the
same as `--color=never`. The terminal cases need a pseudo-terminal, so they
are skipped where there is none (Windows).
"""

import argparse
import os
import select
import subprocess
import sys
import tempfile

try:
    import pty
except ImportError:
    pty = None

INVALID = """\
program color_invalid
implicit none
print *, x
end program
"""

VALID = """\
program color_valid
implicit none
integer :: i
i = 1
print *, i
end program
"""

# The prescanner reports the missing include file.
MISSING_INCLUDE = """\
program color_include
include "color_missing.inc"
end program
"""

# Compiled with -g, ERROR STOP prints a stack trace.
STACKTRACE = """\
program color_stacktrace
implicit none
error stop 1
end program
"""

ESCAPE = b"\x1b["


def run(cmd, cwd, no_color, terminal):
    """Run `cmd` in `cwd` and return its stdout and stderr.

    `no_color` is the value of NO_COLOR, or None to unset it. `terminal` names
    the streams ("stdout", "stderr") connected to a pseudo-terminal; both
    then share it, so the output of both is returned as stderr.
    """
    env = dict(os.environ)
    env.pop("NO_COLOR", None)
    if no_color is not None:
        env["NO_COLOR"] = no_color
    if not terminal:
        p = subprocess.run(cmd, cwd=cwd, env=env, stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=120)
        return p.stdout, p.stderr
    master, slave = pty.openpty()
    streams = {
        "stdout": slave if "stdout" in terminal else subprocess.PIPE,
        "stderr": slave if "stderr" in terminal else subprocess.PIPE,
    }
    p = subprocess.Popen(cmd, cwd=cwd, env=env, stdin=subprocess.DEVNULL,
        **streams)
    os.close(slave)
    # The outputs are small, so a pipe cannot fill up while the terminal is
    # read; a hung compiler still fails the test instead of blocking it.
    out = b""
    while True:
        if not select.select([master], [], [], 120)[0]:
            p.kill()
            break
        try:
            data = os.read(master, 4096)
        except OSError:
            break
        if not data:
            break
        out += data
    os.close(master)
    stdout = p.stdout.read() if p.stdout else b""
    stderr = p.stderr.read() if p.stderr else b""
    p.wait(timeout=120)
    if "stderr" in terminal:
        stderr = out
    else:
        stdout = out
    return stdout, stderr


def check_stacktrace(lfortran, cwd):
    """The stack trace of a program compiled with -g is colored when the
    program's stderr is a terminal, so the object code does not depend on
    where the compiler's output went."""
    failures = []
    compile_ = ["-g", "-c", "stacktrace.f90", "-o"]
    for name, terminal in [("pipe.o", ()), ("terminal.o", ("stderr",))]:
        run([lfortran] + compile_ + [name], cwd, None, terminal)
    with open(os.path.join(cwd, "pipe.o"), "rb") as f:
        pipe = f.read()
    with open(os.path.join(cwd, "terminal.o"), "rb") as f:
        terminal = f.read()
    if pipe != terminal:
        failures.append("lfortran %s: the object code depends on whether "
            "stderr is a terminal" % " ".join(compile_))
    for name, flags in [("colors", []), ("no_colors", ["--no-color"])]:
        run([lfortran, "-g"] + flags + ["stacktrace.f90", "-o", name], cwd,
            None, ())
    _, plain = run([os.path.join(cwd, "colors")], cwd, None, ())
    if b"color_stacktrace" not in plain and b"stacktrace.f90" not in plain:
        # This build prints no stack trace.
        return failures
    cases = [
        ("colors", None, (), False),
        ("colors", None, ("stderr",), True),
        ("colors", "1", ("stderr",), False),
        ("no_colors", None, ("stderr",), False),
    ]
    for name, no_color, terminal, colored in cases:
        _, stderr = run([os.path.join(cwd, name)], cwd, no_color, terminal)
        if (ESCAPE in stderr) != colored:
            failures.append("%s%s (%s on a terminal): the stack trace is "
                "%scolored:\n%r" % (
                "" if no_color is None else "NO_COLOR=%r " % no_color, name,
                ", ".join(terminal) or "nothing",
                "not " if colored else "", stderr))
    return failures


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--workdir", required=True)
    args = parser.parse_args()
    lfortran = os.path.abspath(args.lfortran)
    os.makedirs(args.workdir, exist_ok=True)

    # (arguments, NO_COLOR, streams on a terminal, checked stream, colored)
    compile_ = ["invalid.f90"]
    compile_only = ["-c", "invalid.f90", "-o", "invalid.o"]
    show_ast = ["--show-ast", "valid.f90"]
    fmt = ["fmt", "valid.f90"]
    show_prescan = ["--show-prescan", "include.f90"]
    show_tokens = ["--show-tokens", "include.f90"]
    cases = [
        (compile_, None, (), "stderr", False),
        (compile_only, None, (), "stderr", False),
        (["--color=auto"] + compile_, None, (), "stderr", False),
        (["--color=always"] + compile_, None, (), "stderr", True),
        (["--color=always"] + compile_, "1", (), "stderr", True),
        (["--color=always", "--no-color"] + compile_, None, (), "stderr",
            False),
        (["--color=never"] + compile_, None, (), "stderr", False),
        (show_ast, None, (), "stdout", False),
        (["--color=always"] + show_ast, None, (), "stdout", True),
        (fmt, None, (), "stdout", False),
        (show_prescan, None, (), "stderr", False),
        (show_tokens, None, (), "stderr", False),
        (["--color=always"] + show_prescan, None, (), "stderr", True),
        (["--color=always"] + show_tokens, None, (), "stderr", True),
        (["--color=always"] + fmt, None, (), "stdout", True),
    ]
    if pty is not None:
        tty = ("stderr",)
        both = ("stdout", "stderr")
        cases += [
            (compile_, None, tty, "stderr", True),
            (compile_only, None, tty, "stderr", True),
            (compile_, "1", tty, "stderr", False),
            (compile_only, "1", tty, "stderr", False),
            (compile_, "", tty, "stderr", True),
            (["--color=always"] + compile_, "1", tty, "stderr", True),
            (["--no-color"] + compile_, None, tty, "stderr", False),
            (["--color=never"] + compile_, None, tty, "stderr", False),
            (show_ast, None, both, "stderr", True),
            (show_ast, "1", both, "stderr", False),
            # The AST goes to stdout, which is not a terminal.
            (show_ast, None, tty, "stdout", False),
            (["--no-color"] + show_prescan, None, both, "stderr", False),
            (["--no-color"] + show_tokens, None, both, "stderr", False),
            (fmt, None, ("stdout",), "stdout", True),
            (fmt, "1", ("stdout",), "stdout", False),
            (["fmt", "--no-color", "valid.f90"], None, ("stdout",), "stdout",
                False),
        ]

    failures = []
    with tempfile.TemporaryDirectory(dir=args.workdir) as cwd:
        for name, source in [("invalid", INVALID), ("valid", VALID),
                ("include", MISSING_INCLUDE), ("stacktrace", STACKTRACE)]:
            with open(os.path.join(cwd, name + ".f90"), "w") as f:
                f.write(source)
        for arguments, no_color, terminal, checked, colored in cases:
            stdout, stderr = run([lfortran] + arguments, cwd, no_color,
                terminal)
            output = stdout if checked == "stdout" else stderr
            description = "%s%s (%s on a terminal)" % (
                "" if no_color is None else "NO_COLOR=%r " % no_color,
                " ".join(["lfortran"] + arguments),
                ", ".join(terminal) or "nothing")
            if not output.strip():
                failures.append("%s: wrote nothing to %s\nstdout: %r\n"
                    "stderr: %r" % (description, checked, stdout, stderr))
            elif (ESCAPE in output) != colored:
                failures.append("%s: %s is %scolored:\n%r" % (description,
                    checked, "not " if colored else "", output))
        if pty is not None:
            failures += check_stacktrace(lfortran, cwd)
    if failures:
        sys.exit("\n".join(failures))
    print("ok")


if __name__ == "__main__":
    main()
