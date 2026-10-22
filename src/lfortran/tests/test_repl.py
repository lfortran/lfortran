#!/usr/bin/env python3
"""Drive the interactive prompt (`lfortran` with no files) through stdin: a
pipe or a pseudo-terminal. Also run a compiled program on a pseudo-terminal."""

import argparse
import json
import os
import re
import select
import subprocess
import sys
import tempfile
import threading
import time

ENTER = b"\r"
CTRL_D = b"\x04"


def run_repl(lfortran, cwd, cells, args=(), timeout=60, max_output=1 << 20,
        preexec_fn=None):
    # A prompt that never finishes can print without end, so its output is
    # capped and it is killed after `timeout` seconds.
    keys = b"".join(cell.encode() + ENTER for cell in cells) + CTRL_D
    p = subprocess.Popen([lfortran, "--no-color", *args], cwd=cwd,
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        preexec_fn=preexec_fn)
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


def check_status_file(lfortran):
    # With --status-file, each cell writes a line to the file as it finishes,
    # also when it does not compile (#15512). The cells are numbered as in
    # the diagnostics.
    with tempfile.TemporaryDirectory() as cwd:
        path = os.path.join(cwd, "status")
        code, out = run_repl(lfortran, cwd, ["integer :: x = 41", "x + 1",
            "integer :: y =", "x"], args=["--status-file", path])
        if code != 0 or "Exiting." not in out:
            sys.exit("the prompt did not exit cleanly (code %d):\n%s"
                % (code, out))
        if "<cell 3>" not in out:
            sys.exit("expected a diagnostic for <cell 3>:\n%s" % out)
        with open(path) as f:
            lines = [json.loads(line) for line in f]
        expected = [{"cell": 1, "status": "ok"}, {"cell": 2, "status": "ok"},
            {"cell": 3, "status": "error"}, {"cell": 4, "status": "ok"}]
        if lines != expected:
            sys.exit("expected the status lines %r, got %r"
                % (expected, lines))


def check_status_file_write_error(lfortran):
    # A status line that cannot be written ends the prompt with an error,
    # rather than leave a tool waiting for it. The file may hold one byte,
    # and SIGXFSZ is ignored, so that the write fails instead.
    try:
        import resource
        import signal
    except ImportError:
        print("skipped: no file size limit")
        return

    def limit_file_size():
        signal.signal(signal.SIGXFSZ, signal.SIG_IGN)
        resource.setrlimit(resource.RLIMIT_FSIZE, (1, 1))

    with tempfile.TemporaryDirectory() as cwd:
        path = os.path.join(cwd, "status")
        code, out = run_repl(lfortran, cwd, ["2 + 3", "4 + 5"],
            args=["--status-file", path], preexec_fn=limit_file_size)
    if code != 1 or "cannot write to the status file" not in out \
            or "Exiting." in out:
        sys.exit("expected the prompt to stop at the failed write (code %d):"
            "\n%s" % (code, out))


class PtyRepl:
    """The prompt on a pseudo-terminal, typed at as by a user."""

    def __init__(self, pty, termios, lfortran, cwd, timeout, status=None):
        self.termios = termios
        args = [] if status is None else ["--status-file", status]
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            try:
                os.chdir(cwd)
                os.execv(lfortran, [lfortran, "--no-color", *args])
            finally:
                os._exit(127)
        self.deadline = time.monotonic() + timeout
        self.out = b""
        self.prompts = 0
        self.cells = 0
        self.status = status
        self.statuses = []

    def text(self):
        return re.sub(rb"\x1b\[[0-9;?]*[A-Za-z]", b"", self.out).decode(
            errors="replace")

    def wait(self, done, what):
        # Reads the output until `done()`, answering the cursor position
        # query that starts every prompt as a terminal would. Returns False
        # if the output ends first.
        while not done():
            left = self.deadline - time.monotonic()
            if left <= 0:
                sys.exit("timed out waiting for %s:\n%s" % (what, self.text()))
            if not select.select([self.fd], [], [], min(left, 0.1))[0]:
                continue
            try:
                data = os.read(self.fd, 4096)
            except OSError:
                # Linux reports the end of the output as an error
                data = b""
            if not data:
                return False
            self.out += data
            while self.prompts < self.out.count(b"\x1b[6n"):
                os.write(self.fd, b"\x1b[1;1R")
                self.prompts += 1
        return True

    def check_status(self, finished):
        # While the prompt runs, the status file has a line for each of the
        # first `finished` cells and no others (#15512).
        if self.status is None:
            return
        expected = [{"cell": i + 1, "status": s}
            for i, s in enumerate(self.statuses[:finished])]
        with open(self.status) as f:
            lines = [json.loads(line) for line in f]
        if lines != expected:
            sys.exit("expected the status lines %r, got %r:\n%s"
                % (expected, lines, self.text()))

    def cell(self, text, keys=None, status="ok"):
        if not self.wait(lambda: self.prompts > self.cells, "the prompt"):
            sys.exit("the prompt exited before %r:\n%s" % (text, self.text()))
        # Every cell has its status line by the time the next prompt starts
        self.check_status(self.cells)
        self.cells += 1
        self.statuses.append(status)
        os.write(self.fd, text.encode() + ENTER)
        if keys is not None:
            # The cell reads `keys` from the terminal, which it must have in
            # line mode, not in the raw mode of the prompt.
            if not self.wait(lambda: self.termios.tcgetattr(self.fd)[3]
                    & self.termios.ICANON, "the cell to read"):
                sys.exit("the prompt exited instead of reading in %r:\n%s"
                    % (text, self.text()))
            # A cell still reading has not finished
            self.check_status(self.cells - 1)
            os.write(self.fd, keys)

    def exit(self):
        if self.wait(lambda: self.prompts > self.cells, "the prompt"):
            self.check_status(self.cells)
            os.write(self.fd, CTRL_D)
            self.wait(lambda: False, "the prompt to exit")
        _, status = os.waitpid(self.pid, 0)
        return os.waitstatus_to_exitcode(status)


def check_read_from_terminal(lfortran):
    # A cell reads the lines typed at the terminal (#15284), also after a
    # READ ended by Ctrl-D. Each cell writes its line to the status file as
    # it finishes, also one that does not compile, and none while a READ
    # waits (#15512).
    try:
        import pty
        import termios
    except ImportError:
        print("skipped: no pseudo-terminals")
        return
    with tempfile.TemporaryDirectory() as cwd:
        repl = PtyRepl(pty, termios, lfortran, cwd, timeout=60,
            status=os.path.join(cwd, "status"))
        code = None
        try:
            repl.cell("integer :: n, ios")
            repl.cell("read(*,*) n", b"42" + ENTER)
            repl.cell("print *, n + 1")
            repl.cell("integer :: m =", status="error")
            repl.cell("block; character(len=20) :: s; read(*,'(A)') s; "
                "print *, '[' // trim(s) // ']'; end block",
                b"hello world" + ENTER)
            repl.cell("read(*,*,iostat=ios) n", CTRL_D)
            repl.cell("print *, ios")
            repl.cell("read(*,*) n", b"7" + ENTER)
            repl.cell("print *, 3 * n")
            code = repl.exit()
        finally:
            if code is None:
                os.kill(repl.pid, 9)
                os.waitpid(repl.pid, 0)
            os.close(repl.fd)
    out = repl.text()
    if code != 0 or "Exiting." not in out:
        sys.exit("the prompt did not exit cleanly (code %d):\n%s"
            % (code, out))
    for expected in [r"^\s*43\r?$", r"^\s*\[hello world\]\r?$",
            r"^\s*-1\r?$", r"^\s*21\r?$"]:
        if not re.search(expected, out, re.M):
            sys.exit("expected %r in the prompt output:\n%s"
                % (expected, out))
    if "<cell 4>" not in out:
        sys.exit("expected a diagnostic for <cell 4>:\n%s" % out)


READ_TERMINAL_SOURCE = """\
program read_terminal
    character(len=8) :: a, b, c, d, e
    print '(a)', 'ready'
    read(*, '(a)') a
    read(*, '(a/a)') b, c
    read(*, '(a)') d, e
    print '(11a)', '[', trim(a), '|', trim(b), '|', trim(c), '|', &
        trim(d), '|', trim(e), ']'
end program
"""


def check_compiled_read_from_terminal(lfortran):
    # A compiled program reads the lines typed at the terminal in order, with
    # a format that starts the record at a statement, a slash and a reversion
    # (#15285). A terminal on macOS reports a successful ftell and fseek, but
    # seeking discards the line stdio has buffered, and the terminal cannot
    # send it again.
    try:
        import pty
    except ImportError:
        print("skipped: no pseudo-terminals")
        return
    with tempfile.TemporaryDirectory() as cwd:
        with open(os.path.join(cwd, "read_terminal.f90"), "w") as f:
            f.write(READ_TERMINAL_SOURCE)
        exe = os.path.join(cwd, "read_terminal")
        p = subprocess.run([lfortran, "--no-color", "read_terminal.f90",
            "-o", exe], cwd=cwd, stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT)
        if p.returncode != 0:
            sys.exit("compiling read_terminal.f90 failed:\n%s"
                % p.stdout.decode(errors="replace"))
        pid, fd = pty.fork()
        if pid == 0:
            try:
                os.execv(exe, [exe])
            finally:
                os._exit(127)
        deadline = time.monotonic() + 60
        out = b""
        typed = False
        code = None
        try:
            # The lines are typed once the program has started. A terminal
            # in line mode hands over at most one line per read, as if they
            # were typed one by one. Ctrl-D ends the input, so a program
            # that skipped a line stops at end of file.
            while True:
                left = deadline - time.monotonic()
                if left <= 0:
                    sys.exit("timed out running read_terminal:\n%s"
                        % out.decode(errors="replace"))
                if not select.select([fd], [], [], min(left, 0.1))[0]:
                    continue
                try:
                    data = os.read(fd, 4096)
                except OSError:
                    # Linux reports the end of the output as an error
                    data = b""
                if not data:
                    break
                out += data
                if not typed and b"ready" in out:
                    typed = True
                    for line in [b"one", b"two", b"three", b"four", b"five"]:
                        os.write(fd, line + ENTER)
                    os.write(fd, CTRL_D)
            _, status = os.waitpid(pid, 0)
            code = os.waitstatus_to_exitcode(status)
        finally:
            if code is None:
                os.kill(pid, 9)
                os.waitpid(pid, 0)
            os.close(fd)
    text = out.decode(errors="replace")
    if code != 0 or "[one|two|three|four|five]" not in text:
        sys.exit("read_terminal did not read the typed lines in order "
            "(code %d):\n%s" % (code, text))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    args = parser.parse_args()
    check_keeps_files_in_cwd(os.path.abspath(args.lfortran))
    check_status_file(os.path.abspath(args.lfortran))
    check_status_file_write_error(os.path.abspath(args.lfortran))
    check_read_from_terminal(os.path.abspath(args.lfortran))
    check_compiled_read_from_terminal(os.path.abspath(args.lfortran))
    print("ok")


if __name__ == "__main__":
    main()
