#!/usr/bin/env python3
"""Drive the interactive prompt (`lfortran` with no files) through stdin: a
pipe or a pseudo-terminal, also with Ctrl-C. Also run a compiled program on a
pseudo-terminal."""

import argparse
import codecs
import contextlib
import os
import re
import select
import shutil
import signal
import subprocess
import sys
import tempfile
import threading
import time

ENTER = b"\r"
CTRL_D = b"\x04"
NEW_LINE = "\x0e"  # Ctrl-N: a new line in the cell


@contextlib.contextmanager
def working_directory():
    """A temporary directory to run the prompt in. The kernel the prompt
    started, whose working directory it is too, may still be stopping when
    the prompt has exited, and Windows cannot remove it until then."""
    path = tempfile.mkdtemp()
    try:
        yield path
    finally:
        deadline = time.monotonic() + 30
        while True:
            try:
                shutil.rmtree(path)
                break
            except OSError:
                if time.monotonic() > deadline:
                    raise
                time.sleep(0.1)


def run_repl(lfortran, cwd, cells, timeout=60, max_output=1 << 20, args=()):
    # A prompt that never finishes can print without end, so its output is
    # capped and it is killed after `timeout` seconds.
    keys = b"".join(cell.encode() + ENTER for cell in cells) + CTRL_D
    p = subprocess.Popen([lfortran, "--no-color", *args], cwd=cwd,
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
    with working_directory() as cwd:
        path = os.path.join(cwd, "input")
        with open(path, "wb") as f:
            f.write(sentinel)
        # The incomplete cells make the prompt quote them in a diagnostic,
        # the unterminated string already when it checks for completeness.
        code, out = run_repl(lfortran, cwd, ["integer :: trial_seed = 41",
            "trial_seed + 1", "integer :: x = ", 'x = "abc', "trial_seed",
            "'shared evaluation'", ".true.", "1.2345678901234567_8"])
        if code != 0 or "Exiting." not in out:
            sys.exit("the prompt did not exit cleanly (code %d):\n%s"
                % (code, out))
        # The prompt echoes every keystroke, so the results are matched as
        # whole lines and the cell as a line quoted by a diagnostic.
        for expected in [r"^42\r?$", r"^41\r?$", r'^1 \| x = "abc\r?$',
                r"^shared evaluation\r?$", r"^True\r?$",
                r"^1\.2345678901234567\r?$"]:
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


def check_cell_output(lfortran):
    # An inspection cell's output ends with a newline, a real result has a
    # decimal point and a successful cell shows its warnings.
    cells = ["%%showast" + NEW_LINE + "integer :: shown", "1.0",
        "max0(1, 2_8)"]
    with working_directory() as cwd:
        code, out = run_repl(lfortran, cwd, cells)
        if code != 0 or "Exiting." not in out:
            sys.exit("the prompt did not exit cleanly (code %d):\n%s"
                % (code, out))
        for expected in [r"^\)\r?$", r"^1\.0\r?$", r"^2\r?$",
                r"^warning: Different kinds of args in max0"]:
            if not re.search(expected, out, re.M):
                sys.exit("expected %r in the prompt output:\n%s"
                    % (expected, out))
        # In verbose mode an inspection cell shows its output only.
        code, out = run_repl(lfortran, cwd, cells[:1], args=["-v"])
        if code != 0 or "TranslationUnit" not in out \
                or "Return type" in out:
            sys.exit("unexpected verbose output of an inspection cell "
                "(code %d):\n%s" % (code, out))


class PtyRepl:
    """The prompt on a pseudo-terminal, typed at as by a user."""

    def __init__(self, pty, termios, lfortran, cwd, timeout, args=(), screen=None):
        self.termios = termios
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
        self.screen = screen

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
            if len(self.out) > 8 << 20:
                raise AssertionError("the prompt exceeded the PTY output cap")
            if self.screen is not None:
                for response in self.screen.feed(data):
                    os.write(self.fd, response)
                    self.prompts += 1
            else:
                while self.prompts < self.out.count(b"\x1b[6n"):
                    os.write(self.fd, b"\x1b[1;1R")
                    self.prompts += 1
        return True

    def cell(self, text, keys=None):
        if not self.wait(lambda: self.prompts > self.cells, "the prompt"):
            sys.exit("the prompt exited before %r:\n%s" % (text, self.text()))
        self.cells += 1
        os.write(self.fd, text.encode() + ENTER)
        if keys is not None:
            # The cell reads `keys` from the terminal, which it must have in
            # line mode, not in the raw mode of the prompt.
            if not self.wait(lambda: self.termios.tcgetattr(self.fd)[3]
                    & self.termios.ICANON, "the cell to read"):
                sys.exit("the prompt exited instead of reading in %r:\n%s"
                    % (text, self.text()))
            os.write(self.fd, keys)

    def exit(self):
        if self.wait(lambda: self.prompts > self.cells, "the prompt"):
            os.write(self.fd, CTRL_D)
            self.wait(lambda: False, "the prompt to exit")
        _, status = os.waitpid(self.pid, 0)
        return os.waitstatus_to_exitcode(status)


class Screen:
    """The small VT subset used by the editor; unlike stripping ANSI codes,
    this detects overwritten output, scrolling and misplaced logical cursors."""

    def __init__(self, rows=12, cols=80):
        self.rows, self.cols = rows, cols
        self.row = self.col = 0
        self.lines = [" " * cols for _ in range(rows)]
        self.scrolled = []
        self.pending = ""
        self.decoder = codecs.getincrementaldecoder("utf-8")("replace")
        # A test may type just before a cursor reply to exercise interleaving.
        self.before_report = b""
        self.on_render = None
        self.cursor_visible = True

    def newline(self):
        self.row += 1
        if self.row == self.rows:
            self.scrolled.append(self.lines.pop(0))
            self.lines.append(" " * self.cols)
            self.row -= 1

    def feed(self, data):
        text = self.pending + self.decoder.decode(data)
        self.pending = ""
        replies = []
        i = 0
        while i < len(text):
            c = text[i]
            if c == "\x1b":
                match = re.match(r"\x1b\[([0-9;?]*)([A-Za-z])", text[i:])
                if match is None:
                    self.pending = text[i:]
                    break
                arg, command = match.groups()
                nums = [int(n) if n else 1 for n in arg.split(";")
                        if not arg.startswith("?")]
                if command == "H":
                    self.row = min(self.rows, nums[0]) - 1
                    self.col = min(self.cols, nums[1] if len(nums) > 1 else 1) - 1
                elif command == "K":
                    if arg == "2":
                        self.lines[self.row] = " " * self.cols
                    else:
                        self.lines[self.row] = self.lines[self.row][:self.col] \
                            + " " * (self.cols - self.col)
                elif command == "J" and arg == "2":
                    self.lines = [" " * self.cols for _ in range(self.rows)]
                elif command == "n" and arg == "6":
                    replies.append(self.before_report
                        + ("\x1b[%d;%dR" % (self.row + 1,
                            min(self.col + 1, self.cols))).encode())
                    self.before_report = b""
                elif command in ("h", "l") and arg == "?25":
                    self.cursor_visible = command == "h"
                    if self.cursor_visible and self.on_render:
                        self.on_render(self)
                elif command not in ("m", "h", "l"):
                    raise AssertionError("unsupported screen command: " + match[0])
                i += len(match[0])
                continue
            if c == "\r":
                self.col = 0
            elif c == "\n":
                self.newline()
            elif c >= " ":
                if self.col == self.cols:
                    self.col = 0
                    self.newline()
                line = self.lines[self.row]
                self.lines[self.row] = line[:self.col] + c + line[self.col + 1:]
                self.col += 1
            i += 1
        return replies

    def text(self):
        return "\n".join(line.rstrip() for line in self.scrolled + self.lines)


def check_read_from_terminal(lfortran):
    # A cell reads the lines typed at the terminal (#15284), also after a
    # READ ended by Ctrl-D.
    try:
        import pty
        import termios
    except ImportError:
        print("skipped: no pseudo-terminals")
        return
    with working_directory() as cwd:
        repl = PtyRepl(pty, termios, lfortran, cwd, timeout=60)
        code = None
        try:
            repl.cell("integer :: n, ios")
            repl.cell("read(*,*) n", b"42" + ENTER)
            repl.cell("print *, n + 1")
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


def check_interrupt_at_prompt(lfortran):
    # Ctrl-C at `>>>`, while the line editor has the terminal in raw mode,
    # ends the prompt by SIGINT, as a shell expects, and its kernel. The
    # terminal is left as it was, in line mode with echo.
    try:
        import pty
        import termios
    except ImportError:
        print("skipped: no pseudo-terminals")
        return
    with working_directory() as cwd:
        repl = PtyRepl(pty, termios, lfortran, cwd, timeout=60)
        status = None
        try:
            repl.cell("integer :: n")
            repl.wait(lambda: repl.prompts > repl.cells, "the prompt")
            os.write(repl.fd, b"\x03")
            repl.wait(lambda: False, "the prompt to exit")
            _, status = os.waitpid(repl.pid, 0)
            lflag = termios.tcgetattr(repl.fd)[3]
        finally:
            if status is None:
                os.kill(repl.pid, 9)
                os.waitpid(repl.pid, 0)
            os.close(repl.fd)
    out = repl.text()
    if not os.WIFSIGNALED(status) or os.WTERMSIG(status) != signal.SIGINT:
        sys.exit("Ctrl-C did not end the prompt by SIGINT (status %d):\n%s"
            % (status, out))
    if lflag & (termios.ICANON | termios.ECHO) != termios.ICANON | termios.ECHO:
        sys.exit("Ctrl-C left the terminal in raw mode:\n%s" % out)
    connection = re.search(r"lfortran --connect (\S+)`", out)
    if not connection or os.path.exists(connection.group(1)):
        sys.exit("Ctrl-C left the connection file of the kernel:\n%s" % out)


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
    with working_directory() as cwd:
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
    check_cell_output(os.path.abspath(args.lfortran))
    check_read_from_terminal(os.path.abspath(args.lfortran))
    check_interrupt_at_prompt(os.path.abspath(args.lfortran))
    check_compiled_read_from_terminal(os.path.abspath(args.lfortran))
    print("ok")


if __name__ == "__main__":
    main()
