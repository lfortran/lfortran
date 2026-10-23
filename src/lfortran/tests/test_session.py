"""Checks `lfortran --serve` with several clients: shared state, output, READ
input and errors, through the socket protocol and through `--connect`."""

import argparse
import os
import re
import signal
import socket
import struct
import subprocess
import sys
import tempfile
import time


class Client:
    """A client speaking the session protocol (see src/bin/session.cpp)."""

    def __init__(self, path):
        deadline = time.monotonic() + 60
        while True:
            try:
                self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
                self.sock.connect(path)
                return
            except (FileNotFoundError, ConnectionRefusedError):
                self.sock.close()
                if time.monotonic() > deadline:
                    raise
                time.sleep(0.05)

    def send(self, kind, payload=b""):
        self.sock.sendall(struct.pack("!I", len(payload) + 1) + kind + payload)

    def receive(self):
        def exactly(n):
            data = b""
            while len(data) < n:
                chunk = self.sock.recv(n - len(data))
                if not chunk:
                    sys.exit("the server closed the connection")
                data += chunk
            return data
        size, = struct.unpack("!I", exactly(4))
        data = exactly(size)
        return data[:1], data[1:]

    def run(self, code, records=()):
        """Runs a cell, answering its READs with `records` (None is end of
        file), and returns its frames and status."""
        self.send(b"x", code.encode())
        records = list(records)
        frames = []
        while True:
            kind, payload = self.receive()
            if kind == b".":
                return frames, payload.decode()
            if kind == b"?":
                record = records.pop(0)
                if record is None:
                    self.send(b"e")
                else:
                    self.send(b"i", record.encode())
            else:
                frames.append((kind, payload.decode()))

    def value(self, code):
        frames, status = self.run(code)
        if status != "ok":
            sys.exit("%r failed: %r" % (code, frames))
        results = [p for k, p in frames if k == b"r"]
        return results[0] if results else None


def check(condition, message):
    if not condition:
        sys.exit(message)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    lfortran = parser.parse_args().lfortran

    with tempfile.TemporaryDirectory() as cwd:
        path = os.path.join(cwd, "session.sock")
        server = subprocess.Popen([lfortran, "--no-color", "--serve", path],
            cwd=cwd)
        try:
            a, b = Client(path), Client(path)
            # The clients share one session.
            check(a.run("integer :: x = 41")[1] == "ok", "declaration failed")
            check(b.value("x + 1") == "42", "state is not shared")
            # Output goes to the client that ran the cell.
            frames, _ = a.run("print *, 'hello'\nwrite(0,*) 'warning text'")
            check(any(k == b"1" and "hello" in p for k, p in frames),
                "missing stdout: %r" % frames)
            check(any(k == b"2" and "warning text" in p for k, p in frames),
                "missing stderr: %r" % frames)
            # READ asks the client that ran the cell; output written before
            # the READ arrives before the request.
            frames, status = b.run("print *, 'before'\nread(*,*) x", ["5"])
            check(status == "ok" and frames[0][0] == b"1"
                and "before" in frames[0][1], "READ failed: %r" % frames)
            check(a.value("x") == "5", "READ did not set x")
            frames, status = b.run(
                "integer :: ios\nread(*,*,iostat=ios) x", [None])
            check(status == "ok" and a.value("ios") == "-1",
                "end of file not seen by READ")
            # A cell waiting in READ holds the other clients' cells back.
            b.send(b"x", b"read(*,*) x")
            check(b.receive()[0] == b"?", "no input request")
            a.send(b"x", b"x * 2")
            b.send(b"i", b"21")
            check(b.receive() == (b".", b"ok"), "READ cell did not finish")
            frames = []
            while True:
                kind, payload = a.receive()
                if kind == b".":
                    break
                frames.append((kind, payload))
            check((b"r", b"42") in frames, "queued cell: %r" % frames)
            # Errors and display data.
            frames, status = a.run("integer :: broken(")
            check(status == "error" and any(k == b"2" and "error" in p
                for k, p in frames), "no compiler error: %r" % frames)
            frames, status = a.run("use lfortran_display, only: display_data\n"
                "call display_data('text/plain', 'shown')")
            check(status == "ok" and (b"d", "text/plain\0shown") in frames,
                "no display data: %r" % frames)
            # A client that goes away does not stop the server.
            a.sock.close()
            check(b.value("x + 1") == "22", "server lost after a disconnect")

            # The prompt as a client.
            prompt = subprocess.run([lfortran, "--no-color", "--connect", path],
                input=b"x + 100\rread(*,*) x\r7\rx * 3\r\x04",
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                cwd=cwd, timeout=60)
            out = prompt.stdout.decode(errors="replace")
            check(prompt.returncode == 0, "the prompt failed:\n" + out)
            for expected in [r"^121\r?$", r"^21\r?$"]:
                check(re.search(expected, out, re.M),
                    "expected %r in the prompt output:\n%s" % (expected, out))
            b.sock.close()
        finally:
            server.send_signal(signal.SIGTERM)
            server.wait(timeout=30)
        check(server.returncode == 0, "the server exited with %d"
            % server.returncode)
        check(not os.path.exists(path), "the server left its socket behind")
        missing = subprocess.run([lfortran, "--connect", path],
            input=b"", capture_output=True, timeout=60)
        check(missing.returncode != 0 and b"cannot connect" in missing.stderr,
            "connecting to no server: %r" % missing.stderr)
    print("session checks passed")


if __name__ == "__main__":
    main()
