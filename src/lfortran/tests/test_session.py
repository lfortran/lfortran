"""Shares one Jupyter kernel (`lfortran kernel -f`) between several stock
jupyter_client clients and the prompt (`lfortran --connect`)."""

import argparse
import json
import os
import queue
from pathlib import Path
import re
import shutil
import signal
import socket
import subprocess
import sys
import threading
import time
import uuid

try:
    from jupyter_client import BlockingKernelClient
    from jupyter_client.connect import write_connection_file
except ImportError:
    print("jupyter_client is not installed; skipping")
    sys.exit(77)  # SKIP_RETURN_CODE in CMakeLists.txt


def check(condition, message):
    if not condition:
        raise AssertionError(message)


def busy(seconds):
    """A cell that runs for `seconds`, on every platform"""
    return ("block; integer(8) :: start, now, rate; call system_clock(start, rate); "
        "do; call system_clock(now); if (now - start >= %d * rate) exit; end do; end block"
        % seconds)


def connect(connection):
    client = BlockingKernelClient(connection_file=str(connection))
    client.load_connection_file()
    client.start_channels()
    client.wait_for_ready(timeout=60)
    return client


def parent_id(msg):
    # Xeus sends some messages with a null parent header.
    return (msg["parent_header"] or {}).get("msg_id")


def run(client, code, records=()):
    """Runs a cell, answering its READs with `records` (None is end of
    file), and returns its reply and IOPub messages."""
    records = list(records)
    messages = []
    msg_id = client.execute(code, allow_stdin=True)
    deadline = time.monotonic() + 60
    while True:
        check(time.monotonic() < deadline, "%r did not finish" % code)
        try:
            client.get_stdin_msg(timeout=0.01)
            record = records.pop(0)
            client.input("\x04" if record is None else record)
        except queue.Empty:
            pass
        try:
            msg = client.get_iopub_msg(timeout=0.05)
        except queue.Empty:
            continue
        if parent_id(msg) != msg_id:
            continue
        if msg["msg_type"] == "status" and msg["content"]["execution_state"] == "idle":
            break
        messages.append(msg)
    return reply_to(client, msg_id, 30)["content"], messages


def value(client, code):
    reply, messages = run(client, code)
    check(reply["status"] == "ok", (code, reply, messages))
    results = [m["content"]["data"]["text/plain"] for m in messages
               if m["msg_type"] == "execute_result"]
    return results[0] if results else None


def text(messages, name):
    return "".join(m["content"]["text"] for m in messages
        if m["msg_type"] == "stream" and m["content"]["name"] == name)


def reply_to(client, msg_id, timeout):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        try:
            reply = client.get_shell_msg(timeout=0.1)
        except queue.Empty:
            continue
        if parent_id(reply) == msg_id:
            return reply
    return None


def ordered_output(client):
    """Keep stream labels and producer order, even within one drain interval."""
    delay = 'call execute_command_line("sleep 1")' if os.name == "posix" else busy(1)
    cases = [
        ([("stderr", "B\n"), ("stdout", "C\n")],
            'write(error_unit, "(a)") "B"\nflush(error_unit)\n'
            'write(output_unit, "(a)") "C"\nflush(output_unit)\n'),
        ([("stdout", "A\n"), ("stderr", "B\n"), ("stdout", "C\n"),
            ("stderr", "D\n")],
            'print "(a)", "A"\nwrite(error_unit, "(a)") "B"\n'
            'write(output_unit, "(a)") "C"\nwrite(error_unit, "(a)") "D"\n'),
        ([("stderr", "partial"), ("stdout", "other\n"), ("stderr", "end\n")],
            'write(error_unit, "(a)", advance="no") "partial"\n'
            'write(output_unit, "(a)") "other"\nwrite(error_unit, "(a)") "end"\n'),
    ]
    for iteration in range(3):
        for expected, writes in cases:
            code = ("block\nuse iso_fortran_env, only: output_unit, error_unit\n"
                + writes + delay + "\nend block")
            request = client.execute(code)
            events, last_output = [], None
            deadline = time.monotonic() + 30
            while True:
                check(time.monotonic() < deadline, "ordered cell did not finish")
                try:
                    msg = client.get_iopub_msg(timeout=0.1)
                except queue.Empty:
                    continue
                if parent_id(msg) != request:
                    check(msg["msg_type"] != "stream", ("stale ordered output", msg))
                    continue
                if msg["msg_type"] == "stream":
                    content = msg["content"]
                    events.extend((content["name"], char) for char in content["text"])
                    last_output = time.monotonic()
                if msg["msg_type"] == "status" and msg["content"]["execution_state"] == "idle":
                    break
            reply = reply_to(client, request, 30)
            check(reply and reply["content"]["status"] == "ok", reply)
            wanted = [(name, char) for name, value in expected for char in value]
            check(events == wanted, ("cross-stream order/content", expected, events))
            margin = time.monotonic() - last_output if last_output is not None else 0
            check(margin >= 0.5, ("ordered output withheld until idle", margin))
            reply, messages = run(client, "block\ninteger :: untouched\nend block")
            check(reply["status"] == "ok" and not any(
                msg["msg_type"] == "stream" for msg in messages),
                ("ordered output leaked into next cell", reply, messages))
        print("ordered live streams: repetition", iteration + 1, "passed", flush=True)


def live_output(lfortran, cwd, connection, client):
    """Observe actual output during a cell, not just its eventual contents."""
    ordered_output(client)
    delay = 'call execute_command_line("sleep 3")' if os.name == "posix" else busy(3)
    cases = [("stdout", 'print *, "first"\n' + delay, False),
        ("stdout", 'print *, "first"\n' + delay + '\nprint *, "second"', True),
        ("stderr", 'write(0,*) "first"\n' + delay, False)]
    if os.name == "posix":
        cases.append(("stdout", 'call execute_command_line("printf \'first\\n\'; sleep 3")', False))
    for channel, code, has_second in cases:
        request = client.execute(code)
        messages, first, second = [], None, None
        deadline = time.monotonic() + 30
        while True:
            check(time.monotonic() < deadline, "streaming cell did not finish")
            try:
                msg = client.get_iopub_msg(timeout=0.1)
            except queue.Empty:
                continue
            if parent_id(msg) != request:
                check(msg["msg_type"] != "stream", ("output leaked between cells", msg))
                continue
            now = time.monotonic()
            messages.append(msg)
            out = text(messages, channel)
            if first is None and "first\n" in out:
                first = now
            if second is None and "second\n" in out:
                second = now
            if msg["msg_type"] == "status" and msg["content"]["execution_state"] == "idle":
                break
        reply = reply_to(client, request, 30)
        check(reply and reply["content"]["status"] == "ok", reply)
        check(out.split() == (["first", "second"] if has_second else ["first"]), out)
        gap = now - first if first is not None else 0
        print("live kernel: first-to-idle %.3f s" % gap, flush=True)
        check(gap >= 1.5, "output withheld until idle: %.3f s margin" % gap)
        if has_second:
            check(second - first >= 1.5, "the two prints arrived together")

    # More than a pipe buffer, including characters spanning read boundaries,
    # and enough cells to expose leaked descriptors or stale capture offsets.
    for i in range(20):
        reply, messages = run(client, "print '(a)', repeat('€', 400000)")
        check(reply["status"] == "ok", reply)
        check(text(messages, "stdout") == "€" * 400000 + "\n", "lost or corrupt output")
    reply, messages = run(client, "write(0,*) 'warning'\nprint *, 'after-warning'")
    check(reply["status"] == "ok" and text(messages, "stderr").strip() == "warning"
        and text(messages, "stdout").strip() == "after-warning", messages)

    # A real prompt, connected to the same live kernel. A reader timestamps
    # complete output lines; echoed source is deliberately not a match.
    process = subprocess.Popen([lfortran, "--no-color", "--connect", str(connection)],
        cwd=cwd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    lines = []
    output = bytearray()

    def read():
        pending = b""
        while True:
            chunk = os.read(process.stdout.fileno(), 16384)
            if not chunk:
                return
            output.extend(chunk)
            pending += chunk
            while b"\n" in pending:
                line, pending = pending.split(b"\n", 1)
                lines.append((time.monotonic(), line.strip()))

    reader = threading.Thread(target=read)
    reader.start()
    try:
        deadline = time.monotonic() + 15
        while b">>> " not in output:
            check(process.poll() is None and time.monotonic() < deadline,
                "redirected shared prompt not ready")
            time.sleep(0.01)
        reply, _ = run(client, 'block\nprint *, "shared-pipe"\nend block')
        check(reply["status"] == "ok", reply)
        deadline = time.monotonic() + 10
        while not any(line == b"shared-pipe" for _, line in lines):
            check(time.monotonic() < deadline, "redirected shared prompt withheld output")
            time.sleep(0.01)
        cell = 'print *, "first"\x0e' + delay + '\x0eprint *, "second"\r'
        process.stdin.write((cell * 2 + "\x04").encode())
        process.stdin.close()
        process.wait(timeout=45)
        reader.join(timeout=5)
        check(not reader.is_alive(), "prompt output reader did not stop")
        check(process.returncode == 0, ("prompt failed", lines))
        outputs = [(t, line) for t, line in lines if line in (b"first", b"second")]
        check([line for _, line in outputs] == [b"first", b"second"] * 2, lines)
        for i in (0, 2):
            gap = outputs[i + 1][0] - outputs[i][0]
            print("live prompt: first-to-second %.3f s" % gap, flush=True)
            check(gap >= 1.5, "prompt withheld output: %.3f s margin" % gap)
    finally:
        if process.poll() is None:
            process.kill()
            process.wait()
        reader.join(timeout=5)
        process.stdout.close()
    check(value(client, "6*7") == "42", "prompt departure damaged the session")
    shared_prompt_output(lfortran, cwd, connection, client)


def shared_prompt_output(lfortran, cwd, connection, client):
    """Other clients' output must arrive without submitting the edited cell."""
    if os.name != "posix":
        return  # The PTY harness needs POSIX; the client/editor are portable.
    import fcntl
    import pty
    import select
    import struct
    import termios

    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))
    process = subprocess.Popen([lfortran, "--no-color", "--connect", str(connection)],
        cwd=cwd, stdin=slave, stdout=slave, stderr=slave)
    os.close(slave)
    data = b""

    def receive():
        nonlocal data
        if select.select([master], [], [], 0.02)[0]:
            chunk = os.read(master, 16384)
            check(chunk, "shared prompt closed")
            # Answer the editor's cursor-position queries, as a terminal does.
            if b"\x1b[6n" in chunk:
                os.write(master, b"\x1b[1;1R")
            data += chunk

    def wait_for(predicate, message, timeout=10):
        deadline = time.monotonic() + timeout
        while not predicate():
            check(process.poll() is None and time.monotonic() < deadline,
                (message, data))
            receive()

    try:
        wait_for(lambda: b">>> " in data, "shared prompt not ready")
        # A complete one-PRINT cell used to remain queued until the next key.
        # Also keep an unfinished multiline edit across later output batches.
        for channel, pending, delayed in (("stdout", b"", False),
                ("stdout", b"6*&\x0e", True), ("stderr", b"4", True)):
            if pending:
                os.write(master, pending)
                wait_for(lambda: b"6*" in data if pending != b"4" else b"4" in data,
                    "pending edit not shown")
            data = b""
            code = ('print *, "shared-first"' if channel == "stdout"
                else 'write(0,*) "shared-first"')
            if delayed:
                code += "\n" + busy(3)
            # BLOCK also exercises F23 without its unrelated raw PRINT bug.
            request = client.execute("block\n" + code + "\nend block")
            started = time.monotonic()
            wait_for(lambda: re.search(rb"(?:^|[\r\n]) *shared-first\r?\n", data),
                "shared output withheld while editing")
            first = time.monotonic() - started
            messages = []
            deadline = started + 30
            while True:
                check(time.monotonic() < deadline, "shared cell did not finish")
                receive()
                try:
                    msg = client.get_iopub_msg(timeout=0.01)
                except queue.Empty:
                    continue
                if parent_id(msg) != request:
                    continue
                messages.append(msg)
                if msg["msg_type"] == "status" and msg["content"]["execution_state"] == "idle":
                    break
            reply = reply_to(client, request, 10)
            check(reply and reply["content"]["status"] == "ok", reply)
            check(text(messages, channel).split() == ["shared-first"], messages)
            margin = time.monotonic() - started - first
            print("shared prompt: %s first %.3f s, idle margin %.3f s"
                % (channel, first, margin), flush=True)
            if delayed:
                check(margin >= 1.5, "shared progress arrived only after idle")
        # The edit was 6*&<newline>4. Finish it, then use history and editing.
        data = b""
        os.write(master, b"2\r")
        wait_for(lambda: b"252\r\n" in data
            and b">>> " in data.split(b"252\r\n")[-1],
            "shared output damaged pending edit")
        data = b""
        os.write(master, b"\x1b[A\x7f1\r")
        wait_for(lambda: b"246\r\n" in data
            and b">>> " in data.split(b"246\r\n")[-1],
            "shared output damaged edit history")
        os.write(master, b"\x04")
        wait_for(lambda: b"Exiting." in data, "shared prompt did not exit")
        check(process.wait(timeout=10) == 0, "shared prompt failed")
    finally:
        if process.poll() is None:
            process.kill()
            process.wait()
        os.close(master)
    check(value(client, "6*7") == "42", "shared prompt departure damaged session")


def shared_session(lfortran, cwd, connection):
    a, b = connect(connection), connect(connection)
    clients = [a, b]
    try:
        live_output(lfortran, cwd, connection, a)
        # The clients share one session.
        check(run(a, "integer :: x = 41")[0]["status"] == "ok", "declaration failed")
        check(value(b, "x + 1") == "42", "state is not shared")
        # Output goes to every client, as in Jupyter.
        reply, messages = run(a, "print *, 'hello'\nwrite(0,*) 'warning text'")
        check("hello" in text(messages, "stdout"), messages)
        check("warning text" in text(messages, "stderr"), messages)
        seen = []
        while True:
            try:
                msg = b.get_iopub_msg(timeout=5)
            except queue.Empty:
                break
            seen.append(msg)
            if (msg["msg_type"] == "status" and msg["content"]["execution_state"] == "idle"
                    and "hello" in text(seen, "stdout")):
                break
        check("hello" in text(seen, "stdout"), "the other client missed the output")
        # READ asks the client that ran the cell; output written before the
        # READ arrives before the request.
        reply, messages = run(b, "print *, 'before'\nread(*,*) x", ["5"])
        check(reply["status"] == "ok" and "before" in text(messages, "stdout"),
            (reply, messages))
        check(value(a, "x") == "5", "READ did not set x")
        reply, _ = run(b, "integer :: ios\nread(*,*,iostat=ios) x", [None])
        check(reply["status"] == "ok" and value(a, "ios") == "-1",
            "end of file not seen by READ")
        # A cell waiting in READ holds the other clients' cells back, and
        # only the client that ran it is asked.
        b_id = b.execute("read(*,*) x", allow_stdin=True)
        request = b.get_stdin_msg(timeout=30)
        check(request["msg_type"] == "input_request", request)
        a_id = a.execute("x * 2")
        check(reply_to(a, a_id, 1) is None, "a cell ran while another waited in READ")
        check(not a.stdin_channel.msg_ready(), "the wrong client was asked for input")
        b.input("21")
        check(reply_to(b, b_id, 30)["content"]["status"] == "ok", "READ cell failed")
        check(reply_to(a, a_id, 30)["content"]["status"] == "ok", "queued cell failed")
        check(value(a, "x") == "21", "READ did not set x")
        # The heartbeat answers while a cell runs.
        b_id = b.execute(busy(3))
        time.sleep(2)
        check(a.is_alive(), "no heartbeat while a cell runs")
        check(reply_to(b, b_id, 30)["content"]["status"] == "ok", "long cell failed")
        # Errors and display data.
        reply, messages = run(a, "integer :: broken(")
        check(reply["status"] == "error" and "error" in text(messages, "stderr"),
            (reply, messages))
        reply, messages = run(a, "use lfortran_display, only: display_data\n"
            "call display_data('text/plain', 'shown')")
        check(reply["status"] == "ok" and any(m["msg_type"] == "display_data"
            and m["content"]["data"] == {"text/plain": "shown"} for m in messages),
            messages)
        # A client that goes away does not stop the kernel.
        a.stop_channels()
        clients.remove(a)
        check(value(b, "x + 1") == "22", "kernel lost after a disconnect")

        # The prompt as a client.
        prompt = subprocess.run([lfortran, "--no-color", "--connect", str(connection)],
            input=b"x + 100\rread(*,*) x\r7\rx * 3\r\x04",
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, cwd=cwd, timeout=60)
        out = prompt.stdout.decode(errors="replace")
        check(prompt.returncode == 0, "the prompt failed:\n" + out)
        for expected in [r"^121\r?$", r"^21\r?$"]:
            check(re.search(expected, out, re.M),
                "expected %r in the prompt output:\n%s" % (expected, out))
        check(value(b, "x") == "7", "the prompt's READ did not set x")
        b.shutdown()
    finally:
        for client in clients:
            client.stop_channels()


def prompt_session(lfortran, cwd):
    """The plain prompt runs its cells in a kernel it starts. Other clients
    can join while that prompt is running. When the prompt exits it ends the
    kernel and removes the connection file, including while another client
    is connected."""
    prompt = subprocess.Popen([lfortran, "--no-color"], cwd=cwd,
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    client = None
    try:
        out = b""
        while True:
            line = prompt.stdout.readline()
            check(line, "the prompt did not tell its connection file:\n%s"
                % out.decode(errors="replace"))
            out += line
            joined = re.search(rb"lfortran --connect (\S+)`", line)
            if joined:
                break
        connection = Path(joined.group(1).decode())
        heartbeat = json.loads(connection.read_text())["hb_port"]
        client = connect(connection)
        check(run(client, "integer :: shared = 5")[0]["status"] == "ok", "declaration failed")
        rest, _ = prompt.communicate(b"shared + 1\r\x04", timeout=60)
        out = (out + rest).decode(errors="replace")
        check(prompt.returncode == 0, "the prompt failed:\n" + out)
        check("[other client] integer :: shared = 5" in out,
            "the prompt did not show the other client's cell:\n" + out)
        check(re.search(r"^6\r?$", out, re.M), "the prompt's cell failed:\n" + out)
        # The other client is still connected. The kernel has ended, so do
        # not evaluate through it.
        deadline = time.monotonic() + 30
        while connection.exists() and time.monotonic() < deadline:
            time.sleep(0.05)
        check(not connection.exists(), "the prompt left the connection file")
        listening = time.monotonic() + 30
        while True:
            try:
                socket.create_connection(("127.0.0.1", heartbeat), timeout=1).close()
            except OSError:
                break
            check(time.monotonic() < listening, "the kernel still listens")
            time.sleep(0.1)
    finally:
        if client:
            client.stop_channels()
        if prompt.poll() is None:
            prompt.kill()
            prompt.wait()


def start(lfortran, cwd, connection, log, arguments=()):
    return subprocess.Popen([lfortran, *arguments, "kernel", "-f", str(connection)], cwd=cwd,
        stdin=subprocess.DEVNULL, stdout=log, stderr=log)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--builtin", action="store_true",
        help="the kernel is the built-in one, not the Xeus one")
    parser.add_argument("--streaming-only", action="store_true")
    parser.add_argument("--ordering-only", action="store_true")
    parser.add_argument("--compiler-arg", action="append", default=[])
    args = parser.parse_args()
    cwd = Path.cwd() / ("session-" + uuid.uuid4().hex)
    cwd.mkdir(mode=0o700)
    for name in ("TMPDIR", "TMP", "TEMP"):
        os.environ[name] = str(cwd)
    log_path = cwd / "kernel.log"
    try:
        with log_path.open("wb") as log:
            # A kernel started by a Jupyter frontend, with its connection file.
            connection = cwd / "connection.json"
            write_connection_file(fname=str(connection), key=uuid.uuid4().hex.encode())
            kernel = start(args.lfortran, cwd, connection, log, args.compiler_arg)
            try:
                if args.streaming_only or args.ordering_only:
                    client = connect(connection)
                    try:
                        if args.ordering_only:
                            ordered_output(client)
                        else:
                            live_output(args.lfortran, cwd, connection, client)
                        client.shutdown()
                    finally:
                        client.stop_channels()
                else:
                    shared_session(args.lfortran, cwd, connection)
                kernel.wait(timeout=30)
            finally:
                if kernel.poll() is None:
                    kernel.kill()
                    kernel.wait()
            check(kernel.returncode == 0, "the kernel exited with %d" % kernel.returncode)
            if args.streaming_only or args.ordering_only:
                return
            if args.builtin:
                # A kernel started by hand writes its connection file and
                # removes it when it stops.
                connection = cwd / "session.json"
                kernel = start(args.lfortran, cwd, connection, log)
                client = None
                try:
                    deadline = time.monotonic() + 60
                    while not connection.exists() and time.monotonic() < deadline:
                        time.sleep(0.05)
                    time.sleep(0.2)
                    client = connect(connection)
                    check(value(client, "6 * 7") == "42", "kernel started by hand")
                    if os.name == "posix":
                        check(connection.stat().st_mode & 0o077 == 0,
                            "the connection file is readable by others")
                        kernel.send_signal(signal.SIGTERM)
                    else:
                        # Windows has no SIGTERM for another process.
                        client.shutdown()
                    kernel.wait(timeout=30)
                finally:
                    if client:
                        client.stop_channels()
                    if kernel.poll() is None:
                        kernel.kill()
                        kernel.wait()
                check(kernel.returncode == 0, "the kernel exited with %d" % kernel.returncode)
                check(not connection.exists(), "the kernel left its connection file")
        prompt_session(args.lfortran, cwd)
        missing = subprocess.run([args.lfortran, "--connect", str(cwd / "missing.json")],
            input=b"", capture_output=True, timeout=60)
        check(missing.returncode != 0 and b"connection file" in missing.stderr,
            "connecting to no kernel: %r" % missing.stderr)
    except BaseException:
        if log_path.exists():
            print(log_path.read_text(errors="replace"))
        raise
    finally:
        shutil.rmtree(cwd, ignore_errors=True)
    print("session checks passed")


if __name__ == "__main__":
    main()
