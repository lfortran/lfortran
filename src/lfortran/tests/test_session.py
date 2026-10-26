"""Shares one Jupyter kernel (`lfortran kernel -f`) between several stock
jupyter_client clients and the prompt (`lfortran --connect`)."""

import argparse
import os
import queue
from pathlib import Path
import re
import shutil
import signal
import subprocess
import sys
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


def shared_session(lfortran, cwd, connection):
    a, b = connect(connection), connect(connection)
    clients = [a, b]
    try:
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
    """The plain prompt runs its cells in a kernel it starts, which other
    clients can join and which outlives the prompt while they are connected."""
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
        client = connect(connection)
        check(run(client, "integer :: shared = 5")[0]["status"] == "ok", "declaration failed")
        rest, _ = prompt.communicate(b"shared + 1\r\x04", timeout=60)
        out = (out + rest).decode(errors="replace")
        check(prompt.returncode == 0, "the prompt failed:\n" + out)
        check("[other client] integer :: shared = 5" in out,
            "the prompt did not show the other client's cell:\n" + out)
        check(re.search(r"^6\r?$", out, re.M), "the prompt's cell failed:\n" + out)
        check(value(client, "shared * 2") == "10", "the kernel stopped with the prompt")
        client.stop_channels()
        client = None
        deadline = time.monotonic() + 30
        while connection.exists() and time.monotonic() < deadline:
            time.sleep(0.05)
        check(not connection.exists(), "the kernel did not stop after its last client")
    finally:
        if client:
            client.stop_channels()
        if prompt.poll() is None:
            prompt.kill()
            prompt.wait()


def start(lfortran, cwd, connection, log):
    return subprocess.Popen([lfortran, "kernel", "-f", str(connection)], cwd=cwd,
        stdin=subprocess.DEVNULL, stdout=log, stderr=log)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--builtin", action="store_true",
        help="the kernel is the built-in one, not the Xeus one")
    args = parser.parse_args()
    cwd = Path.cwd() / ("session-" + uuid.uuid4().hex)
    cwd.mkdir(mode=0o700)
    log_path = cwd / "kernel.log"
    try:
        with log_path.open("wb") as log:
            # A kernel started by a Jupyter frontend, with its connection file.
            connection = cwd / "connection.json"
            write_connection_file(fname=str(connection), key=uuid.uuid4().hex.encode())
            kernel = start(args.lfortran, cwd, connection, log)
            try:
                shared_session(args.lfortran, cwd, connection)
                kernel.wait(timeout=30)
            finally:
                if kernel.poll() is None:
                    kernel.kill()
                    kernel.wait()
            check(kernel.returncode == 0, "the kernel exited with %d" % kernel.returncode)
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
