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
        "do; call system_clock(now); if (now - start >= %g * rate) exit; end do; end block"
        % seconds)


def busy_until(gate):
    """A cell that runs until the file `gate` exists, relative to the
    kernel's directory"""
    return ("block; logical :: done; do; inquire(file='%s', exist=done); "
        "if (done) exit; end do; end block" % gate)


def wait_for_ready(client, timeout=60):
    """As client.wait_for_ready(), without its fixed waits: sends
    kernel_info_requests until the status of one arrives on IOPub, which then
    takes all later messages, and takes the replies to them all."""
    deadline = time.monotonic() + timeout
    sent = []
    while True:
        check(time.monotonic() < deadline,
            "the kernel did not reply within %d seconds" % timeout)
        sent.append(client.kernel_info())
        try:
            msg = client.get_iopub_msg(timeout=0.05)
        except queue.Empty:
            continue
        if parent_id(msg) in sent:
            break
    # The rest of the request's statuses, so that they are not taken for
    # those of a later request.
    while not (parent_id(msg) == sent[-1] and msg["msg_type"] == "status"
            and msg["content"]["execution_state"] == "idle"):
        msg = client.get_iopub_msg(timeout=max(0.1, deadline - time.monotonic()))
    replies = set()
    while replies != set(sent):
        reply = client.get_shell_msg(timeout=max(0.1, deadline - time.monotonic()))
        replies.add(parent_id(reply))


def connect(connection, timeout=60):
    client = BlockingKernelClient(connection_file=str(connection))
    client.load_connection_file()
    client.start_channels()
    try:
        wait_for_ready(client, timeout)
    except BaseException:
        client.stop_channels()
        raise
    return client


def parent_id(msg):
    # Xeus sends some messages with a null parent header.
    return (msg["parent_header"] or {}).get("msg_id")


def run(client, code, records=()):
    """Runs a cell, answering its READs with `records` (None is end of
    file), and returns its reply and IOPub messages."""
    import zmq
    records = list(records)
    messages = []
    msg_id = client.execute(code, allow_stdin=True)
    poller = zmq.Poller()
    poller.register(client.iopub_channel.socket, zmq.POLLIN)
    poller.register(client.stdin_channel.socket, zmq.POLLIN)
    deadline = time.monotonic() + 60
    while True:
        check(time.monotonic() < deadline, "%r did not finish" % code)
        poller.poll(100)
        if client.stdin_channel.msg_ready():
            client.get_stdin_msg(timeout=0)
            record = records.pop(0)
            client.input("\x04" if record is None else record)
        if not client.iopub_channel.msg_ready():
            continue
        msg = client.get_iopub_msg(timeout=0)
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


def started(client, msg_id, timeout=30):
    """Waits until the kernel has taken the request `msg_id`."""
    deadline = time.monotonic() + timeout
    while True:
        msg = client.get_iopub_msg(timeout=max(0.1, deadline - time.monotonic()))
        if parent_id(msg) == msg_id and msg["msg_type"] == "status":
            return


def beats(client, timeout=5):
    """Whether the kernel answers a heartbeat within `timeout` seconds"""
    import zmq
    s = client.context.socket(zmq.REQ)
    s.linger = 0
    try:
        s.connect("%s://%s:%d" % (client.transport, client.ip, client.hb_port))
        s.send(b"ping")
        return s.poll(timeout * 1000) and s.recv() == b"ping"
    finally:
        s.close()


def shared_state(lfortran, cwd, connection, a, b, clients):
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
    check(reply_to(a, a_id, 0.2) is None, "a cell ran while another waited in READ")
    check(not a.stdin_channel.msg_ready(), "the wrong client was asked for input")
    b.input("21")
    check(reply_to(b, b_id, 30)["content"]["status"] == "ok", "READ cell failed")
    check(reply_to(a, a_id, 30)["content"]["status"] == "ok", "queued cell failed")
    check(value(a, "x") == "21", "READ did not set x")
    b.shutdown()


def shared_clients(lfortran, cwd, connection, a, b, clients):
    check(run(a, "integer :: x = 21")[0]["status"] == "ok", "declaration failed")
    # The heartbeat answers while a cell runs.
    b_id = b.execute(busy_until("heartbeat-gate"))
    started(b, b_id)
    try:
        check(beats(a), "no heartbeat while a cell runs")
    finally:
        (cwd / "heartbeat-gate").touch()
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


def written(connection, timeout=60):
    """`connection` once the kernel has written it"""
    deadline = time.monotonic() + timeout
    while True:
        try:
            json.loads(connection.read_text())
            return connection
        except (OSError, ValueError):
            check(time.monotonic() < deadline, "the kernel wrote no connection file")
            time.sleep(0.01)


def start(lfortran, cwd, connection):
    """A kernel on `connection` whose output goes to kernel.log in `cwd`"""
    with (cwd / "kernel.log").open("ab") as log:
        return subprocess.Popen([lfortran, "kernel", "-f", str(connection)], cwd=cwd,
            stdin=subprocess.DEVNULL, stdout=log, stderr=log)


def frontend_kernel(session):
    """`session` with two clients `a` and `b` of a kernel started by a
    Jupyter frontend, with its connection file. `session` ends by shutting
    the kernel down through `b`."""
    def case(lfortran, cwd):
        connection = cwd / (session.__name__ + ".json")
        write_connection_file(fname=str(connection), key=uuid.uuid4().hex.encode())
        kernel = start(lfortran, cwd, connection)
        clients = []
        try:
            clients.extend([connect(connection), connect(connection)])
            session(lfortran, cwd, connection, *clients, clients)
            kernel.wait(timeout=30)
        finally:
            for client in clients:
                client.stop_channels()
            if kernel.poll() is None:
                kernel.kill()
                kernel.wait()
        check(kernel.returncode == 0, "the kernel exited with %d" % kernel.returncode)
    case.__name__ = session.__name__
    return case


def hand_started_kernel(lfortran, cwd):
    """A kernel started by hand writes its connection file and removes it
    when it stops."""
    connection = cwd / "session.json"
    kernel = start(lfortran, cwd, connection)
    client = None
    try:
        client = connect(written(connection))
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


def missing_kernel(lfortran, cwd):
    missing = subprocess.run([lfortran, "--connect", str(cwd / "missing.json")],
        input=b"", capture_output=True, timeout=60)
    check(missing.returncode != 0 and b"connection file" in missing.stderr,
        "connecting to no kernel: %r" % missing.stderr)


CASES = {f.__name__: f for f in [frontend_kernel(shared_state),
    frontend_kernel(shared_clients), hand_started_kernel,
    prompt_session, missing_kernel]}
# Only the built-in kernel writes a connection file it is started on.
BUILTIN_ONLY = {"hand_started_kernel"}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--case", choices=CASES, action="append",
        help="run only this case (repeatable)")
    parser.add_argument("--builtin", action="store_true",
        help="the kernel is the built-in one, not the Xeus one")
    parser.add_argument("--all-cases", nargs="+", metavar="CASE",
        help="only check that these are all the cases a run would run")
    args = parser.parse_args()
    cases = [name for name in args.case or CASES
             if args.builtin or name not in BUILTIN_ONLY]
    if args.all_cases:
        check(sorted(args.all_cases) == sorted(cases),
            "the cases are %s, not %s" % (sorted(cases), sorted(args.all_cases)))
        return
    cwd = Path.cwd() / ("session-" + uuid.uuid4().hex)
    cwd.mkdir(mode=0o700)
    log_path = cwd / "kernel.log"
    try:
        for name in cases:
            CASES[name](args.lfortran, cwd)
    except BaseException:
        if log_path.exists():
            print(log_path.read_text(errors="replace"))
        raise
    finally:
        shutil.rmtree(cwd, ignore_errors=True)
    print("session checks passed")


if __name__ == "__main__":
    main()
