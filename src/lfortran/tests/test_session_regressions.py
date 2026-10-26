"""A shared session (`lfortran kernel -f`) with clients that misbehave:
large output, connections that stall, a client that leaves during a READ,
cells queued behind a failing one, and cells that stop the kernel."""

import argparse
import contextlib
import json
import os
from pathlib import Path
import queue
import shutil
import socket
import subprocess
import time
import uuid

from test_session import busy, check, parent_id
from jupyter_client import BlockingKernelClient
from jupyter_client.connect import write_connection_file


def connect(connection):
    client = BlockingKernelClient(connection_file=str(connection))
    client.load_connection_file()
    client.start_channels()
    try:
        client.wait_for_ready(timeout=20)
    except BaseException:
        client.stop_channels()
        raise
    return client


def reply_to(client, msg_id, timeout=20):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        try:
            msg = client.get_shell_msg(timeout=0.1)
        except queue.Empty:
            continue
        if parent_id(msg) == msg_id:
            return msg["content"]
    raise AssertionError("no reply within %d seconds" % timeout)


def idle_order(client, msg_ids, timeout=20):
    """Waits for the idle statuses that end requests, for which clients
    wait; returns the requests in the order of their idle statuses, and the
    types of the IOPub messages of each."""
    order, types = [], {msg_id: [] for msg_id in msg_ids}
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline and len(order) < len(msg_ids):
        try:
            msg = client.get_iopub_msg(timeout=0.1)
        except queue.Empty:
            continue
        if parent_id(msg) not in msg_ids:
            continue
        types[parent_id(msg)].append(msg["msg_type"])
        if msg["msg_type"] == "status" and msg["content"]["execution_state"] == "idle":
            order.append(parent_id(msg))
    check(len(order) == len(msg_ids), "no idle status within %d seconds" % timeout)
    return order, types


def run(client, code, timeout=20, **options):
    """Runs a cell and returns its reply and IOPub messages."""
    msg_id = client.execute(code, **options)
    messages = []
    deadline = time.monotonic() + timeout
    while True:
        check(time.monotonic() < deadline, "%r did not finish" % code)
        try:
            msg = client.get_iopub_msg(timeout=0.1)
        except queue.Empty:
            continue
        if parent_id(msg) != msg_id:
            continue
        if msg["msg_type"] == "status" and msg["content"]["execution_state"] == "idle":
            return reply_to(client, msg_id, timeout), messages
        messages.append(msg)


def value(client, code, timeout=20):
    reply, messages = run(client, code, timeout)
    check(reply["status"] == "ok", (code, reply, messages))
    results = [m["content"]["data"]["text/plain"] for m in messages
               if m["msg_type"] == "execute_result"]
    return results[0] if results else None


@contextlib.contextmanager
def kernel(exe, cwd, ip="127.0.0.1"):
    """A kernel started on a connection file, as a Jupyter frontend does, a
    function that connects a client to it, and the connection file's content
    with its "path"."""
    connection = cwd / ("kernel-" + uuid.uuid4().hex + ".json")
    write_connection_file(str(connection), ip=ip, key=uuid.uuid4().hex.encode())
    clients = []

    def join():
        clients.append(connect(connection))
        return clients[-1]

    with connection.with_suffix(".log").open("wb") as log:
        process = subprocess.Popen([exe, "--no-color", "kernel", "-f", str(connection)],
            cwd=cwd, stdin=subprocess.DEVNULL, stdout=log, stderr=log)
        info = json.loads(connection.read_text())
        info["path"] = str(connection)
        try:
            yield process, join, info
        finally:
            for client in clients:
                client.stop_channels()
            if process.poll() is None:
                process.kill()
                process.wait()


def zmtp(port, socket_type):
    """A raw ZeroMQ connection (ZMTP 3.0, NULL mechanism) that has finished
    its handshake as a socket of `socket_type`."""
    s = socket.create_connection(("127.0.0.1", port), timeout=20)
    s.sendall(b"\xff" + bytes(8) + b"\x7f\x03\x00NULL" + bytes(16) + bytes(32))
    greeting = b""
    while len(greeting) < 64:
        greeting += s.recv(64 - len(greeting))
    ready = b"\x05READY\x0bSocket-Type" + len(socket_type).to_bytes(4, "big") + socket_type
    s.sendall(bytes([4, len(ready)]) + ready)
    flags, size = s.recv(1)[0], s.recv(1)[0]
    check(flags == 4, "no READY command")
    while size > 0:
        size -= len(s.recv(size))
    return s


def large_output(exe, cwd):
    """A stream larger than the most a subscriber that does not read can be
    sent reaches a client that does read, followed by the cell's idle status.
    The subscriber that does not read holds nothing back."""
    with kernel(exe, cwd) as (_, join, ports):
        client = join()
        stalled = zmtp(ports["iopub_port"], b"SUB")
        stalled.sendall(b"\x00\x01\x01")  # subscribes to every message
        size = 70 * 1024 * 1024
        reply, messages = run(client, "print '(a)', repeat('x', %d)" % size, timeout=120)
        check(reply["status"] == "ok", reply)
        text = "".join(m["content"].get("text", "") for m in messages
            if m["msg_type"] == "stream")
        check(text.count("x") >= size, "the client lost the output")
        check(value(client, "6*7") == "42", "the next cell did not run")
        check(value(join(), "7*8") == "56", "another client did not run its cell")
        stalled.close()


def stalled_connections(exe, cwd):
    """Connections that stop during their handshake or in a message hold
    nothing back: other clients run cells and READs, and the kernel shuts
    down when asked. A client that connects during a long cell waits."""
    with kernel(exe, cwd) as (process, join, ports):
        client = join()
        stalled = []
        for name in ["shell_port", "control_port", "stdin_port", "iopub_port", "hb_port"]:
            stalled.append(socket.create_connection(("127.0.0.1", ports[name])))
            stalled.append(socket.create_connection(("127.0.0.1", ports[name])))
            stalled[-1].sendall(b"\xff" + bytes(5))  # part of a greeting
        check(value(client, "6*7") == "42", "a cell did not run")
        check(value(join(), "6*8") == "48", "a new client did not run a cell")
        # A frame of 5 bytes of which only 2 come
        stalled.append(zmtp(ports["shell_port"], b"DEALER"))
        stalled[-1].sendall(b"\x01\x05ab")
        stalled.append(zmtp(ports["stdin_port"], b"DEALER"))
        stalled[-1].sendall(b"\x01\x05ab")
        msg_id = client.execute("integer :: n\nread(*,*) n", allow_stdin=True)
        check(client.get_stdin_msg(timeout=20)["msg_type"] == "input_request", "no READ")
        client.input("5")
        check(reply_to(client, msg_id)["status"] == "ok", "the READ failed")
        check(value(client, "n + 1") == "6", "the READ did not set n")
        check(client.is_alive(), "no heartbeat")
        # A client that connects while a cell runs longer than a handshake
        # may take waits for it.
        msg_id = client.execute(busy(12))
        time.sleep(1)
        check(value(join(), "6*9", timeout=30) == "54", "a client that connected during the cell failed")
        check(reply_to(client, msg_id)["status"] == "ok", "the long cell failed")
        client.shutdown()
        try:
            process.wait(timeout=20)
        except subprocess.TimeoutExpired:
            raise AssertionError("the kernel did not shut down")
        for s in stalled:
            s.close()


def read_disconnect(exe, cwd):
    """A READ whose client disconnects fails with an I/O error: with
    iostat=, the cell gets it and the other clients go on; else it stops
    the kernel with a runtime error, as it stops a program."""
    with kernel(exe, cwd) as (process, join, _ports):
        a, b = join(), join()
        check(run(a, "integer :: x = 1, ios = 0")[0]["status"] == "ok", "declaration failed")
        a.execute("read(*,*,iostat=ios) x", allow_stdin=True)
        check(a.get_stdin_msg(timeout=20)["msg_type"] == "input_request", "no READ")
        a.stop_channels()
        check(int(value(b, "ios")) > 0, "the READ did not fail")
        check(value(b, "x") == "1", "the READ set x")
        c = join()
        c.execute("read(*,*) x", allow_stdin=True)
        check(c.get_stdin_msg(timeout=20)["msg_type"] == "input_request", "no READ")
        c.stop_channels()
        try:
            process.wait(timeout=20)
        except subprocess.TimeoutExpired:
            raise AssertionError("a READ without iostat= went on after its client left")
        check(process.returncode != 0, "the kernel exited with 0 after a failed READ")


def late_stdin(exe, cwd):
    """A READ waits a little for the stdin connection of the client that ran
    the cell, which may come after the cell."""
    with kernel(exe, cwd) as (_, join, ports):
        join()  # once the kernel listens
        client = BlockingKernelClient(connection_file=ports["path"])
        client.load_connection_file()
        client.start_channels(stdin=False)
        try:
            client.wait_for_ready(timeout=20)
            msg_id = client.execute("integer :: n = 1, ios = 0\nread(*,*,iostat=ios) n",
                allow_stdin=True)
            time.sleep(0.3)
            client.stdin_channel.start()
            check(client.get_stdin_msg(timeout=20)["msg_type"] == "input_request", "no READ")
            client.input("7")
            check(reply_to(client, msg_id)["status"] == "ok", "the READ failed")
            check(value(client, "ios") == "0" and value(client, "n") == "7", "the READ failed")
        finally:
            client.stop_channels()


def stop_flushes(exe, cwd):
    """STOP and ERROR STOP end the kernel. Output written before them reaches
    the client, and the code after them does not run."""
    cases = [
        ("print *, 'before'\nif (.true.) stop\nprint *, 'after'", 0, None),
        ("print *, 'before'\nstop 3\nprint *, 'after'", 3, "STOP"),
        ("print *, 'before'\nerror stop\nprint *, 'after'", 1, "ERROR STOP"),
    ]
    for code, status, shown in cases:
        with kernel(exe, cwd) as (process, join, _info):
            client = join()
            msg_id = client.execute(code)
            texts = []
            deadline = time.monotonic() + 20
            exited = None
            while time.monotonic() < deadline:
                if process.poll() is not None and exited is None:
                    exited = time.monotonic()
                if exited is not None and time.monotonic() > exited + 1:
                    break
                try:
                    msg = client.get_iopub_msg(timeout=0.2)
                except queue.Empty:
                    continue
                except Exception:
                    if exited is not None:
                        break
                    raise
                if (msg.get("parent_header") or {}).get("msg_id") != msg_id:
                    continue
                if msg["msg_type"] == "stream":
                    texts.append(msg["content"].get("text", ""))
            got = process.wait(timeout=10)
            blob = "".join(texts)
            check(got == status, "%s exited %d:\n%s" % (code, got, blob))
            check("before" in blob and "after" not in blob, blob)
            if shown:
                check(shown in blob, blob)


def stop_on_error(exe, cwd):
    """The execute_requests that wait behind a cell that fails are aborted
    if it has stop_on_error, else they run."""
    with kernel(exe, cwd) as (_, join, _ports):
        client = join()
        check(run(client, "integer :: qx, gate")[0]["status"] == "ok", "declaration failed")
        for stop, expected in [(True, "0"), (False, "99")]:
            check(run(client, "qx = 0")[0]["status"] == "ok", "assignment failed")
            # The READ holds the kernel until both requests have arrived.
            gate = client.execute("read(*,*) gate", allow_stdin=True)
            check(client.get_stdin_msg(timeout=20)["msg_type"] == "input_request", "no READ")
            failed = client.execute("integer :: broken(", stop_on_error=stop)
            queued = client.execute("qx = 99")
            time.sleep(0.5)
            client.input("1")
            check(reply_to(client, gate)["status"] == "ok", "the READ failed")
            check(reply_to(client, failed)["status"] == "error", "the error was not seen")
            status = reply_to(client, queued)["status"]
            check(status == ("aborted" if stop else "ok"),
                "stop_on_error=%s: the next cell's status is %s" % (stop, status))
            # A client may send its next cell once the failed one is idle.
            # An aborted cell is not run: only its status is published.
            if stop:
                order, types = idle_order(client, [failed, queued])
                check(order == [queued, failed],
                    "the failed cell was idle before the next cell was aborted")
                check(types[queued] == ["status", "status"],
                    "the aborted cell published %s" % types[queued])
            else:
                idle_order(client, [queued])
            check(value(client, "qx") == expected, "stop_on_error=%s: qx" % stop)



CASES = {f.__name__: f for f in [large_output, stalled_connections, read_disconnect,
    late_stdin, stop_flushes, stop_on_error]}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--case", choices=CASES, action="append",
        help="run only this case (repeatable)")
    parser.add_argument("--builtin", action="store_true",
        help="the kernel is the built-in one, not the Xeus one")
    args = parser.parse_args()
    exe = str(Path(args.lfortran).resolve())
    cwd = Path.cwd() / ("session-regressions-" + uuid.uuid4().hex)
    cwd.mkdir(mode=0o700)
    # The prompt writes its kernel's connection file here.
    for name in ["TMPDIR", "TMP", "TEMP"]:
        os.environ[name] = str(cwd)
    failed = []
    try:
        for name in args.case or CASES:
            try:
                CASES[name](exe, cwd)
                print(name + ": passed", flush=True)
            except Exception as error:
                print("%s: FAILED: %s: %s" % (name, type(error).__name__, error), flush=True)
                for log in cwd.glob("kernel-*.log"):
                    print(log.read_text(errors="replace"))
                    log.unlink()
                failed.append(name)
    finally:
        shutil.rmtree(cwd, ignore_errors=True)
    if failed:
        raise SystemExit("failed: " + ", ".join(failed))


if __name__ == "__main__":
    main()
