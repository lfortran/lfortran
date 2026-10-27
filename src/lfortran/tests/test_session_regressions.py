"""A shared session (`lfortran kernel -f`, `lfortran --connect`) with clients
that misbehave: large output, connections that stall, a client that leaves
during a READ, a prompt that ends the kernel it started, a prompt killed or
hung up on, also during a cell that does not return, cells queued behind a
failing one, cells that stop the kernel, and `lfortran -v`. Each case runs
against whichever kernel is built."""

import argparse
import contextlib
import json
import os
from pathlib import Path
import queue
import re
import shutil
import signal
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
def kernel(exe, cwd, ip="127.0.0.1", arguments=()):
    """A kernel started on a connection file, as a Jupyter frontend does, a
    function that connects a client to it, and the connection file's content
    with its "path". `arguments` are compiler options of the kernel process."""
    connection = cwd / ("kernel-" + uuid.uuid4().hex + ".json")
    write_connection_file(str(connection), ip=ip, key=uuid.uuid4().hex.encode())
    clients = []

    def join():
        clients.append(connect(connection))
        return clients[-1]

    with connection.with_suffix(".log").open("wb") as log:
        process = subprocess.Popen(
            [exe, "--no-color", *arguments, "kernel", "-f", str(connection)],
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


def prompt(exe, cwd, arguments=(), stdin=b"", timeout=60, status=0):
    result = subprocess.run([exe, "--no-color", *arguments], input=stdin,
        capture_output=True, cwd=cwd, timeout=timeout)
    check(result.returncode == status, "the prompt exited with %d, not %d:\n%s"
        % (result.returncode, status, (result.stdout + result.stderr).decode(errors="replace")))
    return result.stdout + result.stderr


def started_prompt(exe, cwd):
    """A prompt that starts its kernel, and the kernel's connection file"""
    process = subprocess.Popen([exe, "--no-color"], cwd=cwd,
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    out = b""
    while True:
        line = process.stdout.readline()
        if not line:
            process.kill()
            process.wait()
            raise AssertionError("the prompt did not tell its connection file:\n%r" % out)
        out += line
        joined = re.search(rb"lfortran --connect (\S+)`", line)
        if joined:
            return process, Path(joined.group(1).decode())


def wait_removed(connection, timeout=30):
    deadline = time.monotonic() + timeout
    while connection.exists() and time.monotonic() < deadline:
        time.sleep(0.05)
    return not connection.exists()


def stopped_listening(port, timeout=30):
    """True once nothing accepts connections on `port`."""
    deadline = time.monotonic() + timeout
    while True:
        try:
            socket.create_connection(("127.0.0.1", port), timeout=1).close()
        except OSError:
            return True
        if time.monotonic() >= deadline:
            return False
        time.sleep(0.1)


def children_of(parent):
    """`(pid, command)` of each direct child of `parent`, on POSIX."""
    if os.name != "posix":
        return []
    out = subprocess.check_output(
        ["ps", "-axww", "-o", "ppid=,pid=,command="], text=True)
    found = []
    for line in out.splitlines():
        parts = line.split(None, 2)
        if len(parts) >= 2 and parts[0] == str(parent):
            found.append((int(parts[1]), parts[2] if len(parts) > 2 else ""))
    return found


def kill_pid(pid):
    try:
        os.kill(pid, signal.SIGKILL)
    except OSError:
        pass


def large_output(exe, cwd):
    """A stream larger than the most a subscriber that does not read can be
    sent reaches the prompt, followed by the cell's idle status, for which
    the prompt waits. The subscriber that does not read holds nothing back."""
    with kernel(exe, cwd) as (_, join, ports):
        client = join()
        stalled = zmtp(ports["iopub_port"], b"SUB")
        stalled.sendall(b"\x00\x01\x01")  # subscribes to every message
        size = 70 * 1024 * 1024
        out = prompt(exe, cwd, ["--connect", ports["path"]],
            b"print '(a)', repeat('x', %d)\r6*7\r\x04" % size)
        check(out.count(b"x") >= size, "the prompt lost the output")
        check(re.search(rb"^42\r?$", out, re.M), "the prompt did not run its next cell")
        check(value(client, "7*8") == "56", "another client did not run its cell")
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
        out = prompt(exe, cwd, ["--connect", ports["path"]], b"6*9\r\x04")
        check(re.search(rb"^54\r?$", out, re.M), "the prompt failed:\n" + out.decode(errors="replace"))
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


def prompt_departure(exe, cwd):
    """The prompt that started the kernel ends it on the way out, including
    while another client is connected, and removes the connection file.
    The kernel also stops with a prompt that is killed or hung up on, which
    cannot end it. A prompt that only joined leaves the kernel alone."""
    process, connection = started_prompt(exe, cwd)
    client = None
    kernel_pids = []
    try:
        heartbeat = json.loads(connection.read_text())["hb_port"]
        if os.name == "posix":
            children = [(pid, command) for pid, command in children_of(process.pid)
                if "kernel" in command.split()]
            check(children, "the prompt started no kernel")
            kernel_pids = [pid for pid, _ in children]
            check(all("--managed" not in command for _, command in children),
                "the prompt passed --managed:\n"
                + "\n".join(command for _, command in children))
        client = connect(connection)
        check(run(client, "integer :: kept = 3")[0]["status"] == "ok", "declaration failed")
        # Leave the client connected. The kernel still ends with the prompt,
        # so do not evaluate through it afterwards.
        process.communicate(b"\x04", timeout=60)
        check(process.returncode == 0, "the prompt failed")
        check(wait_removed(connection), "the prompt left the connection file")
        check(stopped_listening(heartbeat), "the kernel still listens")
        kernel_pids = []
    finally:
        if client:
            client.stop_channels()
        if process.poll() is None:
            kernel_pids.extend(pid for pid, command in children_of(process.pid)
                if "kernel" in command.split())
            process.kill()
            process.wait()
        for pid in kernel_pids:
            kill_pid(pid)

    # A prompt that cannot end the kernel itself, as when it is killed or its
    # terminal closes: the kernel stops with it and removes the connection
    # file, also while another client is connected.
    endings = [("killed", lambda process: process.kill())]
    if os.name == "posix":
        endings.append(("hung up", lambda process: process.send_signal(signal.SIGHUP)))
    for how, end in endings:
        process, connection = started_prompt(exe, cwd)
        client = None
        kernel_pids = children_of(process.pid)
        try:
            heartbeat = json.loads(connection.read_text())["hb_port"]
            client = connect(connection)
            check(run(client, "integer :: kept = 3")[0]["status"] == "ok",
                "declaration failed")
            end(process)
            process.wait(timeout=20)
            check(wait_removed(connection), "the kernel of a prompt %s left the connection file"
                % how)
            check(stopped_listening(heartbeat), "the kernel of a prompt %s still listens" % how)
            kernel_pids = []
        finally:
            if client:
                client.stop_channels()
            if process.poll() is None:
                process.kill()
                process.wait()
            for pid, _ in kernel_pids:
                kill_pid(pid)

    with kernel(exe, cwd) as (_, join, ports):
        client = join()
        check(run(client, "integer :: n = 1")[0]["status"] == "ok", "declaration failed")
        out = prompt(exe, cwd, ["--connect", ports["path"]], b"6*7\r\x04")
        check(re.search(rb"^42\r?$", out, re.M),
            "a joined prompt failed:\n" + out.decode(errors="replace"))
        check(value(client, "n + 1") == "2", "a joined prompt ended the kernel")


def prompt_killed_in_cell(exe, cwd):
    """The kernel ends during a cell that does not return, and the connection
    file is removed, when the prompt that started it gets Ctrl-C or is
    killed."""
    endings = [("killed", lambda process: process.kill(), None)]
    if os.name == "posix":
        endings.insert(0, ("Ctrl-C", lambda process: process.send_signal(signal.SIGINT), 130))
    for how, end, status in endings:
        process, connection = started_prompt(exe, cwd)
        kernel_pids = children_of(process.pid)
        try:
            heartbeat = json.loads(connection.read_text())["hb_port"]
            process.stdin.write(b"do while (.true.); end do\r")
            process.stdin.flush()
            time.sleep(2)
            check(process.poll() is None and connection.exists(), "the cell ended")
            end(process)
            process.wait(timeout=20)
            if status is not None:
                check(process.returncode == status, "%s exited with %s"
                    % (how, process.returncode))
            check(wait_removed(connection), "a prompt %s left the connection file" % how)
            check(stopped_listening(heartbeat), "the kernel of a prompt %s still listens" % how)
            kernel_pids = []
        finally:
            if process.poll() is None:
                process.kill()
                process.wait()
            for pid, _ in kernel_pids:
                kill_pid(pid)


def stop_ends_session(exe, cwd):
    """STOP and ERROR STOP stop the kernel, whose output before them reaches
    its clients. The prompt that started the kernel exits with its exit
    status, as it did when it ran its cells itself; another prompt tells that
    the kernel has stopped."""
    # A cell of only `stop` is the expression of a variable named stop.
    for code, status, shown in [(b"if (.true.) stop", 0, b""), (b"stop 3", 3, b"STOP 3"),
            (b"error stop", 1, b"ERROR STOP")]:
        out = prompt(exe, cwd, stdin=b"print *, 'before'\r" + code + b"\r6*7\r\x04",
            status=status)
        check(b"before" in out and shown in out and not re.search(rb"^42\r?$", out, re.M),
            "%s:\n%s" % (code, out.decode(errors="replace")))
    deadline = time.monotonic() + 30
    while list(cwd.glob("lfortran-*.json")) and time.monotonic() < deadline:
        time.sleep(0.05)
    check(not list(cwd.glob("lfortran-*.json")), "a kernel left its connection file")
    with kernel(exe, cwd) as (process, join, ports):
        join()  # once the kernel listens
        out = prompt(exe, cwd, ["--connect", ports["path"]], b"print *, 'shown'\rif (.true.) stop\r\x04",
            status=1)
        check(b"shown" in out and b"the kernel has stopped" in out, out.decode(errors="replace"))
        check(process.wait(timeout=20) == 0, "STOP ended the kernel with an error")


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


def implicit_typing(exe, cwd):
    """Compiler options of the kernel process reach the cells. A name that
    starts with x is real."""
    with kernel(exe, cwd, arguments=["--implicit-typing"]) as (_, join, _ports):
        client = join()
        check(run(client, "x = 4")[0]["status"] == "ok", "implicit typing rejected x")
        check(value(client, "x") == "4.0", "x was not real")


def xeus_connection_files(exe, cwd):
    """The Xeus kernel serves a connection file whose address is a host name.
    The built-in kernel listens on an IPv4 address only."""
    with kernel(exe, cwd, ip="localhost") as (_, join, _ports):
        check(value(join(), "6*7") == "42", "a cell did not run")


def verbose(exe, cwd):
    """`lfortran -v` shows a cell's AST, ASR, LLVM IR and return type, in the
    kernel the prompt starts and in one it joins."""
    def shows_all(out):
        for label in [b"AST:", b"ASR:", b"LLVM IR:", b"Return type:"]:
            check(label in out, "%s missing:\n%s" % (label, out.decode(errors="replace")))
        check(re.search(rb"^2\r?$", out, re.M), "no value:\n" + out.decode(errors="replace"))
    shows_all(prompt(exe, cwd, ["-v"], b"1+1\r\x04"))
    with kernel(exe, cwd) as (_, join, ports):
        join()  # once the kernel listens
        shows_all(prompt(exe, cwd, ["-v", "--connect", ports["path"]], b"1+1\r\x04"))


CASES = {f.__name__: f for f in [large_output, stalled_connections, read_disconnect,
    late_stdin, prompt_departure, prompt_killed_in_cell, stop_ends_session, stop_on_error,
    implicit_typing, verbose, xeus_connection_files]}
# The built-in kernel rejects a host name in the connection file.
XEUS_ONLY = {"xeus_connection_files"}


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
            if args.builtin and name in XEUS_ONLY:
                continue
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
