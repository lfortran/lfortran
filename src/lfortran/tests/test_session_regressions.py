"""A shared session (`lfortran kernel -f`, `lfortran --connect`) with clients
that misbehave: large output, connections that stall, a client that leaves
during a READ, a prompt that ends the kernel it started, a prompt killed or
hung up on, also during a cell that does not return or a READ, cells queued
behind a failing one, cells that stop the kernel, `lfortran -v`, and a kernel
told that another process is its parent. Each case runs against whichever
kernel is built."""

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
import sys
import time
import threading
import uuid

from test_session import busy, check, parent_id
from jupyter_client import BlockingKernelClient
from jupyter_client.connect import write_connection_file
from test_repl import PtyRepl, Screen, CTRL_D, ENTER


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
def kernel(exe, cwd, ip="127.0.0.1", arguments=(), managed=False):
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
            [exe, "--no-color", *arguments, "kernel", "-f", str(connection)] +
            (["--managed"] if managed else []),
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


def started_prompt(exe, cwd, preexec_fn=None):
    """A prompt that starts its kernel, and the kernel's connection file"""
    process = subprocess.Popen([exe, "--no-color"], cwd=cwd, preexec_fn=preexec_fn,
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


def terminate_with_kernel(process):
    """SIGTERM to the prompt and its kernel at once, as `pkill lfortran`"""
    for pid, _ in children_of(process.pid):
        os.kill(pid, signal.SIGTERM)
    process.terminate()


def large_output(exe, cwd):
    """A stream larger than the most a subscriber that does not read can be
    sent reaches the prompt, followed by the cell's idle status, for which
    the prompt waits. The subscriber that does not read holds nothing back."""
    with kernel(exe, cwd) as (_, join, ports):
        client = join()
        stalled = zmtp(ports["iopub_port"], b"SUB")
        stalled.sendall(b"\x00\x01\x01")  # subscribes to every message
        stopped = threading.Event()
        errors = []

        def drain():
            # Only the raw SUB is deliberately stalled, not this healthy client.
            try:
                while not stopped.is_set():
                    try:
                        client.get_iopub_msg(timeout=0.1)
                    except queue.Empty:
                        pass
            except BaseException as error:
                errors.append(error)

        drainer = threading.Thread(target=drain, daemon=True)
        try:
            size = 70 * 1024 * 1024
            drainer.start()
            try:
                out = prompt(exe, cwd, ["--connect", ports["path"]],
                    b"print '(a)', repeat('x', %d)\r6*7\r\x04" % size)
            finally:
                stopped.set()
                drainer.join(timeout=5)
                check(not drainer.is_alive(), "control subscriber did not stop draining")
                if errors:
                    raise errors[0]
            check(out.count(b"x") >= size, "the prompt lost the output")
            check(re.search(rb"^42\r?$", out, re.M), "the prompt did not run its next cell")
            check(value(client, "7*8") == "56", "another client did not run its cell")
        finally:
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
    # file, also while another client is connected. So does a kernel that
    # ends by itself first, as the built-in one does in a READ whose prompt
    # has gone.
    kill = lambda process: process.kill()
    endings = [("killed", None, None, kill),
        ("killed during a READ", b"read *, kept\r", None, kill)]
    if os.name == "posix":
        endings += [("hung up", None, None,
                lambda process: process.send_signal(signal.SIGHUP)),
            ("terminated with its kernel", None, None, terminate_with_kernel),
            # The kernel inherits what the prompt ignores or blocks.
            ("killed while ignoring SIGTERM", None,
                lambda: signal.signal(signal.SIGTERM, signal.SIG_IGN), kill),
            ("killed while blocking SIGTERM", None,
                lambda: signal.pthread_sigmask(signal.SIG_BLOCK, {signal.SIGTERM}), kill)]
    for how, cell, preexec_fn, end in endings:
        process, connection = started_prompt(exe, cwd, preexec_fn)
        client = None
        kernel_pids = children_of(process.pid)
        try:
            heartbeat = json.loads(connection.read_text())["hb_port"]
            client = connect(connection)
            check(run(client, "integer :: kept = 3")[0]["status"] == "ok",
                "declaration failed")
            if cell:
                process.stdin.write(cell)
                process.stdin.flush()
                time.sleep(2)
                check(process.poll() is None and connection.exists(), "the cell ended")
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
    file is removed, when the prompt that started it is killed."""
    endings = [("killed", lambda process: process.kill(), None)]
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


def kernel_parent(exe, cwd):
    """`lfortran kernel --parent` takes only the process that started the
    kernel. Another running process, or what is not a process ID, is an
    error that leaves the connection file alone, as does a kernel that
    cannot start on the file. A parent that has exited, also one not yet
    reaped, as a prompt killed while it starts its kernel, stops the kernel
    and removes the file."""
    connection = cwd / ("kernel-" + uuid.uuid4().hex + ".json")
    write_connection_file(str(connection), ip="127.0.0.1", key=uuid.uuid4().hex.encode())
    content = connection.read_bytes()
    other = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
    try:
        for parent in ["0", "-5", str(other.pid), str(2**32 + 1)]:
            result = subprocess.run(
                [exe, "--no-color", "kernel", "-f", str(connection), "--parent", parent],
                capture_output=True, cwd=cwd, timeout=60)
            check(result.returncode == 1
                and b"is not the parent of this kernel" in result.stderr,
                "--parent %s exited with %d:\n%s" % (parent, result.returncode,
                    (result.stdout + result.stderr).decode(errors="replace")))
            check(connection.read_bytes() == content,
                "--parent %s changed the connection file" % parent)
    finally:
        other.kill()
        other.wait()
        connection.unlink()
    text = "not a connection file\n"
    connection.write_text(text)
    result = subprocess.run(
        [exe, "--no-color", "kernel", "-f", str(connection), "--parent", str(os.getpid())],
        capture_output=True, cwd=cwd, timeout=60)
    check(result.returncode != 0, "the kernel started on a file that is not a connection file")
    check(connection.exists() and connection.read_text() == text,
        "a kernel that did not start removed the file")
    connection.unlink()

    write_connection_file(str(connection), ip="127.0.0.1", key=uuid.uuid4().hex.encode())
    starter = subprocess.Popen([sys.executable, "-c",
        "import os, subprocess, sys\n"
        "kernel = subprocess.Popen([sys.argv[1], '--no-color', 'kernel', '-f', sys.argv[2],"
        " '--parent', str(os.getpid())], stdin=subprocess.DEVNULL,"
        " stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)\n"
        "print(kernel.pid, flush=True)\n"
        "os._exit(0)\n", exe, str(connection)], stdout=subprocess.PIPE)
    kernel_pid = int(starter.stdout.readline())
    try:
        # Not reaped meanwhile
        check(wait_removed(connection, timeout=20),
            "the kernel of a parent that has exited left the connection file")
    finally:
        starter.wait()
        starter.stdout.close()
        if os.name == "posix":
            kill_pid(kernel_pid)
        else:
            subprocess.run(["taskkill", "/F", "/PID", str(kernel_pid)], capture_output=True)


def stop_ends_session(exe, cwd):
    """STOP and ERROR STOP stop the kernel. Output from a preceding cell
    reaches its clients. The prompt that started the kernel exits with its
    exit status; another prompt tells that the kernel has stopped."""
    # Xeus does not guarantee delivery of output queued during exit.
    # test_kernel_evaluation checks STOP diagnostics on the built-in kernel,
    # which flushes its subscribers before exiting, also in Xeus builds.
    # A cell of only `stop` is the expression of a variable named stop.
    for code, status in [(b"if (.true.) stop", 0), (b"stop 3", 3), (b"error stop", 1)]:
        out = prompt(exe, cwd, stdin=b"print *, 'before'\r" + code + b"\r6*7\r\x04",
            status=status)
        check(b"before" in out and not re.search(rb"^42\r?$", out, re.M),
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


@contextlib.contextmanager
def editing_prompt(exe, cwd, connection=None, expected_status=0):
    try:
        import fcntl
        import pty
        import struct
        import termios
    except ImportError:
        print("skipped: no pseudo-terminals")
        yield None
        return
    screen = Screen()
    repl = PtyRepl(pty, termios, exe, cwd, timeout=30,
        args=["--connect", str(connection)] if connection else [], screen=screen)
    fcntl.ioctl(repl.fd, termios.TIOCSWINSZ,
        struct.pack("HHHH", screen.rows, screen.cols, 0, 0))
    reaped = False
    try:
        check(repl.wait(lambda: ">>> " in screen.text()
            and not termios.tcgetattr(repl.fd)[3] & termios.ICANON,
            "an editing prompt"), "prompt ended during startup")
        yield repl
        if expected_status == 0:
            os.write(repl.fd, CTRL_D)
        repl.wait(lambda: False, "prompt exit")
        _, status = os.waitpid(repl.pid, 0)
        reaped = True
        check(os.waitstatus_to_exitcode(status) == expected_status, repl.text())
    finally:
        if not reaped:
            os.kill(repl.pid, signal.SIGKILL)
            os.waitpid(repl.pid, 0)
        os.close(repl.fd)

def test_interrupt_prompt(exe, cwd):
    """Ctrl-C keeps owning/attached prompts and their state alive, also in
    READ and commands; a control interrupt can end READ without a key."""
    def exercise(connection=None):
        client = None
        owned = connection is None
        try:
            with editing_prompt(exe, cwd, connection) as repl:
                if repl is None:
                    return
                if owned:
                    match = re.search(r"lfortran --connect (\S+)`", repl.text())
                    check(match is not None, repl.text())
                    connection = Path(match.group(1))
                client = connect(connection)
                rendered_prompt = 0

                def rendered(screen):
                    nonlocal rendered_prompt
                    if screen.lines[screen.row].startswith(">>> ") and screen.col == 4:
                        rendered_prompt = repl.prompts

                repl.screen.on_render = rendered

                def ready():
                    return (repl.screen.lines[repl.screen.row].startswith(">>> ")
                        and repl.screen.col == 4
                        and repl.screen.cursor_visible
                        and not repl.termios.tcgetattr(repl.fd)[3] & repl.termios.ICANON)

                def submit(code):
                    check(repl.wait(ready, "editing prompt"), repl.text())
                    queries = repl.prompts
                    before = len(repl.out)
                    os.write(repl.fd, code.encode() + ENTER)
                    # Drain the editor's final redraw before Ctrl-C can flush
                    # the PTY output queue, possibly halfway through an escape.
                    check(repl.wait(lambda: b"\r\n\x1b[0m\x1b[?25h"
                        in repl.out[before:], "submitted cell"), repl.text())
                    return queries

                def finished(queries):
                    # A DSR query starts a prompt; it is not a completed redraw.
                    check(repl.wait(lambda: rendered_prompt > queries and ready(),
                        "prompt after cell"), repl.text())

                finished(submit("integer :: kept_interrupt = 41"))
                for code, external in [
                        ("do while (.true.); end do", False),
                        ("call execute_command_line('sleep 4'); print *, 'command-done'", False),
                        ("call execute_command_line('sleep 4'); print *, 'command-done'", False),
                        ("print '(a)', 'waiting-one'; read *, kept_interrupt", False),
                        ("print '(a)', 'waiting-two'; read *, kept_interrupt", True)]:
                    before = len(repl.out)
                    queries = submit(code)
                    if "waiting-" in code:
                        marker = b"waiting-two" if external else b"waiting-one"
                        check(repl.wait(lambda: marker + b"\r\n" in repl.out[before:],
                            "READ output"), repl.text())
                    else:
                        check(repl.wait(lambda: repl.termios.tcgetattr(repl.fd)[3]
                            & repl.termios.ICANON, "silent cell in line mode"), repl.text())
                        def executing():
                            while client.iopub_channel.msg_ready():
                                message = client.get_iopub_msg(timeout=0)
                                if (message["msg_type"] == "execute_input"
                                        and message["content"]["code"].strip() == code):
                                    return True
                            return False
                        # Keep behaving as a terminal while observing the
                        # independent subscriber, including cursor queries.
                        check(repl.wait(executing, "cell execute_input"), repl.text())
                    children = []
                    if "sleep 4" in code:
                        # The observed execution belongs to this connection;
                        # capture only its kernel's own direct command children.
                        kernel_pids = [pid for pid, command in children_of(repl.pid)
                            if "kernel" in command]
                        if not owned:
                            kernel_pids = [process.pid]
                        def command_started():
                            children[:] = [child for pid in kernel_pids
                                for child, _ in children_of(pid)]
                            return bool(children)
                        check(repl.wait(command_started, "owned command child"), repl.text())
                    interrupted = time.monotonic()
                    if external:
                        request = client.session.msg("interrupt_request")
                        client.control_channel.send(request)
                        reply = client.get_control_msg(timeout=5)
                        check(parent_id(reply) == request["header"]["msg_id"], reply)
                    else:
                        os.write(repl.fd, b"\x03")
                    finished(queries)
                    if "sleep 4" in code:
                        check(time.monotonic() - interrupted < 2,
                            "Ctrl-C waited for command completion")
                        check(b"command-done\r\n" not in repl.out[before:],
                            "interrupted command executed its following statement")
                        for pid in children:
                            try:
                                os.kill(pid, 0)
                            except ProcessLookupError:
                                continue
                            raise AssertionError("owned command child was not reaped: %d" % pid)
                    check(b"cell execution interrupted" in repl.out[before:], repl.text())
                    check(connection.exists(), "Ctrl-C removed the connection file")
                    finished(submit("kept_interrupt+1"))
                    check(re.search(r"^42\r?$", repl.text(), re.M), repl.text())
                queries = submit("read *, kept_interrupt")
                check(repl.wait(lambda: repl.termios.tcgetattr(repl.fd)[3]
                    & repl.termios.ICANON, "READ in line mode"), repl.text())
                os.write(repl.fd, b"58\n")
                finished(queries)
                check(value(client, "kept_interrupt") == "58", "READ lost its input")
            if owned:
                check(wait_removed(connection), "owning prompt left its kernel file")
            else:
                check(value(client, "kept_interrupt") == "58",
                    "attached prompt ended the session")
        finally:
            if client:
                client.stop_channels()

    exercise()
    for managed in (False, True):
        with kernel(exe, cwd, managed=managed) as (process, join, ports):
            join()
            exercise(Path(ports["path"]))


def test_idle_updates(exe, cwd):
    """No keys are sent between prompt readiness and other-client output."""
    with kernel(exe, cwd) as (_, join, ports):
        client = join()
        with editing_prompt(exe, cwd, ports["path"]) as repl:
            if repl is None:
                return
            reply, messages = run(client, "print *, 'IDLE_UPDATE_16303'")
            check(reply["status"] == "ok", reply)
            check(any(m["msg_type"] == "stream" for m in messages), messages)
            check(repl.wait(lambda: "[other client] print *, 'IDLE_UPDATE_16303'"
                in repl.screen.text()
                and re.search(r"^\s*IDLE_UPDATE_16303\s*$",
                    repl.screen.text(), re.M)
                and repl.screen.lines[repl.screen.row].startswith(">>> "),
                "idle code, stream and restored prompt"),
                repl.screen.text())
            check(repl.screen.lines[repl.screen.row].startswith(">>> "),
                "output did not restore the idle prompt")


def test_edited_updates(exe, cwd):
    with kernel(exe, cwd) as (_, join, ports):
        client = join()
        with editing_prompt(exe, cwd, ports["path"]) as repl:
            if repl is None:
                return
            screen = repl.screen
            cell = ("block\x0einteger :: local_probe = 4\x0e"
                "print *, local_probe + 1\x0eend block")
            os.write(repl.fd, cell.encode() + b"\x1b[A\x1b[A\x1b[F\x1b[D")
            check(repl.wait(lambda: "2,26" in screen.text(),
                "a multiline cell with a middle-of-line cursor"), screen.text())
            scrolled_before = len(screen.scrolled)
            # A key arriving just before the terminal's DSR response must
            # remain input, not be discarded by the output refresh.
            screen.before_report = b"4"
            reply, _ = run(client, "print '(a)', 'SCROLL_' // repeat('x', 60)\n"
                "print *, 'EDIT_UPDATE_16303'")
            check(reply["status"] == "ok", reply)
            check(repl.wait(lambda: "integer :: local_probe = 44" in screen.text()
                and "EDIT_UPDATE_16303" in screen.text()
                and "2,27" in screen.text()
                and screen.lines[screen.row].startswith("... integer")
                and screen.col == 30, "output and preserved edited cell"),
                screen.text())
            check(len(screen.scrolled) > scrolled_before,
                "the multiline redraw did not exercise bottom scrolling")
            check(screen.lines[screen.row + 2].startswith("... end block"),
                "bottom scrolling lost part of the editor region")
            # This evaluates 44 + 1 only if the logical cursor was preserved.
            os.write(repl.fd, ENTER)
            check(repl.wait(lambda: re.search(r"^\s*45\s*$", screen.text(), re.M)
                and screen.lines[screen.row].startswith(">>> "),
                "the preserved cell's result"), screen.text())
            os.write(repl.fd, b"\x1b[A")
            check(repl.wait(lambda: screen.lines[screen.row].startswith("... end block"),
                "the unchanged multiline history"), screen.text())
            os.write(repl.fd, ENTER)
            check(repl.wait(lambda: len(re.findall(r"^\s*45\s*$",
                screen.text(), re.M)) == 2
                and screen.lines[screen.row].startswith(">>> "),
                "the recalled cell's result"), screen.text())
            # Saved keys may span submission, a READ and the next prompt.
            # None may be lost when raw-mode Terminal instances are replaced.
            screen.before_report = (b"block; integer :: n; read(*,*) n; "
                b"print *, n+1000; end block\r41\r6*7\r")
            reply, _ = run(client, "print *, 'BUFFERED_INPUT_16303'")
            check(reply["status"] == "ok", reply)
            check(repl.wait(lambda: re.search(r"^\s*1041\s*$",
                screen.text(), re.M) and re.search(r"^\s*42\s*$",
                screen.text(), re.M)
                and screen.lines[screen.row].startswith(">>> "),
                "saved READ input and the following cell"), screen.text())


@contextlib.contextmanager
def display_kernel(cwd):
    """A signed Jupyter publisher for precisely fragmented output, independent
    of either kernel's buffering. Bind the client's control endpoint too, even
    though display-only tests send no interrupts. All sockets belong to the
    serving thread."""
    import zmq
    from jupyter_client.session import Session
    commands, answers = queue.Queue(), queue.Queue()
    connection = cwd / ("display-" + uuid.uuid4().hex + ".json")

    def serve():
        try:
            with zmq.Context() as context:
                pub, shell, stdin, control = [context.socket(t) for t in
                    (zmq.PUB, zmq.ROUTER, zmq.ROUTER, zmq.ROUTER)]
                sockets = [pub, shell, stdin, control]
                for s in sockets:
                    s.linger = 0
                ports = [s.bind_to_random_port("tcp://127.0.0.1") for s in sockets]
                session = Session(key=b"editor-test")
                write_connection_file(str(connection), ip="127.0.0.1",
                    key=session.key, iopub_port=ports[0], shell_port=ports[1],
                    stdin_port=ports[2], control_port=ports[3])
                answers.put(connection)
                try:
                    while True:
                        check(not control.poll(0),
                            "display-only requests must not use the control channel")
                        if shell.poll(10):
                            identities, message = session.recv(shell)
                            check(message["msg_type"] == "kernel_info_request",
                                ("unexpected display-only shell request", message))
                            session.send(shell, "kernel_info_reply",
                                content={"status": "ok"}, parent=message,
                                ident=identities)
                            session.send(pub, "status",
                                content={"execution_state": "idle"}, parent=message)
                        try:
                            command = commands.get_nowait()
                        except queue.Empty:
                            continue
                        if command is None:
                            break
                        for type_, content in command:
                            session.send(pub, type_, content=content)
                        answers.put(True)
                finally:
                    for s in sockets:
                        s.close()
        except BaseException as error:
            answers.put(error)

    thread = threading.Thread(target=serve)
    thread.start()

    def answer():
        result = answers.get(timeout=20)
        if isinstance(result, BaseException):
            raise result
        return result

    def publish(messages):
        commands.put(messages)
        answer()

    try:
        yield answer(), publish
    finally:
        commands.put(None)
        thread.join(timeout=20)
        check(not thread.is_alive(), "display kernel did not stop")


def test_fragmented_updates(exe, cwd):
    with display_kernel(cwd) as (connection, publish):
        with editing_prompt(exe, cwd, connection) as repl:
            if repl is None:
                return
            screen = repl.screen
            publish([("stream", {"name": "stdout", "text": "FRAG_A"})])
            check(repl.wait(lambda: "FRAG_A" in screen.text()
                and screen.lines[screen.row].startswith(">>> "), "first fragment"),
                screen.text())
            # Force each fragment to be displayed in a separate editor tick.
            publish([("stream", {"name": "stderr", "text": "_B"})])
            check(repl.wait(lambda: "FRAG_A_B" in screen.text()
                and screen.lines[screen.row].startswith(">>> "), "joined fragments"),
                screen.text())
            publish([("stream", {"name": "stdout", "text": "_C\n"})])
            check(repl.wait(lambda: "FRAG_A_B_C" in screen.text()
                and screen.lines[screen.row].startswith(">>> "), "final fragment"),
                screen.text())
            for width in [screen.cols - 1, screen.cols, screen.cols + 1, 2 * screen.cols]:
                prefix = "EDGE_%d_" % width
                prefix += "x" * (width - len(prefix))
                expected = "\n".join(prefix[i:i + screen.cols]
                    for i in range(0, width, screen.cols))
                publish([("stream", {"name": "stdout", "text": prefix})])
                check(repl.wait(lambda: expected in screen.text()
                    and screen.lines[screen.row].startswith(">>> "),
                    "a fragment at the right margin"), screen.text())
                for fragment in ["\x1b", "[", "0", "m"]:
                    reports = repl.prompts
                    publish([("stream", {"name": "stdout", "text": fragment})])
                    check(repl.wait(lambda: repl.prompts > reports
                        and screen.lines[screen.row].startswith(">>> "),
                        "split zero-width styling at the margin"), screen.text())
                publish([("stream", {"name": "stdout", "text": "B\n"})])
                expected = "\n".join((prefix + "B")[i:i + screen.cols]
                    for i in range(0, width + 1, screen.cols))
                check(repl.wait(lambda: expected in screen.text()
                    and screen.lines[screen.row].startswith(">>> ")
                    and screen.lines[screen.row - 1].rstrip() == expected.split("\n")[-1],
                    "a fragment continuing at the right margin"), screen.text())
            prefix = "LF_EDGE_" + "x" * (screen.cols - len("LF_EDGE_"))
            publish([("stream", {"name": "stdout", "text": prefix})])
            check(repl.wait(lambda: prefix in screen.text()
                and screen.lines[screen.row].startswith(">>> "),
                "a full-width fragment"), screen.text())
            reports = repl.prompts
            publish([("stream", {"name": "stdout", "text": "\n"})])
            check(repl.wait(lambda: repl.prompts > reports
                and screen.lines[screen.row].startswith(">>> "),
                "a separate newline fragment"), screen.text())
            check(screen.lines[screen.row - 1] == prefix,
                "a newline after a full-width fragment added a blank line")
            original = "OLD_" + "x" * (screen.cols - 5) + "Z"
            replacement = "NEW_" + "y" * (screen.cols - 5)
            for fragments in [[replacement], [replacement[:-1], replacement[-1]],
                    ["\x1b", "[", "0", "m", replacement[:-1], replacement[-1]]]:
                publish([("stream", {"name": "stdout", "text": original + "\r"})])
                check(repl.wait(lambda: screen.lines[screen.row - 1] == original
                    and screen.lines[screen.row].startswith(">>> "),
                    "an occupied final column before CR replacement"), screen.text())
                for fragment in fragments:
                    reports = repl.prompts
                    publish([("stream", {"name": "stdout", "text": fragment})])
                    check(repl.wait(lambda: repl.prompts > reports
                        and screen.lines[screen.row].startswith(">>> "),
                        "a separately rendered replacement fragment"), screen.text())
                check(repl.wait(lambda:
                    screen.lines[screen.row - 1] == replacement + "Z"
                    and screen.lines[screen.row].startswith(">>> ")
                    and screen.col == 4, "the untouched final column"),
                    "CR replacement erased transcript data:\n" + screen.text())
                publish([("stream", {"name": "stdout", "text": "Q"})])
                check(repl.wait(lambda: screen.lines[screen.row - 1] == replacement + "Q"
                    and screen.lines[screen.row].startswith(">>> "),
                    "replacement of the final column"), screen.text())
                reports = repl.prompts
                publish([("stream", {"name": "stdout", "text": "\n"})])
                check(repl.wait(lambda: repl.prompts > reports
                    and screen.lines[screen.row - 1] == replacement + "Q"
                    and screen.lines[screen.row].startswith(">>> "),
                    "newline immediately after the final column"), screen.text())
            for separate in [False, True]:
                marker = "CR_SGR_%s_111" % ("SEPARATE" if separate else "COMBINED")
                publish([("stream", {"name": "stdout",
                    "text": marker + "\r" + ("" if separate else "\x1b[0m")})])
                check(repl.wait(lambda: marker in screen.text()
                    and screen.lines[screen.row].startswith(">>> "),
                    "a carriage return with zero-width styling"), screen.text())
                if separate:
                    reports = repl.prompts
                    publish([("stream", {"name": "stdout", "text": "\x1b[0m"})])
                    check(repl.wait(lambda: repl.prompts > reports
                        and screen.lines[screen.row].startswith(">>> "),
                        "a separate zero-width styling fragment"), screen.text())
                check(screen.lines[screen.row - 1].rstrip() == marker
                    and screen.col == 4,
                    "zero-width styling erased the carriage-return transcript:\n"
                    + screen.text())
                replacement = marker.replace("111", "222")
                publish([("stream", {"name": "stdout",
                    "text": replacement + "\n\x1b[0m"})])
                check(repl.wait(lambda: replacement in screen.text()
                    and screen.lines[screen.row].startswith(">>> "),
                    "styled progress replacement and newline"), screen.text())
                check(screen.lines[screen.row - 1].rstrip() == replacement
                    and marker not in screen.text(),
                    "styling changed carriage-return replacement or newline placement")
            publish([("stream", {"name": "stdout", "text": "PROGRESS_111\r"})])
            check(repl.wait(lambda: "PROGRESS_111" in screen.text()
                and screen.lines[screen.row].startswith(">>> "),
                "a carriage-return fragment"), screen.text())
            publish([("stream", {"name": "stdout", "text": "PROGRESS_222\n"})])
            check(repl.wait(lambda: "PROGRESS_222" in screen.text()
                and screen.lines[screen.row].startswith(">>> "),
                "a replacement progress fragment"), screen.text())
            # More than one polling batch, with keys pending throughout.
            progress = set()

            def rendered(screen):
                line = screen.lines[screen.row]
                if line.startswith(">>> x"):
                    if "END_OF_BURST" not in screen.text():
                        progress.add("keyboard")
                    elif not line.startswith(">>> " + "x" * 60):
                        progress.add("client")

            screen.on_render = rendered
            publish([("stream", {"name": "stdout", "text": "tick\n"})] * 200
                + [("stream", {"name": "stdout", "text": "END_OF_BURST\n"})])
            os.write(repl.fd, b"x" * 60)
            check(repl.wait(lambda: screen.lines[screen.row].startswith(">>> " + "x" * 60),
                "keyboard progress during sustained output"), screen.text())
            check("keyboard" in progress,
                "output drained without yielding to a pending key")
            check("client" in progress,
                "continuous typing starved client servicing")
            screen.on_render = None
            publish([("execute_result", {"data": {"text/plain": "RESULT_16303"}}),
                ("display_data", {"data": {"text/plain": "DISPLAY_16303"}}),
                ("display_data", {"data": {"image/png": "test-only"}}),
                ("error", {"traceback": ["DIAGNOSTIC_16303"]})])
            check(repl.wait(lambda: all(s in screen.text() for s in
                ["RESULT_16303", "DISPLAY_16303", "[image/png output]",
                    "DIAGNOSTIC_16303"])
                and screen.lines[screen.row].startswith(">>> " + "x" * 60),
                "results, displays and diagnostics"), screen.text())
            os.write(repl.fd, b"\x15")  # clear the unsubmitted input
            check(repl.wait(lambda: screen.col == 4, "cleared input"), screen.text())


def test_idle_disconnect(exe, cwd):
    with kernel(exe, cwd) as (process, join, ports):
        join()
        with editing_prompt(exe, cwd, ports["path"], expected_status=1) as repl:
            if repl is None:
                return
            process.terminate()
            process.wait(timeout=20)
            check(repl.wait(lambda: "error: the kernel has stopped" in repl.text(),
                "an explicit idle disconnection error"), repl.text())


def test_unicode_updates(exe, cwd):
    cases = [
        ("界" * 40 + "a" * 40 + "Z\n", ["界 " * 40, "a" * 40 + "Z"]),
        ("e\u0301" * 40 + "a" * 40 + "Z\n", ["e\u0301" * 40 + "a" * 40, "Z"]),
        ("x" * 79 + "界Z\n", ["x" * 79, "界 Z"]),
        ("x" * 79 + "e\u0301Z\n", ["x" * 79 + "e\u0301", "Z"]),
        ("界" * 40 + "\u0301Z\n", ["界 " * 39 + "界\u0301", "Z"]),
    ]
    with display_kernel(cwd) as (connection, publish):
        with editing_prompt(exe, cwd, connection) as repl:
            if repl is None:
                return
            screen = repl.screen
            for payload, expected in cases:
                control = Screen()
                control.feed(payload.replace("\n", "\r\n").encode())
                check([line.rstrip() for line in control.lines[:len(expected)]]
                    == [line.rstrip() for line in expected],
                    "the independent Unicode screen control is wrong")
                # Separate every scalar to cross redraw boundaries, including
                # a combining mark following a filled final cell.
                for fragments in [[payload], list(payload)]:
                    for fragment in fragments:
                        reports = repl.prompts
                        publish([("stream", {"name": "stdout", "text": fragment})])
                        check(repl.wait(lambda: repl.prompts > reports
                            and screen.lines[screen.row].startswith(">>> "),
                            "a Unicode fragment and restored editor"), screen.text())
                    actual = screen.lines[screen.row - len(expected):screen.row]
                    check([line.rstrip() for line in actual]
                        == [line.rstrip() for line in expected],
                        "Unicode changed transcript rows:\n" + repr(actual))
                    check(screen.col == 4, "Unicode output moved the editor cursor")


def test_output_error_cleanup(exe, cwd):
    cases = [
        (["\x1b[" + "1" * 65535], "terminal output sequence is too long", None),
        (["\x1b[", "1" * 65535], "terminal output sequence is too long", None),
        (["\x1b[" + "1" * 65535 + "m"], "terminal output sequence is too long", None),
        (["\x1b[2J"], "unsupported terminal output escape sequence", None),
        (["\x0f"], "unsupported terminal output control character", None),
        (["\u0085"], "unsupported terminal output character", None),
        (["x" * 80 + "\u0301" * 32769], "terminal output sequence is too long", None),
        (["\x1b["], "incomplete terminal output sequence", CTRL_D),
        (["\x1b["], "incomplete terminal output sequence", b"1\r"),
    ]
    for fragments, diagnostic, keys in cases:
        with display_kernel(cwd) as (connection, publish):
            with editing_prompt(exe, cwd, connection, expected_status=1) as repl:
                if repl is None:
                    return
                check(repl.screen.cursor_visible, "ready cursor is hidden")
                for i, fragment in enumerate(fragments):
                    reports = repl.prompts
                    publish([("stream", {"name": "stdout", "text": fragment})])
                    if i < len(fragments) - 1 or keys:
                        check(repl.wait(lambda: repl.prompts > reports
                            and repl.screen.lines[repl.screen.row].startswith(">>> "),
                            "a retained incomplete suffix"), repl.text())
                if keys:
                    os.write(repl.fd, keys)
                repl.wait(lambda: False, "rejected output through EOF")
                check("error: " + diagnostic in repl.text(),
                    "no explicit output diagnostic:\n" + repl.text())
                lflag = repl.termios.tcgetattr(repl.fd)[3]
                check(lflag & (repl.termios.ICANON | repl.termios.ECHO)
                    == repl.termios.ICANON | repl.termios.ECHO,
                    "output rejection left the terminal in raw mode")
                check(repl.screen.cursor_visible,
                    "output rejection left the terminal cursor hidden")


def test_utf8_event_fragments(exe, cwd):
    with editing_prompt(exe, cwd, cwd / "unused") as repl:
        if repl is None:
            return
        screen = repl.screen
        expected = ["界 " * 40, "a" * 40 + "Z", "x" * 79 + "e\u0301"]
        check(repl.wait(lambda: screen.row >= 3
            and [line.rstrip() for line in screen.lines[screen.row - 3:screen.row]]
                == [line.rstrip() for line in expected]
            and screen.lines[screen.row].startswith(">>> ")
            and screen.col == 4, "bytewise UTF-8, SGR and margin combining"),
            screen.text())


CASES = {f.__name__: f for f in [test_idle_updates, test_edited_updates,
    test_fragmented_updates, test_unicode_updates, test_idle_disconnect,
    test_output_error_cleanup,
    large_output, stalled_connections, read_disconnect,
    late_stdin, prompt_departure, prompt_killed_in_cell, test_interrupt_prompt,
    kernel_parent, stop_ends_session,
    stop_on_error, implicit_typing, verbose, xeus_connection_files]}
# The built-in kernel rejects a host name in the connection file.
XEUS_ONLY = {"xeus_connection_files"}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--case", choices=CASES, action="append",
        help="run only this case (repeatable)")
    parser.add_argument("--builtin", action="store_true",
        help="the kernel is the built-in one, not the Xeus one")
    parser.add_argument("--prompt-events-fixture",
        help="also check bytewise output using the native editor fixture")
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
        if args.prompt_events_fixture:
            try:
                test_utf8_event_fragments(
                    str(Path(args.prompt_events_fixture).resolve()), cwd)
                print("test_utf8_event_fragments: passed", flush=True)
            except Exception as error:
                print("test_utf8_event_fragments: FAILED: %s" % error, flush=True)
                failed.append("test_utf8_event_fragments")
    finally:
        shutil.rmtree(cwd, ignore_errors=True)
    if failed:
        raise SystemExit("failed: " + ", ".join(failed))


if __name__ == "__main__":
    main()
