"""A shared session (`lfortran kernel -f`, `lfortran --connect`) with clients
that misbehave: large output, connections that stall, a client that leaves
during a READ, before the others or during a cell, a cell that ends the
kernel or starts programs, cells queued behind a failing one, and
`lfortran -v`."""

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

from test_session import check, parent_id
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
def kernel(exe, cwd):
    """A kernel started on a connection file, as a Jupyter frontend does, a
    function that connects a client to it, and the connection file's content
    with its "path"."""
    connection = cwd / ("kernel-" + uuid.uuid4().hex + ".json")
    write_connection_file(str(connection), key=uuid.uuid4().hex.encode())
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


def prompt(exe, cwd, arguments=(), stdin=b"", timeout=60):
    result = subprocess.run([exe, "--no-color", *arguments], input=stdin,
        capture_output=True, cwd=cwd, timeout=timeout)
    check(result.returncode == 0, result.stderr.decode(errors="replace"))
    return result.stdout


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
        msg_id = client.execute("call execute_command_line('sleep 12')")
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
    """A READ whose client disconnects fails with an I/O error, and the
    other clients go on."""
    with kernel(exe, cwd) as (_, join, _ports):
        a, b = join(), join()
        check(run(a, "integer :: x = 1, ios = 0")[0]["status"] == "ok", "declaration failed")
        a.execute("read(*,*,iostat=ios) x", allow_stdin=True)
        check(a.get_stdin_msg(timeout=20)["msg_type"] == "input_request", "no READ")
        a.stop_channels()
        check(int(value(b, "ios")) > 0, "the READ did not fail")
        check(value(b, "x") == "1", "the READ set x")


def read_disconnect_fatal(exe, cwd):
    """A READ without iostat= whose client disconnects fails with an I/O
    error, which ends the kernel, as it ends a Fortran program, and with it
    the session of every client."""
    with kernel(exe, cwd) as (process, join, _ports):
        a, b = join(), join()
        check(run(a, "integer :: x = 1")[0]["status"] == "ok", "declaration failed")
        a.execute("read(*,*) x", allow_stdin=True)
        check(a.get_stdin_msg(timeout=20)["msg_type"] == "input_request", "no READ")
        a.stop_channels()
        try:
            process.wait(timeout=20)
        except subprocess.TimeoutExpired:
            raise AssertionError("the kernel went on after the READ failed")
        check(process.returncode != 0, "the kernel ended without an error")
        msg_id = b.execute("x")
        try:
            reply_to(b, msg_id, timeout=3)
        except AssertionError:
            pass
        else:
            raise AssertionError("another client ran a cell after the kernel ended")


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
    """The kernel a prompt starts goes on while others use it, whether the
    prompt exits or is killed, and stops after its last client leaves."""
    for kill in [False, True]:
        process, connection = started_prompt(exe, cwd)
        client = None
        try:
            client = connect(connection)
            check(run(client, "integer :: kept = 3")[0]["status"] == "ok", "declaration failed")
            if kill:
                process.kill()
                process.wait()
            else:
                process.communicate(b"\x04", timeout=60)
                check(process.returncode == 0, "the prompt failed")
            check(value(client, "kept + 1") == "4", "the kernel stopped with the prompt")
            client.stop_channels()
            client = None
            deadline = time.monotonic() + 30
            while connection.exists() and time.monotonic() < deadline:
                time.sleep(0.05)
            check(not connection.exists(), "the kernel did not stop after its last client")
        finally:
            if client:
                client.stop_channels()
            if process.poll() is None:
                process.kill()
                process.wait()


def started_prompt(exe, cwd):
    """A prompt started on a pipe, and the connection file of its kernel"""
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


def alive(pid):
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    # A zombie that its new parent has not reaped yet has ended.
    stat = Path("/proc/%d/stat" % pid)
    try:
        return stat.read_text().rsplit(")", 1)[1].split()[0] != "Z"
    except (OSError, IndexError):
        return True


def prompt_interrupted(exe, cwd):
    """The kernel a prompt starts stops when the prompt ends during a cell
    that never ends, by Ctrl-C (SIGINT) or otherwise, and no other client
    is connected; so does a program that the cell waits for."""
    spin = (b"integer :: spin = 0\r"
        b"do while (spin >= 0); spin = mod(spin + 1, 1000); end do\r")
    pid = cwd / ("program-" + uuid.uuid4().hex)
    command = b"call execute_command_line('echo $$ > %s; exec sleep 60')\r" % str(pid).encode()
    for cell, stop in [(spin, lambda p: p.send_signal(signal.SIGINT)),
            (spin, lambda p: p.kill()), (command, lambda p: p.send_signal(signal.SIGINT))]:
        process, connection = started_prompt(exe, cwd)
        try:
            ports = json.loads(connection.read_text())
            process.stdin.write(cell)
            process.stdin.flush()
            deadline = time.monotonic() + 20
            while cell == command and not pid.exists() and time.monotonic() < deadline:
                time.sleep(0.05)
            time.sleep(2)
            check(connection.exists(), "the kernel stopped before the prompt")
            stop(process)
            process.wait(timeout=20)
            deadline = time.monotonic() + 30
            while connection.exists() and time.monotonic() < deadline:
                time.sleep(0.05)
            check(not connection.exists(), "the kernel went on without a client")
            # Its sockets close with it.
            deadline = time.monotonic() + 10
            while True:
                try:
                    socket.create_connection(("127.0.0.1", ports["shell_port"]),
                        timeout=1).close()
                except OSError:
                    break
                check(time.monotonic() < deadline, "the kernel still listens")
                time.sleep(0.1)
            if cell == command:
                program = int(pid.read_text())
                deadline = time.monotonic() + 10
                while alive(program) and time.monotonic() < deadline:
                    time.sleep(0.05)
                check(not alive(program), "the program the cell started went on")
        finally:
            if process.poll() is None:
                process.kill()
                process.wait()
            if pid.exists():
                program = int(pid.read_text())
                if alive(program):
                    os.kill(program, signal.SIGKILL)
                pid.unlink()


def many_requests(client, count):
    """Sends `count` kernel_info_requests, a batch at a time, so that the
    client reads the replies, which the kernel does not let pile up."""
    for _ in range(0, count, 1000):
        for _ in range(1000):
            msg_id = client.kernel_info()
        reply_to(client, msg_id, timeout=60)


def idle_prompt(exe, cwd):
    """A prompt that waits for keys, and so does not read, while another
    client runs more requests than the statuses of which a connection may
    leave unread, still runs its next cell: a subscriber loses the oldest
    messages rather than its connection."""
    process, connection = started_prompt(exe, cwd)
    client = None
    try:
        client = connect(connection)
        many_requests(client, 25000)
        out, _ = process.communicate(b"6*7\r\x04", timeout=60)
        check(process.returncode == 0 and re.search(rb"^42\r?$", out, re.M),
            "the prompt lost its kernel:\n" + out.decode(errors="replace"))
    finally:
        if client:
            client.stop_channels()
        if process.poll() is None:
            process.kill()
            process.wait()


def slow_prompt(exe, cwd):
    """A prompt that does not keep up with its own cell's output while
    another client runs many requests gets all of that output and ends the
    cell, though the kernel may drop the cell's idle status for it."""
    process, connection = started_prompt(exe, cwd)
    client = None
    try:
        client = connect(connection)
        size = 20 * 1024 * 1024
        # The prompt's output is not read yet, so it stops writing it.
        process.stdin.write(b"print '(a)', repeat('x', %d)\r" % size)
        process.stdin.flush()
        time.sleep(2)
        many_requests(client, 40000)
        out, _ = process.communicate(b"6*7\r\x04", timeout=60)
        check(out.count(b"x") >= size, "the prompt lost the output")
        check(process.returncode == 0 and re.search(rb"^42\r?$", out, re.M),
            "the prompt did not end the cell:\n" + out[-500:].decode(errors="replace"))
    except subprocess.TimeoutExpired:
        raise AssertionError("the prompt waited for ever for the end of its cell")
    finally:
        if client:
            client.stop_channels()
        if process.poll() is None:
            process.kill()
            process.wait()


def prompt_stop(exe, cwd):
    """A cell that ends the kernel's process with STOP still has its output
    shown, the kernel removes the connection file it wrote, and the prompt
    tells at once that the kernel has gone, also while a program that a cell
    started in the background runs: it does not inherit the kernel's
    sockets."""
    process, connection = started_prompt(exe, cwd)
    pid = cwd / ("sleep-" + uuid.uuid4().hex)
    try:
        out, _ = process.communicate(b"call execute_command_line('sleep 30 & echo $! > %s')\r"
            b"print '(a)', 'before stop'; stop 3\r" % str(pid).encode(), timeout=15)
        check(b"before stop" in out, "the output before STOP was lost:\n%r" % out)
        check(b"the kernel has gone" in out, "the prompt did not tell:\n%r" % out)
        deadline = time.monotonic() + 10
        while connection.exists() and time.monotonic() < deadline:
            time.sleep(0.05)
        check(not connection.exists(), "the kernel left its connection file")
    except subprocess.TimeoutExpired:
        raise AssertionError("the prompt waited for the program the cell started")
    finally:
        if process.poll() is None:
            process.kill()
            process.wait()
        if pid.exists():
            os.kill(int(pid.read_text()), signal.SIGKILL)
            pid.unlink()


def child_processes(exe, cwd):
    """A program that a cell starts gets SIGPIPE's default action, which the
    kernel does not change for itself either, and inherits neither the
    kernel's stdout and stderr, which the cell's output capture saves, nor
    its sockets: they end with the kernel."""
    with kernel(exe, cwd) as (_, join, _ports):
        reply, messages = run(join(), "call execute_command_line("
            "\"sh -c 'kill -PIPE $$; echo survived'; echo checked\")")
        check(reply["status"] == "ok", reply)
        out = "".join(m["content"]["text"] for m in messages if m["msg_type"] == "stream")
        check("checked" in out and "survived" not in out,
            "a program a cell started ignores SIGPIPE: %r" % out)
    connection = cwd / ("kernel-" + uuid.uuid4().hex + ".json")
    write_connection_file(str(connection), key=uuid.uuid4().hex.encode())
    pid = cwd / ("sleep-" + uuid.uuid4().hex)
    process = subprocess.Popen([exe, "--no-color", "kernel", "-f", str(connection)],
        cwd=cwd, stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    client = None
    try:
        client = connect(connection)
        reply, _ = run(client, "call execute_command_line('sleep 30 & echo $! > %s')" % pid)
        check(reply["status"] == "ok", reply)
        client.shutdown()
        try:
            process.communicate(timeout=15)
        except subprocess.TimeoutExpired:
            raise AssertionError("a program a cell started kept the kernel's stdout open")
    finally:
        if client:
            client.stop_channels()
        if process.poll() is None:
            process.kill()
            process.wait()
        if pid.exists():
            os.kill(int(pid.read_text()), signal.SIGKILL)
            pid.unlink()


def signal_during_cell(exe, cwd):
    """SIGTERM or SIGINT ends a kernel during a cell, which may never end."""
    for number in [signal.SIGTERM, signal.SIGINT]:
        with kernel(exe, cwd) as (process, join, _ports):
            client = join()
            client.execute("integer :: spin = 0\n"
                "do while (spin >= 0)\nspin = mod(spin + 1, 1000)\nend do")
            time.sleep(2)
            check(process.poll() is None, "the cell ended")
            process.send_signal(number)
            try:
                process.wait(timeout=20)
            except subprocess.TimeoutExpired:
                raise AssertionError("%s did not end the kernel" % number.name)


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
    read_disconnect_fatal, late_stdin, prompt_departure, prompt_interrupted,
    idle_prompt, slow_prompt, prompt_stop, child_processes, signal_during_cell,
    stop_on_error, verbose]}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--case", choices=CASES, action="append",
        help="run only this case (repeatable)")
    args = parser.parse_args()
    exe = str(Path(args.lfortran).resolve())
    cwd = Path.cwd() / ("session-regressions-" + uuid.uuid4().hex)
    cwd.mkdir(mode=0o700)
    # The prompt writes its kernel's connection file here.
    os.environ["TMPDIR"] = str(cwd)
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
