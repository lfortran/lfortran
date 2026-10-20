"""Optional classic Jupyter interoperability, not a self-client/server test.

The ordinary native tests remain stdlib-only. This suite requires the explicit
Pixi jupyter feature and uses real jupyter_client, pyzmq and ipykernel.
"""
import argparse
import base64
import contextlib
import hashlib
import hmac
import json
import os
from pathlib import Path
from queue import Empty
import shutil
import subprocess
import sys
import threading
import time
import uuid

import zmq
from jupyter_client import BlockingKernelClient
from jupyter_client.connect import write_connection_file
from jupyter_client.session import Session

from test_native_session import Peer, frame, private_directory, text


def connection(root, key=None):
    path = root / (uuid.uuid4().hex + ".json")
    write_connection_file(str(path), ip="127.0.0.1",
        key=uuid.uuid4().hex.encode() if key is None else key)
    return path


def client(path):
    c = BlockingKernelClient(connection_file=str(path))
    c.load_connection_file()
    c.start_channels()
    c.wait_for_ready(timeout=25)
    return c


def wait(c, channel, kind, parent, timeout=20):
    end = time.monotonic() + timeout
    while time.monotonic() < end:
        m = getattr(c, "get_" + channel + "_msg")(timeout=max(.01, end-time.monotonic()))
        if m["msg_type"] == kind and m["parent_header"].get("msg_id") == parent:
            return m
    raise AssertionError((channel, kind, parent))


def done(c, request, status="ok"):
    messages = []
    end = time.monotonic() + 25
    # This deliberately accepts idle-before-reply; the terminal tests exercise
    # the reverse ordering too. Neither result nor stream is a completion fence.
    while time.monotonic() < end:
        m = c.get_iopub_msg(timeout=max(.01, end-time.monotonic()))
        if m["parent_header"].get("msg_id") != request:
            continue
        messages.append(m)
        if m["msg_type"] == "status" and m["content"]["execution_state"] == "idle":
            break
    else:
        raise AssertionError("no correlated IOPub idle")
    reply = wait(c, "shell", "execute_reply", request)
    assert reply["content"]["status"] == status, reply
    return reply, messages


def value(c, code, expected):
    reply, events = done(c, c.execute(code))
    results = [m["content"]["data"]["text/plain"] for m in events
        if m["msg_type"] == "execute_result"]
    assert results == [expected], (code, results, events)
    return reply, events


def stop(process):
    if process and process.poll() is None:
        process.terminate()
        try:
            process.wait(timeout=8)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait(timeout=8)


def heartbeat_echo(path):
    config = json.loads(path.read_text())
    context = zmq.Context()
    socket = context.socket(zmq.REQ)
    try:
        socket.connect("tcp://127.0.0.1:%d" % config["hb_port"])
        for payload in (b"", b"\x00actual-heartbeat\x00"):
            socket.send(payload)
            assert socket.poll(2000), "heartbeat stalled while evaluating/reading"
            assert socket.recv() == payload
    finally:
        socket.close(linger=0)
        context.term()


class Native:
    def __init__(self, executable, root, classic):
        self.local = root / ("local-" + uuid.uuid4().hex + ".json")
        self.log = root / ("native-" + uuid.uuid4().hex + ".log")
        self.output = self.log.open("wb")
        read_fd, write_fd = os.pipe()
        os.set_inheritable(write_fd, True)
        command = [executable, "--serve", "--connection-file", str(self.local),
            "--jupyter-connection-file", str(classic), "--session-ready"]
        if os.name == "nt":
            import msvcrt
            command.append(str(msvcrt.get_osfhandle(write_fd)))
            kwargs = {"close_fds": False}
        else:
            command.append(str(write_fd))
            kwargs = {"pass_fds": (write_fd,)}
        self.process = subprocess.Popen(command, stdin=subprocess.DEVNULL,
            stdout=self.output, stderr=self.output, **kwargs)
        os.close(write_fd)
        timer = threading.Timer(30, self.process.kill)
        timer.start()
        try:
            with os.fdopen(read_fd, "rb", buffering=0) as startup:
                ready = b""
                while not ready.endswith(b"\n"):
                    byte = startup.read(1)
                    assert byte, self.log.read_text(errors="replace")
                    ready += byte
            assert not ready.startswith(b"error"), ready
            self.descriptor = json.loads(self.local.read_text())
        finally:
            timer.cancel()

    def close(self):
        stop(self.process)
        self.output.close()


def native(executable, root):
    for key in (b"real-signed-client", b""):
        path = connection(root, key)
        server = Native(executable, root, path)
        a = b = local = None
        try:
            a, b = client(path), client(path)
            local = Peer(server.descriptor)
            reply, _ = done(a, a.execute("integer :: n = 41, read_n, read_ios"))
            assert reply["content"]["execution_count"] == 1
            request = local.execute("n + 1")
            assert local.wait("execute_result", request)["content"]["data"]["text/plain"] == "42"
            assert local.done(request)["content"]["execution_count"] == 2
            reply, events = value(b, "n + 2", "43")
            assert reply["content"]["execution_count"] == 3
            request = a.execute("integer :: broken(")
            reply, events = done(a, request, "error")
            assert any(m["msg_type"] == "error" for m in events)
            value(b, "n + 3", "44")
            request = a.execute("print *, 'before input'; "
                "read(*,*,iostat=read_ios) read_n; print *, 'after input', read_n")
            prompt = wait(a, "stdin", "input_request", request)
            # The output must arrive without sending the input value.
            before = wait(a, "iopub", "stream", request)
            assert "before input" in before["content"]["text"]
            queued = local.execute("read_n + 10")
            info = b.session.msg("kernel_info_request", {})
            b.control_channel.send(info)
            wait(b, "control", "kernel_info_reply", info["header"]["msg_id"])
            assert a.hb_channel.is_beating(), "heartbeat stalled during READ"
            heartbeat_echo(path)
            with contextlib.suppress(Empty):
                unexpected = b.get_stdin_msg(timeout=.1)
                raise AssertionError(("input leaked to observer", unexpected))
            a.input("42")  # Real API intentionally sends parent_header={}.
            done(a, request)
            assert local.wait("execute_result", queued)["content"]["data"]["text/plain"] == "52"
            local.done(queued)
            request = a.execute("block\ninteger :: i\n"
                "do i=1,3000\nprint *, 'large-output-record-abcdefghijklmnopqrstuvwxyz'\nend do\n"
                "write(0,*) 'stderr-record'\nend block")
            _, events = done(a, request)
            streams = [m["content"] for m in events if m["msg_type"] == "stream"]
            stdout = "".join(m["text"] for m in streams if m["name"] == "stdout")
            assert stdout.count("large-output-record") == 3000, len(stdout)
            assert any(m["name"] == "stderr" and "stderr-record" in m["text"] for m in streams)
            request = a.execute("use lfortran_display\n"
                "call display_data('application/json', '{\"answer\":[42]}')\n"
                "call display_data('text/html', '<b>rich</b>')\ncall clear_output()")
            _, events = done(a, request)
            displays = [m["content"]["data"] for m in events if m["msg_type"] == "display_data"]
            assert displays == [{"application/json": {"answer": [42]}},
                {"text/html": "<b>rich</b>"}], displays
            assert any(m["msg_type"] == "clear_output" for m in events)
            request = a.execute("read(*,*,iostat=read_ios) read_n; print *, read_ios",
                allow_stdin=False)
            _, events = done(a, request)
            assert int("".join(m["content"]["text"] for m in events
                if m["msg_type"] == "stream").strip()) > 0
            count = value(a, "n", "41")[0]["content"]["execution_count"]
            request = a.execute("n=99; print *, 'silent-secret'", silent=True)
            reply, events = done(a, request)
            assert reply["content"]["execution_count"] == count
            assert not any(m["msg_type"] not in ("status",) for m in events)
            request = a.execute("n+1", store_history=False, user_expressions={"x": "n"})
            reply, _ = done(a, request)
            assert reply["content"]["execution_count"] == count
            assert reply["content"]["user_expressions"]["x"]["status"] == "error"
            history = a.history(hist_access_type="tail", n=100)
            entries = wait(a, "shell", "history_reply", history)["content"]["history"]
            assert not any("silent-secret" in e[2] for e in entries)
            for stop_on_error, expected in ((True, "aborted"), (False, "ok")):
                gate = a.execute("read(*,*,iostat=read_ios) read_n")
                wait(a, "stdin", "input_request", gate)
                bad = b.execute("integer :: broken(", stop_on_error=stop_on_error)
                after = b.execute("n + 1")
                barrier = b.kernel_info()
                wait(b, "shell", "kernel_info_reply", barrier)
                a.input("1")
                done(a, gate)
                done(b, bad, "error")
                done(b, after, expected)
            # A real TCP disconnect releases this native READ, rather than
            # letting an unrelated client's queued code become stdin.
            c = client(path)
            request = c.execute("read(*,*,iostat=read_ios) read_n")
            wait(c, "stdin", "input_request", request)
            c.stop_channels()
            value(a, "123", "123")
            # Control and heartbeat remain live while the evaluator spins.
            if key:
                busy = a.execute("read(*,*,iostat=read_ios) read_n")
                wait(a, "stdin", "input_request", busy)
            else:
                busy = a.execute("do\nend do")
                wait(a, "iopub", "execute_input", busy)
            info = b.session.msg("kernel_info_request", {})
            b.control_channel.send(info)
            wait(b, "control", "kernel_info_reply", info["header"]["msg_id"])
            assert b.hb_channel.is_beating()
            heartbeat_echo(path)
            shutdown = b.shutdown()
            wait(b, "control", "shutdown_reply", shutdown)
            server.process.wait(timeout=8)
            assert not server.local.exists()
            assert path.exists(), "server deleted its caller-owned classic file"
            print("native signed=%s: real readiness, shared state, READ, streams, flags, busy control PASS"
                % bool(key), flush=True)
        finally:
            for peer in (a, b):
                if peer:
                    peer.stop_channels()
            if local:
                local.close()
            server.close()


def bridge_start(executable, path, root):
    descriptor = root / ("bridge-" + uuid.uuid4().hex + ".json")
    output = (root / ("bridge-" + uuid.uuid4().hex + ".log")).open("wb")
    process = subprocess.Popen([executable, "--bridge", str(path),
        "--connection-file", str(descriptor)], stdin=subprocess.DEVNULL,
        stdout=output, stderr=output)
    end = time.monotonic() + 30
    while not descriptor.exists():
        assert process.poll() is None, "bridge failed to start"
        assert time.monotonic() < end, "bridge descriptor did not appear"
        # Descriptor existence, followed by the protocol hello, is the barrier.
        threading.Event().wait(.01)
    return process, output, descriptor


def terminal(executable, path, source, expected, root, success=True):
    result = subprocess.run([executable, "--connect", str(path)], input=source,
        text=True, capture_output=True, timeout=35)
    (root / ("terminal-" + uuid.uuid4().hex + ".log")).write_text(
        result.stdout + "\nSTDERR:\n" + result.stderr)
    assert (result.returncode == 0) == success, (result.returncode, result.stdout, result.stderr)
    for item in expected:
        assert item in result.stdout + result.stderr, (item, result.stdout, result.stderr)
    return result


def password_terminal(executable, path):
    if os.name == "nt":
        return  # Native console coverage belongs to Windows; this is a PTY test.
    import pty
    import select
    import termios
    master, slave = pty.openpty()
    original = termios.tcgetattr(slave)
    process = subprocess.Popen([executable, "--connect", str(path)],
        stdin=slave, stdout=slave, stderr=slave)
    output = b""
    pending = b""
    queries = 0

    def until(marker):
        nonlocal output, pending, queries
        end = time.monotonic() + 20
        current = pending
        while marker not in current:
            assert time.monotonic() < end, (marker, current[-2000:])
            readable, _, _ = select.select([master], [], [], max(.01, end-time.monotonic()))
            assert readable, (marker, current[-2000:])
            part = os.read(master, 65536)
            assert part
            current += part
            output += part
            while queries < output.count(b"\x1b[6n"):
                os.write(master, b"\x1b[1;1R")
                queries += 1
        pending = current.split(marker, 1)[1]

    try:
        until(b">>> ")
        os.write(master, b"import getpass; secret = getpass.getpass('pw'+'-prompt: ')\n")
        until(b"pw-prompt: ")
        secret = b"hidden-stdin-value"
        os.write(master, secret + b"\n")
        until(b">>> ")
        os.write(master, b"print('password length', len(secret))\n")
        until(b"password length 18")
        until(b">>> ")
        os.write(master, b"\x04")
        until(b"Exiting.")
        process.wait(timeout=8)
        assert process.returncode == 0
        assert secret not in output, "password echoed to terminal"
        assert termios.tcgetattr(slave) == original, "password terminal mode not restored"
    finally:
        stop(process)
        os.close(master)
        os.close(slave)


def foreign(executable, root):
    path = connection(root)
    kernel_log = (root / "ipykernel.log").open("wb")
    env = dict(os.environ, IPYTHONDIR=str(root / "ipython"))
    process = subprocess.Popen([sys.executable, "-m", "ipykernel_launcher", "-f", str(path)],
        stdin=subprocess.DEVNULL, stdout=kernel_log, stderr=kernel_log, env=env)
    c = None
    bridge = output = a = b = None
    try:
        c = client(path)  # Genuine readiness, not a startup sleep.
        terminal(executable, path,
            "python_n = 40\npython_n + 2\nfor i in range(2):\n    print('loop', i)\n\n"
            "1/0\ninput('python prompt: ')\nhello input\npython_n+3\n",
            ["42", "loop 0", "loop 1", "ZeroDivisionError", "python prompt:",
             "'hello input'", "43"], root)
        password_terminal(executable, path)
        terminal(executable, path, "import time; time.sleep(11); 4321\n",
            ["4321"], root)
        # The explicitly unsupported EOF intentionally leaves input pending.
        # Complete it using the same terminal route is impossible after detach;
        # interrupt through the owning process, never synthesize a user value.
        if os.name != "nt":
            import signal
            terminal(executable, path, "input('EOF probe: ')\n",
                ["standard Jupyter stdin does not support EOF/cancel", "no fake input value"], root,
                success=False)
            end = time.monotonic() + 10
            while True:
                event = c.get_iopub_msg(timeout=max(.01, end-time.monotonic()))
                if event["msg_type"] == "execute_input" and "EOF probe" in event["content"]["code"]:
                    interrupted = event["parent_header"]["msg_id"]
                    break
            process.send_signal(signal.SIGINT)
            while wait(c, "iopub", "status", interrupted)["content"]["execution_state"] != "idle":
                pass
            value(c, "python_n", "40")
        else:
            print("foreign EOF interrupt recovery skipped on Windows", flush=True)
        bridge, output, descriptor = bridge_start(executable, path, root)
        local = json.loads(descriptor.read_text())
        a, b = Peer(local), Peer(local)
        terminal(executable, descriptor,
            "bridge_cli_value = input('bridge CLI prompt: ')\ncli-from-local\nbridge_cli_value\n",
            ["bridge CLI prompt:", "'cli-from-local'"], root)
        request = a.execute("python_n = 81")
        a.done(request)
        request = b.execute("python_n + 1")
        assert b.wait("execute_result", request)["content"]["data"]["text/plain"] == "82"
        b.done(request)
        request = a.execute("bridge_input = input('bridge prompt: ')\nprint(bridge_input)")
        prompt = a.wait("input_request", request)
        assert prompt["parent_header"]["session"] == a.session
        assert prompt["header"]["session"] == local["kernel_session"]
        info = b.send("kernel_info_request", channel="control")
        b.wait("kernel_info_reply", info)
        assert not any(m.get("header", {}).get("msg_type") == "input_request" for m in b.log)
        a.input(prompt, "origin-only")
        a.done(request)
        request = b.execute("print('single-fanout'); python_n + 2")
        b.done(request)
        while a.wait("status", request)["content"]["execution_state"] != "idle":
            pass
        assert text(a.log, request).count("single-fanout") == 1
        for peer in (a, b):
            events = [m for m in peer.log if m.get("parent_header", {}).get("msg_id") == request]
            ids = [m["header"]["msg_id"] for m in events]
            assert len(ids) == len(set(ids)), "duplicate IOPub fanout"
            assert all(m["header"]["session"] == local["kernel_session"] for m in events)
            assert all(m["parent_header"]["session"] == b.session for m in events)
        assert not any(m.get("header", {}).get("msg_type") == "execute_reply"
            and m["parent_header"].get("msg_id") == request for m in a.log)
        request = a.execute("1 / 0")
        a.done(request, "error")
        request = b.execute("python_n + 3")
        assert b.wait("execute_result", request)["content"]["data"]["text/plain"] == "84"
        b.done(request)
        # A bridge refuses the local EOF extension rather than submitting a
        # fake empty standard input value. Only this test's kernel owner
        # performs the explicit interrupt needed to release Python's waiter.
        if os.name != "nt":
            request = b.execute("input('bridge EOF: ')\nraise AssertionError('fake input value')")
            prompt = b.wait("input_request", request)
            b.input(prompt, eof=True)
            try:
                b.wait("execute_reply", request)
                raise AssertionError("bridge accepted unsupported EOF")
            except RuntimeError as error:
                assert "no EOF/cancel" in str(error), error
            b.close()
            import signal
            process.send_signal(signal.SIGINT)
            error = a.wait("error", request)
            assert error["content"]["ename"] == "KeyboardInterrupt", error
            while a.wait("status", request)["content"]["execution_state"] != "idle":
                pass
        # Local shutdown only stops the proxy, never the foreign kernel.
        request = a.send("shutdown_request", {"restart": False}, channel="control")
        reply = a.wait("shutdown_reply", request)
        assert reply["metadata"]["lfortran_bridge_local"]
        bridge.wait(timeout=8)
        assert not descriptor.exists()
        value(c, "python_n + 4", "85")
        # Actual kernel death is detected independently of cell wall time.
        output.close()
        bridge, output, descriptor = bridge_start(executable, path, root)
        lost = Peer(json.loads(descriptor.read_text()))
        try:
            request = lost.execute("import time; time.sleep(60)")
            lost.wait("execute_input", request)
            stop(process)
            bridge.wait(timeout=15)
            assert bridge.returncode != 0
            assert not descriptor.exists()
            assert not any(m.get("header", {}).get("msg_type") == "execute_reply"
                and m["parent_header"].get("msg_id") == request for m in lost.log)
        finally:
            lost.close()
        print("foreign: actual ipykernel terminal, multiline/errors/input; two-peer evaluator-free bridge PASS",
            flush=True)
    finally:
        for peer in (a, b):
            if peer:
                peer.close()
        stop(bridge)
        if output:
            output.close()
        if c:
            with contextlib.suppress(Exception):
                c.shutdown()
            c.stop_channels()
        stop(process)
        kernel_log.close()


def raw_receive(socket, session, kind, parent):
    end = time.monotonic() + 20
    while time.monotonic() < end:
        assert socket.poll(max(1, int((end-time.monotonic())*1000))), "raw reply timed out"
        frames = socket.recv_multipart()
        identities, payload = session.feed_identities(frames)
        message = session.deserialize(payload)
        if message["msg_type"] == kind and message["parent_header"].get("msg_id") == parent:
            return identities, message
    raise AssertionError((kind, parent))


class RawKernel:
    """Independent Session-based wire fixture for nonstandard/binary messages.

    This supplements, rather than replaces, the real ipykernel tests. All its
    sockets are created, used and closed on its own single reactor thread.
    """
    def __init__(self, root):
        self.path = connection(root, b"independent-fixture-key")
        self.config = json.loads(self.path.read_text())
        self.ready = threading.Event()
        self.stopped = threading.Event()
        self.failure = None
        self.seen = []
        self.thread = threading.Thread(target=self.run)
        self.thread.start()
        assert self.ready.wait(10)
        assert not self.failure, self.failure

    def run(self):
        context = zmq.Context()
        sockets = {}
        scheduled = []
        unfinished = set()
        session = Session(key=self.config["key"].encode())
        try:
            for name, kind in [("shell", zmq.ROUTER), ("control", zmq.ROUTER),
                    ("stdin", zmq.ROUTER), ("iopub", zmq.PUB), ("hb", zmq.REP)]:
                sockets[name] = context.socket(kind)
                sockets[name].bind("tcp://127.0.0.1:%d" % self.config[name + "_port"])
            poller = zmq.Poller()
            for name, socket in sockets.items():
                if name != "iopub":
                    poller.register(socket, zmq.POLLIN)

            def send(channel, kind, content, parent, route=(), metadata=None, buffers=(),
                    invalid_first=False):
                message = session.msg(kind, content, parent=parent,
                    metadata=metadata or {})
                message["header"]["version"] = "5.3"
                message["header"]["unknown_header"] = {"preserved": True}
                jsons = [json.dumps(message[f], indent=1, default=str).encode()
                    for f in ("header", "parent_header", "metadata", "content")]
                frames = list(route) + [b"<IDS|MSG>", session.sign(jsons)] + jsons + list(buffers)
                if invalid_first:
                    invalid = list(frames)
                    invalid[len(route)+1] = b"f" * 64
                    sockets[channel].send_multipart(invalid)
                sockets[channel].send_multipart(frames)
                return frames

            def status(state, parent):
                send("iopub", "status", {"execution_state": state}, parent, [b"status"])

            self.ready.set()
            while not self.stopped.is_set():
                for item in list(scheduled):
                    when, route, callback = item
                    if time.monotonic() >= when:
                        callback()
                        unfinished.remove(tuple(route))
                        scheduled.remove(item)
                events = dict(poller.poll(10))
                for name, socket in sockets.items():
                    if socket not in events:
                        continue
                    frames = socket.recv_multipart()
                    if name == "hb":
                        socket.send_multipart(frames)
                        continue
                    route, payload = session.feed_identities(frames)
                    message = session.deserialize(payload)
                    # Session converts ISO dates to datetime. Keep the posted
                    # header strings when constructing this fixture's parents.
                    message["header"]["date"] = json.loads(payload[1])["date"]
                    kind = message["msg_type"]
                    self.seen.append(message)
                    assert tuple(route) not in unfinished, "client submitted before reply AND idle"
                    if kind == "kernel_info_request":
                        status("busy", message)
                        send(name, "kernel_info_reply", {"status": "ok", "protocol_version": "5.4",
                            "implementation": "independent-fixture", "implementation_version": "1",
                            "language_info": {"name": "not-fortran", "version": "1",
                                "mimetype": "text/plain", "file_extension": ".txt"},
                            "banner": "Independent wire fixture", "help_links": []}, message, route)
                        status("idle", message)
                    elif kind == "is_complete_request":
                        pass  # A conforming classic kernel may omit this optional request.
                    elif kind == "execute_request":
                        status("busy", message)
                        code = message["content"]["code"].strip()
                        error = code == "reply-error-only"
                        if not error:
                            send("iopub", "execute_result", {"execution_count": 1,
                                "data": {"text/plain": "901"}, "metadata": {}}, message, [b"result"])
                        content = {"status": "error", "execution_count": 1,
                            "ename": "FixtureError", "evalue": "shell-only failure",
                            "traceback": []} if error else {
                                "status": "ok", "execution_count": 1, "user_expressions": {}}
                        reply = lambda message=message, route=route, content=content: send("shell", "execute_reply",
                            content, message, route)
                        idle = lambda message=message: status("idle", message)
                        if code == "reply-first":
                            reply()
                            callback = idle
                        else:
                            idle()
                            callback = reply
                        unfinished.add(tuple(route))
                        scheduled.append((time.monotonic() + .05, route, callback))
                    elif kind == "extension_request":
                        buffers = [bytes(b) for b in message["buffers"]]
                        sent = send(name, "extension_reply", {
                            "data": {"application/vnd.example+json": {"nested": [1, None, True]}},
                            "unknown_content": {"unchanged": True}}, message, route,
                            {"unknown_metadata": {"a": [None, 2]}}, buffers, invalid_first=True)
                        socket.send_multipart(sent)  # Signed duplicate must not fan out twice.
                        send("iopub", "extension_event", {"unknown": [1, "two", None]},
                            message, [b"topic\x00binary"], {"mime": {"x": True}}, buffers)
        except BaseException as error:
            self.failure = error
            self.ready.set()
        finally:
            for socket in sockets.values():
                socket.close(linger=0)
            context.term()

    def close(self):
        self.stopped.set()
        self.thread.join(timeout=8)
        assert not self.thread.is_alive()
        assert not self.failure, self.failure


def wire_bridge(executable, root):
    fixture = RawKernel(root)
    process = output = a = b = None
    try:
        terminal(executable, fixture.path, "reply-first\nidle-first\nreply-error-only\n",
            ["901", "Independent wire fixture", "FixtureError: shell-only failure",
             "submitting without completeness assistance"], root)
        process, output, descriptor = bridge_start(executable, fixture.path, root)
        local = json.loads(descriptor.read_text())
        a, b = Peer(local), Peer(local)
        for batch in range(4):
            for query in range(40):
                a.send("is_complete_request", {"code": "optional query"})
            barrier = a.send("kernel_info_request")
            a.wait("kernel_info_reply", barrier)
        message = a.make("extension_request", {"unknown": {"nested": [None, 42]}},
            metadata={"unknown_request": [1, {"two": True}]})
        message["header"]["custom_header"] = {"keep": ["all", "fields"]}
        buffers = [b"", b"\x00\xff\x00binary", b"\x00"]
        message["buffers"] = [base64.b64encode(buffer).decode() for buffer in buffers]
        request = message["header"]["msg_id"]
        a.write(frame(message))
        reply = a.wait("extension_reply", request)
        assert reply["header"]["version"] == "5.3"
        assert reply["header"]["unknown_header"] == {"preserved": True}
        assert reply["parent_header"] == message["header"]
        assert reply["metadata"] == {"unknown_metadata": {"a": [None, 2]}}
        assert reply["content"]["unknown_content"] == {"unchanged": True}
        assert [base64.b64decode(buffer) for buffer in reply["buffers"]] == buffers
        for peer in (a, b):
            event = peer.wait("extension_event", request)
            assert event["metadata"] == {"mime": {"x": True}}
            assert [base64.b64decode(buffer) for buffer in event["buffers"]] == buffers
            barrier = peer.send("kernel_info_request")
            peer.wait("kernel_info_reply", barrier)
            assert sum(m.get("header", {}).get("msg_type") == "extension_reply"
                for m in peer.log) == (1 if peer is a else 0)
        captured = next(m for m in fixture.seen if m["msg_type"] == "extension_request")
        assert captured["metadata"] == message["metadata"]
        assert captured["content"] == message["content"]
        assert [bytes(buffer) for buffer in captured["buffers"]] == buffers
        print("wire bridge: independent noncanonical Session frames, unknown fields/MIME, "
            "empty/NUL buffers, replay suppression, both terminal completion orders PASS", flush=True)
    finally:
        for peer in (a, b):
            if peer:
                peer.close()
        stop(process)
        if output:
            output.close()
        fixture.close()


def wire(executable, root):
    path = connection(root, b"exact-byte-HMAC-key")
    config = json.loads(path.read_text())
    server = Native(executable, root, path)
    context = zmq.Context()
    shell = context.socket(zmq.DEALER)
    shell.identity = b"binary\x00outer"
    shell.connect("tcp://127.0.0.1:%d" % config["shell_port"])
    stdin = context.socket(zmq.DEALER)
    stdin.identity = shell.identity
    stdin.connect("tcp://127.0.0.1:%d" % config["stdin_port"])
    other = context.socket(zmq.DEALER)
    other.identity = b"other\x00origin"
    other.connect("tcp://127.0.0.1:%d" % config["shell_port"])
    other_stdin = context.socket(zmq.DEALER)
    other_stdin.identity = other.identity
    other_stdin.connect("tcp://127.0.0.1:%d" % config["stdin_port"])
    session = Session(key=config["key"].encode())
    local = None
    try:
        local = Peer(server.descriptor)
        route = [b"inner\x00route", b""]
        msg = session.msg("kernel_info_request", {})
        msg["header"]["unknown_header"] = {"nested": ["preserved", None]}
        # Noncanonical whitespace/key order is signed as posted, not reencoded.
        jsons = [json.dumps(msg[f], indent=2, default=str).encode()
            for f in ("header", "parent_header", "metadata", "content")]
        signature = hmac.new(config["key"].encode(), b"".join(jsons), hashlib.sha256).hexdigest().encode()
        parts = route + [b"<IDS|MSG>", signature] + jsons + [b"", b"\x00\xffbinary\x00"]
        shell.send_multipart(parts)
        identities, reply = raw_receive(shell, session, "kernel_info_reply", msg["header"]["msg_id"])
        assert identities == route, identities
        assert reply["parent_header"]["unknown_header"] == {"nested": ["preserved", None]}
        # Bad signatures and malformed multipart must not execute or poison a
        # healthy independent local peer. Known signed replay is discarded.
        shell.send_multipart(parts)
        bad = session.msg("execute_request", dict(code="integer :: forged = 1",
            silent=False, store_history=True, allow_stdin=False, user_expressions={},
            stop_on_error=True))
        invalid = session.serialize(bad)
        invalid[1] = b"0" * 64
        shell.send_multipart(route + invalid)
        shell.send_multipart(route + [b"<IDS|MSG>", b"bad", b"{}"])
        shell.send_multipart([b"no-delimiter"])
        good = session.msg("kernel_info_request", {})
        session.send(shell, good, ident=route)
        raw_receive(shell, session, "kernel_info_reply", good["header"]["msg_id"])
        assert not shell.poll(100), "bad signature or replay produced a response"
        request = local.execute("forged")
        local.done(request, "error")
        request = local.execute("integer :: route_value, route_ios")
        local.done(request)
        # Two independent binary routes deliberately share the exact header
        # session. Routing, especially stdin, must not collapse them.
        info = session.msg("kernel_info_request", {})
        session.send(other, info)
        raw_receive(other, session, "kernel_info_reply", info["header"]["msg_id"])
        def execute_message(code):
            return session.msg("execute_request", dict(code=code, silent=False,
                store_history=True, allow_stdin=True, user_expressions={}, stop_on_error=True))
        read = execute_message("read(*,*,iostat=route_ios) route_value")
        session.send(shell, read, ident=route)
        _, prompt = raw_receive(stdin, session, "input_request", read["header"]["msg_id"])
        assert prompt["parent_header"]["session"] == session.session
        session.send(other_stdin, "input_reply", {"value": "999"})
        queued = execute_message("route_value + 1")
        session.send(other, queued)
        barrier = session.msg("kernel_info_request", {})
        session.send(other, barrier)
        raw_receive(other, session, "kernel_info_reply", barrier["header"]["msg_id"])
        session.send(stdin, "input_reply", {"value": "41"}, ident=route)
        identities, reply = raw_receive(shell, session, "execute_reply", read["header"]["msg_id"])
        assert identities == route and reply["content"]["status"] == "ok"
        _, reply = raw_receive(other, session, "execute_reply", queued["header"]["msg_id"])
        assert reply["content"]["status"] == "ok"
        result = local.wait("execute_result", queued["header"]["msg_id"])
        assert result["content"]["data"]["text/plain"] == "42", result
        # Unsupported classic security/startup formats fail before any bind.
        for change, diagnostic in [
            ({"signature_scheme": "hmac-sha1"}, "signature_scheme"),
            ({"curve_publickey": "present"}, "plaintext downgrade"),
            ({"curve_secretkey": ""}, "plaintext downgrade"),
            ({"registration_port": 10000}, "registration handshake"),
            ({"transport": "ipc"}, "TCP connections only"),
            ({"stdin_port": config["shell_port"]}, "distinct integers"),
        ]:
            invalid_path = root / ("invalid-" + uuid.uuid4().hex + ".json")
            invalid_path.write_text(json.dumps(dict(config, **change)))
            invalid_path.chmod(0o600)
            result = subprocess.run([executable, "--bridge", str(invalid_path),
                "--connection-file", str(root / ("unused-" + uuid.uuid4().hex))],
                text=True, capture_output=True, timeout=8)
            assert result.returncode and diagnostic in result.stderr, result.stderr
        print("wire: independent Session, exact-byte HMAC, binary identity stack/empty frames, "
            "bad signatures/replay/malformed/security validation PASS", flush=True)
    finally:
        if local:
            local.close()
        shell.close(linger=0)
        stdin.close(linger=0)
        other.close(linger=0)
        other_stdin.close(linger=0)
        context.term()
        server.close()
    wire_bridge(executable, root)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--mode", choices=["native", "foreign", "wire"], required=True)
    args = parser.parse_args()
    root = Path.cwd() / ("jupyter-zmq-" + uuid.uuid4().hex)
    private_directory(root)
    try:
        globals()[args.mode](str(Path(args.lfortran).resolve()), root)
    except BaseException:
        print("Jupyter failure evidence:", root, flush=True)
        for path in root.glob("*.log"):
            print(path.name, path.read_text(errors="replace")[-12000:], flush=True)
        raise
    else:
        shutil.rmtree(root)


if __name__ == "__main__":
    main()
