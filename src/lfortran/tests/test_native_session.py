#!/usr/bin/env python3
"""Native session protocol, isolation, I/O, lifecycle and terminal regressions.

Only the Python standard library is used. Scratch directories stay beneath the
CTest working directory. Readiness, output and input messages are barriers;
there are no startup sleeps.
"""
import argparse
import base64
import contextlib
import ctypes
import datetime
import json
import os
from pathlib import Path
import queue
import re
import select
import socket
import struct
import subprocess
import tempfile
import threading
import time
import uuid


def private_directory(path):
    os.mkdir(path, 0o700)
    if os.name == "nt":
        from ctypes import wintypes as w
        advapi = ctypes.WinDLL("advapi32", use_last_error=True)
        kernel = ctypes.WinDLL("kernel32", use_last_error=True)
        advapi.GetNamedSecurityInfoW.restype = w.DWORD
        advapi.ConvertSidToStringSidW.argtypes = [
            ctypes.c_void_p, ctypes.POINTER(w.LPWSTR)]
        advapi.ConvertStringSecurityDescriptorToSecurityDescriptorW.argtypes = [
            w.LPCWSTR, w.DWORD, ctypes.POINTER(ctypes.c_void_p),
            ctypes.POINTER(w.DWORD)]
        advapi.SetFileSecurityW.argtypes = [w.LPCWSTR, w.DWORD, ctypes.c_void_p]
        kernel.LocalFree.argtypes = [ctypes.c_void_p]
        kernel.LocalFree.restype = ctypes.c_void_p
        owner, descriptor = ctypes.c_void_p(), ctypes.c_void_p()
        advapi.GetNamedSecurityInfoW.argtypes = [w.LPWSTR, w.DWORD, w.DWORD,
            ctypes.POINTER(ctypes.c_void_p), ctypes.c_void_p, ctypes.c_void_p,
            ctypes.c_void_p, ctypes.POINTER(ctypes.c_void_p)]
        assert advapi.GetNamedSecurityInfoW(str(path), 1, 1, ctypes.byref(owner),
            None, None, None, ctypes.byref(descriptor)) == 0
        sid = w.LPWSTR()
        assert advapi.ConvertSidToStringSidW(owner, ctypes.byref(sid))
        secure = ctypes.c_void_p()
        assert advapi.ConvertStringSecurityDescriptorToSecurityDescriptorW(
            "D:P(A;OICI;FA;;;%s)" % sid.value, 1, ctypes.byref(secure), None)
        assert advapi.SetFileSecurityW(str(path), 4 | 0x80000000, secure)
        for pointer in [secure, sid, descriptor]:
            kernel.LocalFree(pointer)


def frame(value):
    data = json.dumps(value, ensure_ascii=False, separators=(",", ":")).encode()
    return struct.pack("!I", len(data)) + data


def header(session, kind):
    return dict(msg_id=uuid.uuid4().hex, session=session, username="test",
        date=datetime.datetime.now(datetime.timezone.utc).isoformat(),
        msg_type=kind, version="5.4")


class Peer:
    def __init__(self, descriptor, fragmented=False, features=("stdin_eof",)):
        self.session = uuid.uuid4().hex
        self.messages = queue.Queue()
        self.saved = []
        self.log = []
        self.closed = False
        if os.name == "nt":
            self.file = open(descriptor["pipe"], "r+b", buffering=0)
            self.socket = None
        else:
            self.socket = socket.socket(socket.AF_UNIX)
            self.socket.connect(descriptor["socket"])
            self.file = self.socket.makefile("rwb", buffering=0)
        self.reader = threading.Thread(target=self.read, daemon=True)
        self.reader.start()
        hello = frame(dict(lfortran_local_protocol=1,
            client_session=self.session, features=list(features)))
        if fragmented:
            for byte in hello:
                self.write(bytes([byte]))
        else:
            self.write(hello)
        ack = self.next()
        assert ack["lfortran_local_protocol"] == 1, ack
        assert ack["kernel_session"] == descriptor["kernel_session"], ack
        assert ack["jupyter_protocol"] == "5.4", ack
        self.kernel = ack["kernel_session"]

    def read_exactly(self, size):
        data = b""
        while len(data) < size:
            part = self.file.read(size-len(data))
            if not part:
                raise EOFError("kernel disconnected")
            data += part
        return data

    def read(self):
        try:
            while True:
                size, = struct.unpack("!I", self.read_exactly(4))
                assert 0 < size <= 16*1024*1024
                self.messages.put(json.loads(self.read_exactly(size)))
        except (Exception, AssertionError) as error:
            self.messages.put(error)

    def write(self, data):
        view = memoryview(data)
        while view:
            n = self.file.write(view)
            assert n
            view = view[n:]

    def make(self, kind, content=None, channel="shell", parent=None, metadata=None):
        return dict(channel=channel, header=header(self.session, kind),
            parent_header=parent or {}, metadata=metadata or {},
            content=content or {}, buffers=[])

    def send(self, kind, content=None, **kwargs):
        message = self.make(kind, content, **kwargs)
        self.write(frame(message))
        return message["header"]["msg_id"]

    def execute(self, code, **flags):
        content = dict(code=code, silent=False, store_history=True,
            allow_stdin=True, stop_on_error=True, user_expressions={})
        content.update(flags)
        return self.send("execute_request", content)

    def next(self, timeout=20):
        value = self.messages.get(timeout=timeout)
        if isinstance(value, Exception):
            raise value
        self.log.append(value)
        return value

    def wait(self, kind, parent=None):
        def matches(m):
            return m.get("header", {}).get("msg_type") == kind and (
                parent is None or m["parent_header"].get("msg_id") == parent)
        for i, message in enumerate(self.saved):
            if matches(message):
                return self.saved.pop(i)
        deadline = time.monotonic() + 20
        while True:
            value = self.next(max(0.01, deadline-time.monotonic()))
            if "connection_error" in value:
                raise RuntimeError(value["connection_error"])
            if matches(value):
                return value
            self.saved.append(value)

    def done(self, parent, status="ok"):
        reply = self.wait("execute_reply", parent)
        assert reply["content"]["status"] == status, reply
        while True:
            state = self.wait("status", parent)
            if state["content"]["execution_state"] == "idle":
                break
        return reply

    def input(self, prompt, value="", eof=False):
        return self.send("input_reply", {"value": value}, channel="stdin",
            parent=prompt["header"], metadata={"lfortran_stdin_eof": eof})

    def close(self):
        if self.closed:
            return
        self.closed = True
        if self.socket:
            with contextlib.suppress(OSError):
                self.socket.shutdown(socket.SHUT_RDWR)
        self.file.close()
        if self.socket:
            self.socket.close()
        self.reader.join(timeout=2)

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.close()


class Server:
    def __init__(self, executable, root):
        self.file = root / ("session-" + uuid.uuid4().hex + ".json")
        self.peers = []
        read_fd, write_fd = os.pipe()
        os.set_inheritable(write_fd, True)
        command = [executable, "--serve", "--connection-file", str(self.file),
            "--session-ready"]
        if os.name == "nt":
            import msvcrt
            command.append(str(msvcrt.get_osfhandle(write_fd)))
            kwargs = {"close_fds": False}
        else:
            command.append(str(write_fd))
            kwargs = {"pass_fds": (write_fd,)}
        self.process = subprocess.Popen(command, stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, **kwargs)
        os.close(write_fd)
        timer = threading.Timer(20, self.process.kill)
        timer.start()
        try:
            with os.fdopen(read_fd, "rb", buffering=0) as startup:
                ready = b""
                while not ready.endswith(b"\n"):
                    part = startup.read(1)
                    if not part:
                        raise AssertionError("server startup failed: %r" %
                            (self.process.communicate(timeout=10),))
                    ready += part
            assert not ready.startswith(b"error:"), ready
            self.descriptor = json.loads(self.file.read_text())
            assert self.descriptor["pid"] == self.process.pid
        finally:
            timer.cancel()

    def peer(self, **kwargs):
        peer = Peer(self.descriptor, **kwargs)
        self.peers.append(peer)
        return peer

    def shutdown(self):
        if self.process.poll() is None:
            with self.peer() as peer:
                request = peer.send("shutdown_request", {"restart": False},
                    channel="control")
                peer.wait("shutdown_reply", request)
            self.process.wait(timeout=10)
        assert not self.file.exists(), "owned descriptor was not removed"
        if os.name != "nt":
            assert not Path(self.descriptor["socket"]).exists()

    def __enter__(self):
        return self

    def __exit__(self, *args):
        try:
            self.shutdown()
        finally:
            for peer in self.peers:
                peer.close()
            if self.process.poll() is None:
                self.process.kill()
                self.process.wait(timeout=10)
            self.process.stdout.close()
            self.process.stderr.close()


def text(messages, parent, stream=None):
    return "".join(m["content"]["text"] for m in messages
        if m.get("header", {}).get("msg_type") == "stream"
        and m["parent_header"].get("msg_id") == parent
        and (stream is None or m["content"]["name"] == stream))


def shared_state(server):
    a, b = server.peer(fragmented=True), server.peer()
    first = a.execute("integer :: shared_n = 41")
    assert a.done(first)["content"]["execution_count"] == 1
    second = b.execute("shared_n + 1")
    result = b.wait("execute_result", second)
    assert result["content"]["data"]["text/plain"] == "42", result
    assert b.done(second)["content"]["execution_count"] == 2
    bad = a.execute("integer :: broken(")
    a.done(bad, "error")
    fixed = b.execute("shared_n + 2")
    assert b.wait("execute_result", fixed)["content"]["data"]["text/plain"] == "43"
    b.done(fixed)
    a.wait("status", fixed)
    for request in [first, bad]:
        messages = [m for m in a.log if m.get("parent_header", {}).get("msg_id") == request]
        kinds = [m["header"]["msg_type"] for m in messages]
        assert kinds[0:2] == ["status", "execute_input"], kinds
        assert messages[0]["content"]["execution_state"] == "busy"
        assert kinds[-2:] == ["execute_reply", "status"], kinds
        if request == bad:
            assert kinds.index("error") < kinds.index("execute_reply")
    # Replies are private, events are shared and retain the exact parent.
    assert not any(m.get("header", {}).get("msg_type") == "execute_reply" and
        m["parent_header"].get("msg_id") == first for m in b.log)
    for message in a.log + b.log:
        if "header" in message:
            assert message["header"]["session"] == server.descriptor["kernel_session"]
            assert message["header"]["version"] == "5.4"
    query = a.send("is_complete_request", {"code": "do i=1,3\n"})
    assert a.wait("is_complete_reply", query)["content"]["status"] == "incomplete"
    query = a.send("is_complete_request", {"code": "do i=1,3\nprint *,i\nend do\n"})
    assert a.wait("is_complete_reply", query)["content"]["status"] == "complete"
    for code, status in [
            ("integer :: module, function, do", "complete"),
            ("print *, module", "complete"),
            ("pure real function f(x)\n", "incomplete"),
            ("module function f(x)\nend function\n", "complete"),
            ("type :: t\ninteger :: n\nend type\n", "complete")]:
        query = a.send("is_complete_request", {"code": code})
        assert a.wait("is_complete_reply", query)["content"]["status"] == status, code
    query = a.send("complete_request", {"code": "sha", "cursor_pos": 3})
    assert a.wait("complete_reply", query)["content"]["matches"] == []
    query = a.send("inspect_request", {"code": "shared_n", "cursor_pos": 1})
    assert not a.wait("inspect_reply", query)["content"]["found"]
    query = a.execute("%%showfmt\ninteger :: formatted_value")
    a.done(query)
    assert "formatted_value" in text(a.log, query)
    return a, b


def io_and_queue(server, a, b):
    first = a.execute("integer :: read_n, read_ios")
    a.done(first)
    request = a.execute("print *, 'before input'; read(*,*,iostat=read_ios) read_n; "
        "print *, 'after input', read_n")
    prompt = a.wait("input_request", request)
    assert "before input" in text(a.log, request)
    late = server.peer()
    info = late.send("kernel_info_request")
    late.wait("kernel_info_reply", info)
    assert not any(m.get("parent_header", {}).get("msg_id") == request for m in late.log)
    queued = b.execute("read_n + 10")
    info = b.send("kernel_info_request", channel="control")
    assert b.wait("kernel_info_reply", info)["content"]["protocol_version"] == "5.4"
    assert not any(m.get("header", {}).get("msg_type") == "input_request" for m in b.log)
    a.input(prompt, "42")
    reply = a.done(request)
    while late.wait("status", request)["content"]["execution_state"] != "idle":
        pass
    assert "before input" not in text(late.log, request)
    assert "after input" in text(late.log, request)
    late.close()
    assert b.wait("execute_result", queued)["content"]["data"]["text/plain"] == "52"
    assert b.done(queued)["content"]["execution_count"] == reply["content"]["execution_count"] + 1
    assert "after input" in text(a.log, request)
    for flags, eof in [({}, True), ({"allow_stdin": False}, None), ({}, False)]:
        request = a.execute("read(*,*,iostat=read_ios) read_n; print *, read_ios", **flags)
        if eof is not None:
            prompt = a.wait("input_request", request)
            # A blank record is not EOF; provide a value after a blank line.
            a.input(prompt, "" if not eof else "", eof=eof)
            if not eof:
                prompt = a.wait("input_request", request)
                a.input(prompt, "7")
        a.done(request)
        stream = text(a.log, request)
        if eof:
            assert "-1" in stream, stream
        elif eof is None:
            assert int(stream.strip()) > 0, stream
        else:
            assert "0" in stream, stream
    # An actual disconnect releases its input waiter and cancels that origin's
    # queued cells, but does not cancel healthy peers' queued work.
    origin = server.peer()
    request = origin.execute("read(*,*,iostat=read_ios) read_n")
    origin.wait("input_request", request)
    origin.execute("shared_n = -999")
    # Same-route query proves the origin's assignment reached the queue.
    origin.wait("kernel_info_reply", origin.send("kernel_info_request"))
    next_request = b.execute("100 + 23")
    origin.close()
    assert b.wait("execute_result", next_request)["content"]["data"]["text/plain"] == "123"
    b.done(next_request)
    request = b.execute("shared_n")
    assert b.wait("execute_result", request)["content"]["data"]["text/plain"] == "41"
    b.done(request)
    # C stdio output much larger than a pipe, kept separate from stderr.
    request = a.execute("block\ninteger :: i\n"
        "do i=1,3000\nprint *, 'large-output-record-abcdefghijklmnopqrstuvwxyz'\nend do\n"
        "write(0,*) 'stderr-record'\nend block")
    a.done(request)
    assert text(a.log, request, "stdout").count("large-output-record") == 3000
    assert "stderr-record" in text(a.log, request, "stderr"), text(a.log, request)
    assert "stderr-record" not in text(a.log, request, "stdout")


def flags_and_abort(a, b):
    history = a.send("history_request", {"hist_access_type": "tail", "n": 100})
    previous = a.wait("history_reply", history)["content"]["history"]
    count = previous[-1][1]
    silent = a.execute("shared_n = 99; print *, 'private-silent-text'", silent=True)
    assert a.done(silent)["content"]["execution_count"] == count
    no_history = a.execute("shared_n + 1", store_history=False,
        user_expressions={"key": "shared_n"})
    reply = a.done(no_history)
    assert reply["content"]["execution_count"] == count
    assert reply["content"]["user_expressions"]["key"]["status"] == "error"
    history = a.send("history_request", {"hist_access_type": "tail", "n": 100})
    assert a.wait("history_reply", history)["content"]["history"] == previous
    assert not any(m.get("parent_header", {}).get("msg_id") == silent and
        m["header"]["msg_type"] not in ["status", "execute_reply"] for m in a.log)
    # Block on actual READ while both following cells are deterministically queued.
    for stop, expected in [(True, "aborted"), (False, "ok")]:
        gate = a.execute("read(*,*,iostat=read_ios) read_n")
        prompt = a.wait("input_request", gate)
        bad = b.execute("integer :: broken(", stop_on_error=stop)
        after = b.execute("shared_n + 1")
        barrier = b.send("kernel_info_request")
        b.wait("kernel_info_reply", barrier)
        a.input(prompt, "1")
        a.done(gate)
        b.done(bad, "error")
        b.done(after, expected)


def rich_and_utf8(a):
    request = a.execute("use lfortran_display, only: display_data, clear_output\n"
        "print *, 'before display'\ncall display_data('text/plain', 'rich text')\n"
        "call clear_output()\nprint *, 'after display'")
    a.done(request)
    events = [m for m in a.log if m.get("parent_header", {}).get("msg_id") == request]
    kinds = [m["header"]["msg_type"] for m in events]
    assert "display_data" in kinds and "clear_output" in kinds, events
    assert kinds.index("display_data") < kinds.index("clear_output")
    assert "before display" in events[kinds.index("display_data")-1]["content"]["text"]
    request = a.execute("call display_data('application/json', '{\"answer\":[42]}')\n"
        "call display_data('text/html', '<b>rich</b>')")
    a.done(request)
    displays = [m["content"]["data"] for m in a.log
        if m.get("header", {}).get("msg_type") == "display_data"
        and m["parent_header"]["msg_id"] == request]
    assert displays == [{"application/json": {"answer": [42]}},
        {"text/html": "<b>rich</b>"}], displays
    if os.name == "nt":
        return
    # An exact-byte C write splits a multi-byte UTF-8 codepoint across calls.
    request = a.execute("""block
use iso_c_binding, only: c_int, c_size_t, c_char
interface
function raw_write(fd, data, n) bind(c, name="write") result(r)
import c_int, c_size_t, c_char
integer(c_int), value :: fd
character(c_char), intent(in) :: data(*)
integer(c_size_t), value :: n
integer(c_size_t) :: r
end function
end interface
integer(c_size_t) :: r
integer :: ios, n
r = raw_write(1, achar(226), 1_c_size_t)
read(*,*,iostat=ios) n
r = raw_write(1, achar(130)//achar(172)//achar(10), 3_c_size_t)
end block""")
    prompt = a.wait("input_request", request)
    assert "�" not in text(a.log, request)
    a.input(prompt, "1")
    a.done(request)
    assert "€\n" in text(a.log, request), repr(text(a.log, request))
    request = a.execute("print *, achar(255)")
    a.done(request)
    assert "�" in text(a.log, request)


def malformed(server):
    def rejected(edit=None, raw=None, disconnect=False, correlated=False):
        with server.peer() as peer:
            message = None
            if raw is not None:
                peer.write(raw)
            else:
                message = peer.make("kernel_info_request")
                edit(message)
                peer.write(frame(message))
            if not disconnect:
                if correlated:
                    reply = peer.wait(message["header"]["msg_type"].replace("_request", "_reply"),
                        message["header"]["msg_id"])
                    assert reply["content"]["status"] == "error", reply
                barrier = peer.send("kernel_info_request")
                peer.wait("kernel_info_reply", barrier)
                return
            while True:
                try:
                    result = peer.next()
                except EOFError:
                    break
                if "connection_error" in result:
                    break
                raise AssertionError(result)
    rejected(raw=struct.pack("!I", 16*1024*1024+1), disconnect=True)
    rejected(raw=struct.pack("!I", 1) + b"\xff")
    rejected(raw=struct.pack("!I", 1) + b"{")
    rejected(lambda m: m["header"].update(version="6.0"))
    rejected(lambda m: m["header"].update(version="5.invalid"))
    for version in ("5.3", "5.5"):
        with server.peer() as peer:
            message = peer.make("kernel_info_request")
            message["header"]["version"] = version
            peer.write(frame(message))
            reply = peer.wait("kernel_info_reply", message["header"]["msg_id"])
            assert reply["parent_header"]["version"] == version
            assert reply["content"]["protocol_version"] == "5.4"
    rejected(lambda m: m["header"].update(session="wrong"))
    rejected(lambda m: m.update(channel="bogus"))
    rejected(lambda m: m.update(parent_header={"msg_id": "wrong"}), correlated=True)
    rejected(lambda m: m.update(buffers=["?"]))
    rejected(lambda m: m.pop("content"), correlated=True)
    rejected(lambda m: m.update(content=[]), correlated=True)
    rejected(lambda m: m.update(header=header(m["header"]["session"], "execute_request"),
        content={"code": "42", "lfortran_verbose": 1}), correlated=True)
    rejected(lambda m: m.update(header=header(m["header"]["session"], "execute_request"),
        content={"code": "42", "user_expressions": {"x": 1}}), correlated=True)
    nested = b"["*70+b"]"*70
    rejected(raw=struct.pack("!I", len(nested)) + nested)
    # Valid opaque binary buffers and unknown fields are accepted, not treated
    # as source/input. Empty binary frames retain their boundary in the codec.
    with server.peer() as peer:
        first = peer.make("kernel_info_request")
        first["buffers"] = ["", base64.b64encode(bytes(range(256))).decode()]
        first["future_extension"] = {"payload": "preserve in forwarding adapters"}
        second = peer.make("is_complete_request", {"code": "42"})
        peer.write(frame(first) + frame(second))
        peer.wait("kernel_info_reply", first["header"]["msg_id"])
        peer.wait("is_complete_reply", second["header"]["msg_id"])
        peer.write(frame(first))
        peer.wait("kernel_info_reply", peer.send("kernel_info_request"))
        assert sum(m.get("header", {}).get("msg_type") == "kernel_info_reply"
            and m.get("parent_header", {}).get("msg_id") == first["header"]["msg_id"]
            for m in peer.log) == 1
    # Retain the bounded duplicate cache across attachment and session changes.
    with server.peer() as peer:
        reused = peer.make("kernel_info_request")
        peer.write(frame(reused))
        peer.wait("kernel_info_reply", reused["header"]["msg_id"])
    with server.peer() as peer:
        reused["header"]["session"] = peer.session
        peer.write(frame(reused))
        peer.wait("kernel_info_reply", peer.send("kernel_info_request"))
        assert not any(m.get("parent_header", {}).get("msg_id") ==
            reused["header"]["msg_id"] for m in peer.log)
    # Wrong and late input replies cannot consume another origin's input, or
    # destroy a valid completion route.
    with server.peer() as a, server.peer() as b:
        request = a.execute("read(*,*,iostat=read_ios) read_n")
        prompt = a.wait("input_request", request)
        b.input(prompt, "1234")
        b.wait("kernel_info_reply", b.send("kernel_info_request"))
        a.input(prompt, "2")
        a.done(request)
        a.input(prompt, "3")
        a.wait("kernel_info_reply", a.send("kernel_info_request"))
    # Closing partway through a valid frame does not run its source.
    peer = server.peer()
    packet = frame(peer.make("execute_request", {"code": "shared_n = -999"}))
    peer.write(packet[:-1])
    peer.close()
    with server.peer() as probe:
        request = probe.execute("shared_n")
        assert probe.wait("execute_result", request)["content"]["data"]["text/plain"] == "99"
        probe.done(request)
    with server.peer(features=()) as peer:
        request = peer.execute("read(*,*,iostat=read_ios) read_n")
        prompt = peer.wait("input_request", request)
        peer.input(prompt, eof=True)
        peer.wait("kernel_info_reply", peer.send("kernel_info_request"))
        peer.input(prompt, "3")
        peer.done(request)


def observer_editor(executable, root, server):
    if os.name == "nt":
        return
    import pty
    import termios
    from test_repl import PtyRepl
    terminal = PtyRepl(pty, termios, executable, root, timeout=30,
        arguments=("--connect", str(server.file)))
    code = None
    try:
        assert terminal.wait(lambda: terminal.prompts > 0, "observer prompt")
        os.write(terminal.fd, b"shared_n + ")
        with server.peer() as peer:
            request = peer.execute("print *, 'observer-live-marker'")
            peer.done(request)
            assert terminal.wait(lambda: re.search(
                r"observer-live-marker\r?\n", terminal.text()),
                "live output while editing")
            # Another client's READ must not consume this terminal's partial
            # code or switch its editor into canonical input mode.
            request = peer.execute("read(*,*,iostat=read_ios) read_n")
            prompt = peer.wait("input_request", request)
            os.write(terminal.fd, b"5\r")
            peer.input(prompt, "6")
            peer.done(request)
        terminal.cells = 1
        assert terminal.wait(lambda: terminal.prompts > 1, "preserved cell result")
        assert re.search(r"(?m)^104\r?$", terminal.text()), terminal.text()
        code = terminal.exit()
        assert code == 0
    finally:
        if code is None:
            with contextlib.suppress(ProcessLookupError):
                os.kill(terminal.pid, 9)
            os.waitpid(terminal.pid, 0)
        os.close(terminal.fd)


def shutdown_during_input(executable, root):
    with Server(executable, root) as server:
        a, b = server.peer(), server.peer()
        a.done(a.execute("integer :: n, ios"))
        request = a.execute("read(*,*,iostat=ios) n")
        a.wait("input_request", request)
        shutdown = b.send("shutdown_request", {"restart": False}, channel="control")
        b.wait("shutdown_reply", shutdown)
        server.process.wait(timeout=10)


def shutdown_during_evaluation(executable, root):
    with Server(executable, root) as server:
        a, b = server.peer(), server.peer()
        request = a.execute("print *, 'spin-ready'\ndo\nend do")
        while "spin-ready" not in text(a.log, request):
            a.wait("stream", request)
        info = b.send("kernel_info_request", channel="control")
        b.wait("kernel_info_reply", info)
        shutdown = b.send("shutdown_request", {"restart": False}, channel="control")
        b.wait("shutdown_reply", shutdown)
        server.process.wait(timeout=10)


def slow_peer(server):
    if os.name == "nt":
        return
    connection = socket.socket(socket.AF_UNIX)
    connection.settimeout(20)
    connection.connect(server.descriptor["socket"])
    session = uuid.uuid4().hex
    connection.sendall(frame(dict(lfortran_local_protocol=1,
        client_session=session, features=[])))
    # Read only the ready barrier, then deliberately stop consuming.
    def exactly(size):
        data = b""
        while len(data) < size:
            chunk = connection.recv(size-len(data))
            if not chunk:
                return data
            data += chunk
        return data
    size, = struct.unpack("!I", exactly(4))
    assert json.loads(exactly(size))["lfortran_local_protocol"] == 1
    try:
        with server.peer() as good:
            for i in range(38):
                request = good.execute("block\ninteger :: j\n"
                    "do j=1,1024\nprint *, repeat('x',1024)\nend do\nend block",
                    store_history=False)
                good.done(request)
                good.log.clear()
                good.saved.clear()
            query = dict(channel="shell", header=header(session, "kernel_info_request"),
                parent_header={}, metadata={}, content={}, buffers=[])
            connection.sendall(frame(query))
            received = 0
            while True:
                prefix = exactly(4)
                assert prefix, "slow observer lost its route"
                size, = struct.unpack("!I", prefix)
                payload = exactly(size)
                assert len(payload) == size
                message = json.loads(payload)
                assert "connection_error" not in message, message
                received += size
                if (message.get("header", {}).get("msg_type") == "kernel_info_reply" and
                        message["parent_header"]["msg_id"] == query["header"]["msg_id"]):
                    break
            assert received < 38 * 1024 * 1024, "unbounded live-output retention"
    finally:
        connection.close()


def fatal_death(executable, root):
    server = Server(executable, root)
    try:
        peer = server.peer()
        request = peer.execute("stop 7")
        try:
            peer.done(request)
            raise AssertionError("STOP was falsely reported successful")
        except EOFError:
            pass
        assert server.process.wait(timeout=10) != 0
    finally:
        for peer in server.peers:
            peer.close()
        if server.process.poll() is None:
            server.process.kill()
            server.process.wait(timeout=10)
        server.process.stdout.close()
        server.process.stderr.close()
        # Fatal process termination can leave a private stale descriptor.
        # The test owns this child/descriptor, not an arbitrary session.
        if server.file.exists():
            server.file.unlink()
        if os.name != "nt":
            socket_path = Path(server.descriptor["socket"])
            socket_path.unlink(missing_ok=True)
            with contextlib.suppress(OSError):
                socket_path.parent.rmdir()


def security_and_cli(executable, root):
    sentinel = root / "keep.json"
    sentinel.write_text("do not replace")
    for args in [
            ["--serve", "--connection-file", str(sentinel)],
            ["--serve"], ["--serve", "--connect", str(sentinel)],
            ["--connect", str(sentinel), "--connection-file", str(sentinel)],
            ["--serve", "--connection-file", str(sentinel), "not-a-source.f90"],
            ["--serve", "--connection-file", str(sentinel), "--json"],
            ["--connect", str(sentinel), "--show-cpp"],
            ["--bridge", str(sentinel)], ["--jupyter-connection-file", str(sentinel)]]:
        process = subprocess.run([executable] + args, capture_output=True, timeout=20)
        assert process.returncode != 0, (args, process.stdout, process.stderr)
    assert sentinel.read_text() == "do not replace"
    sentinel.unlink()
    if os.name != "nt":
        os.chmod(root, 0o755)
        try:
            process = subprocess.run([executable, "--serve", "--connection-file",
                str(root / "insecure.json")], capture_output=True, timeout=20)
            assert process.returncode != 0 and b"private" in process.stderr
        finally:
            os.chmod(root, 0o700)
    dead = subprocess.run([executable, "--no-color", "--connection-file",
        str(root / "fatal-managed.json")], input=b"stop 7\r",
        capture_output=True, timeout=20, cwd=root)
    assert dead.returncode != 0 and b"unknown" in dead.stderr, (dead.stdout, dead.stderr)
    assert not (root / "fatal-managed.json").exists()
    inferred = subprocess.run([executable, "--no-color", "--infer", "--fast"],
        input=b"native_value = 40\rnative_value + 2\r\x04",
        capture_output=True, timeout=20, cwd=root)
    assert inferred.returncode == 0 and re.search(rb"(?m)^42\r?$", inferred.stdout), (
        inferred.stdout, inferred.stderr)
    separated = subprocess.run([executable, "--no-color"],
        input=b"write(0,*) 'terminal-stderr'\rprint *, 'terminal-stdout'\r"
              b"write(0,*) 'terminal-stderr-again'\r4 + 5\r\x04",
        capture_output=True, timeout=20, cwd=root)
    assert separated.returncode == 0 and b"terminal-stderr" in separated.stderr, (
        separated.stdout, separated.stderr)
    assert b"terminal-stderr-again" in separated.stderr, separated.stderr
    assert b"terminal-stdout" in separated.stdout, separated.stdout
    assert re.search(rb"(?m)^9\r?$", separated.stdout)
    # Managed terminal: actual shared state, another client remains after
    # initial terminal detaches, and the final peer triggers process exit.
    file = root / "managed.json"
    terminal = subprocess.Popen([executable, "--no-color", "--connection-file", str(file)],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=root)
    timer = threading.Timer(30, terminal.kill)
    timer.start()
    peer = None
    try:
        output = b""
        while b">>> " not in output:
            byte = terminal.stdout.read(1)
            assert byte, terminal.communicate(timeout=5)
            output += byte
        descriptor = json.loads(file.read_text())
        peer = Peer(descriptor)
        terminal.stdin.write(b"integer :: managed_value = 18\r")
        terminal.stdin.flush()
        # IOPub idle is the execution barrier, not prompt text or a sleep.
        submitted = peer.wait("execute_input")
        parent = submitted["parent_header"]["msg_id"]
        while peer.wait("status", parent)["content"]["execution_state"] != "idle":
            pass
        terminal.stdin.write(b"\x04")
        terminal.stdin.flush()
        stdout, stderr = terminal.communicate(timeout=10)
        assert terminal.returncode == 0, (stdout, stderr)
        request = peer.execute("managed_value + 1")
        assert peer.wait("execute_result", request)["content"]["data"]["text/plain"] == "19"
        peer.done(request)
        peer.close()
        # Failed attachment gives an explicit error, not a fresh evaluator.
        deadline = time.monotonic() + 10
        changed = threading.Event()
        while file.exists() and time.monotonic() < deadline:
            changed.wait(0.01)
        assert not file.exists(), "last detach did not clean managed session"
        assert sorted(path.name for path in root.iterdir()) == []
    finally:
        timer.cancel()
        if peer:
            peer.close()
        if terminal.poll() is None:
            terminal.kill()
            terminal.wait(timeout=10)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    args = parser.parse_args()
    executable = str(Path(args.lfortran).resolve())
    with tempfile.TemporaryDirectory(prefix="native-session-", dir=".") as directory:
        root = Path(directory).resolve() / "private"
        private_directory(root)
        with Server(executable, root) as server:
            a, b = shared_state(server)
            io_and_queue(server, a, b)
            print("shared state, correlation, streaming and input: ok", flush=True)
            flags_and_abort(a, b)
            rich_and_utf8(a)
            malformed(server)
            observer_editor(executable, root, server)
            print("flags, rich data, framing, UTF-8 and observer editor: ok", flush=True)
            a.close()
            b.close()
            slow_peer(server)
            print("slow peer isolation: ok", flush=True)
        shutdown_during_input(executable, root)
        shutdown_during_evaluation(executable, root)
        fatal_death(executable, root)
        security_and_cli(executable, root)
        print("shutdown, death, security and managed lifecycle: ok", flush=True)
    print("native session: ok")


if __name__ == "__main__":
    main()
