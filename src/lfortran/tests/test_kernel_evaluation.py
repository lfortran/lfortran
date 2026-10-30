"""Drive the optional Xeus kernel through jupyter_client: the Jupyter adapter
over FortranEvaluator::execute()."""

import argparse
import json
import os
from pathlib import Path
import queue
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

# Descriptors the kernel may have open. It needs about 60; a leak of a
# descriptor per cell exhausts them within the cells below.
MAX_OPEN_FILES = 128


def limit_open_files():
    import resource
    _, hard = resource.getrlimit(resource.RLIMIT_NOFILE)
    resource.setrlimit(resource.RLIMIT_NOFILE, (MAX_OPEN_FILES, hard))


def execute(client, code, *, silent=False):
    msg_id = client.execute(code, silent=silent, allow_stdin=False)
    messages = []
    deadline = time.monotonic() + 30
    while True:
        if time.monotonic() >= deadline:
            raise AssertionError("kernel did not become idle")
        msg = client.get_iopub_msg(timeout=max(0.1, deadline - time.monotonic()))
        if msg["parent_header"].get("msg_id") != msg_id:
            continue
        messages.append(msg)
        if msg["msg_type"] == "status" and msg["content"]["execution_state"] == "idle":
            break
    deadline = time.monotonic() + 30
    while True:
        if time.monotonic() >= deadline:
            raise AssertionError("kernel did not reply")
        reply = client.get_shell_msg(timeout=max(0.1, deadline - time.monotonic()))
        if reply["parent_header"].get("msg_id") == msg_id:
            break
    return reply["content"], messages


def check(client, code, expected):
    reply, messages = execute(client, code)
    assert reply["status"] == "ok", reply
    values = [m["content"]["data"]["text/plain"] for m in messages
              if m["msg_type"] == "execute_result"]
    assert values == expected, (code, values, messages)
    return messages


def check_interruption(client, directory):
    check(client, "integer :: saved_interrupt = 41", [])
    check(client, "integer function interrupt_plus(i)\ninteger, intent(in) :: i\n"
          "interrupt_plus=i+1\nend function", [])
    check_command_interruption(client, directory)
    for _ in range(2 if os.name == "posix" else 0):
        started = time.monotonic()
        messages = interrupt_cell(client,
            "call execute_command_line('sleep 4')\nprint *, 'command-done'")
        assert time.monotonic() - started < 2.5, "command cancellation waited for sleep"
        assert not any(m["msg_type"] == "stream" and "command-done" in m["content"]["text"]
                       for m in messages), messages
        check(client, "interrupt_plus(saved_interrupt)", ["42"])
    # Interruption is an error: only the interrupted request's False allows
    # already queued cells to run, regardless of the next request's default.
    msg_id = client.execute("do\nend do", stop_on_error=False)
    deadline = time.monotonic() + 10
    while True:
        msg = client.get_iopub_msg(timeout=max(0.1, deadline - time.monotonic()))
        if (msg["parent_header"].get("msg_id") == msg_id and
                msg["msg_type"] == "execute_input"):
            break
        assert time.monotonic() < deadline, "loop did not start"
    queued = client.execute("interrupt_plus(saved_interrupt)")
    time.sleep(0.2)
    request = client.session.msg("interrupt_request")
    client.control_channel.send(request)
    replies = {}
    deadline = time.monotonic() + 4
    while len(replies) != 2 and time.monotonic() < deadline:
        try:
            msg = client.get_shell_msg(timeout=0.1)
        except queue.Empty:
            continue
        parent = msg["parent_header"].get("msg_id")
        if parent in (msg_id, queued):
            replies[parent] = msg["content"]
    assert msg_id in replies, "running silent loop ignored interrupt_request"
    assert replies[msg_id]["status"] == "error", replies
    assert replies[msg_id]["ename"] == "KeyboardInterrupt", replies
    assert queued in replies and replies[queued]["status"] == "ok", replies
    idle = set()
    values = []
    deadline = time.monotonic() + 4
    while len(idle) != 2 and time.monotonic() < deadline:
        try:
            msg = client.get_iopub_msg(timeout=0.1)
        except queue.Empty:
            continue
        parent = msg["parent_header"].get("msg_id")
        if parent not in (msg_id, queued):
            continue
        if msg["msg_type"] == "status" and msg["content"]["execution_state"] == "idle":
            idle.add(parent)
        if parent == queued and msg["msg_type"] == "execute_result":
            values.append(msg["content"]["data"]["text/plain"])
    assert idle == {msg_id, queued}, idle
    assert values == ["42"], values
    reply = client.get_control_msg(timeout=4)
    assert reply["parent_header"].get("msg_id") == request["header"]["msg_id"], reply
    assert reply["msg_type"] == "interrupt_reply", reply
    check(client, "interrupt_plus(saved_interrupt)", ["42"])
    for silent in (False, True):
        interrupt_cell(client, "saved_interrupt=43\ndo\nend do", silent=silent)
        check(client, "saved_interrupt", ["43"])
    check(client, "subroutine interrupt_spin()\ndo\nend do\nend subroutine", [])
    interrupt_cell(client, "call interrupt_spin()")
    messages = interrupt_cell(client, "print '(a)', 'before-interrupt'\ndo\nend do",
                              started_text="before-interrupt\n")
    assert "".join(m["content"]["text"] for m in messages if m["msg_type"] == "stream"
                   and m["content"]["name"] == "stdout") == "before-interrupt\n", messages
    # Idle and repeated requests must not poison the next cell.
    for _ in range(2):
        request = client.session.msg("interrupt_request")
        client.control_channel.send(request)
        reply = client.get_control_msg(timeout=4)
        assert reply["parent_header"]["msg_id"] == request["header"]["msg_id"], reply
    check(client, "interrupt_plus(saved_interrupt)", ["44"])
    check(client, "integer :: interrupt_read = 17, interrupt_ios", [])
    for code in ("read(*,*,iostat=interrupt_ios) interrupt_read",
                 "read *, interrupt_read",
                 "block\ncharacter(20) :: text\nread(*,'(a)') text\nend block",
                 "block\nreal :: r\nread(*,*,decimal='comma') r\nend block",
                 "read(*,*)"):
        old_prompt = interrupt_cell(client, code, reading=True)
        check(client, "interrupt_read", ["17"])
        printed = check(client, "print '(f3.1)', 1.5", [])
        assert "".join(m["content"]["text"] for m in printed
                       if m["msg_type"] == "stream") == "1.5\n", printed
    # A fresh READ accepts input after cancellation and runtime stdin reset.
    msg_id = client.execute("read *, interrupt_read", allow_stdin=True)
    prompt = client.get_stdin_msg(timeout=5)
    assert prompt["parent_header"]["msg_id"] == msg_id, prompt
    stale = client.session.msg("input_reply", content={"value": "99"}, parent=old_prompt)
    client.stdin_channel.send(stale)
    client.input("29")
    reply, _ = finish_cell(client, msg_id)
    assert reply["status"] == "ok", reply
    check(client, "interrupt_read", ["29"])
    check(client, """module interrupt_final_input
integer :: final_read = 0
type :: final_reader
contains
final :: finish_reader
end type
contains
subroutine finish_reader(x)
type(final_reader), intent(inout) :: x
read *, final_read
end subroutine
end module
use interrupt_final_input""", [])
    msg_id = client.execute("block\ntype(final_reader) :: x\nend block\ndo\nend do",
                            allow_stdin=True, stop_on_error=False)
    prompt = client.get_stdin_msg(timeout=6)
    assert prompt["content"]["lfortran_interruptible"] is False, prompt
    request = client.session.msg("interrupt_request")
    client.control_channel.send(request)
    response = client.get_control_msg(timeout=4)
    assert response["parent_header"]["msg_id"] == request["header"]["msg_id"], response
    try:
        premature = client.get_shell_msg(timeout=0.2)
    except queue.Empty:
        pass
    else:
        raise AssertionError(("protected READ was abandoned", premature))
    client.input("7")
    reply, _ = finish_cell(client, msg_id)
    assert reply["status"] == "error" and reply["ename"] == "KeyboardInterrupt", reply
    check(client, "final_read", ["7"])


def finish_cell(client, msg_id):
    messages = []
    deadline = time.monotonic() + 6
    while time.monotonic() < deadline:
        msg = client.get_iopub_msg(timeout=max(0.1, deadline - time.monotonic()))
        if msg["parent_header"].get("msg_id") != msg_id:
            continue
        messages.append(msg)
        if msg["msg_type"] == "status" and msg["content"]["execution_state"] == "idle":
            break
    else:
        raise AssertionError("interrupted cell did not become idle")
    reply = client.get_shell_msg(timeout=4)
    assert reply["parent_header"]["msg_id"] == msg_id, reply
    return reply["content"], messages


def interrupt_cell(client, code, *, silent=False, reading=False, started_text=None):
    msg_id = client.execute(code, silent=silent, allow_stdin=reading, stop_on_error=False)
    preceding = []
    if reading:
        try:
            prompt = client.get_stdin_msg(timeout=6)
        except queue.Empty:
            reply = client.get_shell_msg(timeout=1)
            seen = []
            while client.iopub_channel.msg_ready():
                seen.append(client.get_iopub_msg(timeout=1))
            raise AssertionError((code, "READ did not ask for input", reply, seen))
        assert prompt["parent_header"]["msg_id"] == msg_id, prompt
    elif started_text:
        deadline = time.monotonic() + 6
        while time.monotonic() < deadline:
            msg = client.get_iopub_msg(timeout=max(0.1, deadline - time.monotonic()))
            if msg["parent_header"].get("msg_id") != msg_id:
                continue
            preceding.append(msg)
            if (msg["msg_type"] == "stream"
                    and started_text in msg["content"]["text"].replace("\r\n", "\n")):
                break
        else:
            raise AssertionError("cell did not stream before interruption")
    else:
        # A silent cell has no execute_input notification.
        time.sleep(0.2)
    deferred = client.session.msg("kernel_info_request")
    client.control_channel.send(deferred)
    with_control = client.control_channel.socket.poll(100)
    assert not with_control, "non-interrupt control request ran during execution"
    interrupts = [client.session.msg("interrupt_request") for _ in range(2)]
    for request in interrupts:
        client.control_channel.send(request)
    reply, messages = finish_cell(client, msg_id)
    messages = preceding + messages
    assert reply["status"] == "error" and reply["ename"] == "KeyboardInterrupt", reply
    assert not any(m["msg_type"] == "execute_result" for m in messages), messages
    if not silent:
        assert [m["msg_type"] for m in messages][-2:] == ["error", "status"], messages
    replies = [client.get_control_msg(timeout=4) for _ in range(3)]
    assert {m["parent_header"]["msg_id"] for m in replies} == {
        deferred["header"]["msg_id"], *(m["header"]["msg_id"] for m in interrupts)}, replies
    return prompt if reading else messages


def waiter_command(waiter, *paths):
    command = " ".join('"%s"' % path.as_posix()
                       for path in (Path(sys.executable), waiter, *paths))
    if os.name == "nt":
        # cmd /c removes the outer quotes before parsing the quoted paths.
        command = '"' + command + '"'
    return command


def process_running(pid):
    if os.name == "nt":
        import ctypes
        api = ctypes.WinDLL("kernel32", use_last_error=True)
        api.OpenProcess.restype = ctypes.c_void_p
        api.OpenProcess.argtypes = [ctypes.c_ulong, ctypes.c_int, ctypes.c_ulong]
        api.WaitForSingleObject.argtypes = [ctypes.c_void_p, ctypes.c_ulong]
        api.CloseHandle.argtypes = [ctypes.c_void_p]
        handle = api.OpenProcess(0x100000, False, pid)  # SYNCHRONIZE
        if not handle:
            return ctypes.get_last_error() != 87  # ERROR_INVALID_PARAMETER
        try:
            return api.WaitForSingleObject(handle, 0) == 258  # WAIT_TIMEOUT
        finally:
            api.CloseHandle(handle)
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    # Orphaned grandchildren are reaped by the OS, not by the kernel process.
    stat = Path("/proc") / str(pid) / "stat"
    if sys.platform.startswith("linux") and stat.exists():
        try:
            return stat.read_text().split(") ", 1)[1][0] != "Z"
        except FileNotFoundError:
            return False
    return True


def check_command_interruption(client, directory):
    # Quoted paths and a shell -> Python -> Python tree, not just shell exec
    # replacing itself with a single sleep process.
    command_dir = directory / "owned command"
    command_dir.mkdir()
    helper = command_dir / "owned child.py"
    pids = command_dir / "pids.json"
    helper.write_text(
        "import json, os, subprocess, sys, time\n"
        "from pathlib import Path\n"
        "child = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(30)'])\n"
        "Path(sys.argv[1]).write_text(json.dumps([os.getpid(), child.pid]))\n"
        "sys.stdout.buffer.write(b'owned-command-started\\r\\n')\n"
        "sys.stdout.buffer.flush()\n"
        "child.wait()\n")
    command = waiter_command(helper, pids).replace("'", "''")
    owned = []
    unrelated = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
    try:
        for _ in range(2):
            started = time.monotonic()
            messages = interrupt_cell(client,
                "call execute_command_line('" + command + "')\nprint *, 'command-done'",
                started_text="owned-command-started\n")
            assert time.monotonic() - started < 2.5, "owned command ignored interrupt"
            owned = json.loads(pids.read_text())
            deadline = time.monotonic() + 2
            while any(process_running(pid) for pid in owned) and time.monotonic() < deadline:
                time.sleep(0.01)
            assert not any(process_running(pid) for pid in owned), ("surviving children", owned)
            assert unrelated.poll() is None, "command cancellation stopped an unrelated process"
            assert not any(m["msg_type"] == "stream" and "command-done" in m["content"]["text"]
                           for m in messages), messages
            check(client, "interrupt_plus(saved_interrupt)", ["42"])
    finally:
        unrelated.terminate()
        unrelated.wait(timeout=10)
        if pids.exists():
            owned = json.loads(pids.read_text())
        for pid in reversed(owned):
            if process_running(pid):
                import signal
                os.kill(pid, signal.SIGTERM)
        pids.unlink(missing_ok=True)
        helper.unlink()
        command_dir.rmdir()
    messages = check(client,
        "integer :: command_exit, command_status\n"
        "call execute_command_line('echo command-stdout', exitstat=command_exit, cmdstat=command_status)\n"
        "if (command_exit /= 0 .or. command_status /= 0) error stop\n"
        "call execute_command_line('echo command-stderr >&2')\n"
        "call execute_command_line('exit 7', exitstat=command_exit, cmdstat=command_status)\n"
        "if (command_exit /= 7 .or. command_status /= 0) error stop\n"
        "call execute_command_line('exit 0', wait=.false., cmdstat=command_status)\n"
        "if (command_status /= 0) error stop", [])
    for name in ("stdout", "stderr"):
        assert any(m["msg_type"] == "stream" and m["content"]["name"] == name
                   and "command-" + name in m["content"]["text"] for m in messages), messages
    if os.name == "posix":
        check(client,
            "block\ninteger :: es, cs\ncharacter(40) :: msg\n"
            "call execute_command_line('LFORTRAN_NO_SUCH_COMMAND_16183 2>/dev/null', &\n"
            "exitstat=es, cmdstat=cs, cmdmsg=msg)\n"
            "if (es /= 127 .or. cs /= 3) error stop\n"
            "if (trim(msg) /= 'Invalid command line') error stop\nend block", [])


def streaming_code(gate, partial=False):
    first = ("write(*,'(a)',advance='no')" if partial else "print '(a)',")
    command = waiter_command(gate.with_name("wait.py"), gate)
    argument = " // &\n".join("    '" + command[i:i+60].replace("'", "''") + "'"
        for i in range(0, len(command), 60))
    return ("program streaming_gate%s\n" % ("_partial" if partial else "") +
            first + " 'before-gate'\n"
            "call execute_command_line( &\n" + argument + ")\n"
            "print '(a)', 'after-gate'\nend program")


def check_streaming(client, directory, partial=False):
    gate = directory / "release"
    # No READ or display hook may flush the first PRINT for us.
    code = streaming_code(gate, partial)
    expected = "before-gate" + ("" if partial else "\n")
    msg_id = client.execute(code, allow_stdin=False)
    text = ""
    seen = []
    try:
        deadline = time.monotonic() + 10
        while expected not in text:
            try:
                msg = client.get_iopub_msg(timeout=max(0.1, deadline - time.monotonic()))
            except queue.Empty:
                raise AssertionError("output did not arrive before gate release: %r" % seen)
            if msg["parent_header"].get("msg_id") != msg_id:
                continue
            seen.append(msg)
            assert msg["msg_type"] != "error", msg
            assert not (msg["msg_type"] == "status" and
                        msg["content"]["execution_state"] == "idle"), msg
            if msg["msg_type"] == "stream":
                text += msg["content"]["text"]
            assert time.monotonic() < deadline, "output did not arrive before gate release"
        assert text == expected, text
    finally:
        gate.touch()
        if sys.exc_info()[0] is not None:
            # Reap the command-line waiter before deleting its gate.
            try:
                reply = client.get_shell_msg(timeout=5)
                print("released gate after failure; execute_reply:", reply["content"])
            except queue.Empty:
                pass
    while True:
        msg = client.get_iopub_msg(timeout=30)
        if msg["parent_header"].get("msg_id") != msg_id:
            continue
        if msg["msg_type"] == "stream":
            text += msg["content"]["text"]
        if msg["msg_type"] == "status" and msg["content"]["execution_state"] == "idle":
            break
    assert text == expected + "after-gate\n", text
    reply = client.get_shell_msg(timeout=30)
    assert reply["parent_header"]["msg_id"] == msg_id, reply
    assert reply["content"]["status"] == "ok", reply
    gate.unlink()


def check_transport_progress(client, directory):
    import zmq

    gate = directory / "transport-release"
    waiting = directory / "transport-waiting"
    waiter = directory / "transport-wait.py"
    waiter.write_text("import sys, time\nfrom pathlib import Path\n"
                      "Path(sys.argv[2]).touch()\n"
                      "while not Path(sys.argv[1]).exists(): time.sleep(0.01)\n")
    subscriber = client.context.socket(zmq.SUB)
    subscriber.setsockopt(zmq.RCVHWM, 1)
    subscriber.setsockopt(zmq.RCVBUF, 65536)
    subscriber.setsockopt(zmq.SUBSCRIBE, b"")
    subscriber.connect("tcp://127.0.0.1:%d" % client.iopub_port)
    session = client.session.clone()
    msg_id = None
    try:
        time.sleep(0.3)
        command = waiter_command(waiter, gate, waiting)
        argument = " // &\n".join("    '" + command[i:i+60].replace("'", "''") + "'"
            for i in range(0, len(command), 60))
        code = ("program transport_progress\ninteger :: j\n"
                "do j=1,1024\nprint '(a)', repeat('x',4095)\nend do\n"
                "print '(a)', 'TRANSPORT-END'\n"
                "call execute_command_line( &\n" + argument + ")\nend program")
        msg_id = client.execute(code, allow_stdin=False)
        deadline = time.monotonic() + 30
        while not waiting.exists() and time.monotonic() < deadline:
            time.sleep(0.01)
        assert waiting.exists(), "cell did not reach transport gate"
        # Let capture pumping reach the end while this subscriber is paused.
        time.sleep(2)
        text = ""
        deadline = time.monotonic() + 10
        while "TRANSPORT-END\n" not in text and time.monotonic() < deadline:
            if not subscriber.poll(100):
                continue
            _, parts = session.feed_identities(subscriber.recv_multipart())
            msg = session.deserialize(parts)
            if msg["parent_header"].get("msg_id") != msg_id:
                continue
            assert msg["msg_type"] != "error", msg
            assert not (msg["msg_type"] == "status" and
                        msg["content"]["execution_state"] == "idle"), msg
            if msg["msg_type"] == "stream":
                text += msg["content"]["text"]
        assert text == ("x" * 4095 + "\n") * 1024 + "TRANSPORT-END\n", (
            "queued output stalled before gate release", len(text))
    finally:
        gate.touch()
        try:
            if msg_id is not None:
                reply = client.get_shell_msg(timeout=15)
                assert reply["parent_header"]["msg_id"] == msg_id, reply
                assert reply["content"]["status"] == "ok", reply
        finally:
            subscriber.close(linger=0)
            for path in (gate, waiting, waiter):
                path.unlink(missing_ok=True)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--streaming-only", action="store_true")
    parser.add_argument("--interrupt-only", action="store_true")
    parser.add_argument("--adapter", choices=("both", "default", "builtin"), default="both")
    parser.add_argument("--mode", choices=("normal", "fast", "f23"), default="normal")
    args = parser.parse_args()
    assert args.streaming_only or args.interrupt_only or args.mode == "normal"
    adapters = [False, True] if builtin_stop_kernel(args.lfortran) else [False]
    if args.adapter != "both":
        adapters = [args.adapter == "builtin"]
    for managed in adapters:
        run_kernel(args.lfortran, managed, args.streaming_only, args.mode, args.interrupt_only)
        if args.streaming_only or args.interrupt_only:
            continue
        for code, status, shown in [("if (.true.) stop", 0, ""), ("stop 3", 3, "STOP 3"),
                ("error stop", 1, "ERROR STOP")]:
            check_stop_flushes(args.lfortran, code, status, shown, managed)


def run_kernel(lfortran, managed, streaming_only=False, mode="normal", interrupt_only=False):
    directory = Path.cwd() / ("kernel-evaluation-" + uuid.uuid4().hex)
    directory.mkdir(mode=0o700)
    connection = directory / "connection.json"
    log = directory / "kernel.log"
    waiter = directory / "wait.py"
    waiter.write_text("import sys, time\nfrom pathlib import Path\n"
                      "while not Path(sys.argv[1]).exists(): time.sleep(0.01)\n")
    client = None
    process = None
    try:
        write_connection_file(fname=str(connection), key=uuid.uuid4().hex.encode("ascii"))
        with log.open("wb") as output:
            options = {"normal": [], "fast": ["--fast"], "f23": ["--std=f23"]}[mode]
            process = subprocess.Popen(
                [lfortran] + options + ["kernel", "-f", str(connection)] +
                (["--managed"] if managed else []),
                stdin=subprocess.DEVNULL, stdout=output, stderr=output,
                preexec_fn=limit_open_files if os.name == "posix" else None,
            )
            client = BlockingKernelClient(connection_file=str(connection))
            client.load_connection_file()
            client.start_channels()
            client.wait_for_ready(timeout=30)
            if not streaming_only:
                check_interruption(client, directory)
            if interrupt_only:
                client.shutdown()
                process.wait(timeout=15)
                assert process.returncode == 0, process.returncode
                print(("built-in" if managed else "default") + " interruption " + mode + ": passed")
                return
            check_streaming(client, directory)
            check_streaming(client, directory, partial=True)
            if managed:
                check_transport_progress(client, directory)
            if streaming_only:
                client.shutdown()
                process.wait(timeout=15)
                assert process.returncode == 0, process.returncode
                print(("built-in" if managed else "default") + " streaming " + mode + ": passed")
                return
            check(client, "integer :: n\nn=41", [])
            check(client, "n+1", ["42"])
            check(client, ".true.", ["True"])
            check(client, "'hello'", ["hello"])
            check(client, "1.2345678901234567_8", ["1.2345678901234567"])
            check(client, "1.0", ["1.0"])
            check(client, "0.1_8", ["0.1"])
            messages = check(client, "max0(1, 2_8)", ["2"])
            assert any(m["msg_type"] == "stream" and m["content"]["name"] == "stderr"
                       and "Different kinds of args in max0" in m["content"]["text"]
                       for m in messages), messages
            for magic in ("ast", "asr", "llvm", "asm", "cpp", "fmt"):
                for first_line in ("%%show" + magic, "%%show" + magic + " \r"):
                    messages = check(client, first_line + "\ninteger :: x", [])
                    assert any(m["msg_type"] == "stream" and m["content"]["text"]
                               for m in messages), (magic, messages)
            messages = check(client, "use lfortran_display; "
                             "print *, 'before-display'; "
                             "call display_data('text/html','<b>hello</b>'); "
                             "call clear_output()", [])
            assert any(m["msg_type"] == "display_data" and
                       m["content"]["data"]["text/html"] == "<b>hello</b>"
                       for m in messages), messages
            assert any(m["msg_type"] == "clear_output" for m in messages), messages
            kinds = [m["msg_type"] for m in messages]
            assert kinds.index("stream") < kinds.index("display_data") < kinds.index("clear_output")
            check(client, "program unavailable\ninteger :: i,ios\n"
                  "read(*,*,iostat=ios) i\nif(ios<=0) error stop\nend program", [])
            for code in ("n+1", "print *, 'silent stream'",
                         "call display_data('text/html', '<b>silent</b>')"):
                reply, messages = execute(client, code, silent=True)
                assert reply["status"] == "ok", reply
                assert not any(m["msg_type"] in ("execute_result", "stream",
                    "display_data") for m in messages), messages
            reply, _ = execute(client, "integer :: broken(")
            assert reply["status"] == "error" and reply["ename"] == "CompilerError", reply
            # An inspection cell does not add its declarations to the session.
            check(client, "%%showasr\ninteger :: inspected", [])
            reply, _ = execute(client, "inspected")
            assert reply["status"] == "error" and reply["ename"] == "CompilerError", reply
            # An internal compiler error: the message in evalue, the stack
            # trace in traceback. Array results are not supported yet (#13456).
            reply, _ = execute(client, "[1, 2, 3]")
            assert reply["status"] == "error", reply
            assert reply["evalue"] == "Return type not supported in interactive mode", reply
            # Text that is not UTF-8 cannot go into a Jupyter message.
            check(client, "achar(200)", ["\ufffd"])
            messages = check(client, "print *, 'a', achar(200), 'b'", [])
            assert any(m["msg_type"] == "stream" and "\ufffd" in m["content"]["text"]
                       for m in messages), messages
            # All of a cell's stdout, however long, and its stderr.
            messages = check(client, "program long_output\ninteger :: i\n"
                             "do i = 1, 3000\nprint '(a)', repeat('x', 39)\nend do\n"
                             "write(0, '(a)') 'to stderr'\nend program", [])
            streams = {"stdout": "", "stderr": ""}
            for m in messages:
                if m["msg_type"] == "stream":
                    streams[m["content"]["name"]] += m["content"]["text"]
            assert streams["stdout"] == ("x" * 39 + "\n") * 3000, len(streams["stdout"])
            assert streams["stderr"] == "to stderr\n", streams["stderr"]
            messages = check(client,
                "program utf8_chunks\nuse iso_fortran_env, only: error_unit\n"
                "print '(a)', repeat('x',4095)//achar(226)//achar(130)//achar(172)\n"
                "write(error_unit,'(a)') repeat('y',4094)//achar(240)//achar(159)//achar(152)//achar(128)\n"
                "end program", [])
            streams = {"stdout": "", "stderr": ""}
            for msg in messages:
                if msg["msg_type"] == "stream":
                    streams[msg["content"]["name"]] += msg["content"]["text"]
            assert streams["stdout"] == "x" * 4095 + "\u20ac\n", streams["stdout"]
            assert streams["stderr"] == "y" * 4094 + "\U0001f600\n", streams["stderr"]
            # Cells must not leak file descriptors (MAX_OPEN_FILES).
            for i in range(100):
                check(client, "%d + 1" % i, [str(i + 1)])
            client.shutdown()
            process.wait(timeout=15)
            assert process.returncode == 0, process.returncode
            print(("built-in" if managed else "default") + " native kernel: passed")
    except BaseException:
        if log.exists():
            print(log.read_text(errors="replace"))
        raise
    finally:
        if client:
            client.stop_channels()
        if process and process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=10)
        connection.unlink(missing_ok=True)
        (directory / "release").unlink(missing_ok=True)
        waiter.unlink(missing_ok=True)
        log.unlink(missing_ok=True)
        directory.rmdir()


def builtin_stop_kernel(lfortran):
    """True when `lfortran kernel --managed` is the built-in kernel.

    That kernel flushes its subscribers from an atexit handler, so stdout
    written before STOP is delivered. A Xeus kernel publishes the same text
    but does not flush it before the process ends. Builds from before the
    flag existed have no built-in kernel to ask.
    """
    help_text = subprocess.run(
        [lfortran, "kernel", "--help"],
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
    ).stdout
    return "--managed" in help_text


def check_stop_flushes(lfortran, stop, status, shown, managed):
    """STOP and ERROR STOP end the kernel with their exit status. On the
    built-in kernel, preceding output and STOP diagnostics are delivered.
    On Windows, STOP must call the host exit() so the atexit flush runs."""
    directory = Path.cwd() / ("kernel-stop-" + uuid.uuid4().hex)
    directory.mkdir(mode=0o700)
    connection = directory / "connection.json"
    log = directory / "kernel.log"
    client = None
    process = None
    try:
        write_connection_file(fname=str(connection), key=uuid.uuid4().hex.encode("ascii"))
        command = [lfortran, "kernel", "-f", str(connection)]
        if managed:
            command.append("--managed")
        with log.open("wb") as output:
            process = subprocess.Popen(
                command,
                stdin=subprocess.DEVNULL, stdout=output, stderr=output,
            )
            client = BlockingKernelClient(connection_file=str(connection))
            client.load_connection_file()
            client.start_channels()
            client.wait_for_ready(timeout=30)
            msg_id = client.execute(
                "print *, 'before-stop'\n" + stop + "\nprint *, 'after-stop'")
            texts = []
            deadline = time.monotonic() + 20
            exited = None
            while time.monotonic() < deadline:
                if process.poll() is not None and exited is None:
                    exited = time.monotonic()
                # The process exiting does not by itself deliver the last
                # messages, so keep reading briefly after it goes.
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
                parent = (msg.get("parent_header") or {}).get("msg_id")
                if parent != msg_id or msg["msg_type"] != "stream":
                    continue
                texts.append(msg["content"].get("text", ""))
            code = process.wait(timeout=10)
            blob = "".join(texts)
            assert code == status, (code, status, blob, log.read_text(errors="replace"))
            if managed:
                assert "before-stop" in blob, blob
                assert shown in blob, (stop, blob)
                assert "after-stop" not in blob, blob
    except BaseException:
        if log.exists():
            print(log.read_text(errors="replace"))
        raise
    finally:
        if client:
            client.stop_channels()
        if process and process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=10)
        connection.unlink(missing_ok=True)
        log.unlink(missing_ok=True)
        directory.rmdir()


if __name__ == "__main__":
    main()
