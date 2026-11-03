"""Drive the optional Xeus kernel through jupyter_client: the Jupyter adapter
over FortranEvaluator::execute()."""

import argparse
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

from test_session import wait_for_ready

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


def waiter_command(waiter, *paths):
    command = " ".join('"%s"' % path.as_posix()
                       for path in (Path(sys.executable), waiter, *paths))
    if os.name == "nt":
        # cmd /c removes the outer quotes before parsing the quoted paths.
        command = '"' + command + '"'
    return command


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


def check_transport_progress(client, directory, process):
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
        # Once the subscription is in place, the kernel's statuses reach it.
        probes = []
        deadline = time.monotonic() + 30
        while not subscriber.poll(50):
            assert time.monotonic() < deadline, "the subscriber got no message"
            probes.append(client.kernel_info())
        replies = set()
        while not replies.issuperset(probes):
            replies.add(client.get_shell_msg(timeout=15)["parent_header"].get("msg_id"))
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
        time.sleep(0.2)
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


def check_values(client, directory, process):
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


def check_errors(client, directory, process):
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


def check_output(client, directory, process):
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


def check_descriptors(client, directory, process):
    """Cells must not leak file descriptors: where the kernel's descriptors
    can be listed, 10 cells do not add 5 (a cell may leave one open for a
    while), else the kernel does not run out of MAX_OPEN_FILES."""
    descriptors = Path("/proc/%d/fd" % process.pid)
    if not descriptors.is_dir():
        for i in range(100):
            check(client, "%d + 1" % i, [str(i + 1)])
        return
    check(client, "0 + 1", ["1"])
    before = len(list(descriptors.iterdir()))
    for i in range(10):
        check(client, "%d + 1" % i, [str(i + 1)])
    after = len(list(descriptors.iterdir()))
    assert after < before + 5, "%d descriptors before 10 cells, %d after" % (before, after)


def check_streams(client, directory, process):
    check_streaming(client, directory)
    check_streaming(client, directory, partial=True)


def in_kernel(checks):
    return lambda lfortran, managed, mode: run_kernel(lfortran, managed, checks, mode)


def stopping(stop, status, shown):
    return lambda lfortran, managed, mode: check_stop_flushes(
        lfortran, stop, status, shown, managed)


CASES = {"streaming": in_kernel(check_streams),
         "transport": in_kernel(check_transport_progress),
         "values": in_kernel(check_values), "errors": in_kernel(check_errors),
         "output": in_kernel(check_output), "descriptors": in_kernel(check_descriptors),
         "stop": stopping("if (.true.) stop", 0, ""),
         "stop_code": stopping("stop 3", 3, "STOP 3"),
         "error_stop": stopping("error stop", 1, "ERROR STOP")}
# Cases for only `lfortran kernel --managed`
MANAGED_ONLY = {"transport"}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--case", choices=CASES, action="append",
        help="run only this case (repeatable)")
    parser.add_argument("--managed", action="store_true",
        help="only on `lfortran kernel --managed`, else only without it")
    parser.add_argument("--streaming-only", action="store_true",
        help="only the streaming cases")
    parser.add_argument("--mode", choices=("normal", "fast", "f23"), default="normal")
    parser.add_argument("--all-cases", nargs="+", metavar="CASE",
        help="only check that these are all the cases")
    args = parser.parse_args()
    cases = (["streaming", "transport"] if args.streaming_only
             else args.case or list(CASES))
    if args.all_cases:
        assert sorted(args.all_cases) == sorted(cases), \
            "the cases are %s, not %s" % (sorted(cases), sorted(args.all_cases))
        return
    assert args.mode == "normal" or set(cases) <= {"streaming", "transport"}, \
        "modes apply to the focused streaming checks"
    adapters = [args.managed] if args.case or args.managed else [False, True]
    if True in adapters and not builtin_stop_kernel(args.lfortran):
        if adapters == [True]:
            print("this kernel has no --managed; skipping")
            sys.exit(77)  # SKIP_RETURN_CODE in CMakeLists.txt
        adapters.remove(True)
    for managed in adapters:
        for case in cases:
            if case in MANAGED_ONLY and not managed:
                continue
            CASES[case](args.lfortran, managed, args.mode)
            print(("built-in " if managed else "default ") + case
                  + ("" if args.mode == "normal" else " " + args.mode) + ": passed")


def run_kernel(lfortran, managed, checks, mode="normal"):
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
            wait_for_ready(client, timeout=30)
            checks(client, directory, process)
            client.shutdown()
            process.wait(timeout=15)
            assert process.returncode == 0, process.returncode
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
            wait_for_ready(client, timeout=30)
            msg_id = client.execute(
                "print *, 'before-stop'\n" + stop + "\nprint *, 'after-stop'")
            texts = []
            deadline = time.monotonic() + 20
            exited = None
            while time.monotonic() < deadline:
                if process.poll() is not None and exited is None:
                    exited = time.monotonic()
                # The process exiting does not by itself deliver the last
                # messages, so keep reading briefly after it goes, until
                # what is checked has come.
                if exited is not None and (time.monotonic() > exited + 1 or not managed
                        or "before-stop" in "".join(texts) and shown in "".join(texts)):
                    break
                try:
                    msg = client.get_iopub_msg(timeout=0.02)
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
