"""Drive the optional Xeus kernel through jupyter_client: the Jupyter adapter
over FortranEvaluator::execute()."""

import argparse
import os
from pathlib import Path
import queue
import shlex
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

# Descriptors the kernel may have open. It needs about 60; a leak of a
# descriptor per cell exhausts them within the cells below.
MAX_OPEN_FILES = 128


def limit_open_files():
    import resource
    _, hard = resource.getrlimit(resource.RLIMIT_NOFILE)
    resource.setrlimit(resource.RLIMIT_NOFILE, (MAX_OPEN_FILES, hard))


def execute(client, code, *, silent=False, on_message=None):
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
        if on_message:
            on_message(msg)
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


def check_live_output(client):
    started = time.monotonic()
    arrivals = []

    def received(msg):
        if msg["msg_type"] == "stream":
            arrivals.append((time.monotonic() - started, msg["content"]))

    delay = ("call execute_command_line('sleep 2')\n" if os.name == "posix" else
             "block\ninteger(8) :: start, now, rate\ncall system_clock(start, rate)\n"
             "do\ncall system_clock(now)\nif(now-start>=2*rate) exit\nend do\nend block\n")
    reply, messages = execute(client,
        "program live_burst\ninteger :: i\nprint '(a)', 'first'\n"
        "print '(a)', 'tail'\n" + delay +
        "do i=1,3000\nprint '(i0)', i\nend do\n"
        "write(0,'(a)') 'stderr'\nprint '(a)', 'last'\nend program",
        on_message=received)
    assert reply["status"] == "ok", reply
    prefix = "first\ntail\n" + "".join(f"{i}\n" for i in range(1, 3001))
    expected = prefix + "last\n"
    stdout = "".join(c["text"] for _, c in arrivals if c["name"] == "stdout")
    assert stdout == expected, (len(stdout), len(expected))
    early = "".join(c["text"] for t, c in arrivals if t < 1.5)
    assert early == "first\ntail\n", arrivals[:2]
    events = [(c["name"], char) for _, c in arrivals for char in c["text"]]
    wanted = [(name, char) for name, text in [
        ("stdout", prefix), ("stderr", "stderr\n"), ("stdout", "last\n")]
        for char in text]
    assert events == wanted, (len(events), len(wanted))
    assert messages[-1]["content"]["execution_state"] == "idle", messages[-1]
    print("live output: first/tail", arrivals[:2], "burst bytes", len(stdout),
          "messages", len(arrivals), flush=True)

    # Coalescing stdout alone is insufficient: alternating streams cannot
    # be merged without destroying their labels and chronological order.
    reply, messages = execute(client,
        "program alternating_burst\ninteger :: i\ndo i=1,3000\n"
        "print '(i0)', i\nwrite(0,'(i0)') i\nend do\nend program")
    assert reply["status"] == "ok", reply
    actual = [(m["content"]["name"], char) for m in messages
              if m["msg_type"] == "stream" for char in m["content"]["text"]]
    expected = [(name, char) for i in range(1, 3001)
                for name in ("stdout", "stderr") for char in f"{i}\n"]
    assert actual == expected, (len(actual), len(expected))
    print("alternating output: ordered bytes", len(actual), flush=True)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--compiler-arg", action="append", default=[])
    parser.add_argument("--streaming-only", action="store_true")
    parser.add_argument("--interrupt-only", action="store_true")
    parser.add_argument("--kernel-arg", action="append", default=[])
    args = parser.parse_args()
    if args.interrupt_only:
        check_interrupts(args.lfortran, args.kernel_arg)
        check_interrupt_cleanup(args.lfortran, args.kernel_arg)
        return
    directory = Path.cwd() / ("kernel-evaluation-" + uuid.uuid4().hex)
    directory.mkdir(mode=0o700)
    connection = directory / "connection.json"
    log = directory / "kernel.log"
    client = None
    process = None
    try:
        write_connection_file(fname=str(connection), key=uuid.uuid4().hex.encode("ascii"))
        with log.open("wb") as output:
            process = subprocess.Popen(
                [args.lfortran, *args.compiler_arg, "kernel", "-f", str(connection)],
                stdin=subprocess.DEVNULL, stdout=output, stderr=output,
                preexec_fn=limit_open_files if os.name == "posix" else None,
            )
            client = BlockingKernelClient(connection_file=str(connection))
            client.load_connection_file()
            client.start_channels()
            client.wait_for_ready(timeout=30)
            check_live_output(client)
            if args.streaming_only:
                client.shutdown()
                process.wait(timeout=15)
                assert process.returncode == 0, process.returncode
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
                             "call display_data('text/html','<b>hello</b>'); "
                             "call clear_output()", [])
            assert any(m["msg_type"] == "display_data" and
                       m["content"]["data"]["text/html"] == "<b>hello</b>"
                       for m in messages), messages
            assert any(m["msg_type"] == "clear_output" for m in messages), messages
            check(client, "program unavailable\ninteger :: i,ios\n"
                  "read(*,*,iostat=ios) i\nif(ios<=0) error stop\nend program", [])
            for code in ("n+1", "call display_data('text/html', '<b>silent</b>')"):
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
            # Cells must not leak file descriptors (MAX_OPEN_FILES).
            for i in range(100):
                check(client, "%d + 1" % i, [str(i + 1)])
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
        log.unlink(missing_ok=True)
        directory.rmdir()
    check_stop_flushes(args.lfortran)
    check_interrupts(args.lfortran)
    check_interrupt_cleanup(args.lfortran)


def check_interrupts(lfortran, arguments=()):
    """Interrupt real, non-returning cells without replacing the session."""
    for managed in (True, False):
        directory = Path.cwd() / ("kernel-interrupt-" + uuid.uuid4().hex)
        directory.mkdir(mode=0o700)
        connection = directory / "connection.json"
        log = directory / "kernel.log"
        client = process = None
        try:
            write_connection_file(fname=str(connection), key=uuid.uuid4().hex.encode("ascii"))
            command = [lfortran, *arguments, "kernel", "-f", str(connection)]
            if managed:
                command.append("--managed")
            with log.open("wb") as output:
                process = subprocess.Popen(command, stdin=subprocess.DEVNULL,
                                           stdout=output, stderr=output)
                client = BlockingKernelClient(connection_file=str(connection))
                client.load_connection_file()
                client.start_channels()
                client.wait_for_ready(timeout=30)
                check(client, "integer :: saved\nsaved=42", [])
                check(client, "subroutine spin()\ndo\nend do\nend subroutine", [])
                codes = ["print '(a)', 'before-interrupt'\ndo\nend do",
                         "call spin()\nsaved=0"]
                if os.name == "posix":
                    child_pid = directory / "child.pid"
                    codes.append('call execute_command_line("sleep 30 & echo $! > '
                                 + shlex.quote(str(child_pid)) + '; wait")\nsaved=0')
                elif os.name == "nt":
                    codes.append('call execute_command_line("ping -n 31 127.0.0.1 > NUL")\nsaved=0')
                for code in codes:
                    running = client.execute(code, stop_on_error=False)
                    deadline = time.monotonic() + 10
                    while True:
                        msg = client.get_iopub_msg(timeout=max(.1, deadline-time.monotonic()))
                        if (msg["parent_header"].get("msg_id") == running
                                and msg["msg_type"] == "execute_input"):
                            break
                        assert time.monotonic() < deadline, "cell did not start"
                    time.sleep(.5)
                    info_id = ""
                    control_execute_id = ""
                    if managed:
                        info = client.session.msg("kernel_info_request", content={})
                        info_id = info["header"]["msg_id"]
                        client.control_channel.send(info)
                        deferred = client.session.msg("execute_request", content={
                            "code": "saved+3", "stop_on_error": False})
                        control_execute_id = deferred["header"]["msg_id"]
                        client.control_channel.send(deferred)
                        # Give the running cell's safepoint pump time to read it.
                        time.sleep(.1)
                    if "sleep 30" in code:
                        deadline = time.monotonic() + 10
                        while not child_pid.exists() and time.monotonic() < deadline:
                            time.sleep(.02)
                        assert child_pid.exists(), "command did not start"
                    by_signal = os.name == "posix" and code.startswith("call spin()")
                    interrupt_id = ""
                    if by_signal:
                        process.send_signal(signal.SIGINT)
                    else:
                        interrupt = client.session.msg("interrupt_request", content={})
                        interrupt_id = interrupt["header"]["msg_id"]
                        client.control_channel.send(interrupt)
                    queued = client.execute("saved+1", stop_on_error=False)
                    replies, idle, values, streams = {}, set(), [], []
                    acknowledged = by_signal
                    info_replied = not managed
                    control_executed = not managed
                    control_values = []
                    deadline = time.monotonic() + 8
                    while time.monotonic() < deadline:
                        for channel in ("shell", "control", "iopub"):
                            try:
                                msg = getattr(client, channel + "_channel").get_msg(timeout=.01)
                            except queue.Empty:
                                continue
                            parent = msg["parent_header"].get("msg_id")
                            kind, content = msg["msg_type"], msg["content"]
                            if parent == interrupt_id and kind == "interrupt_reply":
                                acknowledged = content["status"] == "ok"
                            if parent == info_id and kind == "kernel_info_reply":
                                info_replied = content["status"] == "ok"
                            if parent == control_execute_id:
                                if kind == "execute_reply":
                                    control_executed = content["status"] == "ok"
                                elif kind == "execute_result":
                                    control_values.append(content["data"]["text/plain"])
                            if parent not in (running, queued):
                                continue
                            if kind == "execute_reply":
                                replies[parent] = content
                            elif kind == "status" and content["execution_state"] == "idle":
                                idle.add(parent)
                            elif kind == "execute_result" and parent == queued:
                                values.append(content["data"]["text/plain"])
                            elif kind == "stream" and parent == running:
                                streams.append(content["text"])
                        if (acknowledged and info_replied and control_executed
                                and (not managed or control_values == ["45"])
                                and len(replies) == len(idle) == 2):
                            break
                    assert info_replied, ("control request lost during execution", managed, code)
                    assert control_executed, ("deferred control execution lost", managed, code)
                    if managed:
                        assert control_values == ["45"], control_values
                    assert acknowledged, ("interrupt not serviced", managed, code, replies)
                    assert replies.get(running, {}).get("ename") == "KeyboardInterrupt", replies
                    assert replies.get(queued, {}).get("status") == "ok", replies
                    assert values == ["43"], values
                    assert len(idle) == 2 and process.poll() is None
                    if "before-interrupt" in code:
                        assert "before-interrupt" in "".join(streams), streams
                    if "sleep 30" in code:
                        child = int(child_pid.read_text())
                        deadline = time.monotonic() + 3
                        while time.monotonic() < deadline:
                            try:
                                os.kill(child, 0)
                            except ProcessLookupError:
                                break
                            time.sleep(.02)
                        else:
                            raise AssertionError("interrupted command left its child running")
                    check(client, "saved+2", ["44"])
                client.shutdown()
                assert process.wait(timeout=10) == 0
                print("interrupts: %s %s passed" %
                      ("builtin" if managed else "default", list(arguments)), flush=True)
        except BaseException:
            print(log.read_text(errors="replace"))
            raise
        finally:
            if client:
                client.stop_channels()
            if process and process.poll() is None:
                process.terminate()
                try:
                    process.wait(timeout=3)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait(timeout=3)
            connection.unlink(missing_ok=True)
            log.unlink(missing_ok=True)
            if (directory / "child.pid").exists():
                try:
                    os.kill(int((directory / "child.pid").read_text()), signal.SIGKILL)
                except ProcessLookupError:
                    pass
            (directory / "child.pid").unlink(missing_ok=True)
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


def check_stop_flushes(lfortran):
    """A cell's STOP ends the kernel with status 0. On the built-in kernel,
    stdout written before it is delivered. On Windows, STOP must call the
    host exit() so the flush registered with atexit runs."""
    directory = Path.cwd() / ("kernel-stop-" + uuid.uuid4().hex)
    directory.mkdir(mode=0o700)
    connection = directory / "connection.json"
    log = directory / "kernel.log"
    client = None
    process = None
    managed = builtin_stop_kernel(lfortran)
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
                "print *, 'before-stop'\nif (.true.) stop\nprint *, 'after-stop'")
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
            assert code == 0, (code, blob, log.read_text(errors="replace"))
            if managed:
                assert "before-stop" in blob, blob
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


def check_interrupt_cleanup(lfortran, arguments=()):
    """Cancellation ends owned scopes, not the session's allocations."""
    for managed in (True, False):
        directory = Path.cwd() / ("kernel-cleanup-" + uuid.uuid4().hex)
        directory.mkdir(mode=0o700)
        connection, log = directory / "connection.json", directory / "kernel.log"
        client = process = None
        try:
            write_connection_file(fname=str(connection), key=uuid.uuid4().hex.encode("ascii"))
            command = [lfortran, "--detect-leaks", *arguments, "kernel", "-f", str(connection)]
            if managed:
                command.append("--managed")
            with log.open("wb") as output:
                process = subprocess.Popen(command, stdin=subprocess.DEVNULL,
                                           stdout=output, stderr=output)
                client = BlockingKernelClient(connection_file=str(connection))
                client.load_connection_file()
                client.start_channels()
                client.wait_for_ready(timeout=30)
                check(client, """module interrupt_resources
use iso_c_binding, only: c_int, c_float
integer :: finalized = 0, final_sum = 0
real, allocatable :: kept(:)
type owner
    real, allocatable :: data(:)
contains
    final :: finish
end type
interface
    subroutine report() bind(c, name="dbg_report")
    end subroutine
end interface
contains
subroutine keep()
    allocate(kept(10))
    kept = 3
end subroutine
subroutine release()
    deallocate(kept)
end subroutine
subroutine finish(x)
    type(owner), intent(inout) :: x
    integer :: j
    finalized = finalized + 1
    do j = 1, 100
        final_sum = final_sum + j
    end do
end subroutine
subroutine report_leaks()
    call report()
end subroutine
subroutine work(forever)
    logical, intent(in) :: forever
    real, allocatable :: a(:)
    allocate(a(1024))
    a = 1.0
    do while (forever)
    end do
end subroutine
subroutine outer(forever)
    logical, intent(in) :: forever
    real, allocatable :: a(:)
    allocate(a(512))
    block
        type(owner) :: b
        real :: scratch(4096)
        allocate(b%data(128))
        scratch = 2
        associate(view => a)
            if (size(view) /= 512) error stop
            call work(forever)
        end associate
    end block
end subroutine
integer function spin_result(forever) result(value)
    logical, intent(in) :: forever
    real, allocatable :: a(:)
    allocate(a(1024))
    do while (forever)
    end do
    value = 7
end function
function spin_text(forever) result(value)
    logical, intent(in) :: forever
    character(:), allocatable :: value
    value = 'pending'
    do while (forever)
    end do
end function
subroutine churn(forever)
    logical, intent(in) :: forever
    character(:), allocatable :: text
    text = 'initial'
    do while (forever)
        text = spin_text(.false.) // spin_text(.false.)
    end do
end subroutine
integer(c_int) function c_spin(forever, a) bind(c)
    integer(c_int), value :: forever
    real(c_float), allocatable, intent(out) :: a(:)
    allocate(a(10))
    a = 3
    do while (forever /= 0)
    end do
    c_spin = 7
end function
end module""", [])
                check(client, "use interrupt_resources", [])
                check(client, "integer :: saved", [])
                check(client, "saved=42", [])
                check(client, "call outer(.false.)", [])
                check(client, "len(spin_text(.false.))", ["7"])
                check(client, "finalized", ["1"])

                def no_leaks():
                    request = client.execute("call report_leaks()")
                    text = ""
                    idle = False
                    deadline = time.monotonic() + 10
                    while time.monotonic() < deadline:
                        try:
                            msg = client.get_iopub_msg(timeout=.1)
                        except queue.Empty:
                            assert process.poll() is None, ("leak reporter exited", text)
                            continue
                        if msg["parent_header"].get("msg_id") != request:
                            continue
                        if msg["msg_type"] == "stream":
                            text += msg["content"]["text"]
                        if (msg["msg_type"] == "status"
                                and msg["content"]["execution_state"] == "idle"):
                            idle = True
                            break
                    assert idle and "NO LEAKS FOUND" in text, text
                    assert process.poll() is None, text

                no_leaks()
                for code, count in (("call work(.true.)", 1),
                                    ("call outer(.true.)", 2),
                                    ("saved=spin_result(.true.)", 2),
                                    ("saved=len(spin_text(.true.))", 2),
                                    ("call churn(.true.)", 2),
                                    ("saved=c_spin(1,kept)", 2),
                                    ("call outer(.true.)", 3)):
                    check(client, "call keep()", [])
                    running = client.execute(code, stop_on_error=False)
                    while True:
                        msg = client.get_iopub_msg(timeout=10)
                        if (msg["parent_header"].get("msg_id") == running
                                and msg["msg_type"] == "execute_input"):
                            break
                    time.sleep(.5)
                    client.control_channel.send(client.session.msg("interrupt_request", content={}))
                    deadline = time.monotonic() + 10
                    while True:
                        msg = client.get_shell_msg(timeout=max(.1, deadline-time.monotonic()))
                        if msg["parent_header"].get("msg_id") == running:
                            assert msg["content"]["ename"] == "KeyboardInterrupt", msg
                            break
                        assert time.monotonic() < deadline, "interrupted cell did not return"
                    check(client, "saved+1", ["43"])
                    check(client, "int(sum(kept))", ["30"])
                    check(client, "call release()", [])
                    check(client, "finalized", [str(count)])
                    check(client, "final_sum", [str(count * 5050)])
                    no_leaks()
                client.shutdown()
                assert process.wait(timeout=10) == 0
                print("interrupt cleanup: %s %s passed" %
                      ("builtin" if managed else "default", list(arguments)), flush=True)
        except BaseException:
            print(log.read_text(errors="replace"))
            raise
        finally:
            if client:
                client.stop_channels()
            if process and process.poll() is None:
                process.terminate()
                try:
                    process.wait(timeout=3)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait(timeout=3)
            connection.unlink(missing_ok=True)
            log.unlink(missing_ok=True)
            directory.rmdir()


if __name__ == "__main__":
    main()
