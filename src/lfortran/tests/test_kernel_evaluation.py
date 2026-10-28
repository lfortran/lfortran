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


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    args = parser.parse_args()
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
                [args.lfortran, "kernel", "-f", str(connection)],
                stdin=subprocess.DEVNULL, stdout=output, stderr=output,
                preexec_fn=limit_open_files if os.name == "posix" else None,
            )
            client = BlockingKernelClient(connection_file=str(connection))
            client.load_connection_file()
            client.start_channels()
            client.wait_for_ready(timeout=30)
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
    for code, status, shown in [("if (.true.) stop", 0, ""), ("stop 3", 3, "STOP 3"),
            ("error stop", 1, "ERROR STOP")]:
        check_stop_flushes(args.lfortran, code, status, shown)


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


def check_stop_flushes(lfortran, stop, status, shown):
    """STOP and ERROR STOP end the kernel with their exit status. On the
    built-in kernel, preceding output and STOP diagnostics are delivered.
    On Windows, STOP must call the host exit() so the atexit flush runs."""
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
