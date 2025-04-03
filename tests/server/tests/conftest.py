import os
import shutil
import signal
import sys
from pathlib import Path
from typing import Iterator, Optional

import pytest

from lfortran_language_server.lfortran_lsp_test_client import LFortranLspTestClient


@pytest.fixture
def client(request: pytest.FixtureRequest) -> Iterator[LFortranLspTestClient]:
    server_path = None
    if 'LFORTRAN_PATH' in os.environ:
        server_path = os.environ['LFORTRAN_PATH']
    if server_path is None or not os.path.exists(server_path):
        server_path = shutil.which('lfortran')
    if server_path is None:
        raise RuntimeError('cannot determine location of lfortran')
    server_path = Path(server_path)
    if not (server_path.exists() and os.access(server_path, os.X_OK)):
        raise RuntimeError(f'Invalid or non-executable path to lfortran: {server_path}')

    server_log_path = f"{request.node.name}-server.log"
    client_log_path = f"{request.node.name}-client.log"
    stdout_log_path = f"{request.node.name}-stdout.log"
    stdin_log_path = f"{request.node.name}-stdin.log"
    gdb_log_path = f"{request.node.name}-gdb.log"

    config = {
        "LFortran": {
            "openIssueReporterOnError": False,
            "maxNumberOfProblems": 100,
            "trace": {
                "server": "verbose",
            },
            "compiler": {
                "path": "lfortran",
                "flags": [],
            },
            "log": {
                "path": server_log_path,
                "level": "all",
                "prettyPrint": False,
            },
            "indentSize": 4,
            # NOTE: Regarding timing-out and retrying requests, a timeout of 0
            # means do not time-out (or retry):
            "timeoutMs": 0,
            "retry": {
                "maxAttempts": 3,
                "minSleepTimeMs": 10,
                "maxSleepTimeMs": 300,
            },
        }
    }

    server_args = []

    gdb_path = shutil.which('gdb')
    if gdb_path is not None:
        server_path = Path(gdb_path)
        server_args += [
            "-q", "-batch",
            "-ex", "set logging redirect on",
            "-ex", "set logging debugredirect on",
            "-ex", "set logging overwrite on",
            "-ex", f"set logging file {gdb_log_path}",
            "-ex", "set logging enabled on",
            "-ex", "run",
            "-ex", "bt",
            "--args", str(server_path),
        ]

    server_args += [
        "server",
        "--parent-process-id", str(os.getpid()),
        "--log-level", config["LFortran"]["log"]["level"],
        "--log-path", server_log_path,
        "--timeout-ms", str(config["LFortran"]["timeoutMs"]),
        "--num-request-threads", "1",
        "--num-worker-threads", "1",
        "--config-section", "LFortran",
        "--open-issue-reporter-on-error", str(config["LFortran"]["openIssueReporterOnError"]).lower(),
        "--max-number-of-problems", str(config["LFortran"]["maxNumberOfProblems"]),
        "--trace-server", config["LFortran"]["trace"]["server"],
        "--compiler-path", str(server_path),
        "--log-pretty-print", str(config["LFortran"]["log"]["prettyPrint"]).lower(),
        "--indent-size", str(config["LFortran"]["indentSize"]),
        "--max-retry-attempts", str(config["LFortran"]["retry"]["maxAttempts"]),
        "--min-retry-sleep-time-ms", str(config["LFortran"]["retry"]["minSleepTimeMs"]),
        "--max-retry-sleep-time-ms", str(config["LFortran"]["retry"]["maxSleepTimeMs"]),
        "--extension-id", "lcompilers.lfortran",
        "--execution-strategy", "concurrent",  #-> "parallel" or "concurrent"
    ]

    def print_log(log_path: str, heading: str) -> None:
        header = f"~~ {heading} [{log_path}] ~~"
        border = "~" * len(header)
        print(file=sys.stderr)
        print(border, file=sys.stderr)
        print(header, file=sys.stderr)
        print(border, file=sys.stderr)
        print(file=sys.stderr)
        if os.path.exists(log_path):
            with open(log_path) as f:
                print(f.read(), file=sys.stderr)
        else:
            print(f"Log file does not exist: {log_path}")

    def print_logs() -> None:
        print_log(client_log_path, "Client Logs")
        print_log(stdin_log_path, "Standard Input")
        print_log(stdout_log_path, "Standard Output")
        print_log(server_log_path, "Server Logs")
        print_log(gdb_log_path, "GNU Debugger (GDB)")

    client: Optional[LFortranLspTestClient] = None
    logs_printed = False
    try:
        client = LFortranLspTestClient(
            server_path=server_path,
            server_params=server_args,
            workspace_path=None,
            timeout_ms=1000,
            config=config,
            client_log_path=client_log_path,
            stdout_log_path=stdout_log_path,
            stdin_log_path=stdin_log_path
        )

        with client.serve():
            # Steps abstracted by context manager:
            # 1. Send request: initialize
            # 2. Send notification: initialized
            # 3. <yield client>
            # 4. Send request: shutdown
            # 5. Send notification: exit
            yield client
    except BaseException as e:
        if not isinstance(e, SystemExit) or e.code != 0:
            print_logs()
            logs_printed = True
        raise e
    finally:
        if client is not None and hasattr(client, 'server') \
           and not client.server.poll():
            print(
                'Server did not terminate cleanly, terminating it forcefully ...',
                file=sys.stderr
            )
            os.kill(client.server.pid, signal.SIGKILL)
            if not logs_printed:
                print_logs()
