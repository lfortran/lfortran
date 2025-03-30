import os
import shutil
import signal
import sys
from pathlib import Path
from typing import Iterator

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

    server_args = [
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
    ]

    client = LFortranLspTestClient(server_path, server_args, None, 1000, config, client_log_path)

    def print_logs() -> None:
        if os.path.exists(client_log_path):
            print()
            print("~~~~~~~~~~~~~~~~~", file=sys.stderr)
            print("~~ Client Logs ~~", file=sys.stderr)
            print("~~~~~~~~~~~~~~~~~", file=sys.stderr)
            print()
            with open(client_log_path) as f:
                print(f.read(), file=sys.stderr)
        if os.path.exists(server_log_path):
            print()
            print("~~~~~~~~~~~~~~~~~", file=sys.stderr)
            print("~~ Server Logs ~~", file=sys.stderr)
            print("~~~~~~~~~~~~~~~~~", file=sys.stderr)
            print()
            with open(server_log_path) as f:
                print(f.read(), file=sys.stderr)

    logs_printed = False
    try:
        with client.serve():
            # Steps abstracted by context manager:
            # 1. Send request: initialize
            # 2. Send notification: initialized
            # 3. <yield client>
            # 4. Send request: shutdown
            # 5. Send notification: exit
            yield client
    except Exception as e:
        print_logs()
        logs_printed = True
        raise e
    finally:
        if hasattr(client, 'server') and not client.server.poll():
            print('Server did not terminate cleanly, terminating it forcefully ...', file=sys.stderr)
            os.kill(client.server.pid, signal.SIGKILL)
            if not logs_printed:
                print_logs()
