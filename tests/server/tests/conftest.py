import os
import shutil
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

    log_path = f"{request.node.name}.log"

    timeout_ms = 5000

    server_args = [
        "server",
        "--parent-process-id", str(os.getpid()),
        "--log-level", "trace",
        "--log-path", log_path,
        "--timeout-ms", str(timeout_ms),
    ]

    client = LFortranLspTestClient(server_path, server_args, None, timeout_ms, {
        "LFortran": {
            "openIssueReporterOnError": False,
            "maxNumberOfProblems": 100,
            "trace": {
                "server": "off",
            },
            "compiler": {
                "path": "lfortran",
                "flags": [],
            },
            "log": {
                "path": log_path,
                "level": "trace",
                "prettyPrint": True,
            },
            "indentSize": 4,
            "timeoutMs": timeout_ms,
            "retry": {
                "maxAttempts": 3,
                "minSleepTimeMs": 10,
                "maxSleepTimeMs": 300,
            },
        }
    })

    with client.serve():
        # Steps abstracted by context manager:
        # 1. Send request: initialize
        # 2. Send notification: initialized
        # 3. <yield client>
        # 4. Send request: shutdown
        # 5. Send notification: exit
        yield client
