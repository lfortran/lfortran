import os
import re
import shutil
from pathlib import Path
from typing import Iterator

import pytest

from llanguage_test_client.lsp_test_client import LspTestClient

RE_EXIT_TIMEOUT = re.compile(
    r'^Timed-out after (?:[0-9]*\.)[0-9]+ seconds while awaiting the server to terminate\.$'
)


@pytest.fixture
def client(request: pytest.FixtureRequest) -> Iterator[LspTestClient]:
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

    client = LspTestClient(server_path, server_args, None, timeout_ms, {
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

    with pytest.raises(RuntimeError) as error:
        with client.serve():
            # Steps abstracted by context manager:
            # 1. Send request: initialize
            # 2. Send notification: initialized
            # 3. <yield client>
            # 4. Send request: shutdown
            # 5. Send notification: exit
            yield client
    assert RE_EXIT_TIMEOUT.fullmatch(str(error.value)), str(error.value)
