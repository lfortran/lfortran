#!/usr/bin/env python

import os
import sys
import traceback
from contextlib import contextmanager
from pathlib import Path
from tempfile import NamedTemporaryFile
from typing import Iterator

from llanguage_test_client.lsp_test_client import LspTestClient


@contextmanager
def lsp_test_client() -> Iterator[LspTestClient]:
    serverPath = Path('/home/dylon/.local/bin/lfortran')
    serverArgs = [
        "server",
        "--parent-process-id", str(os.getpid()),
        "--log-level", "trace",
        "--timeout-ms", "100",
    ]
    client = LspTestClient(serverPath, serverArgs, None, 2000, {
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
                "path": "lfortran-language-server.log",
                "level": "trace",
                "prettyPrint": True,
            },
            "indentSize": 4,
            "timeoutMs": 100,
            "retry": {
                "maxAttempts": 3,
                "minSleepTimeMs": 10,
                "maxSleepTimeMs": 300,
            },
        }
    })
    with client.serve():
        yield client


def main() -> int:
    with lsp_test_client() as client:
        with NamedTemporaryFile(prefix="module_test", suffix=".f90", delete=True) \
             as tmp_file:
            doc = client.new_document("fortran")
            doc.write("module module_function_call1\n")
            doc.write("end module module_function_call1\n")
            doc.save(tmp_file.name)

            with open(tmp_file.name) as f:
                assert f.read() == "\n".join([
                    "module module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.cursor = 0,5
            doc.write("foo")
            doc.save()

            with open(tmp_file.name) as f:
                assert f.read() == "\n".join([
                    "modulfooe module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.cursor = 0,5
            doc.replace("bar")
            doc.save()

            with open(tmp_file.name) as f:
                assert f.read() == "\n".join([
                    "modulbare module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.cursor = 0,5
            doc.delete(3)
            doc.save()

            with open(tmp_file.name) as f:
                assert f.read() == "\n".join([
                    "module module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception:
        print(traceback.format_exc(), file=sys.stderr)
