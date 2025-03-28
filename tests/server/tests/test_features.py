from pathlib import Path

from llanguage_test_client.lsp_test_client import LspTestClient


def test_goto_definition(client: LspTestClient):
    path = Path(__file__).absolute().parent.parent.parent / "function_call1.f90"
    doc = client.open_document("fortran", path)
    line, column = 17, 17
    doc.cursor = line, column
    doc.goto_definition()
    assert doc.line == 7
    assert doc.column == 4
