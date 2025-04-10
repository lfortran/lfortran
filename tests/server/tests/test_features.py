from pathlib import Path
from tempfile import NamedTemporaryFile
from typing import List

from lsprotocol.types import (DidChangeConfigurationParams, DocumentHighlight,
                              DocumentSymbol, Hover, MarkupContent, MarkupKind,
                              Position, Range, SymbolKind)

from lfortran_language_server.lfortran_lsp_test_client import \
    LFortranLspTestClient

from llanguage_test_client.lsp_test_client import IncomingEvent


def test_goto_definition(client: LFortranLspTestClient) -> None:
    path = Path(__file__).absolute().parent.parent.parent / "function_call1.f90"
    doc = client.open_document("fortran", path)
    assert client.await_validation(doc.uri, doc.version) is not None
    line, column = 18, 18
    doc.cursor = line, column
    doc.goto_definition()
    assert doc.line == 8
    assert doc.column == 5

def test_diagnostics(client: LFortranLspTestClient) -> None:
    path = Path(__file__).absolute().parent.parent.parent / "function_call1.f90"
    doc = client.open_document("fortran", path)
    assert client.await_validation(doc.uri, doc.version) is not None
    line, column = 21, 1
    doc.cursor = line, column
    doc.write("error")
    validation = client.await_validation(doc.uri, doc.version)
    assert validation is not None
    assert validation["params"]["uri"] == doc.uri
    assert validation["params"]["version"] == doc.version
    diagnostics = validation["params"]["diagnostics"]
    assert len(diagnostics) == 2

    assert diagnostics[0]["message"] == "Statement or Declaration expected inside program, found Variable name"
    assert diagnostics[0]["range"]["start"]["line"] == 20
    assert diagnostics[0]["range"]["start"]["character"] == 0
    assert diagnostics[0]["range"]["end"]["line"] == 20
    assert diagnostics[0]["range"]["end"]["character"] == 5
    assert diagnostics[0]["severity"] == 1
    assert diagnostics[0]["source"] == "lfortran"

    assert diagnostics[1]["message"] == "Variable 'error' is not declared"
    assert diagnostics[1]["range"]["start"]["line"] == 20
    assert diagnostics[1]["range"]["start"]["character"] == 0
    assert diagnostics[1]["range"]["end"]["line"] == 20
    assert diagnostics[1]["range"]["end"]["character"] == 5
    assert diagnostics[1]["severity"] == 1
    assert diagnostics[1]["source"] == "lfortran"

    doc.backspace(5)
    validation = client.await_validation(doc.uri, doc.version)
    assert validation is not None
    assert validation["params"]["uri"] == doc.uri
    assert validation["params"]["version"] == doc.version
    diagnostics = validation["params"]["diagnostics"]
    assert len(diagnostics) == 0

def test_configuration_caching(client: LFortranLspTestClient) -> None:
    with NamedTemporaryFile(
            prefix="test_configuration_caching-",
            suffix=".f90",
            delete=True
    ) as tmp_file:
        doc = client.open_document("fortran", tmp_file.name)
        assert client.await_validation(doc.uri, doc.version) is not None
        doc.write("foo")
        doc.save()
        assert client.await_validation(doc.uri, doc.version) is not None

        def is_config_request_for_doc(event: IncomingEvent) -> bool:
            return event.data.get("method", None) == "workspace/configuration" and \
                len(event.data["params"]["items"]) > 0 and \
                event.data["params"]["items"][0]["scopeUri"] == doc.uri and \
                event.data["params"]["items"][0]["section"] == "LFortran"

        event, index = client.find_incoming_event(is_config_request_for_doc)
        # The config must be requested before it may be cached:
        assert event is not None

        doc.write("bar")
        assert client.await_validation(doc.uri, doc.version) is not None
        event, index = client.find_incoming_event(is_config_request_for_doc, index + 1)
        # The config has been cached, so there is no need to cache it again:
        assert event is None

        # Invalidate the document caches:
        client.send_workspace_did_change_configuration(
            DidChangeConfigurationParams(client.config)
        )

        doc.write("baz")
        assert client.await_validation(doc.uri, doc.version) is not None
        event, index = client.find_incoming_event(is_config_request_for_doc, index + 1)
        # The cache was invalidated so the config must be requested again:
        assert event is not None

def test_rename(client: LFortranLspTestClient) -> None:
    path = Path(__file__).absolute().parent.parent.parent / "function_call1.f90"
    doc = client.open_document("fortran", path)
    assert client.await_validation(doc.uri, doc.version) is not None
    line, column = 4, 20
    doc.cursor = line, column
    doc.rename("foo")
    assert doc.text == "\n".join([
        "module module_function_call1",
        "    type :: softmax",
        "    contains",
        "      foo",
        "    end type softmax",
        "  contains",
        "  ",
        "    pure function foo(self, x) result(res)",
        "      class(softmax), intent(in) :: self",
        "      real, intent(in) :: x(:)",
        "      real :: res(size(x))",
        "    end function foo",
        "  ",
        "    pure function eval_1d_prime(self, x) result(res)",
        "      class(softmax), intent(in) :: self",
        "      real, intent(in) :: x(:)",
        "      real :: res(size(x))",
        # TODO: Once lfortran can rename the function call, swap the following
        # two lines:
        # "      res = self%foo(x)",
        "      res = self%eval_1d(x)",
        "    end function eval_1d_prime",
        "end module module_function_call1",
    ]) + "\n"

def test_document_highlight(client: LFortranLspTestClient) -> None:
    path = Path(__file__).absolute().parent.parent.parent.parent / "examples" / "expr2.f90"
    doc = client.open_document("fortran", path)
    assert client.await_validation(doc.uri, doc.version) is not None
    line, column = 7, 10
    doc.cursor = line, column
    doc.highlight()
    assert doc.highlights is not None
    # NOTE: DocumentHighlight is not hashable, so we cannot perform set comparison ...
    expected_highlights: List[DocumentHighlight] = [
        DocumentHighlight(
            range=Range(
                start=Position(
                    line=3,
                    character=11,
                ),
                end=Position(
                    line=3,
                    character=12,
                ),
            ),
        ),
        DocumentHighlight(
            range=Range(
                start=Position(
                    line=5,
                    character=0,
                ),
                end=Position(
                    line=5,
                    character=1,
                ),
            ),
        ),
        DocumentHighlight(
            range=Range(
                start=Position(
                    line=6,
                    character=9,
                ),
                end=Position(
                    line=6,
                    character=10,
                ),
            ),
        ),
    ]
    for highlight in doc.highlights:
        expected_highlights.remove(highlight)
    assert len(expected_highlights) == 0


def test_document_hover(client: LFortranLspTestClient) -> None:
    path = Path(__file__).absolute().parent.parent.parent / "function_call1.f90"
    doc = client.open_document("fortran", path)
    assert client.await_validation(doc.uri, doc.version) is not None
    line, column = 18, 22
    doc.cursor = line, column
    doc.hover()
    assert doc.preview == Hover(
        contents=MarkupContent(
            kind=MarkupKind.Markdown,
            value="```fortran\nfunction eval_1d(self, x) result(res)\n    softmax, intent(in) :: self\n    real[:], intent(in) :: x\n    real[:] :: res\nend function\n```"
        ),
        range=Range(
            end=Position(
                character=24,
                line=11
            ),
            start=Position(
                character=4,
                line=7
            )
        )
    )


def test_symbol_tree(client: LFortranLspTestClient) -> None:
    path = Path(__file__).absolute().parent.parent.parent.parent / "examples" / "expr2.f90"
    doc = client.open_document("fortran", path)
    assert client.await_validation(doc.uri, doc.version) is not None
    assert doc.symbols == [
        DocumentSymbol(
            children=[
                DocumentSymbol(
                    kind=SymbolKind.Variable,
                    name="x",
                    range=Range(
                        start=Position(
                            line=3,
                            character=11,
                        ),
                        end=Position(
                            line=3,
                            character=12,
                        ),
                    ),
                    selection_range=Range(
                        start=Position(
                            line=3,
                            character=11,
                        ),
                        end=Position(
                            line=3,
                            character=12,
                        ),
                    ),
                ),
            ],
            kind=SymbolKind.Function,
            name="expr2",
            range=Range(
                start=Position(
                    line=0,
                    character=0,
                ),
                end=Position(
                    line=8,
                    character=11,
                ),
            ),
            selection_range=Range(
                start=Position(
                    line=0,
                    character=0,
                ),
                end=Position(
                    line=8,
                    character=11,
                ),
            ),
        ),
    ]
    line, column = 4, 12
    doc.cursor = line, column
    doc.rename("y")  # x -> y
    assert doc.symbols == [
        DocumentSymbol(
            children=[
                DocumentSymbol(
                    kind=SymbolKind.Variable,
                    name="y",
                    range=Range(
                        start=Position(
                            line=3,
                            character=11,
                        ),
                        end=Position(
                            line=3,
                            character=12,
                        ),
                    ),
                    selection_range=Range(
                        start=Position(
                            line=3,
                            character=11,
                        ),
                        end=Position(
                            line=3,
                            character=12,
                        ),
                    ),
                ),
            ],
            kind=SymbolKind.Function,
            name="expr2",
            range=Range(
                start=Position(
                    line=0,
                    character=0,
                ),
                end=Position(
                    line=8,
                    character=11,
                ),
            ),
            selection_range=Range(
                start=Position(
                    line=0,
                    character=0,
                ),
                end=Position(
                    line=8,
                    character=11,
                ),
            ),
        ),
    ]
