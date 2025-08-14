from pathlib import Path
from tempfile import NamedTemporaryFile
from typing import List

import pytest

from lsprotocol.types import (CompletionItem, CompletionItemKind,
                              DidChangeConfigurationParams, DocumentHighlight,
                              DocumentSymbol, Hover, MarkupContent, MarkupKind,
                              Position, Range, SymbolKind)

from lfortran_language_server.lfortran_lsp_test_client import \
    LFortranLspTestClient

from llanguage_test_client.json_rpc import JsonArray
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
    doc.highlight_symbol()
    assert doc.symbol_highlights is not None
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
    for highlight in doc.symbol_highlights:
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

def test_semantic_highlighting(client: LFortranLspTestClient) -> None:
    path = Path(__file__).absolute().parent.parent.parent / "function_call1.f90"
    doc = client.open_document("fortran", path)
    assert client.await_validation(doc.uri, doc.version) is not None
    doc.semantic_highlight()
    expected_highlights = [
        0,0,6,15,0, # Keyword at line=0, column=0, length=6, text=`module`
        0,7,21,8,0, # Variable at line=0, column=7, length=21, text=`module_function_call1`
        1,4,4,15,0, # Keyword at line=1, column=4, length=4, text=`type`
        0,8,7,8,0,  # Variable at line=1, column=12, length=7, text=`softmax`
        1,4,8,15,0, # Keyword at line=2, column=4, length=8, text=`contains`
        1,6,9,8,0,  # Variable at line=3, column=6, length=9, text=`procedure`
        0,13,7,8,0, # Variable at line=3, column=19, length=7, text=`eval_1d`
        1,4,3,15,0, # Keyword at line=4, column=4, length=3, text=`end`
        0,4,4,15,0, # Keyword at line=4, column=8, length=4, text=`type`
        0,5,7,8,0,  # Variable at line=4, column=13, length=7, text=`softmax`
        1,2,8,15,0, # Keyword at line=5, column=2, length=8, text=`contains`
        2,4,4,8,0,  # Variable at line=7, column=4, length=4, text=`pure`
        0,5,8,15,0, # Keyword at line=7, column=9, length=8, text=`function`
        0,9,7,8,0,  # Variable at line=7, column=18, length=7, text=`eval_1d`
        0,8,4,8,0,  # Variable at line=7, column=26, length=4, text=`self`
        0,6,1,8,0,  # Variable at line=7, column=32, length=1, text=`x`
        0,3,6,8,0,  # Variable at line=7, column=35, length=6, text=`result`
        0,7,3,8,0,  # Variable at line=7, column=42, length=3, text=`res`
        1,6,5,8,0,  # Variable at line=8, column=6, length=5, text=`class`
        0,6,7,8,0,  # Variable at line=8, column=12, length=7, text=`softmax`
        0,10,6,8,0, # Variable at line=8, column=22, length=6, text=`intent`
        0,7,2,8,0,  # Variable at line=8, column=29, length=2, text=`in`
        0,7,4,8,0,  # Variable at line=8, column=36, length=4, text=`self`
        1,6,4,15,0, # Keyword at line=9, column=6, length=4, text=`real`
        0,6,6,8,0,  # Variable at line=9, column=12, length=6, text=`intent`
        0,7,2,8,0,  # Variable at line=9, column=19, length=2, text=`in`
        0,7,1,8,0,  # Variable at line=9, column=26, length=1, text=`x`
        1,6,4,15,0, # Keyword at line=10, column=6, length=4, text=`real`
        0,8,3,8,0,  # Variable at line=10, column=14, length=3, text=`res`
        0,4,4,8,0,  # Variable at line=10, column=18, length=4, text=`size`
        0,5,1,8,0,  # Variable at line=10, column=23, length=1, text=`x`
        1,4,3,15,0, # Keyword at line=11, column=4, length=3, text=`end`
        0,4,8,15,0, # Keyword at line=11, column=8, length=8, text=`function`
        0,9,7,8,0,  # Variable at line=11, column=17, length=7, text=`eval_1d`
        2,4,4,8,0,  # Variable at line=13, column=4, length=4, text=`pure`
        0,5,8,15,0, # Keyword at line=13, column=9, length=8, text=`function`
        0,9,13,8,0, # Variable at line=13, column=18, length=13, text=`eval_1d_prime`
        0,14,4,8,0, # Variable at line=13, column=32, length=4, text=`self`
        0,6,1,8,0,  # Variable at line=13, column=38, length=1, text=`x`
        0,3,6,8,0,  # Variable at line=13, column=41, length=6, text=`result`
        0,7,3,8,0,  # Variable at line=13, column=48, length=3, text=`res`
        1,6,5,8,0,  # Variable at line=14, column=6, length=5, text=`class`
        0,6,7,8,0,  # Variable at line=14, column=12, length=7, text=`softmax`
        0,10,6,8,0, # Variable at line=14, column=22, length=6, text=`intent`
        0,7,2,8,0,  # Variable at line=14, column=29, length=2, text=`in`
        0,7,4,8,0,  # Variable at line=14, column=36, length=4, text=`self`
        1,6,4,15,0, # Keyword at line=15, column=6, length=4, text=`real`
        0,6,6,8,0,  # Variable at line=15, column=12, length=6, text=`intent`
        0,7,2,8,0,  # Variable at line=15, column=19, length=2, text=`in`
        0,7,1,8,0,  # Variable at line=15, column=26, length=1, text=`x`
        1,6,4,15,0, # Keyword at line=16, column=6, length=4, text=`real`
        0,8,3,8,0,  # Variable at line=16, column=14, length=3, text=`res`
        0,4,4,8,0,  # Variable at line=16, column=18, length=4, text=`size`
        0,5,1,8,0,  # Variable at line=16, column=23, length=1, text=`x`
        1,6,3,8,0,  # Variable at line=17, column=6, length=3, text=`res`
        0,6,4,8,0,  # Variable at line=17, column=12, length=4, text=`self`
        0,5,7,8,0,  # Variable at line=17, column=17, length=7, text=`eval_1d`
        0,8,1,8,0,  # Variable at line=17, column=25, length=1, text=`x`
        1,4,3,15,0, # Keyword at line=18, column=4, length=3, text=`end`
        0,4,8,15,0, # Keyword at line=18, column=8, length=8, text=`function`
        0,9,13,8,0, # Variable at line=18, column=17, length=13, text=`eval_1d_prime`
        1,0,3,15,0, # Keyword at line=19, column=0, length=3, text=`end`
        0,4,6,15,0, # Keyword at line=19, column=4, length=6, text=`module`
        0,7,21,8,0, # Variable at line=19, column=11, length=21, text=`module_function_call1`
    ]
    assert doc.semantic_highlights is not None
    assert doc.semantic_highlights.data == expected_highlights

def test_code_completion(client: LFortranLspTestClient) -> None:
    path = Path(__file__).absolute().parent.parent.parent / "function_call1.f90"
    doc = client.open_document("fortran", path)
    assert client.await_validation(doc.uri, doc.version) is not None
    line, column = 18, -1
    doc.cursor = line, column
    doc.newline()
    doc.write("self")
    doc.complete()
    expected_completions = [
        CompletionItem(
            label="self",
            kind=CompletionItemKind.Variable
        )
    ]
    assert doc.completions is not None
    assert isinstance(doc.completions, list)
    for completion in doc.completions:
        expected_completions.remove(completion)
    assert len(expected_completions) == 0
    doc.write("%")
    doc.complete()
    expected_completions = [
        CompletionItem(
            label="module_function_call1",
            kind=CompletionItemKind.Module,
        ),
        CompletionItem(
            label="eval_1d",
            kind=CompletionItemKind.Function,
        ),
        CompletionItem(
            label="res",
            kind=CompletionItemKind.Variable,
        ),
        CompletionItem(
            label="self",
            kind=CompletionItemKind.Variable,
        ),
        CompletionItem(
            label="x",
            kind=CompletionItemKind.Variable,
        ),
        CompletionItem(
            label="eval_1d_prime",
            kind=CompletionItemKind.Function,
        ),
        CompletionItem(
            label="1_eval_1d",
            kind=CompletionItemKind.Function,
        ),
        CompletionItem(
            label="softmax",
            kind=CompletionItemKind.Struct,
        ),
    ]
    assert doc.completions is not None
    assert isinstance(doc.completions, list)
    for completion in doc.completions:
        expected_completions.remove(completion)
    assert len(expected_completions) == 0

def test_whole_document_formatting(client: LFortranLspTestClient) -> None:
    path = Path(__file__).absolute().parent.parent.parent / "function_call1.f90"
    doc = client.open_document("fortran", path)
    assert client.await_validation(doc.uri, doc.version) is not None
    doc.format()
    assert doc.text == "\n".join([
        "module module_function_call1",
        "    type :: softmax",
        "",
        "contains",
        "",
        "procedure :: eval_1d",
        "    end type softmax",
        "",
        "contains",
        "",
        "    pure function eval_1d(self, x) result(res)",
        "        class(softmax), intent(in) :: self",
        "        real, intent(in) :: x(:)",
        "        real :: res(size(x))",
        "    end function eval_1d",
        "",
        "",
        "    pure function eval_1d_prime(self, x) result(res)",
        "        class(softmax), intent(in) :: self",
        "        real, intent(in) :: x(:)",
        "        real :: res(size(x))",
        "        res = self%eval_1d(x)",
        "    end function eval_1d_prime",
        "",
        "end module module_function_call1",
        ""
    ])

def test_partial_document_formatting(client: LFortranLspTestClient) -> None:
    path = Path(__file__).absolute().parent.parent.parent / "function_call1.f90"
    doc = client.open_document("fortran", path)
    assert client.await_validation(doc.uri, doc.version) is not None
    doc.select(14, 5, 19, 31)
    doc.format_range()
    assert doc.text == "\n".join([
        "module module_function_call1",
        "    type :: softmax",
        "    contains",
        "      procedure :: eval_1d",
        "    end type softmax",
        "  contains",
        "  ",
        "    pure function eval_1d(self, x) result(res)",
        "      class(softmax), intent(in) :: self",
        "      real, intent(in) :: x(:)",
        "      real :: res(size(x))",
        "    end function eval_1d",
        "  ",
        "    pure function eval_1d_prime(self, x) result(res)",
        "        class(softmax), intent(in) :: self",
        "        real, intent(in) :: x(:)",
        "        real :: res(size(x))",
        "        res = self%eval_1d(x)",
        "    end function eval_1d_prime",
        "",
        "end module module_function_call1",
        ""
    ])

def test_telemetry_event(client: LFortranLspTestClient) -> None:
    telemetry: JsonArray = client.get_telemetry()
    if not any(filter(lambda event: event["key"] == "processUsage", telemetry)):
        pytest.skip("ProcessUsage is not supported on this platform")
