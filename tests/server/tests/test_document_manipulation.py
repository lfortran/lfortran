import os
from tempfile import NamedTemporaryFile

from lsprotocol.types import (
    ClientCapabilities, ClientRegisterCapabilityRequest,
    ClientRegisterCapabilityResponse, CompletionClientCapabilities,
    CompletionClientCapabilitiesCompletionItemType,
    CompletionClientCapabilitiesCompletionItemTypeInsertTextModeSupportType,
    CompletionClientCapabilitiesCompletionItemTypeResolveSupportType,
    CompletionClientCapabilitiesCompletionItemTypeTagSupportType,
    CompletionItemTag, DeleteFilesParams,
    DidChangeConfigurationClientCapabilities, DidChangeConfigurationParams,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, ExitNotification,
    FileRename, HoverClientCapabilities, InitializedNotification,
    InitializedParams, InitializeParams, InitializeRequest, InitializeResponse,
    InitializeResultServerInfoType, InsertTextMode, MarkupKind, Position,
    Range, Registration, RenameFilesParams, SaveOptions, ServerCapabilities,
    ShutdownRequest, TextDocumentClientCapabilities,
    TextDocumentContentChangeEvent_Type1, TextDocumentContentChangeEvent_Type2,
    TextDocumentDidChangeNotification, TextDocumentDidCloseNotification,
    TextDocumentDidOpenNotification, TextDocumentDidSaveNotification,
    TextDocumentIdentifier, TextDocumentItem, TextDocumentSaveReason,
    TextDocumentSyncClientCapabilities, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentWillSaveNotification,
    TextDocumentWillSaveWaitUntilRequest,
    TextDocumentWillSaveWaitUntilResponse, VersionedTextDocumentIdentifier,
    WillSaveTextDocumentParams, WorkspaceClientCapabilities,
    WorkspaceConfigurationRequest, WorkspaceConfigurationResponse,
    WorkspaceDidChangeConfigurationNotification,
    WorkspaceDidRenameFilesNotification, WorkspaceWillDeleteFilesRequest,
    WorkspaceWillRenameFilesRequest, FileDelete, WorkspaceDidDeleteFilesNotification)

from llanguage_test_client.lsp_test_client import LspTestClient, OutgoingEvent


def test_document_manipulation(client: LspTestClient):
    with NamedTemporaryFile(prefix="test_document_manipulation-", suffix=".f90", delete=True) as tmp_file_1:
        with NamedTemporaryFile(prefix="test_document_manipulation-", suffix=".f90", delete=True) as tmp_file_2:
            print(f":: tmp_file_1 => [{tmp_file_1.name}]")
            print(f":: tmp_file_2 => [{tmp_file_2.name}]")
            doc = client.new_document("fortran")
            doc.write("module module_function_call1\n")
            doc.write("end module module_function_call1\n")
            doc.save(tmp_file_1.name)

            assert os.path.exists(tmp_file_1.name)

            def sent_text_document_did_open(event: OutgoingEvent) -> bool:
                if isinstance(event.data, TextDocumentDidOpenNotification):
                    notification = event.data
                    return notification.params.text_document.uri == doc.uri
                return False
            assert client.has_outgoing_event(sent_text_document_did_open)

            with open(tmp_file_1.name) as f:
                assert doc.text == f.read() == "\n".join([
                    "module module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.cursor = 0,5
            doc.write("foo")
            doc.save()

            def sent_text_document_did_write_foo(event: OutgoingEvent) -> bool:
                if isinstance(event.data, TextDocumentDidChangeNotification):
                    notification = event.data
                    params = notification.params
                    if params.text_document.uri == doc.uri:
                        content_changes = params.content_changes
                        if len(content_changes) == 1:
                            content_change = content_changes[0]
                            if isinstance(content_change, TextDocumentContentChangeEvent_Type1):
                                range = content_change.range
                                start = range.start
                                end = range.end
                                return start.line == 0 \
                                    and start.character == 6 \
                                    and end.line == 0 \
                                    and end.character == 6 \
                                    and content_change.text == "foo"
                return False
            assert client.has_outgoing_event(sent_text_document_did_write_foo)

            with open(tmp_file_1.name) as f:
                assert doc.text == f.read() == "\n".join([
                    "modulfooe module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.cursor = 0,5
            doc.replace("bar")
            doc.save()

            def sent_text_document_did_replace_foo_with_bar(event: OutgoingEvent) -> bool:
                if isinstance(event.data, TextDocumentDidChangeNotification):
                    notification = event.data
                    params = notification.params
                    if params.text_document.uri == doc.uri:
                        content_changes = params.content_changes
                        if len(content_changes) == 1:
                            content_change = content_changes[0]
                            if isinstance(content_change, TextDocumentContentChangeEvent_Type1):
                                range = content_change.range
                                start = range.start
                                end = range.end
                                return start.line == 0 \
                                    and start.character == 6 \
                                    and end.line == 0 \
                                    and end.character == 9 \
                                    and content_change.text == "bar"
                return False
            assert client.has_outgoing_event(sent_text_document_did_replace_foo_with_bar)

            with open(tmp_file_1.name) as f:
                assert doc.text == f.read() == "\n".join([
                    "modulbare module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.cursor = 0,5
            doc.delete(3)
            doc.save()

            def sent_text_document_did_delete_bar(event: OutgoingEvent) -> bool:
                if isinstance(event.data, TextDocumentDidChangeNotification):
                    notification = event.data
                    params = notification.params
                    if params.text_document.uri == doc.uri:
                        content_changes = params.content_changes
                        if len(content_changes) == 1:
                            content_change = content_changes[0]
                            if isinstance(content_change, TextDocumentContentChangeEvent_Type1):
                                range = content_change.range
                                start = range.start
                                end = range.end
                                return start.line == 0 \
                                    and start.character == 6 \
                                    and end.line == 0 \
                                    and end.character == 9 \
                                    and content_change.text == ""
                return False
            assert client.has_outgoing_event(sent_text_document_did_delete_bar)

            with open(tmp_file_1.name) as f:
                assert doc.text == f.read() == "\n".join([
                    "module module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.rename(tmp_file_2.name)

            def sent_workspace_did_rename_file(event: OutgoingEvent) -> bool:
                if isinstance(event.data, WorkspaceDidRenameFilesNotification):
                    notification = event.data
                    for record in notification.params.files:
                        if record.old_uri == f"file://{tmp_file_1.name}" \
                           and record.new_uri == f"file://{tmp_file_2.name}":
                            return True
                return False
            assert client.has_outgoing_event(sent_workspace_did_rename_file)

            assert not os.path.exists(tmp_file_1.name)
            assert os.path.exists(tmp_file_2.name)

            with open(tmp_file_2.name) as f:
                assert doc.text == f.read() == "\n".join([
                    "module module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.remove()
            assert not os.path.exists(tmp_file_2.name)
