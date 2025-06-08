import os
from tempfile import NamedTemporaryFile

from lsprotocol.types import (TextDocumentContentChangeEvent_Type1,
                              TextDocumentDidChangeNotification,
                              TextDocumentDidOpenNotification,
                              WorkspaceDidRenameFilesNotification)

from llanguage_test_client.lsp_test_client import OutgoingEvent
from lfortran_language_server.lfortran_lsp_test_client import LFortranLspTestClient


def test_document_manipulation(client: LFortranLspTestClient):
    with NamedTemporaryFile(
            prefix="test_document_manipulation-",
            suffix=".f90",
            delete=True
    ) as tmp_file_1:
        with NamedTemporaryFile(
                prefix="test_document_manipulation-",
                suffix=".f90",
                delete=True
        ) as tmp_file_2:
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

            assert client.await_validation(doc.uri, doc.version) is not None

            rdoc = client.get_remote_document(doc.uri)
            assert doc.uri == rdoc["uri"]
            assert doc.version == rdoc["version"]

            with open(tmp_file_1.name) as f:
                assert doc.text == rdoc["text"] == f.read() == "\n".join([
                    "module module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.cursor = 1,6
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
                            if isinstance(content_change,
                                          TextDocumentContentChangeEvent_Type1):
                                range = content_change.range
                                start = range.start
                                end = range.end
                                return start.line == 0 \
                                    and start.character == 5 \
                                    and end.line == 0 \
                                    and end.character == 5 \
                                    and content_change.text == "foo"
                return False
            assert client.has_outgoing_event(sent_text_document_did_write_foo)

            assert client.await_validation(doc.uri, doc.version) is not None

            rdoc = client.get_remote_document(doc.uri)
            assert doc.uri == rdoc["uri"]
            assert doc.version == rdoc["version"]

            with open(tmp_file_1.name) as f:
                assert doc.text == rdoc["text"] == f.read() == "\n".join([
                    "modulfooe module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.cursor = 1,6
            doc.replace("bar")
            doc.save()

            def sent_text_document_did_replace_foo_with_bar(
                    event: OutgoingEvent
            ) -> bool:
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
                                    and start.character == 5 \
                                    and end.line == 0 \
                                    and end.character == 8 \
                                    and content_change.text == "bar"
                return False
            assert client.has_outgoing_event(
                sent_text_document_did_replace_foo_with_bar
            )

            assert client.await_validation(doc.uri, doc.version) is not None

            rdoc = client.get_remote_document(doc.uri)
            assert doc.uri == rdoc["uri"]
            assert doc.version == rdoc["version"]

            with open(tmp_file_1.name) as f:
                assert doc.text == rdoc["text"] == f.read() == "\n".join([
                    "modulbare module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.backspace()
            doc.save()

            def sent_text_document_did_backspace_r(event: OutgoingEvent) -> bool:
                if isinstance(event.data, TextDocumentDidChangeNotification):
                    notification = event.data
                    params = notification.params
                    if params.text_document.uri == doc.uri:
                        content_changes = params.content_changes
                        if len(content_changes) == 1:
                            content_change = content_changes[0]
                            if isinstance(content_change,
                                          TextDocumentContentChangeEvent_Type1):
                                range = content_change.range
                                start = range.start
                                end = range.end
                                return start.line == 0 \
                                    and start.character == 7 \
                                    and end.line == 0 \
                                    and end.character == 8 \
                                    and content_change.text == ""
                return False
            assert client.has_outgoing_event(sent_text_document_did_backspace_r)

            assert client.await_validation(doc.uri, doc.version) is not None

            rdoc = client.get_remote_document(doc.uri)
            assert doc.uri == rdoc["uri"]
            assert doc.version == rdoc["version"]

            with open(tmp_file_1.name) as f:
                assert doc.text == rdoc["text"] == f.read() == "\n".join([
                    "modulbae module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.cursor = 1,6
            doc.delete(2)
            doc.save()

            def sent_text_document_did_delete_ba(event: OutgoingEvent) -> bool:
                if isinstance(event.data, TextDocumentDidChangeNotification):
                    notification = event.data
                    params = notification.params
                    if params.text_document.uri == doc.uri:
                        content_changes = params.content_changes
                        if len(content_changes) == 1:
                            content_change = content_changes[0]
                            if isinstance(content_change,
                                          TextDocumentContentChangeEvent_Type1):
                                range = content_change.range
                                start = range.start
                                end = range.end
                                return start.line == 0 \
                                    and start.character == 5 \
                                    and end.line == 0 \
                                    and end.character == 7 \
                                    and content_change.text == ""
                return False
            assert client.has_outgoing_event(sent_text_document_did_delete_ba)

            assert client.await_validation(doc.uri, doc.version) is not None

            rdoc = client.get_remote_document(doc.uri)
            assert doc.uri == rdoc["uri"]
            assert doc.version == rdoc["version"]

            with open(tmp_file_1.name) as f:
                assert doc.text == rdoc["text"] == f.read() == "\n".join([
                    "module module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.move(tmp_file_2.name)

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
            open(tmp_file_1.name, "w").close() # ensure it exists during clean-up
            assert os.path.exists(tmp_file_2.name)

            rdoc = client.get_remote_document(doc.uri)
            assert doc.uri == rdoc["uri"]
            assert doc.version == rdoc["version"]

            with open(tmp_file_2.name) as f:
                assert doc.text == rdoc["text"] == f.read() == "\n".join([
                    "module module_function_call1",
                    "end module module_function_call1",
                ]) + "\n"

            doc.remove()
            assert not os.path.exists(tmp_file_2.name)
            open(tmp_file_2.name, "w").close() # ensure it exists during clean-up
