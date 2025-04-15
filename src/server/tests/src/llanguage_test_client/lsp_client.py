from abc import ABC, abstractmethod
from typing import List, Optional, Tuple

from lsprotocol.types import Range, TextDocumentSaveReason

Uri = str
OldUri = Uri
NewUri = Uri
FileRenameMapping = Tuple[OldUri, NewUri]


class LspClient(ABC):

    @abstractmethod
    def text_document_did_open(
            self,
            document_id: int,
            uri: str,
            language_id: str,
            version: int,
            text: str
    ) -> None:
        raise NotImplementedError

    @abstractmethod
    def text_document_did_close(
            self,
            document_id: int,
            uri: Optional[str]
    ) -> None:
        raise NotImplementedError

    @abstractmethod
    def text_document_will_save(
            self,
            uri: str,
            reason: TextDocumentSaveReason
    ) -> None:
        raise NotImplementedError

    @abstractmethod
    def text_document_will_save_wait_until(
            self,
            uri: str,
            reason: TextDocumentSaveReason
    ) -> None:
        raise NotImplementedError

    @abstractmethod
    def text_document_did_save(
            self,
            uri: str,
            text: str
    ) -> None:
        raise NotImplementedError

    @abstractmethod
    def text_document_did_change(
            self,
            uri: str,
            version: int,
            start_line: int,
            start_column: int,
            end_line: int,
            end_column: int,
            full_text: str,
            part_text: str
    ) -> None:
        raise NotImplementedError

    @abstractmethod
    def workspace_will_create_files(self, files: List[Uri]) -> None:
        raise NotImplementedError

    @abstractmethod
    def workspace_did_create_files(self, files: List[Uri]) -> None:
        raise NotImplementedError

    @abstractmethod
    def workspace_will_rename_files(self, files: List[FileRenameMapping]) -> None:
        raise NotImplementedError

    @abstractmethod
    def workspace_did_rename_files(self, files: List[FileRenameMapping]) -> None:
        raise NotImplementedError

    @abstractmethod
    def workspace_will_delete_files(self, files: List[Uri]) -> None:
        raise NotImplementedError

    @abstractmethod
    def workspace_did_delete_files(self, files: List[Uri]) -> None:
        raise NotImplementedError

    @abstractmethod
    def goto_definition(self, uri: str, line: int, column: int) -> None:
        raise NotImplementedError

    @abstractmethod
    def rename(self, uri: str, line: int, column: int, new_name: str) -> None:
        raise NotImplementedError

    @abstractmethod
    def highlight_symbol(self, uri: str, line: int, column: int) -> None:
        raise NotImplementedError

    @abstractmethod
    def hover(self, uri: str, line: int, column: int) -> None:
        raise NotImplementedError

    @abstractmethod
    def semantic_highlight(self, uri: str) -> None:
        raise NotImplementedError

    @abstractmethod
    def complete(self, uri: str, line: int, column: int) -> None:
        raise NotImplementedError

    @abstractmethod
    def format(self, uri: str) -> None:
        raise NotImplementedError

    @abstractmethod
    def format_range(self, uri: str, selection: Range) -> None:
        raise NotImplementedError
