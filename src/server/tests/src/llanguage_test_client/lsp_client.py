from abc import ABC, abstractmethod
from typing import List, Optional, Tuple

from lsprotocol.types import TextDocumentSaveReason

Uri = str
OldUri = Uri
NewUri = Uri
FileRename = Tuple[OldUri, NewUri]


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
    def workspace_did_rename_files(self, files: List[FileRename]) -> None:
        raise NotImplementedError
