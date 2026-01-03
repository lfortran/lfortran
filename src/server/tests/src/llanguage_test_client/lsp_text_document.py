from functools import wraps
from io import StringIO
from pathlib import Path
from typing import Any, Callable, List, Optional, Tuple, Union

from lsprotocol.types import (CompletionItem, CompletionList,
                              DocumentHighlight, DocumentSymbol, Hover,
                              Position, Range, SemanticTokens,
                              SymbolInformation, TextDocumentSaveReason,
                              TextEdit)

from llanguage_test_client.lsp_client import LspClient


def cantor_pair(k1: int, k2: int) -> int:
    return (((k1 + k2) * (k1 + k2 + 1)) // 2) + k2


def cantor_tuple(*ks: int) -> int:
    match len(ks):
        case 0:
            return 0
        case 1:
            k1 = ks[0]
            return k1
        case 2:
            k1, k2 = ks
            return cantor_pair(k1, k2)
        case _:
            kn = ks[-1]
            return cantor_pair(cantor_tuple(*ks[:-1]), kn)


def edit_key_fn(edit: TextEdit) -> int:
    """
    Orders TextEdit elements order according to their starting and ending
    positions.
    """
    range = edit.range
    start = range.start
    end = range.end
    return cantor_tuple(
        start.line, start.character,
        end.line, end.character
    )


def requires_path(fn: Callable) -> Callable:
    @wraps(fn)
    def wrapper(self, *args, **kwargs):
        if self._path is not None:
            fn(self, *args, **kwargs)
    return wrapper


ChangeListener = Callable[[int, Optional[str]], None]


class LspTextDocument:
    client: LspClient
    document_id: int
    language_id: str
    _path: Optional[Path]

    version: int
    buf: StringIO
    is_new: bool
    pos_by_line: List[int]
    len_by_line: List[int]

    change_listeners: List[ChangeListener]

    selection: Optional[Range]
    symbol_highlights: Optional[List[DocumentHighlight]]
    preview: Optional[Hover]
    symbols: Union[List[SymbolInformation], List[DocumentSymbol], None]
    semantic_highlights: Union[SemanticTokens, None]
    completions: Union[List[CompletionItem], CompletionList, None]

    def __init__(
            self,
            client: LspClient,
            document_id: int,
            language_id: str,
            path: Optional[Union[Path, str]] = None
    ) -> None:
        self.client = client
        self.document_id = document_id
        self.language_id = language_id
        if path is not None:
            self.path = path
        else:
            self._path = None
        self.version = 0
        self.buf = StringIO()
        self.pos_by_line = [0]
        self.len_by_line = [1]
        self.is_new = (path is None)
        self.selection = None
        self.symbol_highlights = None
        self.preview = None
        self.symbols = None
        self.change_listeners = []
        self.semantic_highlights = None
        self.completions = None

    def on_change(self, listener: ChangeListener) -> None:
        self.change_listeners.append(listener)

    def signal_change(self) -> None:
        uri = self.uri if self._path is not None else None
        for change_fn in self.change_listeners:
            change_fn(self.document_id, uri)

    @property
    def path(self) -> Path:
        if self._path is not None:
            return self._path
        raise RuntimeError('`path` must be set before referencing it.')

    @path.setter
    def path(self, path: Optional[Union[Path, str]]) -> None:
        match path:
            case str():
                self._path = Path(path)
            case Path():
                self._path = path
            case None:
                pass
        if self._path is not None:
            if not self._path.exists():
                self._path.touch()
            self._path = self._path.absolute()

    @property
    def uri(self) -> str:
        return f"file://{self.path}"

    @property
    def text(self) -> str:
        return self.buf.getvalue()

    def __len__(self) -> int:
        return len(self.text)

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, LspTextDocument) \
            and self.document_id == other.document_id

    def __hash__(self) -> int:
        return hash((self.document_id,))

    @property
    def cursor(self) -> Tuple[int, int]:
        line, column = self.pos_to_linecol(self.position)
        return line + 1, column + 1

    @cursor.setter
    def cursor(self, line_and_col: Tuple[int, int]) -> None:
        line, col = line_and_col
        if line < 0:
            line = len(self.pos_by_line) + line
        else:
            line -= 1
        if col < 0:
            col = self.len_by_line[line] + col
        else:
            col -= 1
        self.seek(line, col)

    @property
    def position(self) -> int:
        return self.buf.tell()

    @position.setter
    def position(self, position: int) -> None:
        num_chars = len(self.text)
        if position < num_chars:
            self.buf.seek(position)
        else:
            raise ValueError(
                f'Position out-of-bounds for text of length {num_chars}: {position}'
            )

    @property
    def line(self) -> int:
        line, _ = self.cursor
        return line

    @property
    def column(self) -> int:
        _, column = self.cursor
        return column

    def recompute_indices(self) -> None:
        self.pos_by_line.clear()
        self.len_by_line.clear()
        col = 0
        self.pos_by_line.append(0)
        for pos, char in enumerate(self.text):
            if char == '\n':
                self.pos_by_line.append(pos + 1)
                self.len_by_line.append(col + 1)
                col = 0
            else:
                col += 1
        self.len_by_line.append(col + 1)

    def load(self) -> None:
        if self._path is not None:
            with open(self.path, "r", encoding="utf-8") as f:
                self.buf.write(f.read())
            self.recompute_indices()
            self.client.text_document_did_open(
                self.document_id,
                self.uri,
                self.language_id,
                self.bump_version(),
                self.text
            )
            self.signal_change()
        else:
            raise RuntimeError('`path` must be set before calling `load()`')

    def close(self) -> None:
        uri = self.uri if self._path is not None else None
        self.client.text_document_did_close(self.document_id, uri)

    def save(self, path: Optional[Union[Path, str]] = None) -> None:
        self.path = path
        if self._path is None:
            raise RuntimeError('`path` must be set before calling `save()`')
        if not self.is_new:
            reason = TextDocumentSaveReason.Manual
            self.client.text_document_will_save_wait_until(self.uri, reason)
            self.client.text_document_will_save(self.uri, reason)
        else:
            self.client.workspace_will_create_files([self.uri])
        with open(self.path, "w+", encoding="utf-8") as f:
            f.write(self.text)
        if self.is_new:
            self.client.workspace_did_create_files([self.uri])
            self.client.text_document_did_open(
                self.document_id,
                self.uri,
                self.language_id,
                self.bump_version(),
                self.text
            )
            self.is_new = False
        self.client.text_document_did_save(self.uri, self.text)

    def move(self, path: Union[Path, str]) -> None:
        old_path = self._path
        if old_path is not None:
            old_uri = self.uri
            self.path = path
            new_path = self.path
            if old_path != new_path:
                new_uri = self.uri
                files = [
                    (old_uri, new_uri),
                ]
                self.client.workspace_will_rename_files(files)
                old_path.replace(new_path)
                self.client.workspace_did_rename_files(files)
        else:
            raise RuntimeError('Must call `save()` before `rename()`')

    def remove(self) -> None:
        if self._path is None:
            raise RuntimeError('`path` must be set before calling `remove()`')
        uri = self.uri
        self.client.workspace_will_delete_files([uri])
        self._path.unlink()
        self._path = None
        self.client.workspace_did_delete_files([uri])

    def bump_version(self) -> int:
        self.version += 1
        return self.version

    def pos_to_linecol(self, position: int) -> Tuple[int, int]:
        num_chars = len(self.text)
        if position <= num_chars:
            num_lines = len(self.len_by_line)
            lower = 0
            upper = num_lines - 1
            line = (lower + upper + 1) // 2
            column = position - self.pos_by_line[line]
            while (column < 0) or (column >= self.len_by_line[line]):
                if column < 0:
                    upper = line - 1
                else:
                    lower = line
                line = (lower + upper + 1) // 2
                column = position - self.pos_by_line[line]
            return line, column
        raise ValueError(
            f'Position out-of-bounds for text of length {num_chars}: {position}'
        )

    def linecol_to_pos(self, line: int, column: int) -> int:
        num_lines = len(self.pos_by_line)
        if line < num_lines:
            num_columns = self.len_by_line[line]
            if column <= num_columns:
                position = self.pos_by_line[line] + column
                return position
            raise ValueError(
                f'Column out-of-bounds for line {line} with {num_columns} column(s): {column}'
            )
        raise ValueError(
            f'Line out-of-bounds for {num_lines} line(s): {line}'
        )

    def seek(self, line: int, column: int) -> None:
        """
        Moves the cursor to the given location (0-indexed line and column).
        """
        position = self.linecol_to_pos(line, column)
        self.buf.seek(position)

    def select(
            self,
            start_line: int,
            start_column: int,
            end_line: int,
            end_column: int
    ) -> None:
        start = Position(start_line - 1, start_column - 1)
        end = Position(end_line - 1, end_column - 1)
        self.selection = Range(start, end)
        if (start_line < end_line) \
           or ((start_line == end_line) and (start_column <= end_column)):
            self.seek(end_line - 1, end_column - 1)
        else:
            self.seek(start_line - 1, start_column - 1)

    def delete(self, length: int = 1) -> None:
        """
        Deletes characters from the current location.
        """
        start = self.buf.tell()
        start_line, start_column = self.pos_to_linecol(start)
        end = min(start + length, len(self.text))
        end_line, end_column = self.pos_to_linecol(end)
        self.buf.seek(end)
        suffix = self.buf.read()
        self.buf.seek(start)
        self.buf.write(suffix)
        self.buf.truncate()
        self.buf.seek(start)
        if self._path is not None:
            self.client.text_document_did_change(
                self.uri, self.bump_version(),
                start_line, start_column,
                end_line, end_column,
                self.text, ""
            )
        self.recompute_indices()
        self.signal_change()

    def backspace(self, length: int = 1) -> None:
        """
        Deletes characters up to the current location.
        """
        stop = self.buf.tell()
        start = stop - length
        self.buf.seek(start)
        self.delete(length)

    def write(self, text: str) -> None:
        """
        Inserts text at the current location (does not overwrite existing text).
        """
        start = self.position
        end = start
        start_line, start_column = self.pos_to_linecol(start)
        end_line, end_column = self.pos_to_linecol(end)
        suffix = self.buf.read()
        self.buf.seek(start)
        self.buf.write(text)
        self.buf.write(suffix)
        if self._path is not None:
            self.client.text_document_did_change(
                self.uri, self.bump_version(),
                start_line, start_column,
                end_line, end_column,
                self.text, text
            )
        self.recompute_indices()
        self.signal_change()

    def newline(self) -> None:
        """
        Inserts a carriage return at the current location.
        """
        self.write('\n')

    def replace(self, text: str) -> None:
        """
        Overwrites (substitutes) text at the current location.
        """
        start = self.position
        end = min(start + len(text), len(self.text))
        start_line, start_column = self.pos_to_linecol(start)
        end_line, end_column = self.pos_to_linecol(end)
        self.buf.write(text)
        if self._path is not None:
            self.client.text_document_did_change(
                self.uri, self.bump_version(),
                start_line, start_column,
                end_line, end_column,
                self.text, text
            )
        self.recompute_indices()
        self.signal_change()

    def apply(self, edits: List[TextEdit]) -> None:
        origin = self.position
        for edit in sorted(edits, key=edit_key_fn, reverse=True):
            range = edit.range
            start = range.start
            end = range.end
            self.seek(end.line, end.character)
            suffix = self.buf.read()
            self.seek(start.line, start.character)
            self.buf.write(edit.new_text)
            # Append the newline if it was replaced
            if end.character == self.len_by_line[end.line]:
                self.buf.write('\n')
            self.buf.write(suffix)
            self.buf.truncate()
        origin = min(origin, len(self.text) - 1)
        self.buf.seek(origin)
        self.recompute_indices()

    @requires_path
    def goto_definition(self) -> None:
        line, column = self.pos_to_linecol(self.position)
        self.client.goto_definition(self.uri, line, column)

    @requires_path
    def rename(self, new_name: str) -> None:
        line, column = self.pos_to_linecol(self.position)
        self.client.rename(self.uri, line, column, new_name)

    @requires_path
    def highlight_symbol(self) -> None:
        line, column = self.pos_to_linecol(self.position)
        self.client.highlight_symbol(self.uri, line, column)

    @requires_path
    def hover(self) -> None:
        line, column = self.pos_to_linecol(self.position)
        self.client.hover(self.uri, line, column)

    @requires_path
    def semantic_highlight(self) -> None:
        self.client.semantic_highlight(self.uri)

    @requires_path
    def complete(self) -> None:
        line, column = self.pos_to_linecol(self.position)
        self.client.complete(self.uri, line, column)

    @requires_path
    def format(self) -> None:
        self.client.format(self.uri)

    @requires_path
    def format_range(self) -> None:
        if self.selection is not None:
            self.client.format_range(self.uri, self.selection)
