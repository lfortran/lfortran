from abc import ABC, abstractmethod
from collections import deque
from contextlib import contextmanager
from pathlib import Path
from typing import Collection, Deque, Iterator, Optional

DEFAULT_INDENT_PATTERN: str = "    "
DEFAULT_COLUMN_WIDTH: int = 80

class FileGenerator(ABC):
    file_path: Path

    indent_level: int = 0
    indent_pattern: str = DEFAULT_INDENT_PATTERN
    column_width: int = DEFAULT_COLUMN_WIDTH

    def __init__(self, file_path: Path) -> None:
        self.file_path = file_path

    @contextmanager
    def indent(self, num_levels: int = 1) -> Iterator["FileGenerator"]:
        self.indent_level += num_levels
        yield self
        self.indent_level -= num_levels

    def __enter__(self):
        return self.open()

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()

    def open(self):
        self.file_handle = self.file_path.open("w")
        return self

    def close(self):
        self.file_handle.close()

    def indentation(self) -> str:
        return self.indent_level * self.indent_pattern

    def inline(
        self,
        message: Optional[str] = None,
        end: Optional[str] = None,
        indent: bool = False
    ) -> None:
        if indent:
            self.file_handle.write(self.indentation())
        if message is not None:
            self.file_handle.write(message)
        if end is not None:
            self.file_handle.write(end)

    def write(self, message: str, end: Optional[str] = "\n", indent: bool = True) -> None:
        self.inline(message=message, end=end, indent=indent)

    def newline(self) -> None:
        self.file_handle.write("\n")

    @abstractmethod
    def generate_code(self) -> None:
        raise NotImplementedError()
