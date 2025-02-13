from abc import ABC, abstractmethod
from collections import deque
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Collection, Deque, Dict, Iterator, Optional

DEFAULT_INDENT_PATTERN: str = "    "

class FileGenerator(ABC):
    file_path: Path
    schema: Dict[str, Any]

    indent_level: int = 0
    indent_pattern: str = DEFAULT_INDENT_PATTERN
    nested_names: Deque[str]

    def __init__(self, file_path: Path, schema: Dict[str, Any]) -> None:
        self.file_path = file_path
        self.schema = schema
        self.nested_names = deque()

    @contextmanager
    def indent(self, num_levels: int = 1) -> Iterator["FileGenerator"]:
        self.indent_level += num_levels
        yield self
        self.indent_level -= num_levels

    @contextmanager
    def nest_name(self, name: str) -> Iterator[str]:
        self.nested_names.append(name)
        yield self.nested_name()
        self.nested_names.pop()

    @contextmanager
    def nested_names_as(self, next_nested_names: Deque[str]) -> Iterator[str]:
        prev_nested_names = self.nested_names
        self.nested_names = next_nested_names
        yield self.nested_name()
        self.nested_names = prev_nested_names

    def nested_name(self, nested_names: Optional[Collection[str]] = None) -> str:
        if nested_names is None:
            nested_names = self.nested_names
        return "_".join(nested_names)

    def __enter__(self):
        return self.open()

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()

    def open(self):
        self.file_handle = self.file_path.open("w")
        return self

    def close(self):
        self.file_handle.close()

    def inline(
        self,
        message: Optional[str] = None,
        end: Optional[str] = None,
        indent: bool = False
    ) -> None:
        if indent:
            self.file_handle.write(self.indent_level * self.indent_pattern)
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
