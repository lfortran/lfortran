import io
import json
from typing import IO, Union

from llanguage_test_client.json_rpc import JsonArray, JsonObject


class LspJsonStream:
    istream: IO[bytes]
    timeout: float

    buf: io.StringIO
    num_bytes: int
    has_content_length: bool
    position: int

    def __init__(self, istream: IO[bytes], timeout: float) -> None:
        self.istream = istream
        self.timeout = timeout
        self.buf = io.StringIO()

    def message(self) -> str:
        return self.buf.getvalue()

    def next_char(self) -> str:
        c: str = self.istream.read(1).decode()
        self.buf.write(c)
        self.position += 1
        return c

    def next(self) -> Union[JsonObject, JsonArray]:
        self.num_bytes = 0
        self.position = 0
        self.buf.seek(0)
        self.buf.truncate(0)
        return self.parse_header_name()

    def escape(self, c: str) -> str:
        match c:
            case '\n':
                return '\\n'
            case '\t':
                return '\\t'
            case '\b':
                return '\\b'
            case '\r':
                return '\\r'
            case '\f':
                return '\\f'
            case _:
                return c

    def parse_header_name(self) -> Union[JsonObject, JsonArray]:
        start: int = self.position
        while True:
            match self.next_char():
                case '\r':
                    length: int = self.position - start - 1
                    c = self.next_char()
                    if c != '\n':
                        raise RuntimeError(
                            f"Expected \\r to be followed by \\n, not '{self.escape(c)}': {self.buf.getvalue()}"
                        )
                    if length == 0 and self.has_content_length:
                        return self.parse_body()
                    raise RuntimeError(
                        f"Reached out-of-sequence newline while parsing header name: {self.buf.getvalue()}"
                    )
                case '\n':
                    raise RuntimeError(
                        f"Reached out-of-sequence newline while parsing header name: {self.buf.getvalue()}"
                    )
                case ':':
                    length: int = self.position - start - 1
                    self.buf.seek(start)
                    header_name = self.buf.read(length).upper()
                    self.buf.seek(0, io.SEEK_END)
                    self.has_content_length = (header_name == "CONTENT-LENGTH")
                    return self.parse_header_value()
                case _:
                    pass

    def parse_header_value(self) -> Union[JsonObject, JsonArray]:
        start: int = self.position
        c: str = self.next_char()
        while (c == ' ') or (c == '\t'):
            c = self.next_char()
        while True:
            match c:
                case '\r':
                    length: int = self.position - start - 1
                    c = self.next_char()
                    if c != '\n':
                        raise RuntimeError(
                            f"Expected \\r to be followed by \\n, not '{self.escape(c)}': {self.buf.getvalue()}"
                        )
                    if self.has_content_length:
                        self.buf.seek(start)
                        header_value = self.buf.read(length)
                        self.buf.seek(0, io.SEEK_END)
                        self.num_bytes = int(header_value)
                    return self.parse_header_name()
                case '\n':
                    raise RuntimeError(
                        f"Reached out-of-sequence newline while parsing header name: {self.buf.getvalue()}"
                    )
                case _:
                    c = self.next_char()

    def parse_body(self) -> Union[JsonObject, JsonArray]:
        body = self.istream.read(self.num_bytes).decode()
        self.buf.write(body)
        return json.loads(body)
