import io
import json
import time
from io import BytesIO
from typing import IO, Optional, Union

from llanguage_test_client.json_rpc import JsonArray, JsonObject


def duration(start_time: float) -> float:
    return time.perf_counter() - start_time


class LspJsonStream:
    istream: IO[bytes]
    timeout_s: float

    buf: BytesIO
    num_bytes: int
    has_content_length: bool
    position: int
    sleep_s: float = 0.01

    def __init__(self, istream: IO[bytes], timeout_s: float) -> None:
        self.istream = istream
        self.timeout_s = timeout_s
        self.buf = BytesIO()

    def message(self) -> str:
        return self.buf.getvalue().decode("utf-8")

    def next_byte(self) -> bytes:
        start_time = time.perf_counter()
        while duration(start_time) < self.timeout_s:
            try:
                bs: Optional[bytes] = self.istream.read(1)
                if bs is not None:
                    self.buf.write(bs)
                    self.position += 1
                    return bs
            except BlockingIOError:
                # Try again ...
                pass
            time.sleep(self.sleep_s)
        raise RuntimeError(
            f'Timed-out after {self.timeout_s} seconds while reading from the stream:\n{self.message()}'
        )

    def next(self) -> Union[JsonObject, JsonArray]:
        self.num_bytes = 0
        self.position = 0
        self.buf.seek(0)
        self.buf.truncate(0)
        return self.parse_header_name()

    def escape(self, bs: bytes) -> bytes:
        match bs:
            case b'\n':
                return b'\\n'
            case b'\t':
                return b'\\t'
            case b'\b':
                return b'\\b'
            case b'\r':
                return b'\\r'
            case b'\f':
                return b'\\f'
            case _:
                return bs

    def parse_header_name(self) -> Union[JsonObject, JsonArray]:
        start: int = self.position
        while True:
            match self.next_byte():
                case b'\r':
                    length: int = self.position - start - 1
                    b = self.next_byte()
                    if b != b'\n':
                        raise RuntimeError(
                            f"Expected \\r to be followed by \\n, not '{self.escape(b)}':\n{self.message()}"
                        )
                    if length == 0 and self.has_content_length:
                        return self.parse_body()
                    raise RuntimeError(
                        f"Reached out-of-sequence newline while parsing header name:\n{self.message()}"
                    )
                case b'\n':
                    raise RuntimeError(
                        f"Reached out-of-sequence newline while parsing header name:\n{self.message()}"
                    )
                case b':':
                    length: int = self.position - start - 1
                    self.buf.seek(start)
                    header_name: str = self.buf.read(length).upper().decode("utf-8")
                    self.buf.seek(0, io.SEEK_END)
                    self.has_content_length = (header_name == "CONTENT-LENGTH")
                    return self.parse_header_value()
                case _:
                    pass

    def parse_header_value(self) -> Union[JsonObject, JsonArray]:
        start: int = self.position
        b = self.next_byte()
        while (b == b' ') or (b == b'\t'):
            b = self.next_byte()
        while True:
            match b:
                case b'\r':
                    length: int = self.position - start - 1
                    b = self.next_byte()
                    if b != b'\n':
                        raise RuntimeError(
                            f"Expected \\r to be followed by \\n, not '{self.escape(b)}':\n{self.message()}"
                        )
                    if self.has_content_length:
                        self.buf.seek(start)
                        header_value: str = self.buf.read(length).decode("utf-8")
                        self.buf.seek(0, io.SEEK_END)
                        self.num_bytes = int(header_value)
                    return self.parse_header_name()
                case b'\n':
                    raise RuntimeError(
                        f"Reached out-of-sequence newline while parsing header name:\n{self.message()}"
                    )
                case _:
                    b = self.next_byte()

    def parse_body(self) -> Union[JsonObject, JsonArray]:
        remaining_bytes = self.num_bytes
        start_time = time.perf_counter()
        while (remaining_bytes > 0) and (duration(start_time) < self.timeout_s):
            try:
                bs = self.istream.read(remaining_bytes)
                if bs is not None:
                    self.buf.write(bs)
                    remaining_bytes -= len(bs)
                    continue
            except BlockingIOError:
                pass
            time.sleep(0.01)
        if remaining_bytes > 0:
            raise RuntimeError(
                f'Timed-out after {self.timeout_s} seconds while reading from the stream:\n{self.message()}'
            )
        self.buf.seek(self.buf.tell() - self.num_bytes)
        body: str = self.buf.read().decode("utf-8")
        self.buf.seek(0, io.SEEK_END)
        return json.loads(body)
