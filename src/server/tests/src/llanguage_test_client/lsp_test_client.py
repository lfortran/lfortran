import json
import os
import re
import signal
import subprocess
import sys
import threading
import time
import traceback
from collections import defaultdict
from contextlib import contextmanager
from dataclasses import dataclass
from datetime import datetime, timezone
from functools import wraps
from io import BytesIO
from pathlib import Path
from typing import (IO, Any, BinaryIO, Callable, Dict, Iterator, List,
                    Optional, Tuple, Union)

import psutil

from cattrs import Converter

from lsprotocol import converters
from lsprotocol.types import (ClientCapabilities,
                              ClientRegisterCapabilityRequest,
                              ClientRegisterCapabilityResponse,
                              CompletionParams, CreateFilesParams,
                              DeleteFilesParams,
                              DidChangeConfigurationClientCapabilities,
                              DidChangeConfigurationParams,
                              DidChangeTextDocumentParams,
                              DidCloseTextDocumentParams,
                              DidOpenTextDocumentParams,
                              DidSaveTextDocumentParams,
                              DocumentFormattingParams,
                              DocumentHighlightParams,
                              DocumentRangeFormattingParams,
                              DocumentSymbolParams, ExitNotification,
                              FileCreate, FileDelete,
                              FileOperationClientCapabilities, FileRename,
                              FormattingOptions, HoverParams,
                              InitializedNotification, InitializedParams,
                              InitializeParams, InitializeRequest,
                              InitializeResponse,
                              InitializeResultServerInfoType, Position, Range,
                              Registration, RenameFilesParams, SaveOptions,
                              SemanticTokensParams, ServerCapabilities,
                              ShutdownRequest, TelemetryEventNotification,
                              TextDocumentClientCapabilities,
                              TextDocumentCompletionRequest,
                              TextDocumentContentChangeEvent_Type1,
                              TextDocumentContentChangeEvent_Type2,
                              TextDocumentDidChangeNotification,
                              TextDocumentDidCloseNotification,
                              TextDocumentDidOpenNotification,
                              TextDocumentDidSaveNotification,
                              TextDocumentDocumentHighlightRequest,
                              TextDocumentDocumentSymbolRequest,
                              TextDocumentFormattingRequest,
                              TextDocumentHoverRequest, TextDocumentIdentifier,
                              TextDocumentItem,
                              TextDocumentRangeFormattingRequest,
                              TextDocumentSaveReason,
                              TextDocumentSemanticTokensFullRequest,
                              TextDocumentSyncClientCapabilities,
                              TextDocumentSyncKind, TextDocumentSyncOptions,
                              TextDocumentWillSaveNotification,
                              TextDocumentWillSaveWaitUntilRequest,
                              TextDocumentWillSaveWaitUntilResponse,
                              VersionedTextDocumentIdentifier,
                              WillSaveTextDocumentParams,
                              WorkspaceClientCapabilities,
                              WorkspaceConfigurationRequest,
                              WorkspaceConfigurationResponse,
                              WorkspaceDidChangeConfigurationNotification,
                              WorkspaceDidCreateFilesNotification,
                              WorkspaceDidDeleteFilesNotification,
                              WorkspaceDidRenameFilesNotification,
                              WorkspaceWillCreateFilesRequest,
                              WorkspaceWillDeleteFilesRequest,
                              WorkspaceWillRenameFilesRequest)

from llanguage_test_client.json_rpc import JsonObject, JsonValue
from llanguage_test_client.lsp_client import FileRenameMapping, LspClient, Uri
from llanguage_test_client.lsp_json_stream import LspJsonStream
from llanguage_test_client.lsp_text_document import LspTextDocument


def requires_server_capabilities(fn: Callable) -> Callable:
    @wraps(fn)
    def wrapper(self, *args, **kwargs):
        # NOTE: If an error occurred during the server's initialization, the
        # `server_capabilities` attribute may never have been set.
        if hasattr(self, 'server_capabilities'):
            return fn(self, *args, **kwargs)
        return False
    return wrapper


# NOTE: File URIs follow one of the following schemes:
# 1. `file:/path` (no hostname)
# 2. `file:///path` (empty hostname)
# 3. `file://hostname/path`
# NOTE: All we are interested in is the `/path` portion
RE_FILE_URI = re.compile(r"^file:(?://[^/]*)?", re.IGNORECASE)


def timestamp() -> str:
    now_utc = datetime.now(timezone.utc)
    return now_utc.strftime('%Y-%m-%dT%H:%M:%S.%f')[:-3] + 'Z'


class SpyIO(IO[bytes]):
    stream: IO[bytes]
    spy: BinaryIO
    buf: BytesIO

    def __init__(self, stream: IO[bytes], spy: BinaryIO) -> None:
        self.stream = stream
        self.spy = spy
        self.buf = BytesIO()

    def escape(self, bs: bytes) -> bytes:
        self.buf.seek(0)
        self.buf.truncate(0)
        for b in bs:
            match b:
                case 0x00:  #-> null
                    self.buf.write(b'\\0')
                case 0x07:  #-> bell
                    self.buf.write(b'\\a')
                case 0x08:  #-> backspace
                    self.buf.write(b'\\b')
                case 0x09:  #-> horizontal tab
                    self.buf.write(b'\\t')
                case 0x0A:  #-> line feed
                    self.buf.write(b'\\n')
                case 0x0B:  #-> vertical tab
                    self.buf.write(b'\\v')
                case 0x0C:  #-> form feed
                    self.buf.write(b'\\f')
                case 0x0D:  #-> carriage return
                    self.buf.write(b'\\r')
                # case 0x1A:  #-> Control-Z
                #     self.buf.write(b'^Z')
                case 0x1B:  #-> escape
                    self.buf.write(b'\\e')
                case 0x5C:  #-> backslash
                    self.buf.write(b'\\\\')
                case _:
                    self.buf.write(b.to_bytes(2, sys.byteorder))
        return self.buf.getvalue()

    def read(self, size: int = -1, /) -> Union[bytes, Any]:
        bs = self.stream.read(size)
        self.spy.write(f'[{timestamp()}] READ: '.encode("utf-8"))
        if bs is not None and len(bs) > 0:
            self.spy.write(self.escape(bs))
        else:
            self.spy.write(b'\xe2\x90\x80')  #-> U+2400 Unicode null symbol
        self.spy.write(b'\n')
        return bs

    def write(self, b) -> int:
        self.spy.write(f'[{timestamp()}] WRITE: '.encode("utf-8"))
        num_bytes = self.stream.write(b)
        if num_bytes is not None and num_bytes > 0:
            self.spy.write(self.escape(b[:num_bytes]))
        else:
            self.spy.write(b'\xe2\x90\x80')  #-> U+2400 Unicode null symbol
        self.spy.write(b'\n')
        return num_bytes

    def flush(self) -> None:
        self.stream.flush()
        self.spy.flush()


@dataclass
class Event:
    data: Any


@dataclass
class IncomingEvent(Event):
    pass


@dataclass
class OutgoingEvent(Event):
    pass


EventPredFn = Callable[[Event], bool]
IncomingEventPredFn = Callable[[IncomingEvent], bool]
OutgoingEventPredFn = Callable[[OutgoingEvent], bool]


Callback = Callable[[Any, JsonObject], None]
ServerProcess = subprocess.Popen[bytes]


SECONDS_PER_MILLISECOND: float = 1. / 1000.


class LspTestClient(LspClient):
    server_path: Path
    server_params: List[str]
    workspace_path: Optional[Path]
    timeout_s: float

    server: ServerProcess
    process: psutil.Process
    message_stream: LspJsonStream
    ostream: IO[bytes]
    buf: BytesIO
    converter: Converter
    serial_request_id: int
    config: Dict[str, Any]
    server_capabilities: ServerCapabilities
    server_info: Optional[InitializeResultServerInfoType]
    dynamic_registrations: Dict[str, List[Registration]]
    events: List[Event]
    serial_document_id: int
    documents_by_id: Dict[int, LspTextDocument]
    documents_by_uri: Dict[str, LspTextDocument]
    active_document: Optional[LspTextDocument]
    requests_by_id: Dict[JsonValue, Any]
    responses_by_id: Dict[JsonValue, Any]
    callbacks_by_id: Dict[JsonValue, Tuple[Any, Callback]]
    stop: threading.Event
    stderr_printer: threading.Thread
    client_log_path: str
    stdout_log_path: str
    stdin_log_path: str

    def __init__(
            self,
            server_path: Path,
            server_params: List[str],
            workspace_path: Optional[Path],
            timeout_ms: float,
            config: Dict[str, Any],
            client_log_path: str,
            stdout_log_path: str,
            stdin_log_path: str
    ) -> None:
        self.server_path = server_path
        self.server_params = server_params
        self.workspace_path = workspace_path
        self.timeout_s = timeout_ms * SECONDS_PER_MILLISECOND
        self.buf = BytesIO()
        self.converter = converters.get_converter()
        self.serial_request_id = 0
        self.config = config
        self.dynamic_registrations = defaultdict(list)
        self.events = list()
        self.serial_document_id = 0
        self.documents_by_id = dict()
        self.documents_by_uri = dict()
        self.active_document = None
        self.requests_by_id = dict()
        self.responses_by_id = dict()
        self.callbacks_by_id = dict()
        self.stop = threading.Event()
        self.stderr_printer = threading.Thread(
            target=self.print_stderr,
            args=tuple()
        )
        self.client_log_path = client_log_path
        self.stdout_log_path = stdout_log_path
        self.stdin_log_path = stdin_log_path

    def print_stderr(self) -> None:
        buf = BytesIO()
        try:
            if self.server.stderr is not None:
                while self.check_server():
                    try:
                        bs = self.server.stderr.read(4096)
                        if bs is not None:
                            buf.seek(0)
                            buf.truncate(0)
                            while (bs is not None) and (len(bs) > 0):
                                buf.write(bs)
                                bs = self.server.stderr.read(4096)
                            print(buf.getvalue().decode('utf-8'), end='', file=sys.stderr)
                            continue
                    except BlockingIOError:
                        pass
                    time.sleep(0.100)
        except BaseException as e:
            print(
                "Caught unhandled exception while printing stderr logs from the server:", e,
                file=sys.stderr
            )
            traceback.print_exc(file=sys.stderr)
        finally:
            # Print whatever is left in the buffer
            buf.seek(0)
            buf.truncate(0)
            if self.server.stderr is not None:
                bs = self.server.stderr.read(4096)
                while (bs is not None) and (len(bs) > 0):
                    buf.write(bs)
                    bs = self.server.stderr.read(4096)
            print(buf.getvalue().decode('utf-8'), file=sys.stderr)

    def find_incoming_event(self, pred: IncomingEventPredFn, lower_index: int = 0) \
            -> Tuple[Optional[IncomingEvent], int]:
        upper_bound = len(self.events)
        for index in range(lower_index, upper_bound):
            event = self.events[index]
            if isinstance(event, IncomingEvent) and pred(event):
                return event, index
        return None, max(upper_bound - 1, 0)

    def has_incoming_event(self, pred: IncomingEventPredFn) -> bool:
        return self.find_incoming_event(pred)[0] is not None

    def has_outgoing_event(self, pred: OutgoingEventPredFn) -> bool:
        for event in self.events:
            if isinstance(event, OutgoingEvent) and pred(event):
                return True
        return False

    def next_request_id(self) -> int:
        request_id = self.serial_request_id
        self.serial_request_id += 1
        return request_id

    def next_document_id(self) -> int:
        document_id = self.serial_document_id
        self.serial_document_id += 1
        return document_id

    @contextmanager
    def serve(self) -> Iterator["LspTestClient"]:
        with open(self.client_log_path, "w") as log_file:
            self.log_file = log_file
            with open(self.stdout_log_path, "wb") as stdout_log:
                self.stdout_log = stdout_log
                with open(self.stdin_log_path, "wb") as stdin_log:
                    self.stdin_log = stdin_log
                    self.initialize()
                    yield self
                    self.terminate()

    def new_document(self, language_id: str) -> LspTextDocument:
        return self.open_document(language_id, None)

    def open_document(
            self,
            language_id: str,
            path: Optional[Union[Path, str]],
            make_active: bool = True
    ) -> LspTextDocument:
        document_id = self.next_document_id()
        document = LspTextDocument(self, document_id, language_id, path)
        document.on_change(self.update_document_symbols)
        self.documents_by_id[document_id] = document
        if path is not None:
            self.documents_by_uri[document.uri] = document
            document.load()
        if make_active:
            self.active_document = document
        return document

    def get_document(self, language_id: str, uri: str) -> LspTextDocument:
        document = self.documents_by_uri.get(uri, None)
        if document is not None:
            return document
        path = RE_FILE_URI.sub(uri, "")
        return self.open_document(language_id, path)

    def await_response(
            self,
            request_id: int
    ) -> Any:
        message = self.responses_by_id.get(request_id, None)
        if message is not None:
            return message
        while not self.stop.is_set():
            message = self.receive_message()
            if "result" in message:
                response_id = message.get("id", None)
                if request_id == response_id:
                    return message
            # NOTE: The response may have been recorder by a nested
            # iteration of `await_response`
            message = self.responses_by_id.get(request_id, None)
            if message is not None:
                return message

    def initialize_params(self) -> InitializeParams:
        params = InitializeParams(
            capabilities=ClientCapabilities(
                workspace=WorkspaceClientCapabilities(
                    did_change_configuration=DidChangeConfigurationClientCapabilities(
                        dynamic_registration=True,
                    ),
                    configuration=True,
                    file_operations=FileOperationClientCapabilities(
                        did_create=True,
                        will_create=True,
                        did_rename=True,
                        will_rename=True,
                        did_delete=True,
                        will_delete=True,
                    ),
                ),
                text_document=TextDocumentClientCapabilities(
                    synchronization=TextDocumentSyncClientCapabilities(
                        dynamic_registration=True,
                        will_save=True,
                        will_save_wait_until=False,
                        did_save=True,
                    ),
                ),
            ),
        )
        return params

    def initialized_params(self) -> InitializedParams:
        params = InitializedParams()
        return params

    def initialize(self) -> None:
        self.server = subprocess.Popen(
            [self.server_path] + self.server_params,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            bufsize=0,
        )

        if self.server.stdout is None:
            raise RuntimeError("Cannot read from server stdout")
        if self.server.stdin is None:
            raise RuntimeError("Cannot write to server stdin")
        if self.server.stderr is None:
            raise RuntimeError("Cannot read from server stderr")

        # Make process stdout non-blocking
        stdout_fd = self.server.stdout.fileno()
        os.set_blocking(stdout_fd, False)
        stderr_fd = self.server.stderr.fileno()
        os.set_blocking(stderr_fd, False)

        self.process = psutil.Process(self.server.pid)

        self.message_stream = LspJsonStream(
            SpyIO(self.server.stdout, self.stdout_log),
            self.timeout_s,
            self.check_server
        )
        self.ostream = SpyIO(self.server.stdin, self.stdin_log)

        self.stderr_printer.start()

        initialize_id = self.send_initialize(self.initialize_params())
        self.await_response(initialize_id)

        self.send_initialized(self.initialized_params())

    def terminate(self) -> None:
        shutdown_id = self.send_shutdown()
        self.await_response(shutdown_id)

        self.send_exit()

        self.stop.set()
        self.stderr_printer.join()

        try:
            if self.timeout_s > 0.0:
                self.server.wait(timeout=self.timeout_s)
            else:
                self.server.wait()
        except subprocess.TimeoutExpired as e:
            self.kill_server()
            raise RuntimeError(
                f"Timed-out after {self.timeout_s} seconds while awaiting the server to terminate."
            ) from e

    def kill_server(self) -> None:
        self.stop.set()
        if self.server.poll() is None:
            print(
                'Server did not terminate cleanly, terminating it forcefully ...',
                file=sys.stderr
            )
            try:
                os.kill(self.server.pid, signal.SIGINT)
                self.server.wait(timeout=1.0)
            except ProcessLookupError:
                pass
            finally:
                if self.server.poll() is None:
                    try:
                        os.kill(self.server.pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass

    def check_server(self) -> bool:
        try:
            if self.process.is_running():
                if self.process.status() == psutil.STATUS_ZOMBIE:
                    self.kill_server()
                    print(
                        "ServerProcess has become defunct and will no longer respond",
                        file=sys.stderr
                    )
                else:
                    # The server is running as expected; there's nothing to do
                    pass
            else:
                self.stop.set()
                print(
                    f"ServerProcess crashed with status: {self.server.returncode}",
                    file=sys.stderr
                )
        except BaseException as e:
            self.stop.set()
            raise e
        finally:
            return not self.stop.is_set()

    def send_message(self, message: Any) -> None:
        self.events.append(OutgoingEvent(message))
        body = json.dumps(
            message,
            separators=(",",":"),
            default=self.converter.unstructure,
        ).encode("utf-8")
        self.buf.seek(0)
        self.buf.truncate(0)
        self.buf.write(f"Content-Length: {len(body)}\r\n".encode("utf-8"))
        self.buf.write(b"\r\n")
        self.buf.write(body)
        print(
            f"[{timestamp()}] Sending:\n{self.buf.getvalue().decode('utf-8')}",
            file=self.log_file
        )
        self.log_file.flush()
        if self.check_server():
            self.ostream.write(self.buf.getvalue())
            self.ostream.flush()

    def send_request(
            self,
            request_id: int,
            request: Any,
            callback: Callback
    ) -> int:
        self.requests_by_id[request_id] = request
        self.callbacks_by_id[request_id] = (request, callback)
        self.send_message(request)
        return request_id

    def receive_message(self) -> JsonObject:
        if self.check_server():
            message = self.message_stream.next()
            print(
                f"[{timestamp()}] Receiving:\n{self.message_stream.message()}",
                file=self.log_file
            )
            match message:
                case dict():
                    self.events.append(IncomingEvent(message))
                    self.dispatch_message(message)
                    return message
                case _:
                    raise RuntimeError(
                        f"Unsupported message type ({type(message)}): {message}"
                    )
        else:
            raise RuntimeError("Server has terminated.")

    def dispatch_message(self, message: JsonObject) -> None:
        if 'method' in message:
            self.dispatch_method(message)
        elif 'result' in message:
            self.dispatch_response(message)
        elif 'error' in message:
            self.dispatch_error(message)
        else:
            # notification response
            pass

    def dispatch_method(self, message: JsonObject) -> None:
        match message["method"]:
            case "client/registerCapability":
                request = self.converter.structure(
                    message,
                    ClientRegisterCapabilityRequest
                )
                response = self.receive_client_register_capability(request)
                self.send_message(response)
            case "workspace/configuration":
                request = self.converter.structure(
                    message,
                    WorkspaceConfigurationRequest
                )
                response = self.receive_workspace_configuration(request)
                self.send_message(response)
            case "telemetry/event":
                notification = self.converter.structure(
                    message,
                    TelemetryEventNotification
                )
                self.receive_telemetry_event(notification)

    def respond_to_notification(self) -> None:
        self.send_message({"id": None, "result": None, "jsonrpc": "2.0"})

    def dispatch_response(self, response: JsonObject) -> None:
        response_id = response.get("id", None)
        if response_id is not None:
            self.responses_by_id[response_id] = response
            record = self.callbacks_by_id.get(response_id, None)
            if record is not None:
                request, callback = record
                callback(request, response)
            else:
                raise ValueError(
                    f'No requests exists for response Id: {response_id}'
                )

    def dispatch_error(self, message: JsonObject) -> None:
        raise RuntimeError(
            f'ServerProcess responded with an error: {message}'
        )

    def serialize_json(self, message: Any) -> str:
        return json.dumps(message, default=self.converter.unstructure)

    def receive_telemetry_event(self, notification: TelemetryEventNotification) -> None:
        # Check the event in the logs
        pass

    def send_initialize(self, params: InitializeParams) -> int:
        request_id = self.next_request_id()
        request = InitializeRequest(request_id, params)
        return self.send_request(request_id, request, self.receive_initialize)

    def receive_initialize(self, request: Any, message: JsonObject) -> None:
        response = self.converter.structure(
            message,
            InitializeResponse
        )
        result = response.result
        self.server_capabilities = result.capabilities
        self.server_info = result.server_info

    def send_initialized(self, params: InitializedParams) -> None:
        notification = InitializedNotification(params)
        self.send_message(notification)

    def send_shutdown(self) -> int:
        request_id = self.next_request_id()
        request = ShutdownRequest(request_id)
        return self.send_request(request_id, request, self.receive_shutdown)

    def receive_shutdown(self, request: Any, message: JsonObject) -> None:
        # response = self.converter.structure(
        #     message,
        #     ShutdownResponse
        # )
        pass

    def send_exit(self) -> None:
        notification = ExitNotification()
        self.send_message(notification)

    def receive_client_register_capability(
            self,
            request: ClientRegisterCapabilityRequest
    ) -> ClientRegisterCapabilityResponse:
        for registration in request.params.registrations:
            registrations = self.dynamic_registrations[registration.method]
            registrations.append(registration)
            match registration.method:
                case "workspace/didChangeConfiguration":
                    self.send_workspace_did_change_configuration(
                        DidChangeConfigurationParams(self.config)
                    )
        response = ClientRegisterCapabilityResponse(request.id)
        return response

    def send_workspace_did_change_configuration(
            self,
            params: DidChangeConfigurationParams
    ) -> None:
        notification = WorkspaceDidChangeConfigurationNotification(params)
        self.send_message(notification)

    def receive_workspace_configuration(
            self,
            request: WorkspaceConfigurationRequest
    ) -> WorkspaceConfigurationResponse:
        configs = []
        for item in request.params.items:
            config: Any = self.config
            if item.section is not None:
                for name in item.section.split("."):
                    config = config.get(name, None)
                    if config is None:
                        break
            configs.append(config)
        response = WorkspaceConfigurationResponse(request.id, configs)
        return response

    @requires_server_capabilities
    def server_supports_text_document_did_open_or_close(self) -> bool:
        text_document_sync = self.server_capabilities.text_document_sync
        if isinstance(text_document_sync, TextDocumentSyncOptions):
            return bool(text_document_sync.open_close)
        return False

    def send_text_document_did_open(
            self,
            params: DidOpenTextDocumentParams
    ) -> None:
        notification = TextDocumentDidOpenNotification(params)
        self.send_message(notification)

    def text_document_did_open(
            self,
            document_id: int,
            uri: str,
            language_id: str,
            version: int,
            text: str
    ) -> None:
        document = self.documents_by_id[document_id]
        self.documents_by_uri[uri] = document
        if self.server_supports_text_document_did_open_or_close():
            params = DidOpenTextDocumentParams(
                text_document=TextDocumentItem(
                    uri=uri,
                    language_id=language_id,
                    version=version,
                    text=text
                )
            )
            self.send_text_document_did_open(params)

    def send_text_document_did_close(
            self,
            params: DidCloseTextDocumentParams
    ) -> None:
        notification = TextDocumentDidCloseNotification(params)
        self.send_message(notification)

    def text_document_did_close(
            self,
            document_id: int,
            uri: Optional[str]
    ) -> None:
        del self.documents_by_id[document_id]
        if self.active_document is not None \
           and self.active_document.document_id == document_id:
            for prev_document_id in range(document_id - 1, -1, -1):
                prev_document = self.documents_by_id.get(prev_document_id, None)
                if prev_document is not None:
                    self.active_document = prev_document
        if uri is not None:
            del self.documents_by_uri[uri]
            if self.server_supports_text_document_did_open_or_close():
                params = DidCloseTextDocumentParams(
                    text_document=TextDocumentIdentifier(
                        uri=uri
                    )
                )
                self.send_text_document_did_close(params)

    def send_text_document_will_save(
            self,
            params: WillSaveTextDocumentParams
    ) -> None:
        notification = TextDocumentWillSaveNotification(params)
        self.send_message(notification)

    @requires_server_capabilities
    def server_supports_text_document_will_save(self) -> bool:
        text_document_sync = self.server_capabilities.text_document_sync
        if isinstance(text_document_sync, TextDocumentSyncOptions):
            return bool(text_document_sync.will_save)
        return False

    def text_document_will_save(
            self,
            uri: str,
            reason: TextDocumentSaveReason
    ) -> None:
        if self.server_supports_text_document_will_save():
            params = WillSaveTextDocumentParams(
                text_document=TextDocumentIdentifier(
                    uri=uri
                ),
                reason=reason
            )
            self.send_text_document_will_save(params)

    def send_text_document_will_save_wait_until(
            self,
            params: WillSaveTextDocumentParams
    ) -> int:
        request_id = self.next_request_id()
        request = TextDocumentWillSaveWaitUntilRequest(request_id, params)
        return self.send_request(
            request_id,
            request,
            self.receive_text_document_will_save_wait_until
        )

    def receive_text_document_will_save_wait_until(
            self,
            request: Any,
            message: JsonObject
    ) -> None:
        response = self.converter.structure(
            message,
            TextDocumentWillSaveWaitUntilResponse
        )
        if response.result is not None:
            uri = request.uri
            document = self.documents_by_uri[uri]
            document.apply(response.result)

    @requires_server_capabilities
    def server_supports_text_document_will_save_wait_until(self) -> bool:
        text_document_sync = self.server_capabilities.text_document_sync
        if isinstance(text_document_sync, TextDocumentSyncOptions):
            return bool(text_document_sync.will_save_wait_until)
        return False

    def text_document_will_save_wait_until(
            self,
            uri: str,
            reason: TextDocumentSaveReason
    ) -> None:
        if self.server_supports_text_document_will_save_wait_until():
            params = WillSaveTextDocumentParams(
                text_document=TextDocumentIdentifier(
                    uri=uri
                ),
                reason=reason
            )
            request_id = self.send_text_document_will_save_wait_until(params)
            self.await_response(request_id)

    def send_text_document_did_save(
            self,
            params: DidSaveTextDocumentParams
    ) -> None:
        notification = TextDocumentDidSaveNotification(params)
        self.send_message(notification)

    @requires_server_capabilities
    def server_supports_full_text_on_save(self) -> bool:
        text_document_sync = self.server_capabilities.text_document_sync
        if isinstance(text_document_sync, TextDocumentSyncOptions):
            save = text_document_sync.save
            if isinstance(save, SaveOptions):
                return bool(save.include_text)
        return False

    @requires_server_capabilities
    def server_supports_did_save(self) -> bool:
        text_document_sync = self.server_capabilities.text_document_sync
        if isinstance(text_document_sync, TextDocumentSyncOptions):
            return text_document_sync.save is not None
        return False

    def text_document_did_save(self, uri: str, text: str) -> None:
        if self.server_supports_did_save():
            params = DidSaveTextDocumentParams(
                text_document=TextDocumentIdentifier(
                    uri=uri
                ),
                text=text if self.server_supports_full_text_on_save() else None
            )
            self.send_text_document_did_save(params)

    def send_text_document_did_change(
            self,
            params: DidChangeTextDocumentParams
    ) -> None:
        notification = TextDocumentDidChangeNotification(params)
        self.send_message(notification)

    @requires_server_capabilities
    def server_supports_text_document_sync_kind(
            self,
            kind: TextDocumentSyncKind
    ) -> bool:
        text_document_sync = self.server_capabilities.text_document_sync
        if text_document_sync is not None:
            match text_document_sync:
                case TextDocumentSyncKind():
                    return kind == text_document_sync
                case TextDocumentSyncOptions():
                    if text_document_sync.change is not None:
                        return kind == text_document_sync.change
        return kind == TextDocumentSyncKind.Full

    @requires_server_capabilities
    def server_supports_text_document_did_change(self) -> bool:
        text_document_sync = self.server_capabilities.text_document_sync
        if isinstance(text_document_sync, TextDocumentSyncOptions):
            return text_document_sync.change is not None \
                and text_document_sync.change != TextDocumentSyncKind.None_
        return False

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
        if self.server_supports_text_document_did_change():
            params = DidChangeTextDocumentParams(
                text_document=VersionedTextDocumentIdentifier(
                    version=version,
                    uri=uri
                ),
                content_changes=[
                    TextDocumentContentChangeEvent_Type1(
                        range=Range(
                            start=Position(
                                line=start_line,
                                character=start_column
                            ),
                            end=Position(
                                line=end_line,
                                character=end_column
                            )
                        ),
                        text=part_text
                    ) \
                    if self.server_supports_text_document_sync_kind(
                        TextDocumentSyncKind.Incremental
                    ) \
                    else TextDocumentContentChangeEvent_Type2(
                        text=full_text
                    )
                ]
            )
            self.send_text_document_did_change(params)

    @requires_server_capabilities
    def server_supports_workspace_will_create_files(self) -> bool:
        workspace = self.server_capabilities.workspace
        if workspace is not None:
            file_operations = workspace.file_operations
            if file_operations is not None:
                # TODO: Implement a parser for additional pattern matching
                return file_operations.will_create is not None
        return False

    def send_workspace_will_create_files(self, params: CreateFilesParams) -> int:
        request_id = self.next_request_id()
        request = WorkspaceWillCreateFilesRequest(request_id, params)
        return self.send_request(
            request_id,
            request,
            self.receive_workspace_will_create_files
        )

    def receive_workspace_will_create_files(
            self,
            request: Any,
            message: JsonObject
    ) -> None:
        # TODO: Update the workspace as requested by the response
        pass

    def workspace_will_create_files(self, files: List[Uri]) -> None:
        if self.server_supports_workspace_will_create_files():
            params = CreateFilesParams(
                files=[FileCreate(uri) for uri in files]
            )
            request_id = self.send_workspace_will_create_files(params)
            self.await_response(request_id)

    @requires_server_capabilities
    def server_supports_workspace_did_create_files(self) -> bool:
        workspace = self.server_capabilities.workspace
        if workspace is not None:
            file_operations = workspace.file_operations
            if file_operations is not None:
                # TODO: Implement a parser for additional pattern matching
                return file_operations.did_create is not None
        return False

    def send_workspace_did_create_files(self, params: CreateFilesParams) -> None:
        notification = WorkspaceDidCreateFilesNotification(params)
        self.send_message(notification)

    def workspace_did_create_files(self, files: List[Uri]) -> None:
        for uri in files:
            if uri in self.documents_by_uri:
                del self.documents_by_uri[uri]
        if self.server_supports_workspace_did_create_files():
            params = CreateFilesParams(
                files=[FileCreate(uri) for uri in files]
            )
            self.send_workspace_did_create_files(params)

    @requires_server_capabilities
    def server_supports_workspace_will_rename_files(self) -> bool:
        workspace = self.server_capabilities.workspace
        if workspace is not None:
            file_operations = workspace.file_operations
            if file_operations is not None:
                # TODO: Implement a parser for additional pattern matching
                return file_operations.will_rename is not None
        return False

    def send_workspace_will_rename_files(self, params: RenameFilesParams) -> int:
        request_id = self.next_request_id()
        request = WorkspaceWillRenameFilesRequest(request_id, params)
        return self.send_request(
            request_id,
            request,
            self.receive_workspace_will_rename_files
        )

    def receive_workspace_will_rename_files(
            self,
            request: Any,
            message: JsonObject
    ) -> None:
        # TODO: Update the workspace as requested by the response
        pass

    def workspace_will_rename_files(self, files: List[FileRenameMapping]) -> None:
        if self.server_supports_workspace_will_rename_files():
            params = RenameFilesParams(
                files=[FileRename(old_uri, new_uri) for old_uri, new_uri in files]
            )
            request_id = self.send_workspace_will_rename_files(params)
            self.await_response(request_id)

    @requires_server_capabilities
    def server_supports_workspace_did_rename_files(self) -> bool:
        workspace = self.server_capabilities.workspace
        if workspace is not None:
            file_operations = workspace.file_operations
            if file_operations is not None:
                # TODO: Implement a parser for additional pattern matching
                return file_operations.did_rename is not None
        return False

    def send_workspace_did_rename_files(self, params: RenameFilesParams) -> None:
        notification = WorkspaceDidRenameFilesNotification(params)
        self.send_message(notification)

    def workspace_did_rename_files(self, files: List[FileRenameMapping]) -> None:
        for old_uri, new_uri in files:
            document = self.documents_by_uri[old_uri]
            del self.documents_by_uri[old_uri]
            self.documents_by_uri[new_uri] = document
        if self.server_supports_workspace_did_rename_files():
            params = RenameFilesParams(
                files=[FileRename(old_uri, new_uri) for old_uri, new_uri in files]
            )
            self.send_workspace_did_rename_files(params)

    @requires_server_capabilities
    def server_supports_workspace_will_delete_files(self) -> bool:
        workspace = self.server_capabilities.workspace
        if workspace is not None:
            file_operations = workspace.file_operations
            if file_operations is not None:
                # TODO: Implement a parser for additional pattern matching
                return file_operations.will_delete is not None
        return False

    def send_workspace_will_delete_files(self, params: DeleteFilesParams) -> int:
        request_id = self.next_request_id()
        request = WorkspaceWillDeleteFilesRequest(request_id, params)
        return self.send_request(
            request_id,
            request,
            self.receive_workspace_will_delete_files
        )

    def receive_workspace_will_delete_files(
            self,
            request: Any,
            message: JsonObject
    ) -> None:
        # TODO: Update the workspace as requested by the response
        pass

    def workspace_will_delete_files(self, files: List[Uri]) -> None:
        if self.server_supports_workspace_will_delete_files():
            params = DeleteFilesParams(
                files=[FileDelete(uri) for uri in files]
            )
            request_id = self.send_workspace_will_delete_files(params)
            self.await_response(request_id)

    @requires_server_capabilities
    def server_supports_workspace_did_delete_files(self) -> bool:
        workspace = self.server_capabilities.workspace
        if workspace is not None:
            file_operations = workspace.file_operations
            if file_operations is not None:
                # TODO: Implement a parser for additional pattern matching
                return file_operations.did_delete is not None
        return False

    def send_workspace_did_delete_files(self, params: DeleteFilesParams) -> None:
        notification = WorkspaceDidDeleteFilesNotification(params)
        self.send_message(notification)

    def workspace_did_delete_files(self, files: List[Uri]) -> None:
        for uri in files:
            if uri in self.documents_by_uri:
                del self.documents_by_uri[uri]
        if self.server_supports_workspace_did_delete_files():
            params = DeleteFilesParams(
                files=[FileDelete(uri) for uri in files]
            )
            self.send_workspace_did_delete_files(params)

    @requires_server_capabilities
    def server_supports_document_highlight(self) -> bool:
        return self.server_capabilities.document_highlight_provider is not None

    def send_text_document_document_highlight(
            self,
            params: DocumentHighlightParams
    ) -> int:
        request_id = self.next_request_id()
        request = TextDocumentDocumentHighlightRequest(request_id, params)
        return self.send_request(
            request_id,
            request,
            self.receive_text_document_document_highlight
        )

    def receive_text_document_document_highlight(
            self,
            request: Any,
            message: JsonObject
    ) -> None:
        # NOTE: Implement this in the concrete subclass because it may require
        # language-specific information (such as the language identifier for the
        # associated text document).
        raise NotImplementedError

    def highlight_symbol(self, uri: str, line: int, column: int) -> None:
        if self.server_supports_document_highlight():
            params = DocumentHighlightParams(
                text_document=TextDocumentIdentifier(
                    uri=uri,
                ),
                position=Position(
                    line=line,
                    character=column,
                ),
            )
            request_id = self.send_text_document_document_highlight(params)
            self.await_response(request_id)

    @requires_server_capabilities
    def server_supports_document_hover(self) -> bool:
        return self.server_capabilities.hover_provider is not None

    def send_text_document_hover(
            self,
            params: HoverParams
    ) -> int:
        request_id = self.next_request_id()
        request = TextDocumentHoverRequest(request_id, params)
        return self.send_request(
            request_id,
            request,
            self.receive_text_document_hover
        )

    def receive_text_document_hover(
            self,
            request: Any,
            message: JsonObject
    ) -> None:
        # NOTE: Implement this in the concrete subclass because it may require
        # language-specific information (such as the language identifier for the
        # associated text document).
        raise NotImplementedError

    def hover(self, uri: str, line: int, column: int) -> None:
        if self.server_supports_document_hover():
            params = HoverParams(
                text_document=TextDocumentIdentifier(
                    uri=uri,
                ),
                position=Position(
                    line=line,
                    character=column,
                ),
            )
            request_id = self.send_text_document_hover(params)
            self.await_response(request_id)

    @requires_server_capabilities
    def server_supports_document_symbols(self) -> bool:
        return self.server_capabilities.document_symbol_provider is not None

    def send_text_document_document_symbol(
            self,
            params: DocumentSymbolParams
    ) -> int:
        request_id = self.next_request_id()
        request = TextDocumentDocumentSymbolRequest(request_id, params)
        return self.send_request(
            request_id,
            request,
            self.receive_text_document_document_symbol
        )

    def receive_text_document_document_symbol(
            self,
            request: Any,
            message: JsonObject
    ) -> None:
        # NOTE: Implement this in the concrete subclass because it may require
        # language-specific information (such as the language identifier for the
        # associated text document).
        raise NotImplementedError

    def update_document_symbols(self, document_id: int, uri: Optional[str]) -> None:
        if (uri is not None) and self.server_supports_document_symbols():
            params = DocumentSymbolParams(
                text_document=TextDocumentIdentifier(
                    uri=uri
                )
            )
            request_id = self.send_text_document_document_symbol(params)
            self.await_response(request_id)

    @requires_server_capabilities
    def server_supports_semantic_highlight(self) -> bool:
        return self.server_capabilities.semantic_tokens_provider is not None

    def send_text_document_semantic_tokens_full(
            self,
            params: SemanticTokensParams
    ) -> int:
        request_id = self.next_request_id()
        request = TextDocumentSemanticTokensFullRequest(request_id, params)
        return self.send_request(
            request_id,
            request,
            self.receive_text_document_semantic_tokens_full
        )

    def receive_text_document_semantic_tokens_full(
            self,
            request: Any,
            message: JsonObject
    ) -> None:
        # NOTE: Implement this in the concrete subclass because it may require
        # language-specific information (such as the language identifier for the
        # associated text document).
        raise NotImplementedError

    def semantic_highlight(self, uri: str) -> None:
        if self.server_supports_document_highlight():
            params = SemanticTokensParams(
                text_document=TextDocumentIdentifier(
                    uri=uri,
                ),
            )
            request_id = self.send_text_document_semantic_tokens_full(params)
            self.await_response(request_id)

    @requires_server_capabilities
    def server_supports_code_completion(self) -> bool:
        return self.server_capabilities.completion_provider is not None

    def send_text_document_completion(
            self,
            params: CompletionParams
    ) -> int:
        request_id = self.next_request_id()
        request = TextDocumentCompletionRequest(request_id, params)
        return self.send_request(
            request_id,
            request,
            self.receive_text_document_completion
        )

    def receive_text_document_completion(
            self,
            request: Any,
            message: JsonObject
    ) -> None:
        # NOTE: Implement this in the concrete subclass because it may require
        # language-specific information (such as the language identifier for the
        # associated text document).
        raise NotImplementedError

    def complete(self, uri: str, line: int, column: int) -> None:
        if self.server_supports_document_highlight():
            params = CompletionParams(
                text_document=TextDocumentIdentifier(
                    uri=uri,
                ),
                position=Position(
                    line=line,
                    character=column,
                ),
            )
            request_id = self.send_text_document_completion(params)
            self.await_response(request_id)

    @requires_server_capabilities
    def server_supports_formatting(self) -> bool:
        return self.server_capabilities.document_formatting_provider is not None

    def send_text_document_formatting(
            self,
            params: DocumentFormattingParams
    ) -> int:
        request_id = self.next_request_id()
        request = TextDocumentFormattingRequest(request_id, params)
        return self.send_request(
            request_id,
            request,
            self.receive_text_document_formatting
        )

    def receive_text_document_formatting(
            self,
            request: Any,
            message: JsonObject
    ) -> None:
        # NOTE: Implement this in the concrete subclass because it may require
        # language-specific information (such as the language identifier for the
        # associated text document).
        raise NotImplementedError

    def format(self, uri: str) -> None:
        if self.server_supports_formatting():
            params = DocumentFormattingParams(
                text_document=TextDocumentIdentifier(
                    uri=uri,
                ),
                options=FormattingOptions(
                    tab_size=4,
                    insert_spaces=True,
                ),
            )
            request_id = self.send_text_document_formatting(params)
            self.await_response(request_id)

    @requires_server_capabilities
    def server_supports_range_formatting(self) -> bool:
        return self.server_capabilities.document_range_formatting_provider is not None

    def send_text_document_range_formatting(
            self,
            params: DocumentRangeFormattingParams
    ) -> int:
        request_id = self.next_request_id()
        request = TextDocumentRangeFormattingRequest(request_id, params)
        return self.send_request(
            request_id,
            request,
            self.receive_text_document_range_formatting
        )

    def receive_text_document_range_formatting(
            self,
            request: Any,
            message: JsonObject
    ) -> None:
        # NOTE: Implement this in the concrete subclass because it may require
        # language-specific information (such as the language identifier for the
        # associated text document).
        raise NotImplementedError

    def format_range(self, uri: str, selection: Range) -> None:
        if self.server_supports_range_formatting():
            params = DocumentRangeFormattingParams(
                text_document=TextDocumentIdentifier(
                    uri=uri,
                ),
                range=selection,
                options=FormattingOptions(
                    tab_size=4,
                    insert_spaces=True,
                ),
            )
            request_id = self.send_text_document_range_formatting(params)
            self.await_response(request_id)
