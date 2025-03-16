import io
import json
import subprocess
from collections import defaultdict
from contextlib import contextmanager
from pathlib import Path
from typing import IO, Any, Dict, Iterator, List, Optional

from cattrs import Converter

from lsprotocol import converters
from lsprotocol.types import (
    ClientCapabilities, ClientRegisterCapabilityRequest,
    ClientRegisterCapabilityResponse, CompletionClientCapabilities,
    CompletionClientCapabilitiesCompletionItemType,
    CompletionClientCapabilitiesCompletionItemTypeInsertTextModeSupportType,
    CompletionClientCapabilitiesCompletionItemTypeResolveSupportType,
    CompletionClientCapabilitiesCompletionItemTypeTagSupportType,
    CompletionItemTag, DidChangeConfigurationClientCapabilities,
    DidChangeConfigurationParams, ExitNotification, HoverClientCapabilities,
    InitializedNotification, InitializedParams, InitializeParams,
    InitializeRequest, InitializeResponse, InsertTextMode, MarkupKind,
    Registration, ShutdownRequest, ShutdownResponse,
    TextDocumentClientCapabilities, TextDocumentSyncClientCapabilities,
    WorkspaceClientCapabilities, WorkspaceConfigurationRequest,
    WorkspaceConfigurationResponse,
    WorkspaceDidChangeConfigurationNotification,
    InitializeResultServerInfoType,
    ServerCapabilities)

from llanguage.json_rpc import JsonObject
from llanguage.lsp_json_stream import LspJsonStream


class LspTestClient:
    server_path: Path
    server_params: List[str]
    workspace_path: Optional[Path]
    timeout: float

    server: subprocess.Popen[str]
    message_stream: LspJsonStream
    ostream: IO[str]
    buf: io.StringIO
    converter: Converter
    serial_request_id: int
    config: Dict[str, Any]
    server_capabilities: ServerCapabilities
    server_info: Optional[InitializeResultServerInfoType]
    dynamic_registrations: Dict[str, List[Registration]]

    def __init__(
            self,
            server_path: Path,
            server_params: List[str],
            workspace_path: Optional[Path],
            timeout: float,
            config: Dict[str, Any]
    ) -> None:
        self.server_path = server_path
        self.server_params = server_params
        self.workspace_path = workspace_path
        self.timeout = timeout
        self.buf = io.StringIO()
        self.converter = converters.get_converter()
        self.serial_request_id = 0
        self.config = config
        self.dynamic_registrations = defaultdict(list)

    def next_request_id(self) -> int:
        curr_id = self.serial_request_id
        self.serial_request_id += 1
        return curr_id

    @contextmanager
    def serve(self) -> Iterator["LFortranLspTestClient":]
        self.initialize()
        yield self
        self.terminate()

    def check_request(self, request_id: int, response: Any) -> None:
        if request_id != response.id:
            raise ValueError(
                f"Expected response.id to be {request_id} but was {response.id}"
            )

    def check_notification(self, response: JsonObject) -> None:
        pass

    def initialize_params(self) -> InitializeParams:
        params = InitializeParams(
            capabilities=ClientCapabilities(
                workspace=WorkspaceClientCapabilities(
                    did_change_configuration=DidChangeConfigurationClientCapabilities(
                        dynamic_registration=True
                    ),
                    configuration=True
                ),
                text_document=TextDocumentClientCapabilities(
                    synchronization=TextDocumentSyncClientCapabilities(
                        dynamic_registration=True,
                        will_save=True,
                        will_save_wait_until=False,
                        did_save=True,
                    ),
                    completion=CompletionClientCapabilities(
                        dynamic_registration=True,
                        completion_item=CompletionClientCapabilitiesCompletionItemType(
                            snippet_support=False,
                            commit_characters_support=False,
                            documentation_format=[
                                MarkupKind.PlainText,
                                MarkupKind.Markdown
                            ],
                            deprecated_support=True,
                            preselect_support=False,
                            tag_support=CompletionClientCapabilitiesCompletionItemTypeTagSupportType(
                                value_set=[
                                    CompletionItemTag.Deprecated
                                ]
                            ),
                            insert_replace_support=False,
                            resolve_support=CompletionClientCapabilitiesCompletionItemTypeResolveSupportType(
                                properties=[]
                            ),
                            insert_text_mode_support=CompletionClientCapabilitiesCompletionItemTypeInsertTextModeSupportType(
                                value_set=[
                                    InsertTextMode.AsIs,
                                    InsertTextMode.AdjustIndentation
                                ]
                            ),
                            label_details_support=True
                        ),
                        context_support=True
                    ),
                    hover=HoverClientCapabilities(
                        dynamic_registration=True,
                        content_format=[
                            MarkupKind.PlainText,
                            MarkupKind.Markdown
                        ]
                    )
                )
            )
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
            text=True
        )
        if self.server.stdout is None:
            raise RuntimeError("Cannot read from server stdout")
        if self.server.stdin is None:
            raise RuntimeError("Cannot write to server stdin")
        if self.server.stderr is None:
            raise RuntimeError("Cannot read from server stderr")
        self.message_stream = LspJsonStream(self.server.stdout, self.timeout)
        self.ostream = self.server.stdin

        initialize_id = self.send_initialize(self.initialize_params())
        self.check_request(initialize_id, self.receive_message())

        self.send_initialized(self.initialized_params())
        self.check_notification(self.receive_message())

    def terminate(self) -> None:
        shutdown_id = self.send_shutdown()
        self.check_request(shutdown_id, self.receive_message())

        self.send_exit()
        self.check_notification(self.receive_message())

        try:
            self.server.wait(timeout=5)
        except subprocess.TimeoutExpired as e:
            raise RuntimeError(
                f"Timed-out after 5 seconds while awaiting the server to terminate."
            ) from e

    def check_server(self) -> None:
        if self.server.poll():
            raise RuntimeError(
                f"Server crashed with status: {self.server.returncode}"
            )

    def send_message(self, message: Any) -> None:
        body = json.dumps(message, default=self.converter.unstructure)
        self.buf.seek(0)
        self.buf.truncate(0)
        self.buf.write(f"Content-Length: {len(body)}\r\n")
        self.buf.write("\r\n")
        self.buf.write(body)
        self.check_server()
        self.ostream.write(self.buf.getvalue())

    def receive_message(self) -> Any:
        self.check_server()
        message = self.message_stream.next()
        match message:
            case dict():
                return self.dispatch_message(message)
            case _:
                raise RuntimeError(
                    f"Unsupported message type ({type(message)}): {message}"
                )

    def dispatch_message(self, message: JsonObject) -> Any:
        if 'method' in message:
            return self.dispatch_method(message)
        elif 'result' in message:
            return self.dispatch_response(message)
        elif 'error' in message:
            return self.dispatch_error(message)
        else:
            pass

    def dispatch_method(self, message: JsonObject) -> Any:
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

    def dispatch_response(self, message: JsonObject) -> Any:
        pass

    def dispatch_error(self, message: JsonObject) -> Any:
        pass

    def serialize_json(self, message: Any) -> str:
        return json.dumps(message, default=self.converter.unstructure)

    def send_initialize(self, params: InitializeParams) -> int:
        request_id = self.next_request_id()
        request = InitializeRequest(request_id, params)
        self.send_message(request)
        return request_id

    def receive_initialize(self, response: InitializeResponse) -> None:
        result = response.result
        self.server_capabilities = result.capabilities
        self.server_info = result.server_info

    def send_initialized(self, params: InitializedParams) -> None:
        notification = InitializedNotification(params)
        self.send_message(notification)

    def send_shutdown(self) -> int:
        request_id = self.next_request_id()
        request = ShutdownRequest(request_id)
        self.send_message(request)
        return request_id

    def receive_shutdown(self, response: ShutdownResponse) -> None:
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
                        break;
            configs.append(config)
        response = WorkspaceConfigurationResponse(request.id, configs)
        return response

