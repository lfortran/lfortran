from pathlib import Path

from llanguage_server.cxx.visitors import BaseCPlusPlusLspVisitor
from llanguage_server.lsp.datatypes import LspSpec
from llanguage_server.lsp.utils import (method_to_camel_case, receive_fn,
                                        send_fn)
from llanguage_server.lsp.visitors import LspAnalysisPipeline
from llanguage_server.lsp.datatypes import LspSpec, LspSymbol


class CPlusPlusLspLanguageServerHeaderGenerator(BaseCPlusPlusLspVisitor):

    def __init__(
            self,
            output_dir: Path,
            schema: LspSpec,
            pipeline: LspAnalysisPipeline,
            namespace: str
    ) -> None:
        specification_source = output_dir / "lsp_language_server.h"
        super().__init__(specification_source, schema, pipeline, namespace)

    def generate_receive_request(self, request_spec: LspSpec) -> None:
        request_method = request_spec["method"]
        request_name = method_to_camel_case(request_method)
        result_name = f'{request_name}Result'
        params_spec = request_spec.get("params", None)
        params = []
        if params_spec is not None:
            params.append(f'{params_spec["name"]} &params',)
        self.gen_fn_decl(
            receive_fn(request_method),
            result_name,
            params=params,
            docs=request_spec.get("documentation", None),
            virtual=True
        )
        self.newline()

    def generate_send_request(self, request_spec: LspSpec) -> None:
        request_method = request_spec["method"]
        send_request = send_fn(request_method)
        params_spec = request_spec.get("params", None)
        params = []
        if params_spec is not None:
            params.append(f'{params_spec["name"]} &params')
        self.gen_fn_decl(
            send_request,
            'int',
            params=params,
            docs=request_spec.get("documentation", None),
            virtual=True
        )
        self.newline()

    def generate_receive_response(self, request_spec: LspSpec) -> None:
        request_method = request_spec["method"]
        request_name = method_to_camel_case(request_method)
        result_spec = request_spec.get("result", None)
        params = []
        if result_spec is not None:
            result_name = f'{request_name}Result'
            if result_spec["kind"] == "base":
                params.append(f'{result_name} params')
            else:
                params.append(f'{result_name} &params')
        self.gen_fn_decl(
            receive_fn(request_method),
            params=params,
            virtual=True
        )
        self.newline()

    def generate_outgoing_request_handlers(self, request_spec: LspSpec) -> None:
        self.generate_send_request(request_spec)
        self.generate_receive_response(request_spec)

    def visit_request_symbol(self, request_symbol: LspSymbol) -> None:
        request_spec = request_symbol.spec
        match request_spec["messageDirection"]:
            case "clientToServer":
                self.generate_receive_request(request_spec)
            case "serverToClient":
                self.generate_outgoing_request_handlers(request_spec)
            case "both":
                self.generate_receive_request(request_spec)
                self.generate_outgoing_request_handlers(request_spec)
            case _:
                raise ValueError(
                    f'Unsupported messageDirection: {request_spec["messageDirection"]}'
                )

    def generate_receive_notification(self, notification_spec: LspSpec) -> None:
        notification_method = notification_spec["method"]
        params_spec = notification_spec.get("params", None)
        params = []
        if params_spec is not None:
            params.append(f'{params_spec["name"]} &params')
        self.gen_fn_decl(
            receive_fn(notification_method),
            params=params,
            docs=notification_spec.get("documentation", None),
            virtual=True
        )
        self.newline()

    def generate_send_notification(self, notification_spec: LspSpec) -> None:
        notification_method = notification_spec["method"]
        params_spec = notification_spec.get("params", None)
        params=[]
        if params_spec is not None:
            params.append(f'{params_spec["name"]} &params')
        self.gen_fn_decl(
            send_fn(notification_method),
            params=params,
            docs=notification_spec.get("documentation", None),
            virtual=True
        )
        self.newline()

    def visit_notification_symbol(self, notification_symbol: LspSymbol) -> None:
        notification_spec = notification_symbol.spec
        match notification_spec["messageDirection"]:
            case "clientToServer":
                self.generate_receive_notification(notification_spec)
            case "serverToClient":
                self.generate_send_notification(notification_spec)
            case "both":
                self.generate_receive_notification(notification_spec)
                self.generate_send_notification(notification_spec)
            case _:
                raise ValueError(
                    f'Unsupported messageDirection: {notification_spec["messageDirection"]}'
                )

    def generate_dispatch_request(self) -> None:
        self.gen_fn_decl('dispatch', params=[
            'ResponseMessage &response',
            'RequestMessage &request',
            'IncomingRequest method',
        ])
        self.newline()

    def generate_dispatch_notification(self) -> None:
        self.gen_fn_decl('dispatch', params=[
            'ResponseMessage &response',
            'NotificationMessage &notification',
            'IncomingNotification method',
        ])
        self.newline()

    def generate_dispatch_response(self) -> None:
        self.gen_fn_decl('dispatch', params=[
            'ResponseMessage &response',
            'const std::string &method',
        ])
        self.newline()

    def generate_require_request_params(self) -> None:
        self.gen_fn_decl(
            'requireMessageParams',
            ret_type='MessageParams &',
            params=[
                'RequestMessage &request',
            ],
            specs='const'
        )
        self.newline()

    def generate_require_notification_params(self) -> None:
        self.gen_fn_decl(
            'requireMessageParams',
            ret_type='MessageParams &',
            params=[
                'NotificationMessage &notification',
            ],
            specs='const'
        )
        self.newline()

    def generate_constructor(self) -> None:
        self.gen_constructor_decl('LspLanguageServer', params=[
            'ls::MessageQueue &incomingMessages',
            'ls::MessageQueue &outgoingMessages',
            'lsl::Logger &logger',
        ])
        self.newline()

    def generate_fields(self) -> None:
        self.write('LspTransformer transformer;')
        self.write('LspJsonSerializer serializer;')
        self.write('std::atomic_int serialRequestId = 0;')
        self.newline()

    def generate_next_request_id(self) -> None:
        self.gen_fn_decl(
            'nextRequestId',
            ret_type='int',
        )
        self.newline()

    def generate_send_request_message(self) -> None:
        self.gen_fn_decl(
            'send',
            params=[
                'const RequestMessage &request',
            ],
            virtual=True,
        )
        self.newline()

    def generate_build_request(self) -> None:
        self.gen_fn_decl(
            'buildRequest',
            'RequestMessage',
            params=[
                'const std::string &method',
            ],
        )
        self.newline()

    def generate_send_custom_request(self) -> None:
        self.gen_fn_decl(
            'sendRequest',
            'int',
            params=[
                'const std::string &method',
                'MessageParams &params',
            ],
        )
        self.newline()

    def generate_send_custom_request_no_params(self) -> None:
        self.gen_fn_decl(
            'sendRequest',
            'int',
            params=[
                'const std::string &method',
            ],
        )
        self.newline()

    def generate_send_notification_message(self) -> None:
        self.gen_fn_decl(
            'send',
            params=[
                'const NotificationMessage &notification',
            ],
        )
        self.newline()

    def generate_build_notification(self) -> None:
        self.gen_fn_decl(
            'buildNotification',
            'NotificationMessage',
            params=[
                'const std::string &method',
            ],
        )
        self.newline()

    def generate_send_custom_notification(self) -> None:
        self.gen_fn_decl(
            'sendNotification',
            params=[
                'const std::string &method',
                'MessageParams &params',
            ],
        )
        self.newline()

    def generate_send_custom_notification_no_params(self) -> None:
        self.gen_fn_decl(
            'sendNotification',
            params=[
                'const std::string &method',
            ],
        )
        self.newline()

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        self.generate_disclaimer()
        self.pragma_once()
        self.newline()
        self.gen_include('atomic')
        self.newline()
        self.gen_include('server/language_server.h')
        self.gen_include('server/logger.h')
        self.gen_include('server/lsp_json_serializer.h')
        self.gen_include('server/lsp_specification.h')
        self.gen_include('server/lsp_transformer.h')
        self.newline()
        with self.gen_namespace(self.namespace):
            self.write('namespace ls = LCompilers::LLanguageServer;')
            self.write('namespace lsl = LCompilers::LLanguageServer::Logging;')
            self.newline()
            with self.gen_class('LspLanguageServer', sups=['ls::LanguageServer']):
                with self.gen_protected():
                    self.generate_constructor()
                    self.generate_fields()
                    self.generate_next_request_id()
                    self.generate_send_request_message()
                    self.generate_build_request()
                    self.generate_send_custom_request()
                    self.generate_send_custom_request_no_params()
                    self.generate_send_notification_message()
                    self.generate_build_notification()
                    self.generate_send_custom_notification()
                    self.generate_send_custom_notification_no_params()
                    self.generate_dispatch_request()
                    self.generate_dispatch_notification()
                    self.generate_dispatch_response()
                    self.generate_require_request_params()
                    self.generate_require_notification_params()
                    super().generate_code()
