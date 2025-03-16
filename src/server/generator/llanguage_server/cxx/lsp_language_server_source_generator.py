from pathlib import Path

from llanguage_server.cxx.lsp_file_generator import gensym_context
from llanguage_server.cxx.visitors import BaseCPlusPlusLspVisitor
from llanguage_server.lsp.datatypes import LspSpec
from llanguage_server.lsp.utils import (lower_first, method_to_camel_case,
                                        receive_fn, rename_enum, rename_field,
                                        send_fn, upper_first)
from llanguage_server.lsp.visitors import (LspAnalysisPipeline, LspSymbol)


class CPlusPlusLspLanguageServerSourceGenerator(BaseCPlusPlusLspVisitor):

    def __init__(
            self,
            output_dir: Path,
            schema: LspSpec,
            pipeline: LspAnalysisPipeline,
            namespace: str
    ) -> None:
        specification_source = output_dir / "lsp_language_server.cpp"
        super().__init__(specification_source, schema, pipeline, namespace)

    def generate_constructor(self) -> None:
        with self.gen_constructor(
                'LspLanguageServer',
                params=[
                    'ls::MessageQueue &incomingMessages',
                    'ls::MessageQueue &outgoingMessages',
                    'lsl::Logger &logger',
                ],
                sups=[
                    ('ls::LanguageServer', [
                        'incomingMessages',
                        'outgoingMessages',
                        'logger',
                    ]),
                ],
        ):
            self.write('// empty')
        self.newline()

    def generate_dispatch_request(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::dispatch',
                params=[
                    'ResponseMessage &response',
                    'RequestMessage &request',
                    'IncomingRequest method'
                ]
        ):
            with self.gen_switch('method'):
                for request_spec in self.schema["requests"]:
                    if request_spec["messageDirection"] in ["clientToServer", "both"]:
                        request_method = request_spec["method"]
                        request_name = method_to_camel_case(request_method)
                        result_name = f'{request_name}Result'
                        method_enum = method_to_camel_case(request_method)
                        with self.gen_case('IncomingRequest', method_enum):
                            params_spec = request_spec.get("params", None)
                            if params_spec is not None:
                                self.write('MessageParams &messageParams = requireMessageParams(request);')
                                self.write(f'{params_spec["name"]} requestParams =')
                                with self.indent():
                                    self.write(f'transformer.as{request_name}Params(messageParams);')
                                self.write(f'{result_name} result =')
                                with self.indent(): self.write(f'{receive_fn(request_method)}(requestParams);')
                                self.write(f'response.result =')
                                with self.indent():
                                    self.write(f'transformer.{lower_first(result_name)}ToAny(result);')
                            else:
                                self.write(f'{result_name} result = {receive_fn(request_method)}();')
                                self.write(f'response.result =')
                                with self.indent():
                                    self.write(f'transformer.{lower_first(result_name)}ToAny(result);')
                            self.gen_break()
                with self.gen_default():
                    self.gen_throw(
                        'LSP_EXCEPTION',
                        'ErrorCodes::MethodNotFound',
                        '"Unsupported request method: \\"" + request.method + "\\""'
                    )
        self.newline()

    def generate_dispatch_notification(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::dispatch',
                params=[
                    'ResponseMessage &/*response*/',
                    'NotificationMessage &notification',
                    'IncomingNotification method'
                ]
        ):
            with self.gen_switch('method'):
                for notification_spec in self.schema["notifications"]:
                    if notification_spec["messageDirection"] in ["clientToServer", "both"]:
                        notification_method = notification_spec["method"]
                        notification_name = method_to_camel_case(notification_method)
                        notification_enum = method_to_camel_case(notification_method)
                        with self.gen_case('IncomingNotification', notification_enum):
                            params_spec = notification_spec.get("params", None)
                            if params_spec is not None:
                                self.gen_assign(
                                    'MessageParams &messageParams',
                                    'requireMessageParams(notification)'
                                )
                                self.write(f'{params_spec["name"]} notificationParams =')
                                with self.indent():
                                    self.write(f'transformer.as{notification_name}Params(messageParams);')
                                self.write(f'{receive_fn(notification_method)}(notificationParams);')
                            else:
                                self.write(f'{receive_fn(notification_method)}();')
                            self.gen_break()
                with self.gen_default():
                    with self.gen_if('notification.method.compare(0, 2, "$/") == 0', end=''):
                        self.write('// NOTE: If a server or client receives notifications starting with "$/"')
                        self.write('// it is free to ignore the notification:')
                        self.write('logger.debug()')
                        with self.indent():
                            self.write(f'<< "No handler exists for method: \\"" << notification.method << "\\""')
                            self.write('<< std::endl;')
                    with self.gen_else():
                        self.gen_throw(
                            'LSP_EXCEPTION',
                            'ErrorCodes::MethodNotFound',
                            '"Unsupported notification method: \\"" + notification.method + "\\""'
                        )
        self.newline()

    def generate_dispatch_response(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::dispatch',
                params=[
                    'ResponseMessage &response',
                    'const std::string &method'
                ]
        ):
            self.write('RequestId requestId;')
            with self.gen_switch('response.id.type()'):
                with self.gen_case('ResponseIdType', 'integer'):
                    self.gen_assign('requestId', 'response.id.integer()')
                    self.gen_break()
                with self.gen_case('ResponseIdType', 'string'):
                    self.gen_assign('requestId', 'response.id.string()')
                    self.gen_break()
                with self.gen_default():
                    self.gen_throw_invalid_params(
                        '("Invalid ResponseId type: " +'
                        ' ResponseIdTypeNames.at(response.id.type()))'
                    )
            self.newline()
            self.write('OutgoingRequest request;')
            with self.gen_try():
                self.write('request = outgoingRequestByValue(method);')
            with self.gen_catch('std::invalid_argument &/*e*/'):
                self.write('goto invalidMethod;')
            self.newline()
            with self.gen_switch('request'):
                for request_spec in self.schema["requests"]:
                    if request_spec["messageDirection"] in ["serverToClient", "both"]:
                        request_method = request_spec["method"]
                        request_name = method_to_camel_case(request_method)
                        result_name = f'{request_name}Result'
                        request_enum = method_to_camel_case(request_method)
                        with self.gen_case('OutgoingRequest', request_enum):
                            result_spec = request_spec.get("result", None)
                            if result_spec is not None:
                                with self.gen_if('!response.result.has_value()'):
                                    self.write(f'logger.error()')
                                    with self.indent():
                                        self.write(f'<< ("Missing required attribute for method "')
                                        self.write(f'    "\\"{request_method}\\": result")')
                                        self.write('<< std::endl;')
                                    self.write('return;')
                                self.gen_assign(
                                    'LSPAny &result',
                                    f'response.result.value()'
                                )
                                self.gen_assign(
                                    f'{result_name} params',
                                    f'transformer.anyTo{upper_first(result_name)}(result)'
                                )
                                self.write(f'{receive_fn(request_method)}(requestId, params);')
                            else:
                                self.write(f'{receive_fn(request_method)}(requestId)')
                            self.gen_break()
                with self.gen_default():
                    self.write('invalidMethod:')
                    self.write(f'logger.error()')
                    with self.indent():
                        self.write('<< "Unsupported request method: \\""')
                        self.write('<< method << "\\"";')
        self.newline()

    def generate_require_request_params(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::requireMessageParams',
                'MessageParams &',
                params=[
                    'RequestMessage &request'
                ],
                specs='const'
        ):
            with self.gen_if('request.params.has_value()'):
                self.write('return request.params.value();')
            self.gen_throw_invalid_params(
                '("RequestMessage.params must be defined for "',
                ' "method=\\"" + request.method + "\\"")'
            )
        self.newline()

    def generate_require_notification_params(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::requireMessageParams',
                'MessageParams &',
                params=[
                    'NotificationMessage &notification',
                ],
                specs='const'
        ):
            with self.gen_if('notification.params.has_value()'):
                self.gen_return('notification.params.value()')
            self.gen_throw_invalid_params(
                '"NotificationMessage.params must be defined for method=\\"" + notification.method + "\\""'
            )
        self.newline()

    def generate_receive_request(self, request_spec: LspSpec) -> None:
        request_method = request_spec["method"]
        request_name = method_to_camel_case(request_method)
        result_name = f'{request_name}Result'
        params = []
        params_spec = request_spec.get("params", None)
        if params_spec is not None:
            params.append(f'{params_spec["name"]} &/*params*/')
        receive_request = receive_fn(request_method)
        self.write(f'// request: "{request_method}"')
        with self.gen_fn(
                f'LspLanguageServer::{receive_request}',
                result_name,
                params=params
        ):
            self.gen_throw(
                'LSP_EXCEPTION',
                'ErrorCodes::MethodNotFound',
                f'"No handler exists for method: \\"{request_method}\\""'
            )
        self.newline()

    def generate_receive_notification(self, notification_spec: LspSpec) -> None:
        notification_method = notification_spec["method"]
        receive_notification = receive_fn(notification_method)
        params = []
        params_spec = notification_spec.get("params", None)
        if params_spec is not None:
            params.append(f'{params_spec["name"]} &/*params*/')
        self.write(f'// notification: "{notification_method}"')
        with self.gen_fn(f'LspLanguageServer::{receive_notification}', params=params):
            if notification_method.startswith("$/"):
                self.write('// NOTE: If a server or client receives notifications starting with "$/" it')
                self.write('// is free to ignore the notification:')
                self.write('logger.debug()')
                with self.indent():
                    self.write(f'<< "No handler exists for method: \\"{notification_method}\\""')
                    self.write('<< std::endl;')
            else:
                self.gen_throw(
                    'LSP_EXCEPTION',
                    'ErrorCodes::MethodNotFound',
                    f'"No handler exists for method: \\"{notification_method}\\""'
                )
        self.newline()

    @gensym_context
    def generate_send_request(self, request_spec: LspSpec) -> None:
        request_method = request_spec["method"]
        send_request = send_fn(request_method)
        params = []
        params_spec = request_spec.get("params", None)
        if params_spec is not None:
            params.append(f'{params_spec["name"]} &params')
        self.write(f'// request: "{request_method}"')
        with self.gen_fn(
                f'LspLanguageServer::{send_request}',
                'int',
                params=params
        ):
            method_name = self.gensym_init('method', f'"{request_method}"')
            if params_spec is not None:
                params_name = self.gensym_init(
                    'params',
                    'transformer.asMessageParams(params)'
                )
                self.gen_return(f'sendRequest({method_name}, {params_name})')
            else:
                self.gen_return(f'sendRequest({method_name})')
        self.newline()

    def generate_receive_response(self, request_spec: LspSpec) -> None:
        request_method = request_spec["method"]
        receive_request = receive_fn(request_method)
        request_name = method_to_camel_case(request_method)
        params = [
            'const RequestId &/*requestId*/',
        ]
        result_spec = request_spec.get("result", None)
        if result_spec is not None:
            result_name = f'{request_name}Result'
            if result_spec['kind'] == 'base':
                if result_spec['name'] == 'string':
                    params.append(f'{result_name} &/*params*/')
                else:
                    params.append(f'{result_name} /*params*/')
            else:
                params.append(f'{result_name} &/*params*/')
        self.write(f'// request: "{request_method}"')
        with self.gen_fn(f'LspLanguageServer::{receive_request}', params=params):
            self.write(f'logger.debug()')
            with self.indent():
                self.write(f'<< "No handler exists for method: \\"{request_method}\\""')
                self.write(f'<< std::endl;')
        self.newline()

    @gensym_context
    def generate_send_notification(self, notification_spec: LspSpec) -> None:
        notification_method = notification_spec["method"]
        send_notification = send_fn(notification_method)
        params = []
        params_spec = notification_spec.get("params", None)
        if params_spec is not None:
            params.append(f'{params_spec["name"]} &params')
        self.write(f'// notification: "{notification_method}"')
        with self.gen_fn(f'LspLanguageServer::{send_notification}', params=params):
            method_name = self.gensym_init('method', f'"{notification_method}"')
            if params_spec is not None:
                params_name = self.gensym_init(
                    'params',
                    'transformer.asMessageParams(params)'
                )
                self.gen_call('sendNotification', method_name, params_name)
            else:
                self.gen_call('sendNotification', method_name)
        self.newline()

    def generate_prepare(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::prepare',
                params=[
                    'std::string &buffer',
                    'const std::string &response'
                ],
                specs='const'
        ):
            self.write('buffer.append("Content-Type: application/vscode-jsonrpc; charset=utf-8\\r\\n");')
            self.write('buffer.append("Content-Length: ");')
            self.write('buffer.append(std::to_string(response.length()));')
            self.write('buffer.append("\\r\\n");')
            self.write('buffer.append("\\r\\n");')
            self.write('buffer.append(response);')
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

    @gensym_context
    def generate_send_request_message(self) -> None:
        with self.gen_fn(
            'LspLanguageServer::send',
            params=[
                'const RequestMessage &request',
            ],
        ):
            any_name = self.gensym_init(
                'any',
                'transformer.requestMessageToAny(request)'
            )
            message_name = self.gensym_init(
                'message',
                f'serializer.serialize({any_name})'
            )
            self.gen_call('ls::LanguageServer::send', message_name)
        self.newline()

    @gensym_context
    def generate_build_request(self) -> None:
        with self.gen_fn(
            'LspLanguageServer::buildRequest',
            'RequestMessage',
            params=[
                'const std::string &method',
            ],
        ):
            request_name = self.gensym_decl('RequestMessage', 'request')
            self.gen_assign(f'{request_name}.jsonrpc', 'JSON_RPC_VERSION')
            self.gen_assign(f'{request_name}.method', 'method')
            request_id_name = self.gensym_init('requestId', 'nextRequestId()')
            self.gen_assign(f'{request_name}.id', request_id_name)
            self.gen_return(request_name)
        self.newline()

    @gensym_context
    def generate_send_custom_request(self) -> None:
        with self.gen_fn(
            'LspLanguageServer::sendRequest',
            'int',
            params=[
                'const std::string &method',
                'MessageParams &params',
            ],
        ):
            request_name = self.gensym_init('request', 'buildRequest(method)')
            self.gen_assign(f'{request_name}.params', 'std::move(params)')
            self.gen_call('send', request_name)
            self.gen_return(f'{request_name}.id.integer()')
        self.newline()

    @gensym_context
    def generate_send_custom_request_no_params(self) -> None:
        with self.gen_fn(
            'LspLanguageServer::sendRequest',
            'int',
            params=[
                'const std::string &method',
            ],
        ):
            request_name = self.gensym_init('request', 'buildRequest(method)')
            self.gen_call('send', request_name)
            self.gen_return(f'{request_name}.id.integer()')
        self.newline()

    @gensym_context
    def generate_send_notification_message(self) -> None:
        with self.gen_fn(
            'LspLanguageServer::send',
            params=[
                'const NotificationMessage &notification',
            ],
        ):
            any_name = self.gensym_init(
                'any',
                'transformer.notificationMessageToAny(notification)'
            )
            message_name = self.gensym_init(
                'message',
                f'serializer.serialize({any_name})'
            )
            self.gen_call('ls::LanguageServer::send', message_name)
        self.newline()

    @gensym_context
    def generate_build_notification(self) -> None:
        with self.gen_fn(
            'LspLanguageServer::buildNotification',
            'NotificationMessage',
            params=[
                'const std::string &method',
            ],
        ):
            notification_name = self.gensym_decl(
                'NotificationMessage',
                'notification'
            )
            self.gen_assign(f'{notification_name}.jsonrpc', 'JSON_RPC_VERSION')
            self.gen_assign(f'{notification_name}.method', 'method')
            self.gen_return(notification_name)
        self.newline()

    @gensym_context
    def generate_send_custom_notification(self) -> None:
        with self.gen_fn(
            'LspLanguageServer::sendNotification',
            params=[
                'const std::string &method',
                'MessageParams &params',
            ],
        ):
            notification_name = self.gensym_init(
                'notification',
                'buildNotification(method)'
            )
            self.gen_assign(f'{notification_name}.params', 'std::move(params)')
            self.gen_call('send', notification_name)
        self.newline()

    @gensym_context
    def generate_send_custom_notification_no_params(self) -> None:
        with self.gen_fn(
            'LspLanguageServer::sendNotification',
            params=[
                'const std::string &method',
            ],
        ):
            notification_name = self.gensym_init(
                'notification',
                'buildNotification(method)'
            )
            self.gen_call('send', notification_name)
        self.newline()

    @gensym_context
    def generate_next_request_id(self) -> None:
        with self.gen_fn('LspLanguageServer::nextRequestId', 'int'):
            self.gen_return('serialRequestId++')
        self.newline()

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        self.generate_disclaimer()
        self.gen_include('cctype')
        self.gen_include('cstdio')
        self.gen_include('iostream')
        self.gen_include('stdexcept')
        self.newline()
        self.gen_include('server/lsp_exception.h')
        self.gen_include('server/lsp_json_parser.h')
        self.gen_include('server/lsp_language_server.h')
        self.gen_include('server/lsp_specification.h')
        self.newline()
        with self.gen_namespace(self.namespace):
            self.newline()
            self.generate_constructor()
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
