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
        params_spec = request_spec.get("params", None)
        params = []
        if params_spec is not None:
            params.append(f'{params_spec["name"]} &params')
        self.gen_fn_decl(
            send_fn(request_method),
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

    def generate_dispatch_methods(self) -> None:
        self.gen_fn_decl('dispatch', params=[
            'ResponseMessage &response',
            'RequestMessage &request'
        ])
        self.newline()
        self.gen_fn_decl('dispatch', params=[
            'ResponseMessage &response',
            'NotificationMessage &notification'
        ])
        self.newline()
        self.gen_fn_decl('dispatch', params=[
            'ResponseMessage &response',
            'std::string &traceId',
            'const LSPAny &document'
        ])
        self.newline()

    def generate_prepare(self) -> None:
        self.gen_fn_decl(
            'prepare',
            params=[
                'std::string &buffer',
                'const std::string &response'
            ],
            specs='const',
            override=True
        )
        self.newline()

    def generate_require_message_params(self) -> None:
        self.gen_fn_decl(
            'requireMessageParams',
            ret_type='MessageParams &',
            params=[
                'RequestMessage &request',
            ],
            specs='const'
        )
        self.newline()
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
            'std::size_t numRequestThreads',
            'std::size_t numWorkerThreads',
            'lsl::Logger &logger',
            'const std::string &configSection'
        ])
        self.newline()

    def generate_synchronized_send(self) -> None:
        self.gen_fn_decl('send', params=[
            'const std::string &request',
            'std::size_t sendId'
        ])
        self.newline()

    def generate_handle(self) -> None:
        self.gen_fn_decl('handle', params=[
            'const std::string &incoming',
            'std::size_t sendId'
        ])
        self.newline()

    def generate_log_receive_trace(self) -> None:
        self.gen_fn_decl('logReceiveTrace', params=[
            'const std::string &messageType',
            'const std::string &traceId',
            'const std::optional<MessageParams> &optionalParams'
        ])
        self.newline()

    def generate_log_receive_response_trace(self) -> None:
        self.gen_fn_decl('logReceiveResponseTrace', params=[
            'const std::string &traceId',
            'const LSPAny &document'
        ])
        self.newline()

    def generate_log_send_response_trace(self) -> None:
        self.gen_fn_decl('logSendResponseTrace', params=[
            'const std::string &traceId',
            'const std::chrono::time_point<std::chrono::high_resolution_clock> &start',
            'const LSPAny &response'
        ])
        self.newline()

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        self.generate_disclaimer()
        self.pragma_once()
        self.newline()
        self.gen_include('atomic')
        self.gen_include('chrono')
        self.gen_include('condition_variable')
        self.gen_include('mutex')
        self.gen_include('thread')
        self.gen_include('unordered_map')
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
            self.write('namespace lst = LCompilers::LLanguageServer::Threading;')
            self.newline()
            with self.gen_class('LspLanguageServer', sups=['ls::LanguageServer']):
                with self.gen_public():
                    self.generate_constructor()
                    self.write('auto isTerminated() const -> bool override;')
                with self.gen_protected():
                    self.write('const std::string configSection;')
                    self.write('std::thread listener;')
                    self.write('lst::ThreadPool requestPool;')
                    self.write('lst::ThreadPool workerPool;')
                    self.write('std::atomic_size_t serialSendId = 0;')
                    self.write('std::atomic_size_t pendingSendId = 0;')
                    self.write('std::condition_variable sent;')
                    self.write('std::mutex sentMutex;')
                    self.write('LspJsonSerializer serializer;')
                    self.write('LspTransformer transformer;')
                    self.write('std::unique_ptr<InitializeParams> _initializeParams;')
                    self.write('std::atomic_bool _initialized = false;')
                    self.write('std::atomic_bool _shutdown = false;')
                    self.write('std::atomic_bool _exit = false;')
                    self.write('std::atomic_int serialRequestId = 0;')
                    self.write('std::unordered_map<int, std::string> callbacksById;')
                    self.write('std::mutex callbackMutex;')
                    self.write('std::atomic<TraceValues> trace{TraceValues::Off};')
                    self.newline()
                    self.write('auto nextSendId() -> std::size_t;')
                    self.write('auto nextRequestId() -> int;')
                    self.write('auto isInitialized() const -> bool;')
                    self.write('auto isShutdown() const -> bool;')
                    self.write('auto isRunning() const -> bool;')
                    self.write('auto join() -> void override;')
                    self.write('auto listen() -> void;')
                    self.write('auto notifySent() -> void;')
                    self.write('auto to_string(const RequestId &requestId) -> std::string;')
                    self.newline()
                    self.generate_synchronized_send()
                    self.generate_handle()
                    self.generate_log_receive_trace()
                    self.generate_log_receive_response_trace()
                    self.generate_log_send_response_trace()
                    self.write('auto initializeParams() const -> const InitializeParams &;')
                    self.write('auto assertInitialized() -> void;')
                    self.write('auto assertRunning() -> void;')
                    self.newline()
                    self.generate_dispatch_methods()
                    self.generate_prepare()
                    self.generate_require_message_params()
                    super().generate_code()
