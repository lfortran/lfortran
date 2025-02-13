from pathlib import Path
from typing import Any, Dict, Tuple

from llanguage_server.cxx.file_generator import CPlusPlusLspFileGenerator
from llanguage_server.utils import method_to_camel_case, receive_fn, send_fn


class CPlusPlusLspLanguageServerHeaderGenerator(CPlusPlusLspFileGenerator):

    def __init__(
        self,
        output_dir: Path,
        schema: Dict[str, Any],
        namespace: str,
        symbols: Dict[str, Tuple[str, Dict[str, Any]]]
    ) -> None:
        specification_source = output_dir / "lsp_language_server.h"
        super().__init__(specification_source, schema, namespace, symbols)

    def generate_incoming_request_handlers(self) -> None:
        self.write('// ================= //')
        self.write('// Incoming Requests //')
        self.write('// ================= //')
        self.newline()
        for request_spec in self.schema["requests"]:
            if request_spec["messageDirection"] == "clientToServer":
                request_method = request_spec["method"]
                request_name = method_to_camel_case(request_method)
                result_name = f'{request_name}Result'
                self.generate_docstring(request_spec.get("documentation", None))
                params_spec = request_spec.get("params", None)
                if params_spec is not None:
                    self.write(f'virtual auto {receive_fn(request_method)}(')
                    with self.indent(): self.write(f'{params_spec["name"]} &params')
                    self.write(f') -> {result_name};')
                else:
                    self.write(f'virtual auto {receive_fn(request_method)}() -> {result_name};')
                self.newline()

    def generate_incoming_notification_handlers(self) -> None:
        self.write('// ====================== //')
        self.write('// Incoming Notifications //')
        self.write('// ====================== //')
        self.newline()
        for notification_spec in self.schema["notifications"]:
            if notification_spec["messageDirection"] == "clientToServer":
                notification_method = notification_spec["method"]
                notification_name = method_to_camel_case(notification_method)
                self.generate_docstring(notification_spec.get("documentation", None))
                params_spec = notification_spec.get("params", None)
                if params_spec is not None:
                    self.write(f'virtual auto {receive_fn(notification_method)}(')
                    with self.indent(): self.write(f'{params_spec["name"]} &params')
                    self.write(f') -> void;')
                else:
                    self.write(f'virtual auto {receive_fn(notification_method)}() -> void;')
                self.newline()

    def generate_outgoing_request_handlers(self) -> None:
        self.write('// ================= //')
        self.write('// Outgoing Requests //')
        self.write('// ================= //')
        self.newline()
        for request_spec in self.schema["requests"]:
            if request_spec["messageDirection"] == "serverToClient":
                request_method = request_spec["method"]
                request_name = method_to_camel_case(request_method)
                self.generate_docstring(request_spec.get("documentation", None))
                params_spec = request_spec.get("params", None)
                if params_spec is not None:
                    self.write(f'virtual auto {send_fn(request_method)}(')
                    with self.indent(): self.write(f'{params_spec["name"]} &params')
                    self.write(f') -> int;')
                else:
                    self.write(f'virtual auto {send_fn(request_method)}() -> int;')
                self.newline()
                self.generate_docstring(request_spec.get("documentation", None))
                result_spec = request_spec.get("result", None)
                if result_spec is not None:
                    result_name = f'{request_name}Result'
                    self.write(f'virtual auto {receive_fn(request_method)}(')
                    with self.indent():
                        if result_spec["kind"] == "base":
                            self.write(f'{result_name} params')
                        else:
                            self.write(f'{result_name} &params')
                    self.write(f') -> void;')
                else:
                    self.write(f'virtual auto {receive_fn(request_method)}() -> void;')
                self.newline()

    def generate_outgoing_notification_handlers(self) -> None:
        self.write('// ====================== //')
        self.write('// Outgoing Notifications //')
        self.write('// ====================== //')
        self.newline()
        for notification_spec in self.schema["notifications"]:
            if notification_spec["messageDirection"] == "serverToClient":
                notification_method = notification_spec["method"]
                notification_name = method_to_camel_case(notification_method)
                self.generate_docstring(notification_spec.get("documentation", None))
                params_spec = notification_spec.get("params", None)
                if params_spec is not None:
                    self.write(f'virtual auto {send_fn(notification_method)}(')
                    with self.indent(): self.write(f'{params_spec["name"]} &params')
                    self.write(f') -> void;')
                else:
                    self.write(f'virtual auto {send_fn(notification_method)}() -> void;')
                self.newline()

    def generate_dispatch_methods(self) -> None:
        self.write('auto dispatch(')
        with self.indent():
            self.write('ResponseMessage &response,')
            self.write('RequestMessage &request')
        self.write(') -> void;')
        self.newline()
        self.write('auto dispatch(')
        with self.indent():
            self.write('ResponseMessage &response,')
            self.write('NotificationMessage &notification')
        self.write(') -> void;')
        self.newline()
        self.write('auto dispatch(')
        with self.indent():
            self.write('ResponseMessage &response,')
            self.write('std::string &traceId,')
            self.write('const LSPAny &document')
        self.write(') -> void;')
        self.newline()

    def generate_prepare(self) -> None:
        self.write('auto prepare(')
        with self.indent():
            self.write('std::string &buffer,')
            self.write('const std::string &response')
        self.write(') const -> void override;')
        self.newline()

    def generate_require_message_params(self) -> None:
        self.write('auto requireMessageParams(')
        with self.indent():
            self.write('RequestMessage &request')
        self.write(') const -> MessageParams &;')
        self.newline()
        self.write('auto requireMessageParams(')
        with self.indent():
            self.write('NotificationMessage &notification')
        self.write(') const -> MessageParams &;')
        self.newline()

    def generate_next_send_id(self) -> None:
        self.write('inline auto nextSendId() -> std::size_t {')
        with self.indent():
            self.write('return serialSendId++;')
        self.write('}')
        self.newline()

    def generate_next_request_id(self) -> None:
        self.write('inline auto nextRequestId() -> int {')
        with self.indent():
            self.write('return serialRequestId++;')
        self.write('}')
        self.newline()

    def generate_is_initialized(self) -> None:
        self.write('inline auto isInitialized() const -> bool {')
        with self.indent():
            self.write('return _initialized;')
        self.write('}')
        self.newline()

    def generate_is_shutdown(self) -> None:
        self.write('inline auto isShutdown() const -> bool {')
        with self.indent():
            self.write('return _shutdown;')
        self.write('}')
        self.newline()

    def generate_is_running(self) -> None:
        self.write('inline auto isRunning() const -> bool {')
        with self.indent():
            self.write('return !_shutdown;')
        self.write('}')
        self.newline()

    def generate_constructor(self) -> None:
        self.write('LspLanguageServer(')
        with self.indent():
            self.write('ls::MessageQueue &incomingMessages,')
            self.write('ls::MessageQueue &outgoingMessages,')
            self.write('std::size_t numRequestThreads,')
            self.write('std::size_t numWorkerThreads,')
            self.write('lsl::Logger &logger,')
            self.write('const std::string &configSection')
        self.write(');')
        self.newline()

    def generate_synchronized_send(self) -> None:
        self.write('auto send(')
        with self.indent():
            self.write('const std::string &request,')
            self.write('std::size_t sendId')
        self.write(') -> void;')
        self.newline()

    def generate_handle(self) -> None:
        self.write('auto handle(')
        with self.indent():
            self.write('const std::string &incoming,')
            self.write('std::size_t sendId')
        self.write(') -> void;')
        self.newline()

    def generate_log_receive_trace(self) -> None:
        self.write('auto logReceiveTrace(')
        with self.indent():
            self.write('const std::string &messageType,')
            self.write('const std::string &traceId,')
            self.write('const std::optional<MessageParams> &optionalParams')
        self.write(') -> void;')
        self.newline()

    def generate_log_receive_response_trace(self) -> None:
        self.write('auto logReceiveResponseTrace(')
        with self.indent():
            self.write('const std::string &traceId,')
            self.write('const LSPAny &document')
        self.write(') -> void;')
        self.newline()

    def generate_log_send_response_trace(self) -> None:
        self.write('auto logSendResponseTrace(')
        with self.indent():
            self.write('const std::string &traceId,')
            self.write('const std::chrono::time_point<std::chrono::high_resolution_clock> &start,')
            self.write('const LSPAny &response')
        self.write(') -> void;')
        self.newline()

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        self.generate_disclaimer()
        self.write('#pragma once')
        self.newline()
        self.write('#include <atomic>')
        self.write('#include <chrono>')
        self.write('#include <condition_variable>')
        self.write('#include <mutex>')
        self.write('#include <thread>')
        self.write('#include <unordered_map>')
        self.newline()
        self.write('#include <server/language_server.h>')
        self.write('#include <server/logger.h>')
        self.write('#include <server/lsp_json_serializer.h>')
        self.write('#include <server/lsp_transformer.h>')
        self.write('#include <server/specification.h>')
        self.newline()
        self.write(f'namespace {self.namespace} {{')
        with self.indent():
            self.write('namespace ls = LCompilers::LLanguageServer;')
            self.write('namespace lsl = LCompilers::LLanguageServer::Logging;')
            self.write('namespace lst = LCompilers::LLanguageServer::Threading;')
            self.newline()
            self.write('class LspLanguageServer : public ls::LanguageServer {')
            self.write('public:')
            with self.indent():
                self.generate_constructor()
                self.generate_is_initialized();
                self.generate_is_shutdown();
                self.generate_is_running()
                self.write('auto isTerminated() const -> bool override;')
            self.write('protected:')
            with self.indent():
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
                self.write('std::atomic<TraceValues> trace{TraceValues::OFF};')
                self.newline()
                self.generate_next_send_id()
                self.generate_next_request_id()
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
                self.generate_incoming_request_handlers()
                self.generate_incoming_notification_handlers()
                self.generate_outgoing_request_handlers()
                self.generate_outgoing_notification_handlers()
            self.write('}; // class LspLanguageServer')
            self.newline()
        self.write(f'}} // namespace {self.namespace}')
