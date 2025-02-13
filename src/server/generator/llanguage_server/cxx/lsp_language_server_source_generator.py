from pathlib import Path
from typing import Any, Dict, Tuple

from llanguage_server.file_generator import DEFAULT_INDENT_PATTERN
from llanguage_server.cxx.file_generator import CPlusPlusLspFileGenerator
from llanguage_server.utils import (lower_first, method_to_camel_case,
                                    method_to_underscore, receive_fn,
                                    rename_enum, send_fn, upper_first)


class CPlusPlusLspLanguageServerSourceGenerator(CPlusPlusLspFileGenerator):

    def __init__(
        self,
        output_dir: Path,
        schema: Dict[str, Any],
        namespace: str,
        symbols: Dict[str, Tuple[str, Dict[str, Any]]]
    ) -> None:
        specification_source = output_dir / "lsp_language_server.cpp"
        super().__init__(specification_source, schema, namespace, symbols)

    def generate_constructor(self) -> None:
        self.write('LspLanguageServer::LspLanguageServer(')
        with self.indent():
            self.write('ls::MessageQueue &incomingMessages,')
            self.write('ls::MessageQueue &outgoingMessages,')
            self.write('std::size_t numRequestThreads,')
            self.write('std::size_t numWorkerThreads,')
            self.write('lsl::Logger &logger,')
            self.write('const std::string &configSection')
        self.write(') : ls::LanguageServer(')
        self.write('        incomingMessages,')
        self.write('        outgoingMessages,')
        self.write('        logger')
        self.write('    )')
        self.write('    , configSection(configSection)')
        self.write('    , listener([this]() {')
        with self.indent():
            self.write('    listen();')
        self.write('    })')
        self.write('    , requestPool("request", numRequestThreads, logger)')
        self.write('    , workerPool("worker", numWorkerThreads, logger)')
        self.write('    , transformer(logger)')
        self.write('{')
        with self.indent():
            self.write('callbacksById.reserve(256);')
        self.write('}')
        self.newline()

    def generate_join(self) -> None:
        self.write('auto LspLanguageServer::join() -> void {')
        with self.indent():
            self.write('if (listener.joinable()) {')
            with self.indent():
                self.write('listener.join();')
            self.write('}')
            self.write('requestPool.join();')
            self.write('workerPool.join();')
        self.write('}')
        self.newline()

    def generate_listen(self) -> None:
        self.write('auto LspLanguageServer::listen() -> void {')
        with self.indent():
            self.write('try {')
            with self.indent():
                self.write('while (!_exit) {')
                with self.indent():
                    self.write('const std::string message = incomingMessages.dequeue();')
                    self.write('if (!_exit) {')
                    with self.indent():
                        self.write('std::size_t sendId = nextSendId();')
                        self.write('requestPool.execute([this, message, sendId](')
                        with self.indent():
                            self.write('const std::string &threadName,')
                            self.write('const std::size_t threadId')
                        self.write(') {')
                        with self.indent():
                            self.write('try {')
                            with self.indent():
                                self.write('handle(message, sendId);')
                            self.write('} catch (std::exception &e) {')
                            with self.indent():
                                self.write('std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());')
                                self.write('logger.error()')
                                with self.indent():
                                    self.write('<< "[" << threadName << "_" << threadId << "] "')
                                    self.write('<< "Failed to handle message: " << message')
                                    self.write('<< std::endl;')
                                self.write('logger.error()')
                                with self.indent():
                                    self.write('<< "[" << threadName << "_" << threadId << "] "')
                                    self.write('<< "Caught unhandled exception: " << e.what()')
                                    self.write('<< std::endl;')
                            self.write('}')
                        self.write('});')
                    self.write('}')
                self.write('}')
            self.write('} catch (std::exception &e) {')
            with self.indent():
                self.write('logger.warn()')
                with self.indent():
                    self.write('<< "[LspLanguageServer] Interrupted while dequeuing messages: "')
                    self.write('<< e.what()')
                    self.write('<< std::endl;')
            self.write('}')
            self.write('logger.debug()')
            with self.indent():
                self.write('<< "[LspLanguageServer] Incoming-message listener terminated."')
                self.write('<< std::endl;')
        self.write('}')
        self.newline()

    def generate_notify_sent(self) -> None:
        self.write('auto LspLanguageServer::notifySent() -> void {')
        with self.indent():
            self.write('++pendingSendId;')
            self.write('{')
            with self.indent():
                self.write('std::unique_lock<std::mutex> sentLock(sentMutex);')
                self.write('sent.notify_all();')
            self.write('}')
        self.write('}')
        self.newline()

    def generate_synchronized_send(self) -> None:
        self.write('auto LspLanguageServer::send(')
        with self.indent():
            self.write('const std::string &message,')
            self.write('std::size_t sendId')
        self.write(') -> void {')
        with self.indent():
            self.write('// -------------------------------------------------------------------------')
            self.write('// NOTE: The LSP spec requires responses to be returned in roughly the same')
            self.write('// order of receipt of their corresponding requests. Some types of responses')
            self.write('// may be returned out-of-order, but in order to support those we will need')
            self.write('// to implement a sort of dependency graph. Without knowledge of their')
            self.write('// dependencies, we must respond to all requests in order of receipt.')
            self.write('// -------------------------------------------------------------------------')
            self.write('{')
            with self.indent():
                self.write('std::unique_lock<std::mutex> sentLock(sentMutex);')
                self.write('sent.wait(sentLock, [this, sendId]{')
                with self.indent():
                    self.write('return (pendingSendId == sendId) || _exit;')
                self.write('});')
            self.write('}')
            self.write('if ((pendingSendId == sendId) && !_exit) {')
            with self.indent():
                self.write('ls::LanguageServer::send(message);')
                self.write('notifySent();')
            self.write('}')
        self.write('}')
        self.newline()

    def generate_log_receive_trace(self) -> None:
        self.write('auto LspLanguageServer::logReceiveTrace(')
        with self.indent():
            self.write('const std::string &messageType,')
            self.write('const std::string &traceId,')
            self.write('const std::optional<MessageParams> &optionalParams')
        self.write(') -> void {')
        with self.indent():
            self.write('if ((trace >= TraceValues::MESSAGES) && (traceId.length() > 0)) {')
            with self.indent():
                self.write('LogTraceParams params;')
                self.write(f'params.message = "Received " + messageType + " \'" + traceId + "\'.";')
                self.write('if ((trace >= TraceValues::VERBOSE) && optionalParams.has_value()) {')
                with self.indent():
                    self.write('const MessageParams &messageParams = optionalParams.value();')
                    self.write('MessageParamsType messageParamsType =')
                    with self.indent():
                        self.write('static_cast<MessageParamsType>(messageParams.index());')
                    self.write('switch (messageParamsType) {')
                    self.write(f'case MessageParamsType::{rename_enum("object")}: {{')
                    with self.indent():
                        self.write('const LSPObject &object = std::get<LSPObject>(messageParams);')
                        self.write(f'params.verbose = "Params: " + serializer.pprint(object, "{DEFAULT_INDENT_PATTERN}");')
                        self.write('break;')
                    self.write('}')
                    self.write(f'case MessageParamsType::{rename_enum("array")}: {{')
                    with self.indent():
                        self.write('const LSPArray &array = std::get<LSPArray>(messageParams);')
                        self.write(f'params.verbose =')
                        with self.indent():
                            self.write(f'"Params: " + serializer.pprint(array, "{DEFAULT_INDENT_PATTERN}");')
                        self.write('break;')
                    self.write('}')
                    self.write('}')
                self.write('}')
                self.write('sendLogTrace(params);')
            self.write('}')
        self.write('}')
        self.newline()

    def generate_log_receive_response_trace(self) -> None:
        self.write('auto LspLanguageServer::logReceiveResponseTrace(')
        with self.indent():
            self.write('const std::string &traceId,')
            self.write('const LSPAny &document')
        self.write(') -> void {')
        with self.indent():
            self.write('if (trace >= TraceValues::MESSAGES) {')
            with self.indent():
                self.write('LogTraceParams params;')
                self.write('if (traceId.length() > 0) {')
                with self.indent():
                    self.write(f'params.message = "Received response \'" + traceId + "\'.";')
                self.write('} else {')
                with self.indent():
                    self.write(f'params.message = "Received response.";')
                self.write('}')
                self.write('if (trace >= TraceValues::VERBOSE) {')
                with self.indent():
                    self.write('LSPAnyType documentType = static_cast<LSPAnyType>(document.index());')
                    self.write(f'if (documentType == LSPAnyType::{rename_enum("object")}) {{')
                    with self.indent():
                        self.write('const LSPObject &object = std::get<LSPObject>(document);')
                        self.write('auto iter = object.find("result");')
                        self.write('if (iter != object.end()) {')
                        with self.indent():
                            self.write('const LSPAny &result = *iter->second;')
                            self.write(f'params.verbose =')
                            with self.indent():
                                self.write(f'"Result: " + serializer.pprint(result, "{DEFAULT_INDENT_PATTERN}");')
                        self.write('} else if ((iter = object.find("error")) != object.end()) {')
                        with self.indent():
                            self.write('const LSPAny &error = *iter->second;')
                            self.write(f'params.verbose =')
                            with self.indent():
                                self.write(f'"Error: " + serializer.pprint(error, "{DEFAULT_INDENT_PATTERN}");')
                        self.write('} else {')
                        with self.indent():
                            self.write(f'params.verbose = "No result returned.";')
                        self.write('}')
                    self.write('} else {')
                    with self.indent():
                        self.write('logger.error()')
                        with self.indent():
                            self.write('<< "Cannot log verbose message for response of type LSPAnyType::"')
                            self.write('<< LSPAnyTypeNames.at(documentType)')
                            self.write('<< std::endl;')
                    self.write('}')
                self.write('}')
                self.write('sendLogTrace(params);')
            self.write('}')
        self.write('}')
        self.newline()

    def generate_log_send_response_trace(self) -> None:
        self.write('auto LspLanguageServer::logSendResponseTrace(')
        with self.indent():
            self.write('const std::string &traceId,')
            self.write('const std::chrono::time_point<std::chrono::high_resolution_clock> &start,')
            self.write('const LSPAny &response')
        self.write(') -> void {')
        with self.indent():
            self.write('if ((trace >= TraceValues::MESSAGES) && (traceId.length() > 0)) {')
            with self.indent():
                self.write('const auto end = std::chrono::high_resolution_clock::now();')
                self.write('const auto duration =')
                with self.indent():
                    self.write('std::chrono::duration_cast<std::chrono::milliseconds>(end - start);')
                self.write('LogTraceParams params;')
                self.write('params.message =')
                with self.indent():
                    self.write('"Sending response \'" + traceId + "\'. Processing request took " +')
                    self.write('std::to_string(duration.count()) + "ms";')
                self.write('if (trace >= TraceValues::VERBOSE) {')
                with self.indent():
                    self.write('LSPAnyType responseType = static_cast<LSPAnyType>(response.index());')
                    self.write(f'if (responseType == LSPAnyType::{rename_enum("object")}) {{')
                    with self.indent():
                        self.write('const LSPObject &object = std::get<LSPObject>(response);')
                        self.write('auto iter = object.find("result");')
                        self.write('if (iter != object.end()) {')
                        with self.indent():
                            self.write('const LSPAny &result = *iter->second;')
                            self.write(f'params.verbose =')
                            with self.indent():
                                self.write(f'"Result: " + serializer.pprint(result, "{DEFAULT_INDENT_PATTERN}");')
                        self.write('} else if ((iter = object.find("error")) != object.end()) {')
                        with self.indent():
                            self.write('const LSPAny &error = *iter->second;')
                            self.write(f'params.verbose =')
                            with self.indent():
                                self.write(f'"Error: " + serializer.pprint(error, "{DEFAULT_INDENT_PATTERN}");')
                        self.write('} else {')
                        with self.indent():
                            self.write(f'params.verbose = "No result returned.";')
                        self.write('}')
                    self.write('} else {')
                    with self.indent():
                        self.write('logger.error()')
                        with self.indent():
                            self.write('<< "Cannot log verbose message for response of type LSPAnyType::"')
                            self.write('<< LSPAnyTypeNames.at(responseType)')
                            self.write('<< std::endl;')
                    self.write('}')
                self.write('}')
                self.write('sendLogTrace(params);')
            self.write('}')
        self.write('}')
        self.newline()

    def generate_handle(self) -> None:
        self.write('auto LspLanguageServer::handle(')
        with self.indent():
            self.write('const std::string &incoming,')
            self.write('std::size_t sendId')
        self.write(') -> void {')
        with self.indent():
            self.write('const auto start = std::chrono::high_resolution_clock::now();')
            self.write('ResponseMessage response;')
            self.write('std::string traceId;')
            self.write('try {')
            with self.indent():
                self.write('// The language server protocol always uses “2.0” as the jsonrpc version.')
                self.write('response.jsonrpc = JSON_RPC_VERSION;')
                self.write('response.id = nullptr;')
                self.newline()
                self.write('LspJsonParser parser(incoming);')
                self.write('std::unique_ptr<LSPAny> document = parser.parse();')
                self.write('LSPAnyType documentType = static_cast<LSPAnyType>(document->index());')
                self.newline()
                self.write(f'if (documentType != LSPAnyType::{rename_enum("object")}) {{')
                with self.indent():
                    self.write('// TODO: Add support for batched messages, i.e. multiple messages within')
                    self.write('// an array.')
                    self.write(f'if (documentType == LSPAnyType::{rename_enum("array")}) {{')
                    with self.indent():
                        self.write('throw LSP_EXCEPTION(')
                        with self.indent():
                            self.write('ErrorCodes::INVALID_PARAMS,')
                            self.write('"Batched requests are not supported (currently)."')
                        self.write(');')
                    self.write('}')
                    self.write('throw LSP_EXCEPTION(')
                    with self.indent():
                        self.write('ErrorCodes::INVALID_PARAMS,')
                        self.write('"Invalid request message: " + incoming')
                    self.write(');')
                self.write('}')
                self.newline()
                self.write('const LSPObject &object = std::get<LSPObject>(*document);')
                self.write('LSPObject::const_iterator iter;')
                self.newline()
                self.write('if ((iter = object.find("id")) != object.end()) {')
                with self.indent():
                    self.write('response.id = transformer.anyToResponseId(*iter->second);')
                self.write('}')
                self.newline()
                self.write('if ((iter = object.find("method")) != object.end()) {')
                with self.indent():
                    self.write('const std::string &method =')
                    with self.indent():
                        self.write('transformer.anyToString(*iter->second);')
                    self.write('if (isIncomingRequest(method)) {')
                    with self.indent():
                        self.write('if (static_cast<ResponseIdType>(response.id.index()) ==')
                        with self.indent(2): self.write('ResponseIdType::NULL_TYPE) {')
                        with self.indent():
                            self.write('throw LSP_EXCEPTION(')
                            with self.indent():
                                self.write('ErrorCodes::INVALID_PARAMS,')
                                self.write('"Missing request method=\\"" + method + "\\" attribute: id"')
                            self.write(');')
                        self.write('}')
                        self.write('std::unique_ptr<RequestMessage> request =')
                        with self.indent():
                            self.write('transformer.anyToRequestMessage(*document);')
                        self.write('if (trace >= TraceValues::MESSAGES) {')
                        with self.indent():
                            self.write('traceId = request->method + " - (" + to_string(request->id) + ")";')
                            self.write('logReceiveTrace("request", traceId, request->params);')
                        self.write('}')
                        self.write('response.jsonrpc = request->jsonrpc;')
                        self.write('dispatch(response, *request);')
                    self.write('} else if (isIncomingNotification(method)) {')
                    with self.indent():
                        self.write('if (static_cast<ResponseIdType>(response.id.index()) !=')
                        with self.indent(2):
                            self.write('ResponseIdType::NULL_TYPE) {')
                        with self.indent():
                            self.write('throw LSP_EXCEPTION(')
                            with self.indent():
                                self.write('ErrorCodes::INVALID_PARAMS,')
                                self.write('"Notification method=\\"" + method + "\\" must not contain the attribute: id"')
                            self.write(');')
                        self.write('}')
                        self.write('std::unique_ptr<NotificationMessage> notification =')
                        with self.indent():
                            self.write('transformer.anyToNotificationMessage(*document);')
                        self.write('if (trace >= TraceValues::MESSAGES) {')
                        with self.indent():
                            self.write('traceId = notification->method;')
                            self.write('logReceiveTrace("notification", traceId, notification->params);')
                        self.write('}')
                        self.write('response.jsonrpc = notification->jsonrpc;')
                        self.write('dispatch(response, *notification);')
                    self.write('} else {')
                    with self.indent():
                        self.write('throw LSP_EXCEPTION(')
                        with self.indent():
                            self.write('ErrorCodes::INVALID_REQUEST,')
                            self.write('"Unsupported method: \\"" + method + "\\""')
                        self.write(');')
                    self.write('}')
                self.write('} else if ((iter = object.find("result")) != object.end()) {')
                with self.indent():
                    self.write('notifySent();')
                    self.write('response.result = transformer.copy(iter->second);')
                    self.write('dispatch(response, traceId, *document);')
                    self.write('return;')
                self.write('} else if ((iter = object.find("error")) != object.end()) {')
                with self.indent():
                    self.write('notifySent();')
                    self.write('response.error = transformer.anyToResponseError(*iter->second);')
                    self.write('dispatch(response, traceId, *document);')
                    self.write('return;')
                self.write("} else {")
                with self.indent():
                    self.write('throw LSP_EXCEPTION(')
                    with self.indent():
                        self.write('ErrorCodes::INVALID_REQUEST,')
                        self.write('"Missing required attribute: method"')
                    self.write(');')
                self.write('}')
            self.write('} catch (const LspException &e) {')
            with self.indent():
                self.write('logger.error()')
                with self.indent():
                    self.write('<< "[" << e.file() << ":" << e.line() << "] "')
                    self.write('<< e.what()')
                    self.write('<< std::endl;')
                self.write('std::unique_ptr<ResponseError> error =')
                with self.indent():
                    self.write('std::make_unique<ResponseError>();')
                self.write(f'switch (static_cast<ErrorCodeType>(e.code().index())) {{')
                self.write('case ErrorCodeType::ERROR_CODES: {')
                with self.indent():
                    self.write('ErrorCodes errorCode = std::get<ErrorCodes>(e.code());')
                    self.write('error->code = static_cast<int>(errorCode);')
                    self.write('break;')
                self.write('}')
                self.write('case ErrorCodeType::LSP_ERROR_CODES: {')
                with self.indent():
                    self.write('LSPErrorCodes errorCode =')
                    with self.indent():
                        self.write('std::get<LSPErrorCodes>(e.code());')
                    self.write('error->code = static_cast<int>(errorCode);')
                    self.write('break;')
                self.write('}')
                self.write('}')
                self.write('error->message = e.what();')
                self.write('response.error = std::move(error);')
            self.write('} catch (const std::exception &e) {')
            with self.indent():
                self.write('logger.error()')
                with self.indent():
                    self.write('<< "Caught unhandled exception: "')
                    self.write('<< e.what() << std::endl;')
                self.write('std::unique_ptr<ResponseError> error =')
                with self.indent():
                    self.write('std::make_unique<ResponseError>();')
                self.write('error->code = static_cast<int>(ErrorCodes::INTERNAL_ERROR);')
                self.write('error->message =')
                with self.indent():
                    self.write('("An unexpected exception occurred. If it continues, "')
                    self.write(' "please file a ticket.");')
                self.write('response.error = std::move(error);')
            self.write('}')
            self.write('std::unique_ptr<LSPAny> any =')
            with self.indent():
                self.write('transformer.responseMessageToAny(response);')
            self.write('logSendResponseTrace(traceId, start, *any);')
            self.write('const std::string outgoing = serializer.serialize(*any);')
            self.write('send(outgoing, sendId);')
        self.write('}')
        self.newline()

    def generate_is_terminated(self) -> None:
        self.write('auto LspLanguageServer::isTerminated() const -> bool {')
        with self.indent():
            self.write('return _exit;')
        self.write('}')
        self.newline()

    def generate_initialize_params(self) -> None:
        self.write('auto LspLanguageServer::initializeParams(')
        self.write(') const -> const InitializeParams & {')
        with self.indent():
            self.write('if (_initializeParams) {')
            with self.indent():
                self.write('return *_initializeParams;')
            self.write('}')
            self.write('throw std::logic_error("Server has not been initialized.");')
        self.write('}')
        self.newline()

    def generate_assert_initialized(self) -> None:
        self.write('auto LspLanguageServer::assertInitialized() -> void{')
        with self.indent():
            self.write('if (!_initialized) {')
            with self.indent():
                self.write('throw LSP_EXCEPTION(')
                with self.indent():
                    self.write('ErrorCodes::SERVER_NOT_INITIALIZED,')
                    self.write('"Method \\"initialize\\" must be called first."')
                self.write(');')
            self.write('}')
        self.write('}')
        self.newline()

    def generate_assert_running(self) -> None:
        self.write('auto LspLanguageServer::assertRunning() -> void {')
        with self.indent():
            self.write('if (_shutdown) {')
            with self.indent():
                self.write('throw LSP_EXCEPTION(')
                with self.indent():
                    self.write('LSPErrorCodes::REQUEST_FAILED,')
                    self.write('"Server has shutdown and cannot accept new requests."')
                self.write(');')
            self.write('}')
        self.write('}')
        self.newline()

    def generate_request_id_to_string(self) -> None:
        self.write('auto LspLanguageServer::to_string(')
        with self.indent():
            self.write('const RequestId &requestId')
        self.write(') -> std::string {')
        with self.indent():
            self.write('RequestIdType requestIdType =')
            with self.indent():
                self.write('static_cast<RequestIdType>(requestId.index());')
            self.write('switch (requestIdType) {')
            self.write(f'case RequestIdType::{rename_enum("integer")}: {{')
            with self.indent():
                self.write('return std::to_string(std::get<integer_t>(requestId));')
            self.write('}')
            self.write(f'case RequestIdType::{rename_enum("string")}: {{')
            with self.indent():
                self.write('return std::get<string_t>(requestId);')
            self.write('}')
            self.write('default: {')
            with self.indent():
                self.write('throw LSP_EXCEPTION(')
                with self.indent():
                    self.write('ErrorCodes::INVALID_PARAMS,')
                    self.write('("Cannot log request trace with Id of type RequestIdType::" +')
                    self.write(' RequestIdTypeNames.at(requestIdType))')
                self.write(');')
            self.write('}')
            self.write('}')
        self.write('}')

    def generate_dispatch_request(self) -> None:
        self.write('auto LspLanguageServer::dispatch(')
        with self.indent():
            self.write('ResponseMessage &response,')
            self.write('RequestMessage &request')
        self.write(') -> void {')
        with self.indent():
            self.write('IncomingRequest method;')
            self.write('try {')
            with self.indent():
                self.write('method = incomingRequestByValue(request.method);')
            self.write('} catch (std::invalid_argument &e) {')
            with self.indent():
                self.write('goto invalidMethod;')
            self.write('}')
            self.write('assertRunning();')
            self.write('if (method != IncomingRequest::INITIALIZE) {')
            with self.indent():
                self.write('assertInitialized();')
            self.write('} else {')
            with self.indent():
                self.write('bool expected = false;    // a reference is required')
                self.write('if (!_initialized.compare_exchange_strong(expected, true)) {')
                with self.indent():
                    self.write('throw LSP_EXCEPTION(')
                    with self.indent():
                        self.write('ErrorCodes::INVALID_REQUEST,')
                        self.write('"Server may be initialized only once."')
                    self.write(');')
                self.write('}')
            self.write('}')
            self.write('switch (method) {')
            for request_spec in self.schema["requests"]:
                if request_spec["messageDirection"] == "clientToServer":
                    request_method = request_spec["method"]
                    request_name = method_to_camel_case(request_method)
                    result_name = f'{request_name}Result'
                    self.write(f'case IncomingRequest::{method_to_underscore(request_method)}: {{')
                    with self.indent():
                        params_spec = request_spec.get("params", None)
                        if params_spec is not None:
                            is_initialize = request_method == "initialize"
                            num_levels = int(is_initialize)
                            if is_initialize:
                                self.write('try {')
                            with self.indent(num_levels):
                                self.write('MessageParams &messageParams = requireMessageParams(request);')
                                self.write(f'std::unique_ptr<{params_spec["name"]}> requestParams =')
                                with self.indent(): self.write(f'transformer.as{request_name}Params(messageParams);')
                                self.write(f'{result_name} result =')
                                with self.indent(): self.write(f'{receive_fn(request_method)}(*requestParams);')
                                self.write(f'response.result =')
                                with self.indent():
                                    self.write(f'transformer.{lower_first(result_name)}ToAny(result);')
                                if is_initialize:
                                    self.write('_initializeParams = std::move(requestParams);')
                            if is_initialize:
                                self.write('} catch (LspException &e) {')
                                with self.indent():
                                    self.write('bool expected = true;')
                                    self.write('if (!_initialized.compare_exchange_strong(expected, false)) {')
                                    with self.indent():
                                        self.write('throw LSP_EXCEPTION(')
                                        with self.indent():
                                            self.write('ErrorCodes::INVALID_REQUEST,')
                                            self.write('"Server initialization out of sync."')
                                        self.write(');')
                                    self.write('}')
                                    self.write('throw e;')
                                self.write('}')
                        else:
                            self.write(f'{result_name} result = {receive_fn(request_method)}();')
                            self.write(f'response.result =')
                            with self.indent():
                                self.write(f'transformer.{lower_first(result_name)}ToAny(result);')
                        self.write('break;')
                    self.write('}')
            self.write('default: {')
            self.write('invalidMethod:')
            with self.indent():
                self.write('throw LSP_EXCEPTION(')
                with self.indent():
                    self.write('ErrorCodes::METHOD_NOT_FOUND,')
                    self.write('"Unsupported request method: \\"" + request.method + "\\""')
                self.write(');')
            self.write('}')
            self.write('}')
        self.write('}')
        self.newline()

    def generate_dispatch_notification(self) -> None:
        self.write('auto LspLanguageServer::dispatch(')
        with self.indent():
            self.write('ResponseMessage &/*response*/,')
            self.write('NotificationMessage &notification')
        self.write(') -> void {')
        with self.indent():
            self.write('IncomingNotification method;')
            self.write('try {')
            with self.indent():
                self.write('method = incomingNotificationByValue(notification.method);')
            self.write('} catch (std::invalid_argument &e) {')
            with self.indent():
                self.write('goto invalidMethod;')
            self.write('}')
            self.write('if (method != IncomingNotification::EXIT) {')
            with self.indent():
                self.write('if (!_initialized) {')
                with self.indent():
                    self.write('// Notifications should be dropped, except for the exit notification.')
                    self.write('// This will allow the exit of a server without an initialize request.')
                    self.write('return;')
                self.write('}')
                self.write('assertRunning();')
            self.write('}')
            self.write('switch (method) {')
            for notification_spec in self.schema["notifications"]:
                if notification_spec["messageDirection"] == "clientToServer":
                    notification_method = notification_spec["method"]
                    notification_name = method_to_camel_case(notification_method)
                    self.write(f'case IncomingNotification::{method_to_underscore(notification_method)}: {{')
                    with self.indent():
                        params_spec = notification_spec.get("params", None)
                        if params_spec is not None:
                            self.write('MessageParams &messageParams = requireMessageParams(notification);')
                            self.write(f'std::unique_ptr<{params_spec["name"]}> notificationParams =')
                            with self.indent(): self.write(f'transformer.as{notification_name}Params(messageParams);')
                            self.write(f'{receive_fn(notification_method)}(*notificationParams);')
                        else:
                            self.write(f'{receive_fn(notification_method)}();')
                        self.write('break;')
                    self.write('}')
            self.write('default: {')
            self.write('invalidMethod:')
            with self.indent():
                self.write('if (notification.method.compare(0, 2, "$/") == 0) {')
                with self.indent():
                    self.write('// NOTE: If a server or client receives notifications starting with "$/"')
                    self.write('// it is free to ignore the notification:')
                    self.write('logger.debug()')
                    with self.indent():
                        self.write(f'<< "No handler exists for method: \\"" << notification.method << "\\""')
                        self.write('<< std::endl;')
                self.write('} else {')
                with self.indent():
                    self.write('throw LSP_EXCEPTION(')
                    with self.indent():
                        self.write('ErrorCodes::METHOD_NOT_FOUND,')
                        self.write('"Unsupported notification method: \\"" + notification.method + "\\""')
                    self.write(');')
                self.write('}')
            self.write('}')
            self.write('}')
        self.write('}')
        self.newline()

    def generate_dispatch_response(self) -> None:
        self.write('auto LspLanguageServer::dispatch(')
        with self.indent():
            self.write('ResponseMessage &response,')
            self.write('std::string &traceId,')
            self.write('const LSPAny &document')
        self.write(') -> void {')
        with self.indent():
            self.write('ResponseIdType responseIdType =')
            with self.indent():
                self.write('static_cast<ResponseIdType>(response.id.index());')
            self.write('switch (responseIdType) {')
            self.write(f'case ResponseIdType::{rename_enum("integer")}: {{')
            with self.indent():
                self.write('int responseId = std::get<int>(response.id);')
                self.write('std::unique_lock<std::mutex> callbackLock(callbackMutex);')
                self.write('auto iter = callbacksById.find(responseId);')
                self.write('if (iter == callbacksById.end()) {')
                with self.indent():
                    self.write('logger.error() << "Cannot locate request with id: " << responseId << std::endl;')
                    self.write('return;')
                self.write('}')
                self.write('const std::string method = iter->second;  // copy before delete')
                self.write('callbacksById.erase(iter);')
                self.write('callbackLock.unlock();')
                self.write('traceId = method + " - (" + std::to_string(responseId) + ")";')
                self.write('logReceiveResponseTrace(traceId, document);')
                self.newline()
                self.write('OutgoingRequest request;')
                self.write('try {')
                with self.indent():
                    self.write('request = outgoingRequestByValue(method);')
                self.write('} catch (std::invalid_argument &e) {')
                with self.indent():
                    self.write('goto invalidMethod;')
                self.write('}')
                self.newline()
                self.write('switch (request) {')
                for request_spec in self.schema["requests"]:
                    if request_spec["messageDirection"] == "serverToClient":
                        request_method = request_spec["method"]
                        request_name = method_to_camel_case(request_method)
                        result_name = f'{request_name}Result'
                        self.write(f'case OutgoingRequest::{method_to_underscore(request_method)}: {{')
                        with self.indent():
                            result_spec = request_spec.get("result", None)
                            if result_spec is not None:
                                self.write('if (!response.result.has_value()) {')
                                with self.indent():
                                    self.write(f'logger.error()')
                                    with self.indent():
                                        self.write(f'<< "Missing required attribute for method \\"{request_method}\\": result"')
                                        self.write('<< std::endl;')
                                    self.write('return;')
                                self.write('}')
                                self.write('std::unique_ptr<LSPAny> &result = response.result.value();')
                                symbol_name = result_name
                                symbol_spec = result_spec
                                symbol_kind = symbol_spec["kind"]
                                while (symbol_kind == "reference") and (symbol_name in self.symbols):
                                    symbol_kind, symbol_spec = self.symbols[symbol_name]
                                    symbol_name = symbol_spec["name"]
                                if symbol_kind == "structure":
                                    self.write(f'std::unique_ptr<{result_name}> params =')
                                else:
                                    self.write(f'{result_name} params =')
                                with self.indent():
                                    self.write(f'transformer.anyTo{upper_first(result_name)}(*result);')
                                if symbol_kind == "structure":
                                    self.write(f'{receive_fn(request_method)}(*params);')
                                else:
                                    self.write(f'{receive_fn(request_method)}(params);')
                            else:
                                self.write(f'{receive_fn(request_method)}()')
                            self.write('break;')
                        self.write('}')
                self.write('default: {')
                self.write('invalidMethod:')
                with self.indent():
                    self.write(f'logger.error() << "Unsupported request method: \\"" << method << "\\"";')
                self.write('}')
                self.write('}')
                self.write('break;')
            self.write('}')
            self.write(f'case ResponseIdType::{rename_enum("null")}: {{')
            with self.indent():
                self.write('logReceiveResponseTrace(traceId, document);')
                self.write('break;')
            self.write('}')
            self.write('default: {')
            with self.indent():
                self.write('logger.error()')
                with self.indent():
                    self.write('<< "Cannot dispatch response with id of type ResponseIdType::"')
                    self.write('<< ResponseIdTypeNames.at(responseIdType)')
                    self.write('<< std::endl;')
                self.write('return;')
            self.write('}')
            self.write('}')
        self.write('}')
        self.newline()

    def generate_require_request_params(self) -> None:
        self.write('auto LspLanguageServer::requireMessageParams(')
        with self.indent():
            self.write('RequestMessage &request')
        self.write(') const -> MessageParams & {')
        with self.indent():
            self.write('if (request.params.has_value()) {')
            with self.indent():
                self.write('return request.params.value();')
            self.write('}')
            self.write('throw LSP_EXCEPTION(')
            with self.indent():
                self.write('ErrorCodes::INVALID_PARAMS,')
                self.write('"RequestMessage.params must be defined for method=\\"" + request.method + "\\""')
            self.write(');')
        self.write('}')
        self.newline()

    def generate_require_notification_params(self) -> None:
        self.write('auto LspLanguageServer::requireMessageParams(')
        with self.indent():
            self.write('NotificationMessage &notification')
        self.write(') const -> MessageParams & {')
        with self.indent():
            self.write('if (notification.params.has_value()) {')
            with self.indent():
                self.write('return notification.params.value();')
            self.write('}')
            self.write('throw LSP_EXCEPTION(')
            with self.indent():
                self.write('ErrorCodes::INVALID_PARAMS,')
                self.write('"NotificationMessage.params must be defined for method=\\"" + notification.method + "\\""')
            self.write(');')
        self.write('}')
        self.newline()

    def generate_require_message_params(self) -> None:
        self.generate_require_request_params()
        self.generate_require_notification_params()

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
                self.write(f'// request: "{request_method}"')
                params_spec = request_spec.get("params", None)
                if params_spec is not None:
                    self.write(f'auto LspLanguageServer::{receive_fn(request_method)}(')
                    match request_method:
                        case "initialize":
                            with self.indent():
                                self.write(f'{params_spec["name"]} &params')
                        case _:
                            with self.indent():
                                self.write(f'{params_spec["name"]} &/*params*/')
                    self.write(f') -> {result_name} {{')
                else:
                    self.write(f'auto LspLanguageServer::{receive_fn(request_method)}() -> {result_name} {{')
                with self.indent():
                    match request_method:
                        case "initialize":
                            self.write('{ // Initialize internal parameters')
                            with self.indent():
                                self.write('if (params.trace.has_value()) {')
                                with self.indent():
                                    self.write('trace = params.trace.value();')
                                self.write('}')
                            self.write('}')
                            self.newline()
                            self.write('InitializeResult result;')
                            self.newline()
                            self.write('std::unique_ptr<ServerCapabilities> capabilities =')
                            with self.indent():
                                self.write('std::make_unique<ServerCapabilities>();')
                            self.newline()
                            self.write('return result;')
                        case "shutdown":
                            self.write('bool expected = false;')
                            self.write('if (_shutdown.compare_exchange_strong(expected, true)) {')
                            with self.indent():
                                self.write('{')
                                with self.indent():
                                    self.write('logger.info() << "Shutting down server." << std::endl;')
                                self.write('}')
                            self.write('}')
                            self.write('return nullptr;')
                        case _:
                            self.write('throw LSP_EXCEPTION(')
                            with self.indent():
                                self.write('ErrorCodes::METHOD_NOT_FOUND,')
                                self.write(f'"No handler exists for method: \\"{request_method}\\""')
                            self.write(');')
                self.write('}')
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
                self.write(f'// notification: "{notification_method}"')
                params_spec = notification_spec.get("params", None)
                if params_spec is not None:
                    self.write(f'auto LspLanguageServer::{receive_fn(notification_method)}(')
                    match notification_method:
                        case "$/setTrace":
                            with self.indent(): self.write(f'{params_spec["name"]} &params')
                        case _:
                            with self.indent(): self.write(f'{params_spec["name"]} &/*params*/')
                    self.write(f') -> void {{')
                else:
                    self.write(f'auto LspLanguageServer::{receive_fn(notification_method)}() -> void {{')
                with self.indent():
                    match notification_method:
                        case "exit":
                            self.write('bool expected = false;')
                            self.write('if (_exit.compare_exchange_strong(expected, true)) {')
                            with self.indent():
                                self.write('{')
                                with self.indent():
                                    self.write('logger.info() << "Exiting server." << std::endl;')
                                self.write('}')
                                self.write('expected = false;')
                                self.write('if (_shutdown.compare_exchange_strong(expected, true)) {')
                                with self.indent():
                                    self.write('logger.error()')
                                    with self.indent():
                                        self.write('<< "Server exited before being notified to shutdown!"')
                                        self.write('<< std::endl;')
                                self.write('}')
                                self.write('incomingMessages.stopNow();')
                                self.write('requestPool.stopNow();')
                                self.write('workerPool.stopNow();')
                                self.write('// TODO: Find a better way to terminate the message stream:')
                                self.write('std::cin.putback(\'\\0\');')
                                self.write('fclose(stdin);')
                            self.write('}')
                        case "initialized":
                            self.write('// empty')
                        case "$/setTrace":
                            self.write('trace = params.value;')
                        case _:
                            if notification_method.startswith("$/"):
                                self.write('// NOTE: If a server or client receives notifications starting with "$/" it')
                                self.write('// is free to ignore the notification:')
                                self.write('logger.debug()')
                                with self.indent():
                                    self.write(f'<< "No handler exists for method: \\"{notification_method}\\""')
                                    self.write('<< std::endl;')
                            else:
                                self.write('throw LSP_EXCEPTION(')
                                with self.indent():
                                    self.write('ErrorCodes::METHOD_NOT_FOUND,')
                                    self.write(f'"No handler exists for method: \\"{notification_method}\\""')
                                self.write(');')
                self.write('}')
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
                self.write(f'// request: "{request_method}"')
                params_spec = request_spec.get("params", None)
                if params_spec is not None:
                    self.write(f'auto LspLanguageServer::{send_fn(request_method)}(')
                    with self.indent(): self.write(f'{params_spec["name"]} &params')
                    self.write(f') -> int {{')
                else:
                    self.write(f'auto LspLanguageServer::{send_fn(request_method)}() -> int {{')
                with self.indent():
                    self.write('RequestMessage request;')
                    self.write('request.jsonrpc = JSON_RPC_VERSION;')
                    self.write('int requestId = nextRequestId();')
                    self.write('request.id = requestId;')
                    self.write('{')
                    with self.indent():
                        self.write('std::unique_lock<std::mutex> callbackLock(callbackMutex);')
                        self.write(f'callbacksById.emplace(requestId, "{request_method}");')
                    self.write('}')
                    self.write(f'request.method = "{request_method}";')
                    if params_spec is not None:
                        self.write('request.params = transformer.asMessageParams(params);')
                    self.write('std::unique_ptr<LSPAny> any =')
                    with self.indent():
                        self.write('transformer.requestMessageToAny(request);')
                    self.write('const std::string message = serializer.serialize(*any);')
                    self.write('ls::LanguageServer::send(message);')
                    self.write('return requestId;')
                self.write('}')
                self.newline()
                result_spec = request_spec.get("result", None)
                if result_spec is not None:
                    result_name = f'{request_name}Result'
                    self.write(f'auto LspLanguageServer::{receive_fn(request_method)}(')
                    with self.indent():
                        self.inline(result_name, indent=True)
                        if result_spec["kind"] == "base":
                            self.inline(' /*params*/', end='\n')
                        else:
                            self.inline(' &/*params*/', end='\n')
                    self.write(') -> void {')
                else:
                    self.write(f'auto LspLanguageServer::{receive_fn(request_method)}() -> void {{')
                with self.indent():
                    self.write(f'logger.debug()')
                    with self.indent():
                        self.write(f'<< "No handler exists for method: \\"{request_method}\\""')
                        self.write(f'<< std::endl;')
                self.write('}')
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
                self.write(f'// notification: "{notification_method}"')
                params_spec = notification_spec.get("params", None)
                if params_spec is not None:
                    self.write(f'auto LspLanguageServer::{send_fn(notification_method)}(')
                    with self.indent(): self.write(f'{params_spec["name"]} &params')
                    self.write(f') -> void {{')
                else:
                    self.write(f'auto LspLanguageServer::{send_fn(notification_method)}() -> void {{')
                with self.indent():
                    self.write('NotificationMessage notification;')
                    self.write('notification.jsonrpc = JSON_RPC_VERSION;')
                    self.write(f'notification.method = "{notification_method}";')
                    if params_spec is not None:
                        self.write('notification.params = transformer.asMessageParams(params);')
                    self.write('std::unique_ptr<LSPAny> any =')
                    with self.indent():
                        self.write('transformer.notificationMessageToAny(notification);')
                    self.write('const std::string message = serializer.serialize(*any);')
                    self.write('ls::LanguageServer::send(message);')
                self.write('}')
                self.newline()

    def generate_prepare(self) -> None:
        self.write('auto LspLanguageServer::prepare(')
        with self.indent():
            self.write('std::string &buffer,')
            self.write('const std::string &response')
        self.write(') const -> void {')
        with self.indent():
            self.write('buffer.append("Content-Type: application/vscode-jsonrpc; charset=utf-8\\r\\n");')
            self.write('buffer.append("Content-Length: ");')
            self.write('buffer.append(std::to_string(response.length()));')
            self.write('buffer.append("\\r\\n");')
            self.write('buffer.append("\\r\\n");')
            self.write('buffer.append(response);')
        self.write('}')
        self.newline()

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        self.generate_disclaimer()
        self.write('#include <cctype>')
        self.write('#include <chrono>')
        self.write('#include <cstdio>')
        self.write('#include <iostream>')
        self.write('#include <stdexcept>')
        self.newline()
        self.write('#include <server/specification.h>')
        self.write('#include <server/lsp_exception.h>')
        self.write('#include <server/lsp_json_parser.h>')
        self.write('#include <server/lsp_language_server.h>')
        self.newline()
        self.write(f'namespace {self.namespace} {{')
        self.newline()
        with self.indent():
            self.generate_constructor()
            self.generate_join()
            self.generate_listen()
            self.generate_notify_sent()
            self.generate_synchronized_send()
            self.generate_request_id_to_string()
            self.generate_log_receive_trace()
            self.generate_log_receive_response_trace()
            self.generate_log_send_response_trace()
            self.generate_handle()
            self.generate_is_terminated()
            self.generate_initialize_params()
            self.generate_assert_initialized()
            self.generate_assert_running();
            self.generate_prepare()
            self.generate_dispatch_request()
            self.generate_dispatch_notification()
            self.generate_dispatch_response()
            self.generate_require_message_params()
            self.generate_incoming_request_handlers()
            self.generate_incoming_notification_handlers()
            self.generate_outgoing_request_handlers()
            self.generate_outgoing_notification_handlers()
        self.write(f'}} // namespace {self.namespace}')
