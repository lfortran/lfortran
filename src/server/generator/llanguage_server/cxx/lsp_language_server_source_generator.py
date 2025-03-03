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
                    'std::size_t numRequestThreads',
                    'std::size_t numWorkerThreads',
                    'lsl::Logger &logger',
                    'const std::string &configSection'
                ],
                sups=[
                    ('ls::LanguageServer', [
                        'incomingMessages',
                        'outgoingMessages',
                        'logger'
                    ]),
                ],
                inits=[
                    ('configSection', ['configSection']),
                    ('listener', ['[this]{ listen(); }']),
                    ('requestPool', [
                        '"request"',
                        'numRequestThreads',
                        'logger'
                    ]),
                    ('workerPool', [
                        '"worker"',
                        'numWorkerThreads',
                        'logger'
                    ])
                ]
        ):
            self.write('callbacksById.reserve(256);')
        self.newline()

    def generate_next_send_id(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::nextSendId',
                'std::size_t'
        ):
            self.gen_return('serialSendId++')
        self.newline()

    def generate_next_request_id(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::nextRequestId',
                'int'
        ):
            self.gen_return('serialRequestId++')
        self.newline()

    def generate_is_initialized(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::isInitialized',
                'bool',
                specs='const'
        ):
            self.gen_return('_initialized')
        self.newline()

    def generate_is_shutdown(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::isShutdown',
                'bool',
                specs='const'
        ):
            self.gen_return('_shutdown')
        self.newline()

    def generate_is_running(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::isRunning',
                'bool',
                specs='const'
        ):
            self.gen_return('!_shutdown')
        self.newline()

    def generate_join(self) -> None:
        with self.gen_fn('LspLanguageServer::join'):
            with self.gen_if('listener.joinable()'):
                self.write('listener.join();')
                self.write('logger.debug()')
                with self.indent():
                    self.write('<< "[LspLanguageServer] Incoming-message listener terminated."')
                    self.write('<< std::endl;')
            self.write('requestPool.join();')
            self.write('logger.debug()')
            with self.indent():
                self.write('<< "[LspLanguageServer] Request thread-pool terminated."')
                self.write('<< std::endl;')
            self.write('workerPool.join();')
            self.write('logger.debug()')
            with self.indent():
                self.write('<< "[LspLanguageServer] Worker thread-pool terminated."')
                self.write('<< std::endl;')
        self.newline()

    def generate_listen(self) -> None:
        with self.gen_fn('LspLanguageServer::listen'):
            with self.gen_try():
                with self.gen_while('!_exit'):
                    self.gen_assign(
                        'const std::string message',
                        'incomingMessages.dequeue()'
                    )
                    with self.gen_if('!_exit'):
                        self.write('std::size_t sendId = nextSendId();')
                        self.write('requestPool.execute([this, message, sendId](')
                        with self.indent():
                            self.write('const std::string &threadName,')
                            self.write('const std::size_t threadId')
                        self.write(') {')
                        with self.indent():
                            with self.gen_try():
                                self.write('handle(message, sendId);')
                            with self.gen_catch('std::exception &e'):
                                self.write('std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());')
                                self.write('logger.error()')
                                with self.indent():
                                    self.write('<< "[" << threadName << "_" << threadId << "] "')
                                    self.write('"Failed to handle message: " << message')
                                    self.write('<< std::endl;')
                                self.write('logger.error()')
                                with self.indent():
                                    self.write('<< "[" << threadName << "_" << threadId << "] "')
                                    self.write('"Caught unhandled exception: " << e.what()')
                                    self.write('<< std::endl;')
                        self.write('});')
            with self.gen_catch('std::exception &e'):
                with self.gen_if('e.what() != lst::DEQUEUE_FAILED_MESSAGE', end=''):
                    self.write('logger.error()')
                    with self.indent():
                        self.write('<< "[LspLanguageServer] "')
                        self.write('"Unhandled exception caught: " << e.what()')
                        self.write('<< std::endl;')
                with self.gen_else():
                    self.write('logger.trace()')
                    with self.indent():
                        self.write('<< "[LspLanguageServer] "')
                        self.write('"Interrupted while dequeuing messages: " << e.what()')
                        self.write('<< std::endl;')
        self.newline()

    def generate_notify_sent(self) -> None:
        with self.gen_fn('LspLanguageServer::notifySent'):
            self.write('++pendingSendId;')
            with self.block():
                self.write('std::unique_lock<std::mutex> sentLock(sentMutex);')
                self.write('sent.notify_all();')
        self.newline()

    def generate_synchronized_send(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::send',
                params=[
                    'const std::string &message',
                    'std::size_t sendId'
                ]
        ):
            self.write('// -------------------------------------------------------------------------')
            self.write('// NOTE: The LSP spec requires responses to be returned in roughly the same')
            self.write('// order of receipt of their corresponding requests. Some types of responses')
            self.write('// may be returned out-of-order, but in order to support those we will need')
            self.write('// to implement a sort of dependency graph. Without knowledge of their')
            self.write('// dependencies, we must respond to all requests in order of receipt.')
            self.write('// -------------------------------------------------------------------------')
            with self.block():
                self.write('std::unique_lock<std::mutex> sentLock(sentMutex);')
                self.write('sent.wait(sentLock, [this, sendId]{')
                with self.indent():
                    self.write('return (pendingSendId == sendId) || _exit;')
                self.write('});')
            with self.gen_if('(pendingSendId == sendId) && !_exit'):
                self.write('ls::LanguageServer::send(message);')
                self.write('notifySent();')
        self.newline()

    def generate_log_receive_trace(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::logReceiveTrace',
                'void',
                params=[
                    'const std::string &messageType',
                    'const std::string &traceId',
                    'const std::optional<MessageParams> &optionalParams'
                ]
        ):
            with self.gen_if('(trace >= TraceValues::Messages) && (traceId.length() > 0)'):
                self.write('LogTraceParams params;')
                self.gen_assign(
                    'params.message',
                    '"Received " + messageType + " \'" + traceId + "\'."'
                )
                with self.gen_if('(trace >= TraceValues::Verbose) && optionalParams.has_value()'):
                    self.gen_assign(
                        'const MessageParams &messageParams',
                        'optionalParams.value()'
                    )
                    with self.gen_switch('messageParams.type()'):
                        object_field = rename_field('object')
                        with self.gen_case('MessageParamsType', 'object'):
                            self.gen_assign(
                                'const LSPObject &object',
                                f'messageParams.{object_field}()'
                            )
                            self.gen_assign(
                                'params.verbose',
                                '"Params: " + serializer.pprint(object)'
                            )
                            self.write('break;')
                        array_field = rename_field('array')
                        with self.gen_case('MessageParamsType', 'array'):
                            self.gen_assign(
                                'const LSPArray &array',
                                f'messageParams.{array_field}()'
                            )
                            self.gen_assign(
                                'params.verbose',
                                '"Params: " + serializer.pprint(array)'
                            )
                            self.write('break;')
                        with self.gen_case('MessageParamsType', 'Uninitialized'):
                            self.gen_throw(
                                'LSP_EXCEPTION',
                                'ErrorCodes::InternalError',
                                '"MessageParams has not been initialized"'
                            )
                self.write('sendLogTrace(params);')
        self.newline()

    @gensym_context
    def generate_log_receive_response_trace(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::logReceiveResponseTrace',
                params=[
                    'const std::string &traceId',
                    'const LSPAny &document'
                ]
        ):
            with self.gen_if('trace >= TraceValues::Messages'):
                self.write('LogTraceParams params;')
                with self.gen_if('traceId.length() > 0', end=''):
                    self.write(f'params.message = "Received response \'" + traceId + "\'.";')
                with self.gen_else():
                    self.write(f'params.message = "Received response.";')
                with self.gen_if('trace >= TraceValues::Verbose'):
                    object_enum = rename_enum('object')
                    with self.gen_if(f'document.type() == LSPAnyType::{object_enum}', end=''):
                        object_field = rename_field('object')
                        object_name = self.gensym_ref('object', f'document.{object_field}()')
                        iter_name = self.gensym_init('iter', f'{object_name}.find("result")')
                        with self.gen_if(f'{iter_name} != {object_name}.end()', end=''):
                            self.write(f'const LSPAny &result = *{iter_name}->second;')
                            self.write(f'params.verbose = "Result: " + serializer.pprint(result);')
                        with self.gen_elif(f'({iter_name} = {object_name}.find("error")) != {object_name}.end()', end=''):
                            error_name = self.gensym_ref('error', f'*{iter_name}->second')
                            self.gen_assign(
                                'params.verbose',
                                f'"Error: " + serializer.pprint({error_name})'
                            )
                        with self.gen_else():
                            self.write(f'params.verbose = "No result returned.";')
                    with self.gen_else():
                        self.write('logger.error()')
                        with self.indent():
                            self.write('<< "Cannot log verbose message for response of type LSPAnyType::"')
                            self.write('<< LSPAnyTypeNames.at(document.type())')
                            self.write('<< std::endl;')
                self.write('sendLogTrace(params);')
        self.newline()

    @gensym_context
    def generate_log_send_response_trace(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::logSendResponseTrace',
                params=[
                    'const std::string &traceId',
                    'const std::chrono::time_point<std::chrono::high_resolution_clock> &start',
                    'const LSPAny &response'
                ]
        ):
            with self.gen_if('(trace >= TraceValues::Messages) && (traceId.length() > 0)'):
                end_name = self.gensym_init('end', 'std::chrono::high_resolution_clock::now()')
                duration_name = self.gensym_init(
                    'duration',
                    f'std::chrono::duration_cast<std::chrono::milliseconds>({end_name} - start)'
                )
                self.write('LogTraceParams params;')
                self.write(f'params.message =')
                with self.indent():
                    self.write('"Sending response \'" + traceId + "\'. Processing request took " +')
                    self.write(f'std::to_string({duration_name}.count()) + "ms";')
                with self.gen_if('trace >= TraceValues::Verbose'):
                    object_enum = rename_enum('object')
                    object_field = rename_field('object')
                    with self.gen_if(f'response.type() == LSPAnyType::{object_enum}', end=''):
                        object_name = self.gensym_ref(f'object', f'response.{object_field}()')
                        iter_name = self.gensym_init('iter', f'{object_name}.find("result")')
                        with self.gen_if(f'{iter_name} != {object_name}.end()', end=''):
                            result_name = self.gensym_ref('result', f'*{iter_name}->second')
                            self.gen_assign(
                                f'params.verbose',
                                f'"Result: " + serializer.pprint({result_name})'
                            )
                        with self.gen_elif(f'({iter_name} = {object_name}.find("error")) != {object_name}.end()', end=''):
                            error_name = self.gensym_ref('error', f'*{iter_name}->second')
                            self.gen_assign(
                                f'params.verbose',
                                f'"Error: " + serializer.pprint({error_name})'
                            )
                        with self.gen_else():
                            self.write(f'params.verbose = "No result returned.";')
                    with self.gen_else():
                        self.write('logger.error()')
                        with self.indent():
                            self.write('<< "Cannot log verbose message for response of type LSPAnyType::"')
                            self.write('<< LSPAnyTypeNames.at(response.type())')
                            self.write('<< std::endl;')
                self.write('sendLogTrace(params);')
        self.newline()

    def generate_handle(self) -> None:
        with self.gen_fn('LspLanguageServer::handle', params=[
            'const std::string &incoming',
            'std::size_t sendId'
        ]):
            object_enum = rename_enum('object')
            array_enum = rename_enum('array')
            object_field = rename_field('object')
            self.gen_assign('const auto start', 'std::chrono::high_resolution_clock::now()')
            self.write('ResponseMessage response;')
            self.write('std::string traceId;')
            with self.gen_try():
                self.write('// The language server protocol always uses “2.0” as the jsonrpc version.')
                self.write(f'response.jsonrpc = JSON_RPC_VERSION;')
                self.write(f'response.id = nullptr;')
                self.newline()
                self.write('LspJsonParser parser(incoming);')
                self.write('std::unique_ptr<LSPAny> document = parser.parse();')
                self.newline()
                with self.gen_if(f'document->type() != LSPAnyType::{object_enum}'):
                    self.write('// TODO: Add support for batched messages, i.e. multiple messages within')
                    self.write('// an array.')
                    with self.gen_if(f'document->type() == LSPAnyType::{array_enum}'):
                        self.gen_throw_invalid_params(
                            '"Batched requests are not supported (currently)."'
                        )
                    self.gen_throw_invalid_params(
                        '"Invalid request message: " + incoming'
                    )
                self.newline()
                self.write(f'const LSPObject &object = document->{object_field}();')
                self.write('LSPObject::const_iterator iter;')
                self.newline()
                with self.gen_if(f'(iter = object.find("id")) != object.end()'):
                    self.write(f'response.id = transformer.anyToResponseId(*iter->second);')
                self.newline()
                with self.gen_if(f'(iter = object.find("method")) != object.end()', end=''):
                    self.write('const std::string &method =')
                    with self.indent():
                        self.write('transformer.anyToString(*iter->second);')
                    with self.gen_if('isIncomingRequest(method)', end=''):
                        with self.gen_if('response.id.type() == ResponseIdType::Null'):
                            self.gen_throw(
                                'LSP_EXCEPTION',
                                'ErrorCodes::InvalidParams',
                                '"Missing request method=\\"" + method + "\\" attribute: id"'
                            )
                        self.write('RequestMessage request =')
                        with self.indent():
                            self.write('transformer.anyToRequestMessage(*document);')
                        with self.gen_if('trace >= TraceValues::Messages'):
                            self.write(f'traceId = request.method + " - (" + to_string(request.id) + ")";')
                            self.write(f'logReceiveTrace("request", traceId, request.params);')
                        self.write(f'response.jsonrpc = request.jsonrpc;')
                        self.write(f'dispatch(response, request);')
                    with self.gen_elif('isIncomingNotification(method)', end=''):
                        with self.gen_if('response.id.type() != ResponseIdType::Null'):
                            self.gen_throw_invalid_params(
                                '"Notification method=\\"" + method + "\\" must not contain the attribute: id"'
                            )
                        self.write('NotificationMessage notification =')
                        with self.indent():
                            self.write('transformer.anyToNotificationMessage(*document);')
                        with self.gen_if('trace >= TraceValues::Messages'):
                            self.write(f'traceId = notification.method;')
                            self.write(f'logReceiveTrace("notification", traceId, notification.params);')
                        self.write(f'response.jsonrpc = notification.jsonrpc;')
                        self.write(f'dispatch(response, notification);')
                    with self.gen_else():
                        self.gen_throw(
                            'LSP_EXCEPTION',
                            'ErrorCodes::InvalidRequest',
                            '"Unsupported method: \\"" + method + "\\""'
                        )
                with self.gen_elif(f'(iter = object.find("result")) != object.end()', end=''):
                    self.write('notifySent();')
                    self.write(f'response.result = transformer.copy(*iter->second);')
                    self.write(f'dispatch(response, traceId, *document);')
                    self.write('return;')
                with self.gen_elif(f'(iter = object.find("error")) != object.end()', end=''):
                    self.write('notifySent();')
                    self.write(f'response.error = transformer.anyToResponseError(*iter->second);')
                    self.write(f'dispatch(response, traceId, *document);')
                    self.write('return;')
                with self.gen_else():
                    self.gen_throw(
                        'LSP_EXCEPTION',
                        'ErrorCodes::InvalidRequest',
                        '"Missing required attribute: method"'
                    )
            with self.gen_catch('const LspException &e', end=''):
                self.write('logger.error()')
                with self.indent():
                    self.write('<< "[" << e.file() << ":" << e.line() << "] "')
                    self.write('<< e.what()')
                    self.write('<< std::endl;')
                self.write('ResponseError error;')
                with self.gen_switch('e.code().type'):
                    with self.gen_case('ErrorCodeType', 'errorCodes'):
                        error_codes_field = rename_field('ErrorCodes')
                        self.write(f'ErrorCodes errorCode = e.code().{error_codes_field};')
                        self.write('error.code = static_cast<int>(errorCode);')
                        self.write('break;')
                    with self.gen_case('ErrorCodeType', 'lspErrorCodes'):
                        lsp_error_codes_field = rename_field('LSPErrorCodes')
                        self.write(f'LSPErrorCodes errorCode = e.code().{lsp_error_codes_field};')
                        self.write('error.code = static_cast<int>(errorCode);')
                        self.write('break;')
                self.write('error.message = e.what();')
                self.write(f'response.error = std::move(error);')
            with self.gen_catch('const std::exception &e'):
                self.write('logger.error()')
                with self.indent():
                    self.write('<< "Caught unhandled exception: "')
                    self.write('<< e.what() << std::endl;')
                self.write('ResponseError error;')
                self.write('error.code = static_cast<int>(ErrorCodes::InternalError);')
                self.write('error.message =')
                with self.indent():
                    self.write('("An unexpected exception occurred. If it continues, "')
                    self.write(' "please file a ticket.");')
                self.write(f'response.error = std::move(error);')
            self.gen_assign(
                'LSPAny any',
                'transformer.responseMessageToAny(response)'
            )
            self.write(f'logSendResponseTrace(traceId, start, any);')
            self.write('const std::string outgoing = serializer.serialize(any);')
            self.write('send(outgoing, sendId);')
        self.newline()

    def generate_is_terminated(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::isTerminated',
                'bool',
                specs='const'
        ):
            self.write('return _exit;')
        self.newline()

    def generate_initialize_params(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::initializeParams',
                'const InitializeParams &',
                specs='const'
        ):
            with self.gen_if('_initializeParams'):
                self.write('return *_initializeParams;')
            self.write('throw std::logic_error("Server has not been initialized.");')
        self.newline()

    def generate_assert_initialized(self) -> None:
        with self.gen_fn('LspLanguageServer::assertInitialized'):
            with self.gen_if('!_initialized'):
                self.write('throw LSP_EXCEPTION(')
                with self.indent():
                    self.write('ErrorCodes::ServerNotInitialized,')
                    self.write('"Method \\"initialize\\" must be called first."')
                self.write(');')
        self.newline()

    def generate_assert_running(self) -> None:
        with self.gen_fn('LspLanguageServer::assertRunning'):
            with self.gen_if('_shutdown'):
                self.write('throw LSP_EXCEPTION(')
                with self.indent():
                    self.write('LSPErrorCodes::RequestFailed,')
                    self.write('"Server has shutdown and cannot accept new requests."')
                self.write(');')
        self.newline()

    def generate_request_id_to_string(self) -> None:
        integer_field = rename_field('integer')
        string_field = rename_field('string')
        with self.gen_fn(
                'LspLanguageServer::to_string',
                'std::string',
                params=[
                    'const RequestId &requestId',
                ]
        ):
            with self.gen_switch('requestId.type()'):
                with self.gen_case('RequestIdType', 'integer'):
                    self.write(f'return std::to_string(requestId.{integer_field}());')
                with self.gen_case('RequestIdType', 'string'):
                    self.write(f'return requestId.{string_field}();')
                with self.gen_default():
                    self.gen_throw_invalid_params(
                        '("Cannot log request trace with Id of type RequestIdType::" +',
                        ' RequestIdTypeNames.at(requestId.type()))'
                    )
        self.newline()

    def generate_dispatch_request(self) -> None:
        with self.gen_fn(
                'LspLanguageServer::dispatch',
                params=[
                    'ResponseMessage &response',
                    'RequestMessage &request'
                ]
        ):
            self.write('IncomingRequest method;')
            with self.gen_try():
                self.write('method = incomingRequestByValue(request.method);')
            with self.gen_catch('std::invalid_argument &/*e*/'):
                self.write('goto invalidMethod;')
            self.write('assertRunning();')
            with self.gen_if('method != IncomingRequest::Initialize', end=''):
                self.write('assertInitialized();')
            with self.gen_else():
                self.write('bool expected = false;    // a reference is required')
                with self.gen_if('!_initialized.compare_exchange_strong(expected, true)'):
                    self.gen_throw(
                        'LSP_EXCEPTION',
                        'ErrorCodes::InvalidRequest',
                        '"Server may be initialized only once."'
                    )
            with self.gen_switch('method'):
                for request_spec in self.schema["requests"]:
                    if request_spec["messageDirection"] == "clientToServer":
                        request_method = request_spec["method"]
                        request_name = method_to_camel_case(request_method)
                        result_name = f'{request_name}Result'
                        method_enum = method_to_camel_case(request_method)
                        with self.gen_case('IncomingRequest', method_enum):
                            params_spec = request_spec.get("params", None)
                            if params_spec is not None:
                                is_initialize = request_method == "initialize"
                                num_levels = int(is_initialize)
                                if is_initialize:
                                    self.write('try {')
                                with self.indent(num_levels):
                                    self.write('MessageParams &messageParams = requireMessageParams(request);')
                                    self.write(f'{params_spec["name"]} requestParams =')
                                    with self.indent(): self.write(f'transformer.as{request_name}Params(messageParams);')
                                    self.write(f'{result_name} result =')
                                    with self.indent(): self.write(f'{receive_fn(request_method)}(requestParams);')
                                    self.write(f'response.result =')
                                    with self.indent():
                                        self.write(f'transformer.{lower_first(result_name)}ToAny(result);')
                                    if is_initialize:
                                        self.gen_assign(
                                            '_initializeParams',
                                            'std::make_unique<InitializeParams>(std::move(requestParams))'
                                        )
                                if is_initialize:
                                    self.write('} catch (LspException &e) {')
                                    with self.indent():
                                        self.write('bool expected = true;')
                                        self.write('if (!_initialized.compare_exchange_strong(expected, false)) {')
                                        with self.indent():
                                            self.write('throw LSP_EXCEPTION(')
                                            with self.indent():
                                                self.write('ErrorCodes::InvalidRequest,')
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
                            self.gen_break()
                with self.gen_default():
                    self.write('invalidMethod:')
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
                    'NotificationMessage &notification'
                ]
        ):
            self.write('IncomingNotification method;')
            with self.gen_try():
                self.write('method = incomingNotificationByValue(notification.method);')
            with self.gen_catch('std::invalid_argument &/*e*/'):
                self.write('goto invalidMethod;')
            with self.gen_if('method != IncomingNotification::Exit'):
                with self.gen_if('!_initialized'):
                    self.write('// Notifications should be dropped, except for the exit notification.')
                    self.write('// This will allow the exit of a server without an initialize request.')
                    self.write('return;')
                self.write('assertRunning();')
            with self.gen_switch('method'):
                for notification_spec in self.schema["notifications"]:
                    if notification_spec["messageDirection"] == "clientToServer":
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
                                with self.indent(): self.write(f'transformer.as{notification_name}Params(messageParams);')
                                self.write(f'{receive_fn(notification_method)}(notificationParams);')
                            else:
                                self.write(f'{receive_fn(notification_method)}();')
                            self.gen_break()
                with self.gen_default():
                    self.write('invalidMethod:')
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
                    'std::string &traceId',
                    'const LSPAny &document'
                ]
        ):
            with self.gen_switch('response.id.type()'):
                with self.gen_case('ResponseIdType', 'integer'):
                    integer_field = rename_field("integer")
                    self.write(f'int responseId = response.id.{integer_field}();')
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
                    with self.gen_try():
                        self.write('request = outgoingRequestByValue(method);')
                    with self.gen_catch('std::invalid_argument &/*e*/'):
                        self.write('goto invalidMethod;')
                    self.newline()
                    with self.gen_switch('request'):
                        for request_spec in self.schema["requests"]:
                            if request_spec["messageDirection"] == "serverToClient":
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
                                        self.write(f'{receive_fn(request_method)}(params);')
                                    else:
                                        self.write(f'{receive_fn(request_method)}()')
                                    self.gen_break()
                        with self.gen_default():
                            self.write('invalidMethod:')
                            self.write(f'logger.error()')
                            with self.indent():
                                self.write('<< "Unsupported request method: \\""')
                                self.write('<< method << "\\"";')
                    self.gen_break()
                with self.gen_case('ResponseIdType', 'null'):
                    self.write('logReceiveResponseTrace(traceId, document);')
                    self.write('break;')
                with self.gen_default():
                    self.write('logger.error()')
                    with self.indent():
                        self.write('<< "Cannot dispatch response with id of type ResponseIdType::"')
                        self.write('<< ResponseIdTypeNames.at(response.id.type())')
                        self.write('<< std::endl;')
                    self.write('return;')
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

    def generate_require_message_params(self) -> None:
        self.generate_require_request_params()
        self.generate_require_notification_params()

    def generate_receive_request(self, request_spec: LspSpec) -> None:
        request_method = request_spec["method"]
        request_name = method_to_camel_case(request_method)
        result_name = f'{request_name}Result'
        params = []
        params_spec = request_spec.get("params", None)
        if params_spec is not None:
            if request_method == "initialize":
                params.append(f'{params_spec["name"]} &params')
            else:
                params.append(f'{params_spec["name"]} &/*params*/')
        receive_request = receive_fn(request_method)
        self.write(f'// request: "{request_method}"')
        with self.gen_fn(
                f'LspLanguageServer::{receive_request}',
                result_name,
                params=params
        ):
            match request_method:
                case "initialize":
                    with self.gen_if('params.trace.has_value()'):
                        self.gen_assign('trace', 'params.trace.value()')
                    self.newline()
                    self.write('InitializeResult result;')
                    self.newline()
                    self.gen_return('result')
                case "shutdown":
                    self.write('bool expected = false;')
                    with self.gen_if('_shutdown.compare_exchange_strong(expected, true)'):
                        self.write('logger.info() << "Shutting down server." << std::endl;')
                    self.gen_return('nullptr')
                case _:
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
            match notification_method:
                case "$/setTrace":
                    params.append(f'{params_spec["name"]} &params')
                case _:
                    params.append(f'{params_spec["name"]} &/*params*/')
        self.write(f'// notification: "{notification_method}"')
        with self.gen_fn(f'LspLanguageServer::{receive_notification}', params=params):
            match notification_method:
                case "exit":
                    self.write('bool expected = false;')
                    with self.gen_if('_exit.compare_exchange_strong(expected, true)'):
                        self.write('logger.info() << "Exiting server." << std::endl;')
                        self.write('expected = false;')
                        with self.gen_if('_shutdown.compare_exchange_strong(expected, true)'):
                            self.write('logger.error()')
                            with self.indent():
                                self.write('<< "Server exited before being notified to shutdown!"')
                                self.write('<< std::endl;')
                        self.write('incomingMessages.stopNow();')
                        self.write('requestPool.stopNow();')
                        self.write('workerPool.stopNow();')
                        self.write('// NOTE: Notify the message stream that the server is terminating.')
                        self.write('// NOTE: This works because the null character is not included in')
                        self.write('// the JSON spec.')
                        self.write('std::cin.putback(\'\\0\');')
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
                        self.gen_throw(
                            'LSP_EXCEPTION',
                            'ErrorCodes::MethodNotFound',
                            f'"No handler exists for method: \\"{notification_method}\\""'
                        )
        self.newline()

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
            self.write('RequestMessage request;')
            self.write('request.jsonrpc = JSON_RPC_VERSION;')
            self.write('int requestId = nextRequestId();')
            self.write('request.id = requestId;')
            with self.block():
                self.write('std::unique_lock<std::mutex> callbackLock(callbackMutex);')
                self.write(f'callbacksById.emplace(requestId, "{request_method}");')
            self.write(f'request.method = "{request_method}";')
            if params_spec is not None:
                self.write('request.params = transformer.asMessageParams(params);')
            self.gen_assign(
                'LSPAny any',
                'transformer.requestMessageToAny(request)'
            )
            self.gen_assign(
                'const std::string message',
                'serializer.serialize(any)'
            )
            self.write('ls::LanguageServer::send(message);')
            self.write('return requestId;')
        self.newline()

    def generate_receive_response(self, request_spec: LspSpec) -> None:
        request_method = request_spec["method"]
        receive_request = receive_fn(request_method)
        request_name = method_to_camel_case(request_method)
        params = []
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

    def generate_send_notification(self, notification_spec: LspSpec) -> None:
        notification_method = notification_spec["method"]
        send_notification = send_fn(notification_method)
        params = []
        params_spec = notification_spec.get("params", None)
        if params_spec is not None:
            params.append(f'{params_spec["name"]} &params')
        self.write(f'// notification: "{notification_method}"')
        with self.gen_fn(f'LspLanguageServer::{send_notification}', params=params):
            self.write('NotificationMessage notification;')
            self.write('notification.jsonrpc = JSON_RPC_VERSION;')
            self.write(f'notification.method = "{notification_method}";')
            if params_spec is not None:
                self.gen_assign(
                    'notification.params',
                    'transformer.asMessageParams(params)'
                )
            self.gen_assign(
                'LSPAny any',
                'transformer.notificationMessageToAny(notification)'
            )
            self.gen_assign(
                'const std::string message',
                'serializer.serialize(any)'
            )
            self.write('ls::LanguageServer::send(message);')
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

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        self.generate_disclaimer()
        self.gen_include('cctype')
        self.gen_include('chrono')
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
            self.generate_next_send_id()
            self.generate_next_request_id()
            self.generate_is_initialized();
            self.generate_is_shutdown();
            self.generate_is_running()
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
            super().generate_code()
