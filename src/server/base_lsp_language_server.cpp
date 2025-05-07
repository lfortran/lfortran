#include <cctype>
#include <chrono>
#include <exception>
#include <memory>
#include <mutex>
#include <shared_mutex>
#include <stdexcept>
#include <string>
#include <tuple>

#ifdef _WIN32
#include <windows.h>
#include <TlHelp32.h>
#else
#include <signal.h>
#include <errno.h>
#endif

#include <server/base_lsp_language_server.h>
#include <server/lsp_exception.h>
#include <server/lsp_issue_reporter.h>
#include <server/lsp_json_parser.h>
#include <server/lsp_language_server.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

namespace LCompilers::LanguageServerProtocol {
    using namespace std::chrono_literals;

    namespace lsr = LCompilers::LanguageServerProtocol::Reporter;

    RunTracer::RunTracer(
        BaseLspLanguageServer *server,
        const std::string &taskType
    ) : server(server)
      , taskType(taskType)
    {
        // empty
    }

    RunTracer::~RunTracer() {
        stop();
    }

    auto RunTracer::stop() -> void {
        if (!stopped) {
            server->stopRunning(taskType);
            stopped = true;
        }
    }

    BaseLspLanguageServer::BaseLspLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        lsl::Logger &logger,
        const std::string &configSection,
        const std::string &extensionId,
        const std::string &compilerVersion,
        int parentProcessId,
        std::shared_ptr<lsc::LspConfigTransformer> lspConfigTransformer,
        std::shared_ptr<lsc::LspConfig> workspaceConfig,
        std::atomic_bool &start,
        std::condition_variable &startChanged,
        std::mutex &startMutex
    ) : LspLanguageServer(
        incomingMessages,
        outgoingMessages,
        logger
      )
      , logger(logger.having("BaseLspLanguageServer"))
      , configSection(configSection)
      , extensionId(extensionId)
      , compilerVersion(compilerVersion)
      , parentProcessId(parentProcessId)
      , lspConfigTransformer(std::move(lspConfigTransformer))
      , pu(logger)
      , listener([this, &logger, &start, &startChanged, &startMutex]{
          logger.threadName("BaseLspLanguageServer_listener");
          if (!start) {
              std::unique_lock<std::mutex> startLock(startMutex);
              startChanged.wait(startLock, [&start]{
                  return start.load();
              });
          }
          listen();
      })
    {
        documentsByUri.reserve(256);
        configsByUri.reserve(256);
        lspConfigsByUri.reserve(256);
        updateWorkspaceConfig(std::move(workspaceConfig));
    }

    auto BaseLspLanguageServer::nextSendId() -> std::size_t
    {
        return serialSendId++;
    }

    auto BaseLspLanguageServer::isInitialized() const -> bool
    {
        return _initialized;
    }

    auto BaseLspLanguageServer::isShutdown() const -> bool
    {
        return _shutdown;
    }

    auto BaseLspLanguageServer::isRunning() const -> bool
    {
        return !_shutdown;
    }

    auto BaseLspLanguageServer::isTerminated() const -> bool
    {
        return _exit;
    }

    auto BaseLspLanguageServer::initializeParams() const
    -> const InitializeParams &
    {
        if (_initializeParams) {
            return *_initializeParams;
        }
        throw std::logic_error("Server has not been initialized.");
    }

    auto BaseLspLanguageServer::assertInitialized() -> void
    {
        if (!_initialized) {
            throw LSP_EXCEPTION(
                ErrorCodes::ServerNotInitialized,
                "Method \"initialize\" must be called first."
            );
        }
    }

    auto BaseLspLanguageServer::assertRunning() -> void
    {
        if (_shutdown) {
            throw LSP_EXCEPTION(
                LSPErrorCodes::RequestFailed,
                "Server has shutdown and cannot accept new requests."
            );
        }
    }

    auto BaseLspLanguageServer::prepare(
        std::string &buffer,
        const std::string &response
    ) const -> void
    {
        buffer.append("Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n");
        buffer.append("Content-Length: ");
        buffer.append(std::to_string(response.length()));
        buffer.append("\r\n");
        buffer.append("\r\n");
        buffer.append(response);
    }

    auto BaseLspLanguageServer::isProcessRunning(int pid) -> bool {
#ifdef _WIN32
        HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, pid);
        if (hProcess == NULL) {
            return false;
        }
        DWORD exitCode;
        if (GetExitCodeProcess(hProcess, &exitCode)) {
            CloseHandle(hProcess);
            return exitCode == STILL_ACTIVE;
        }
        CloseHandle(hProcess);
        return false;
#else
        if (kill(pid, 0) == 0) {
            return true;
        } else {
            return errno != ESRCH;
        }
#endif
    }

    auto BaseLspLanguageServer::checkParentProcessId() -> void {
        if ((parentProcessId >= 0) && !isProcessRunning(parentProcessId)) {
            logger.error()
                << "Parent process terminated before terminating server."
                << std::endl;
            shutdown();
            exit();
        }
    }

    auto BaseLspLanguageServer::send(const RequestMessage &request) -> void {
        LspLanguageServer::send(request);
    }

    auto BaseLspLanguageServer::cancelRequest(int requestId) -> void {
        CancelParams params;
        params.id = requestId;
        sendCancelRequest(params);
    }

    auto BaseLspLanguageServer::collectMessageQueueTelemetry(
        const std::string &key,
        ls::MessageQueue &queue
    ) -> LSPAny {
        LSPObject data;
        data.emplace("name", std::make_unique<LSPAny>(toAny(queue.name())));
        data.emplace("size", std::make_unique<LSPAny>(toAny(queue.size())));
        data.emplace("seen", std::make_unique<LSPAny>(toAny(queue.seen())));
        LSPObject event;
        event.emplace("key", std::make_unique<LSPAny>(toAny(key)));
        event.emplace("value", std::make_unique<LSPAny>(toAny(data)));
        LSPAny any;
        any = std::move(event);
        return any;
    }

    auto BaseLspLanguageServer::collectTelemetry() -> LSPAny {
        LSPArray events;
        {
            LSPObject event;
            event.emplace("key", std::make_unique<LSPAny>(toAny("timestamp")));
            event.emplace(
                "value",
                std::make_unique<LSPAny>(toAny(std::chrono::system_clock::now()))
            );
            std::unique_ptr<LSPAny> &any =
                events.emplace_back(std::make_unique<LSPAny>());
            (*any) = std::move(event);
        }
        if (pu.isValid()) {
            LSPObject data;
            data.emplace(
                "memoryUtilization",
                std::make_unique<LSPAny>(toAny(pu.memoryUtilization()))
            );
            data.emplace(
                "cpuUsage",
                std::make_unique<LSPAny>(toAny(pu.cpuUsage()))
            );
            LSPObject event;
            event.emplace("key", std::make_unique<LSPAny>(toAny("processUsage")));
            event.emplace("value", std::make_unique<LSPAny>(toAny(data)));
            std::unique_ptr<LSPAny> &any =
                events.emplace_back(std::make_unique<LSPAny>());
            (*any) = std::move(event);
        }
        events.emplace_back(
            std::make_unique<LSPAny>(
                collectMessageQueueTelemetry(
                    "incomingMessages",
                    incomingMessages
                )
            )
        );
        events.emplace_back(
            std::make_unique<LSPAny>(
                collectMessageQueueTelemetry(
                    "outgoingMessages",
                    outgoingMessages
                )
            )
        );
        {
            LSPObject event;
            event.emplace(
                "key",
                std::make_unique<LSPAny>(
                    transformer.stringToAny("runningHistogram")
                )
            );
            auto readLock = LSP_READ_LOCK(runningMutex, "running");
            event.emplace(
                "value",
                std::make_unique<LSPAny>(
                    toAny(runningHistogram)
                )
            );
            readLock.unlock();
            std::unique_ptr<LSPAny> &any =
                events.emplace_back(std::make_unique<LSPAny>());
            (*any) = std::move(event);
        }
#ifdef DEBUG
        {
            LSPObject waitingEvent;
            waitingEvent.emplace(
                "key",
                std::make_unique<LSPAny>(
                    transformer.stringToAny("waiting")
                )
            );
            LSPObject ownerEvent;
            ownerEvent.emplace(
                "key",
                std::make_unique<LSPAny>(
                    transformer.stringToAny("owners")
                )
            );
            auto ownerLock = LSP_READ_LOCK(ownerMutex, "owner");
            waitingEvent.emplace(
                "value",
                std::make_unique<LSPAny>(
                    toAny(waitingById)
                )
            );
            ownerEvent.emplace(
                "value",
                std::make_unique<LSPAny>(
                    toAny(ownersById)
                )
            );
            ownerLock.unlock();
            std::unique_ptr<LSPAny> &waitingAny =
                events.emplace_back(std::make_unique<LSPAny>());
            (*waitingAny) = std::move(waitingEvent);
            std::unique_ptr<LSPAny> &ownerAny =
                events.emplace_back(std::make_unique<LSPAny>());
            (*ownerAny) = std::move(ownerEvent);
        }
#endif // DEBUG
        LSPAny any;
        any = std::move(events);
        return any;
    }

    auto BaseLspLanguageServer::sendTelemetry() -> void {
        LSPAny data = collectTelemetry();
        sendTelemetry_event(data);
    }

#ifdef DEBUG
    auto BaseLspLanguageServer::toAny(const ls::OwnerRecord &record) const -> LSPAny {
        LSPObject object;
        object.emplace("thread", std::make_unique<LSPAny>(toAny(record.thread)));
        object.emplace("file", std::make_unique<LSPAny>(toAny(record.file)));
        object.emplace("line", std::make_unique<LSPAny>(toAny(record.line)));
        object.emplace("timestamp", std::make_unique<LSPAny>(toAny(record.timestamp)));
        LSPAny any;
        any = std::move(object);
        return any;
    }
#endif // DEBUG

    auto BaseLspLanguageServer::toAny(const char *value) const -> LSPAny {
        return transformer.stringToAny(value);
    }

    auto BaseLspLanguageServer::toAny(const int value) const -> LSPAny {
        return transformer.integerToAny(value);
    }

    auto BaseLspLanguageServer::toAny(const time_point_t &timePoint) const -> LSPAny {
        LSPAny any;
        any = lsl::formatTimePoint(timePoint);
        return any;
    }

    auto BaseLspLanguageServer::toAny(const std::string &value) const -> LSPAny {
        return transformer.stringToAny(value);
    }

    auto BaseLspLanguageServer::toAny(const LSPAny &any) const -> LSPAny {
        return any;
    }

    auto BaseLspLanguageServer::toAny(LSPObject &object) const -> LSPAny {
        LSPAny any;
        any = std::move(object);
        return any;
    }

    auto BaseLspLanguageServer::toAny(LSPArray &array) const -> LSPAny {
        LSPAny any;
        any = std::move(array);
        return any;
    }

    auto BaseLspLanguageServer::toAny(std::size_t value) const -> LSPAny {
        return transformer.uintegerToAny(static_cast<uinteger_t>(value));
    }

    auto BaseLspLanguageServer::toAny(double value) const -> LSPAny {
        return transformer.decimalToAny(value);
    }

    auto BaseLspLanguageServer::toAny(bool value) const -> LSPAny {
        return transformer.booleanToAny(value);
    }

    auto BaseLspLanguageServer::startRunning(
        const std::string &taskType
    ) -> RunTracer {
        auto startTime = std::chrono::system_clock::now();
        auto writeLock = LSP_WRITE_LOCK(runningMutex, "running");
        auto iter = runningHistogram.find(taskType);
        std::map<std::string, time_point_t> &startTimesByThread =
            ((iter == runningHistogram.end())
             ? runningHistogram.emplace_hint(
                 iter,
                 std::piecewise_construct,
                 std::forward_as_tuple(taskType),
                 std::forward_as_tuple()
               )->second
             : iter->second);
        startTimesByThread.insert_or_assign(logger.threadName(), startTime);
        writeLock.unlock();
        return RunTracer(this, taskType);
    }

    auto BaseLspLanguageServer::stopRunning(const std::string &taskType) -> void {
        auto readLock = LSP_READ_LOCK(runningMutex, "running");
        auto histIter = runningHistogram.find(taskType);
        if (histIter != runningHistogram.end()) {
            readLock.unlock();
            auto writeLock = LSP_WRITE_LOCK(runningMutex, "running");
            histIter = runningHistogram.find(taskType);
            if (histIter != runningHistogram.end()) {
                std::map<std::string, time_point_t> &startTimesByThread =
                    histIter->second;
                auto threadIter = startTimesByThread.find(logger.threadName());
                if (threadIter != startTimesByThread.end()) {
                    startTimesByThread.erase(threadIter);
                    if (startTimesByThread.empty()) {
                        runningHistogram.erase(histIter);
                    }
                }
            }
        }
    }

    auto BaseLspLanguageServer::handle(
        const std::string &incoming,
        std::size_t sendId,
        std::shared_ptr<std::atomic_bool> taskIsRunning
    ) -> void {
        const auto start = std::chrono::high_resolution_clock::now();
        ResponseMessage response;
        std::string traceId;
        try {
            // The language server protocol always uses “2.0” as the jsonrpc version.
            response.jsonrpc = JSON_RPC_VERSION;
            response.id = nullptr;

            LspJsonParser parser(incoming);
            std::unique_ptr<LSPAny> document = parser.parse();

            if (document->type() != LSPAnyType::Object) {
                // TODO: Add support for batched messages, i.e. multiple messages within
                // an array.
                if (document->type() == LSPAnyType::Array) {
                    throw LSP_EXCEPTION(
                        ErrorCodes::InvalidParams,
                        "Batched requests are not supported (currently)."
                    );
                }
                throw LSP_EXCEPTION(
                    ErrorCodes::InvalidParams,
                    "Invalid request message: " + incoming
                );
            }

            const LSPObject &object = document->object();
            LSPObject::const_iterator iter;

            if ((iter = object.find("id")) != object.end()) {
                response.id = transformer.anyToResponseId(*iter->second);
            }

            if ((iter = object.find("method")) != object.end()) {
                const std::string &method =
                    transformer.anyToString(*iter->second);
                if (isIncomingRequest(method)) {
                    handleRequest(*document, response, method, traceId, taskIsRunning);
                } else if (isIncomingNotification(method)) {
                    handleNotification(*document, response, method, traceId, taskIsRunning);
                } else {
                    throw LSP_EXCEPTION(
                        ErrorCodes::InvalidRequest,
                        "Unsupported method: \"" + method + "\""
                    );
                }
            } else {
                handleResponse(*document, response, traceId, taskIsRunning);
                return;
            }
        } catch (const LspException &e) {
            ResponseError error;
            error.message = "[";
            error.message.append(e.file());
            error.message.append(":");
            error.message.append(std::to_string(e.line()));
            error.message.append("] ");
            error.message.append(e.what());
            switch (e.code().type) {
            case ErrorCodeType::ErrorCodes: {
                ErrorCodes errorCode = e.code().errorCodes;
                error.code = static_cast<int>(errorCode);
                break;
            }
            case ErrorCodeType::LspErrorCodes: {
                LSPErrorCodes errorCode = e.code().lspErrorCodes;
                error.code = static_cast<int>(errorCode);
                break;
            }
            }
            logger.error() << error.message << std::endl;
            response.error = std::move(error);
        } catch (...) {
            ResponseError error;
            error.code = static_cast<int>(ErrorCodes::InternalError);
            error.message = formatException(
                "Caught unhandled exception",
                std::current_exception()
            );
            logger.error() << error.message << std::endl;
            response.error = std::move(error);
        }
        if (*taskIsRunning) {
            LSPAny any = transformer.responseMessageToAny(response);
            logSendResponseTrace(traceId, start, any);
            const std::string outgoing = serializer.serialize(any);
            logger.debug()
                << "Input/Output Messages:" << std::endl
                << "Incoming: " << incoming << std::endl
                << "Outgoing: " << outgoing << std::endl;
            send(outgoing, sendId);
        } else {
            logger.debug()
                << "Message canceled before sending response."
                << std::endl;
        }
        if (response.error.has_value()) {
            const ResponseError &error = response.error.value();
            auto workspaceLock = LSP_READ_LOCK(workspaceMutex, "workspace");
            if ((error.code == static_cast<int>(ErrorCodes::InternalError)) &&
                workspaceConfig->openIssueReporterOnError) {
                try {
                    lsr::InternalErrorReporter reporter(
                        serializer,
                        transformer,
                        extensionId,
                        compilerVersion,
                        initializeParams(),
                        *workspaceConfig,
                        *lspConfigTransformer,
                        incoming,
                        traceId,
                        error
                    );
                    std::string issueTitle = reporter.title();
                    std::string issueBody = reporter.body();
                    sendOpenIssue(issueTitle, issueBody);
                } catch (...) {
                    logger.error()
                        << formatException(
                            "Failed to open issue",
                            std::current_exception()
                        )
                        << std::endl;
                }
            }
        }
    }

    auto BaseLspLanguageServer::handleRequest(
        const LSPAny &document,
        ResponseMessage &response,
        const std::string &method,
        std::string &traceId,
        std::shared_ptr<std::atomic_bool> taskIsRunning
    ) -> void {
        if (response.id.type() == ResponseIdType::Null) {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                "Missing request method=\"" + method + "\" attribute: id"
            );
        }
        RequestMessage request =
            transformer.anyToRequestMessage(document);
        std::string requestId = to_string(request.id);
        {
            auto lock = LSP_MUTEX_LOCK(activeMutex, "active-requests");
            auto iter = activeRequests.find(requestId);
            if (iter == activeRequests.end()) {
                activeRequests.emplace_hint(iter, requestId, taskIsRunning);
            } else {
                if (!*iter->second) {
                    logger.debug()
                        << "Request with method=" << request.method << ", id=" << requestId << " canceled before dispatch."
                        << std::endl;
                    return;
                }
                iter->second = taskIsRunning;
            }
        }
        traceId = request.method + " - (" + requestId + ")";
        if (trace >= TraceValues::Messages) {
            logReceiveTrace("request", traceId, request.params);
        }
        response.jsonrpc = request.jsonrpc;
        if (!*taskIsRunning) {
            logger.debug()
                << "Request with method=" << request.method << ", id=" << requestId << " canceled before dispatch."
                << std::endl;
            return;
        }
        dispatch(response, request);
        {
            auto lock = LSP_MUTEX_LOCK(activeMutex, "active-requests");
            auto iter = activeRequests.find(requestId);
            if (iter != activeRequests.end()) {
                activeRequests.erase(iter);
            }
        }
    }

    auto BaseLspLanguageServer::handleNotification(
        const LSPAny &document,
        ResponseMessage &response,
        const std::string &method,
        std::string &traceId,
        std::shared_ptr<std::atomic_bool> taskIsRunning
    ) -> void {
        if (response.id.type() != ResponseIdType::Null) {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                "Notification method=\"" + method + "\" must not contain the attribute: id"
            );
        }
        NotificationMessage notification =
            transformer.anyToNotificationMessage(document);
        traceId = notification.method;
        if (trace >= TraceValues::Messages) {
            logReceiveTrace("notification", traceId, notification.params);
        }
        response.jsonrpc = notification.jsonrpc;
        if (!*taskIsRunning) {
            logger.debug()
                << "Notification with method=" << notification.method << " canceled before dispatch."
                << std::endl;
            return;
        }
        dispatch(response, notification);
    }

    auto BaseLspLanguageServer::handleResponse(
        const LSPAny &document,
        ResponseMessage &response,
        std::string &traceId,
        std::shared_ptr<std::atomic_bool> taskIsRunning
    ) -> void {
        const LSPObject &object = document.object();
        LSPObject::const_iterator iter;

        if ((iter = object.find("result")) != object.end()) {
            if (*taskIsRunning) {
                response.result = transformer.copy(*iter->second);
                dispatch(response, traceId, document);
            } else {
                logger.debug()
                    << "Response with result canceled before dispatch."
                    << std::endl;
            }
            return;
        } else if ((iter = object.find("error")) != object.end()) {
            if (*taskIsRunning) {
                response.error = transformer.anyToResponseError(*iter->second);
                dispatch(response, traceId, document);
            } else {
                logger.debug()
                    << "Response with error canceled before dispatch."
                    << std::endl;
            }
            return;
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidRequest,
                "Neither of the mutually-exclusive, required ResponseMessage attributes exist: result or error."
            );
        }
    }

    auto BaseLspLanguageServer::sendOpenIssue(
        const std::string &issueTitle,
        const std::string &issueBody
    ) -> void {
        LSPObject object;
        object.emplace(
            "issueType",
            std::make_unique<LSPAny>(
                transformer.integerToAny(
                    static_cast<integer_t>(lsr::IssueType::Bug)
                )
            )
        );
        object.emplace(
            "issueSource",
            std::make_unique<LSPAny>(
                transformer.integerToAny(
                    static_cast<integer_t>(lsr::IssueSource::Extension)
                )
            )
        );
        object.emplace(
            "extensionId",
            std::make_unique<LSPAny>(
                transformer.stringToAny(extensionId)
            )
        );
        object.emplace(
            "issueTitle",
            std::make_unique<LSPAny>(
                transformer.stringToAny(issueTitle)
            )
        );
        object.emplace(
            "issueBody",
            std::make_unique<LSPAny>(
                transformer.stringToAny(issueBody)
            )
        );
        MessageParams params;
        params = std::move(object);
        sendNotification("$/openIssue", params);
    }

    auto BaseLspLanguageServer::dispatch(
        ResponseMessage &response,
        RequestMessage &request
    ) -> void {
        IncomingRequest method{};
        try {
            method = incomingRequestByValue(request.method);
        } catch (std::invalid_argument &/*e*/) {
            throw LSP_EXCEPTION(
                ErrorCodes::MethodNotFound,
                "Unsupported request method: \"" + request.method + "\""
            );
        }
        assertRunning();
        if (method != IncomingRequest::Initialize) {
            assertInitialized();
        } else {
            bool expected = false;    // a reference is required
            if (!_initialized.compare_exchange_strong(expected, true)) {
                throw LSP_EXCEPTION(
                    ErrorCodes::InvalidRequest,
                    "Server may be initialized only once."
                );
            }
        }
        const std::string taskType = "request:" + request.method;
        auto tracer = startRunning(taskType);
        LspLanguageServer::dispatch(response, request, method);
    }

    auto BaseLspLanguageServer::dispatch(
        ResponseMessage &response,
        NotificationMessage &notification
    ) -> void {
        IncomingNotification method{};
        try {
            method = incomingNotificationByValue(notification.method);
        } catch (std::invalid_argument &/*e*/) {
            if (notification.method.compare(0, 2, "$/") == 0) {
                // NOTE: If a server or client receives notifications starting with "$/"
                // it is free to ignore the notification:
                logger.debug()
                    << "No handler exists for method: \"" << notification.method << "\""
                    << std::endl;
            } else {
                throw LSP_EXCEPTION(
                    ErrorCodes::MethodNotFound,
                    "Unsupported notification method: \"" + notification.method + "\""
                );
            }
        }
        if (method != IncomingNotification::Exit) {
            if (!_initialized) {
                // Notifications should be dropped, except for the exit notification.
                // This will allow the exit of a server without an initialize request.
                return;
            }
            assertRunning();
        }
        const std::string taskType = "notification:" + notification.method;
        auto tracer = startRunning(taskType);
        LspLanguageServer::dispatch(response, notification, method);
    }

    auto BaseLspLanguageServer::dispatch(
        ResponseMessage &response,
        std::string &traceId,
        const LSPAny &document
    ) -> void
    {
        switch (response.id.type()) {
        case ResponseIdType::Integer: {
            int responseId = response.id.integer();
            std::string method;
            {
                auto requestLock = LSP_READ_LOCK(requestMutex, "requests");
                auto iter = requestsById.find(responseId);
                if (iter == requestsById.end()) {
                    logger.error()
                        << "Cannot locate request with id: " << responseId
                        << std::endl;
                    return;
                }
                std::shared_ptr<RequestMessage> request = iter->second;
                method = request->method;
            }
            traceId = method + " - (" + std::to_string(responseId) + ")";
            logReceiveResponseTrace(traceId, document);
            const std::string taskType = "response:" + method;
            auto tracer = startRunning(taskType);
            LspLanguageServer::dispatch(response, method);
            break;
        }
        case ResponseIdType::Null: {
            logReceiveResponseTrace(traceId, document);
            break;
        }
        default: {
            logger.error()
                << "Cannot dispatch response with id of type ResponseIdType::"
                << ResponseIdTypeNames.at(response.id.type())
                << std::endl;
            return;
        }
        }
    }

    auto BaseLspLanguageServer::to_string(
        const RequestId &requestId
    ) -> std::string
    {
        switch (requestId.type()) {
        case RequestIdType::Integer: {
            return std::to_string(requestId.integer());
        }
        case RequestIdType::String: {
            return requestId.string();
        }
        default: {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                ("Cannot log request trace with Id of type RequestIdType::" +
                 RequestIdTypeNames.at(requestId.type()))
            );
        }
        }
    }

    auto BaseLspLanguageServer::toJsonString(const LSPAny &any) -> std::string {
        auto workspaceLock = LSP_READ_LOCK(workspaceMutex, "workspace");
        if (workspaceConfig->log.prettyPrint) {
            return serializer.pprint(any);
        }
        workspaceLock.unlock();
        return serializer.serialize(any);
    }

    auto BaseLspLanguageServer::toJsonString(const LSPArray &array) -> std::string {
        auto workspaceLock = LSP_READ_LOCK(workspaceMutex, "workspace");
        if (workspaceConfig->log.prettyPrint) {
            return serializer.pprint(array);
        }
        workspaceLock.unlock();
        return serializer.serialize(array);
    }

    auto BaseLspLanguageServer::toJsonString(const LSPObject &object) -> std::string {
        auto workspaceLock = LSP_READ_LOCK(workspaceMutex, "workspace");
        if (workspaceConfig->log.prettyPrint) {
            return serializer.pprint(object);
        }
        workspaceLock.unlock();
        return serializer.serialize(object);
    }

    auto BaseLspLanguageServer::logReceiveTrace(
        const std::string &messageType,
        const std::string &traceId,
        const std::optional<MessageParams> &optionalParams
    ) -> void
    {
        if ((trace >= TraceValues::Messages) && (traceId.length() > 0)) {
            LogTraceParams params;
            params.message = "Received " + messageType + " '" + traceId + "'.";
            if ((trace >= TraceValues::Verbose) && optionalParams.has_value()) {
                const MessageParams &messageParams = optionalParams.value();
                switch (messageParams.type()) {
                case MessageParamsType::Object: {
                    const LSPObject &object = messageParams.object();
                    params.verbose = "Params: " + toJsonString(object);
                    break;
                }
                case MessageParamsType::Array: {
                    const LSPArray &array = messageParams.array();
                    params.verbose = "Params: " + toJsonString(array);
                    break;
                }
                case MessageParamsType::Uninitialized: {
                    throw LSP_EXCEPTION(
                        ErrorCodes::InternalError,
                        "MessageParams has not been initialized"
                    );
                }
                }
            }
            sendLogTrace(params);
        }
    }

    auto BaseLspLanguageServer::logReceiveResponseTrace(
        const std::string &traceId,
        const LSPAny &document
    ) -> void
    {
        if (trace >= TraceValues::Messages) {
            LogTraceParams params;
            if (traceId.length() > 0) {
                params.message = "Received response '" + traceId + "'.";
            } else {
                params.message = "Received response.";
            }
            if (trace >= TraceValues::Verbose) {
                if (document.type() == LSPAnyType::Object) {
                    const auto &object_0 = document.object();
                    auto iter_0 = object_0.find("result");
                    if (iter_0 != object_0.end()) {
                        const LSPAny &result = *iter_0->second;
                        params.verbose = "Result: " + toJsonString(result);
                    } else if ((iter_0 = object_0.find("error")) != object_0.end()) {
                        const auto &error_0 = *iter_0->second;
                        params.verbose =
                            "Error: " + toJsonString(error_0);
                    } else {
                        params.verbose = "No result returned.";
                    }
                } else {
                    logger.error()
                        << "Cannot log verbose message for response of type LSPAnyType::"
                        << LSPAnyTypeNames.at(document.type())
                        << std::endl;
                }
            }
            sendLogTrace(params);
        }
    }

    auto BaseLspLanguageServer::logSendResponseTrace(
        const std::string &traceId,
        const std::chrono::time_point<std::chrono::high_resolution_clock> &start,
        const LSPAny &response
    ) -> void
    {
        if ((trace >= TraceValues::Messages) && (traceId.length() > 0)) {
            auto end_0 = std::chrono::high_resolution_clock::now();
            auto duration_0 =
                std::chrono::duration_cast<milliseconds_t>(end_0 - start);
            LogTraceParams params;
            params.message =
                "Sending response '" + traceId + "'. Processing request took " +
                std::to_string(duration_0.count()) + "ms";
            if (trace >= TraceValues::Verbose) {
                if (response.type() == LSPAnyType::Object) {
                    const auto &object_0 = response.object();
                    auto iter_0 = object_0.find("result");
                    if (iter_0 != object_0.end()) {
                        const auto &result_0 = *iter_0->second;
                        params.verbose =
                            "Result: " + toJsonString(result_0);
                    } else if ((iter_0 = object_0.find("error")) != object_0.end()) {
                        const auto &error_0 = *iter_0->second;
                        params.verbose =
                            "Error: " + toJsonString(error_0);
                    } else {
                        params.verbose = "No result returned.";
                    }
                } else {
                    logger.error()
                        << "Cannot log verbose message for response of type LSPAnyType::"
                        << LSPAnyTypeNames.at(response.type())
                        << std::endl;
                }
            }
            sendLogTrace(params);
        }
    }

    auto BaseLspLanguageServer::getConfig(
        const DocumentUri &uri
    ) -> const std::shared_ptr<lsc::LspConfig> {
        auto readLock = LSP_READ_LOCK(lspConfigMutex, "lsp-configs");
        auto iter = lspConfigsByUri.find(uri);
        if (iter != lspConfigsByUri.end()) {
            return iter->second;
        }

        readLock.unlock();

        std::shared_ptr<LSPAny> config = getConfig(uri, configSection);
        std::shared_ptr<lsc::LspConfig> lspConfig =
            lspConfigTransformer->anyToLspConfig(*config);

        auto writeLock = LSP_WRITE_LOCK(lspConfigMutex, "lsp-configs");

        iter = lspConfigsByUri.find(uri);
        if (iter != lspConfigsByUri.end()) {
            return iter->second;
        }

        auto record = lspConfigsByUri.emplace(uri, std::move(lspConfig));
        return record.first->second;
    }

    auto BaseLspLanguageServer::invalidateConfigCaches() -> void {
        {
            auto writeLock = LSP_WRITE_LOCK(configMutex, "configs");
            configsByUri.clear();
            logger.debug()
                << "Invalidated document configuration cache."
                << std::endl;
        }
        {
            auto writeLock = LSP_WRITE_LOCK(lspConfigMutex, "lsp-configs");
            lspConfigsByUri.clear();
            logger.debug()
                << "Invalidated LSP configuration cache."
                << std::endl;
        }
    }

    // ================= //
    // Incoming Requests //
    // ================= //

    // request: "initialize"
    auto BaseLspLanguageServer::receiveInitialize(
        const RequestMessage &/*request*/,
        InitializeParams &params
    ) -> InitializeResult {
        if (params.trace.has_value()) {
            trace = params.trace.value();
        }

        InitializeResult result;

        { // Initialize internal parameters
            if (params.processId.type() == _InitializeParams_processIdType::Integer) {
                parentProcessId = params.processId.integer();
            } else {
                logger.warn()
                    << "No parent Process Id (PID) specified."
                    << std::endl;
            }

            if (parentProcessId < 0) {
                shutdown();
                exit();
                throw LSP_EXCEPTION(
                    ErrorCodes::InvalidParams,
                    "No parent Process Id (PID) specified."
                );
            }

            const ClientCapabilities &capabilities = params.capabilities;
            if (capabilities.workspace.has_value()) {
                const WorkspaceClientCapabilities &workspace =
                    capabilities.workspace.value();

                clientSupportsWorkspaceConfigRequests =
                    workspace.configuration.has_value()
                    && workspace.configuration.value();

                if (workspace.didChangeConfiguration.has_value()) {
                    const DidChangeConfigurationClientCapabilities &didChangeConfiguration =
                        workspace.didChangeConfiguration.value();
                    clientSupportsWorkspaceConfigChangeNotifications =
                        didChangeConfiguration.dynamicRegistration.has_value()
                        && didChangeConfiguration.dynamicRegistration.value();
                }
            }
            logger.debug()
                << "clientSupportsWorkspaceConfigRequests = "
                << clientSupportsWorkspaceConfigRequests
                << std::endl;
            logger.debug()
                << "clientSupportsWorkspaceConfigChangeNotifications = "
                << clientSupportsWorkspaceConfigChangeNotifications
                << std::endl;
        }

        ServerCapabilities &capabilities = result.capabilities;

        {
            // ------------------------- //
            // TextDocument Sync Options //
            // ------------------------- //
            SaveOptions saveOptions;
            saveOptions.includeText = false;

            TextDocumentSyncOptions_save save;
            save = std::move(saveOptions);

            TextDocumentSyncOptions textDocumentSyncOptions;
            textDocumentSyncOptions.openClose = true;
            textDocumentSyncOptions.change = TextDocumentSyncKind::Incremental;
            textDocumentSyncOptions.save = std::move(save);

            ServerCapabilities_textDocumentSync textDocumentSync;
            textDocumentSync = std::move(textDocumentSyncOptions);

            capabilities.textDocumentSync = std::move(textDocumentSync);
        }

        {
            ServerCapabilities_workspace &workspace = capabilities.workspace.emplace();
            FileOperationOptions &fileOperations = workspace.fileOperations.emplace();
            FileOperationRegistrationOptions &didRename = fileOperations.didRename.emplace();

            FileOperationFilter &fortranFilter = didRename.filters.emplace_back();
            fortranFilter.scheme = "file";
            fortranFilter.pattern.glob = "**/*.{f,for,f90,f95,f03}";
        }

        _initializeParams =
            std::make_unique<InitializeParams>(std::move(params));

        return result;
    }

    auto BaseLspLanguageServer::shutdown() -> bool {
        bool expected = false;
        if (_shutdown.compare_exchange_strong(expected, true)) {
            logger.info() << "Shutting down server." << std::endl;
            return true;
        }
        return false;
    }

    // request: "shutdown"
    auto BaseLspLanguageServer::receiveShutdown(
        const RequestMessage &/*request*/
    ) -> ShutdownResult {
        shutdown();
        return nullptr;
    }

    // notification: "initialized"
    auto BaseLspLanguageServer::receiveInitialized(
        const NotificationMessage &/*notification*/,
        InitializedParams &/*params*/
    ) -> void {
        if (clientSupportsWorkspaceConfigChangeNotifications) {
            const std::string method = "workspace/didChangeConfiguration";

            LSPObject configOptions;
            {
                std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();
                (*any) = configSection;
                configOptions.emplace("section", std::move(any));
            }

            LSPAny options;
            options = std::move(configOptions);

            Registration registration;
            registration.id = method;
            registration.method = method;
            registration.registerOptions = std::move(options);

            RegistrationParams params;
            params.registrations.push_back(std::move(registration));

            sendClient_registerCapability(params);
        }
    }

    // notification: "$/setTrace"
    auto BaseLspLanguageServer::receiveSetTrace(
        const NotificationMessage &/*notification*/,
        SetTraceParams &params
    ) -> void
    {
        trace = params.value;
    }

    auto BaseLspLanguageServer::exit() -> bool {
        bool expected = false;
        if (_exit.compare_exchange_strong(expected, true)) {
            logger.info() << "Exiting server." << std::endl;
            expected = false;
            if (_shutdown.compare_exchange_strong(expected, true)) {
                logger.error()
                    << "Server exited before being notified to shutdown!"
                    << std::endl;
            }
            incomingMessages.stopNow();
            return true;
        }
        return false;
    }

    // notification: "exit"
    auto BaseLspLanguageServer::receiveExit(
        const NotificationMessage &/*notification*/
    ) -> void {
        exit();
    }

    auto BaseLspLanguageServer::receiveClient_registerCapability(
        const RequestMessage &/*request*/,
        const ResponseMessage &/*response*/,
        Client_RegisterCapabilityResult /*params*/
    ) -> void {
        // empty
    }

    // notification: "workspace/didRenameFiles"
    auto BaseLspLanguageServer::receiveWorkspace_didRenameFiles(
        const NotificationMessage &/*notification*/,
        RenameFilesParams &params
    ) -> void {
        for (const FileRename &param : params.files) {
            const std::string &oldUri = param.oldUri;
            const std::string &newUri = param.newUri;
            {
                auto readLock = LSP_READ_LOCK(documentMutex, "documents");
                auto iter = documentsByUri.find(oldUri);
                if (iter != documentsByUri.end()) {
                    readLock.unlock();
                    auto writeLock = LSP_WRITE_LOCK(documentMutex, "documents");
                    std::shared_ptr<LspTextDocument> &textDocument = iter->second;
                    {
                        auto documentLock = LSP_WRITE_LOCK(textDocument->mutex(), "document:" + oldUri);
                        textDocument->setUri(newUri);
                    }
                    documentsByUri.emplace(newUri, std::move(textDocument));
                    documentsByUri.erase(iter);
                }
            }
        }
    }

    auto BaseLspLanguageServer::updateWorkspaceConfig(
        std::shared_ptr<lsc::LspConfig> workspaceConfig
    ) -> void {
        auto writeLock = LSP_WRITE_LOCK(workspaceMutex, "workspace");
        updateLogLevel(*workspaceConfig);
        updatePrettyPrintIndentSize(*workspaceConfig);
        this->workspaceConfig = std::move(workspaceConfig);
    }

    auto BaseLspLanguageServer::updateLogLevel(
        lsc::LspConfig &workspaceConfig
    ) -> void {
        if (!this->workspaceConfig ||
            (this->workspaceConfig->log.level != workspaceConfig.log.level)) {
            try {
                logger.setLevel(workspaceConfig.log.level);
            } catch (...) {
                logger.error()
                    << formatException(
                        "Caught unhandled exception while updating log level",
                        std::current_exception()
                    )
                    << std::endl;
            }
        }
    }

    auto BaseLspLanguageServer::updatePrettyPrintIndentSize(
        lsc::LspConfig &workspaceConfig
    ) -> void {
        if (!this->workspaceConfig ||
            (this->workspaceConfig->indentSize != workspaceConfig.indentSize)) {
            std::string indent{""};
            for (unsigned int i = 0; i < workspaceConfig.indentSize; ++i) {
                indent.append(" ");
            }
            serializer.setIndent(indent);
        }
    }

    // notification: "workspace/didChangeConfiguration"
    auto BaseLspLanguageServer::receiveWorkspace_didChangeConfiguration(
        const NotificationMessage &/*notification*/,
        DidChangeConfigurationParams &params
    ) -> void {
        invalidateConfigCaches();
        LSPAny &settings = params.settings;
        switch (settings.type()) {
        case LSPAnyType::Object: {
            const LSPObject &object = settings.object();
            auto iter = object.find(configSection);
            if (iter != object.end()) {
                std::shared_ptr<lsc::LspConfig> lspConfig =
                    lspConfigTransformer->anyToLspConfig(*iter->second);
                updateWorkspaceConfig(std::move(lspConfig));
            } else {
                logger.warn()
                    << "Unable to locate configuration settings for section: "
                    << configSection
                    << std::endl;
            }
            break;
        }
        default: {
            logger.error()
                << "Unsupported settings type: LSPAnyType::"
                << LSPAnyTypeNames.at(settings.type())
                << std::endl;
        }
        }
    }

    // notification: "textDocument/didOpen"
    auto BaseLspLanguageServer::receiveTextDocument_didOpen(
        const NotificationMessage &/*notification*/,
        DidOpenTextDocumentParams &params
    ) -> void {
        const TextDocumentItem &textDocumentItem = params.textDocument;
        const DocumentUri &uri = textDocumentItem.uri;
        const std::string &languageId = textDocumentItem.languageId;
        int version = textDocumentItem.version;
        const std::string &text = textDocumentItem.text;
        {
            auto writeLock = LSP_WRITE_LOCK(documentMutex, "documents");
            auto iter = documentsByUri.find(uri);
            if (iter == documentsByUri.end()) {
                documentsByUri.emplace_hint(
                    iter, uri,
                    std::make_shared<LspTextDocument>(
                        uri,
                        languageId,
                        version,
                        text,
                        logger
                    )
                );
            } else {
                std::shared_ptr<LspTextDocument> document = iter->second;
                writeLock.unlock();
                document->update(languageId, version, text);
            }
        }
    }

    // request: "workspace/configuration"
    auto BaseLspLanguageServer::receiveWorkspace_configuration(
        const RequestMessage &request,
        const ResponseMessage &/*response*/,
        Workspace_ConfigurationResult &params
    ) -> void {
        const RequestId &requestId = request.id;
        auto writeLock = LSP_WRITE_LOCK(configMutex, "configs");
        auto iter = pendingConfigsById.find(requestId.integer());
        if (iter != pendingConfigsById.end()) {
            auto &pairs = iter->second;
            if (params.size() != pairs.size()) {
                throw LSP_EXCEPTION(
                    ErrorCodes::InvalidParams,
                    ("Number of config params differs: " +
                     std::to_string(params.size()) +
                     " != " +
                     std::to_string(pairs.size()))
                );
            }
            for (std::size_t i = 0; i < params.size(); ++i) {
                LSPAny &config = params[i];
                auto &pair = pairs[i];
                const DocumentUri &uri = pair.first;
                auto &promise = pair.second;
                auto record = configsByUri.emplace(
                    uri,
                    std::make_shared<LSPAny>(
                        std::move(config)
                    )
                );
                promise.set_value(record.first->second);
                auto pendingIter = pendingConfigsByUri.find(uri);
                if (pendingIter != pendingConfigsByUri.end()) {
                    pendingConfigsByUri.erase(pendingIter);
                }
                pendingConfigsById.erase(iter);
            }
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InternalError,
                "No record exists of this config being requested. Please file a ticket."
            );
        }
    }

    auto BaseLspLanguageServer::getDocument(
        const DocumentUri &uri
    ) -> std::shared_ptr<LspTextDocument> {
        auto readLock = LSP_READ_LOCK(documentMutex, "documents");
        auto iter = documentsByUri.find(uri);
        if (iter != documentsByUri.end()) {
            return iter->second;
        }
        readLock.unlock();
        try {
            auto writeLock = LSP_WRITE_LOCK(documentMutex, "documents");
            iter = documentsByUri.find(uri);
            if (iter != documentsByUri.end()) {
                return iter->second;
            }

            const auto &record = documentsByUri.emplace_hint(
                iter,
                std::piecewise_construct,
                std::forward_as_tuple(uri),
                std::forward_as_tuple(
                    std::make_shared<LspTextDocument>(uri, logger)
                )
            );

            return record->second;
        } catch (const std::runtime_error &e) {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                ("Failed to load document with uri=\"" + uri + "\": " + e.what())
            );
        }
    }

    // request: "$/document"
    auto BaseLspLanguageServer::receiveDocument(
        const RequestMessage &/*request*/,
        DocumentParams &params
    ) -> DocumentResult {
        DocumentResult result;
        std::shared_ptr<LspTextDocument> document = getDocument(params.uri);
        {
            auto readLock = LSP_READ_LOCK(document->mutex(), "document:" + params.uri);
            result.uri = document->uri();
            result.version = document->version();
            result.text = document->text();
        }
        return result;
    }

    // request: "$/telemetry"
    auto BaseLspLanguageServer::receiveTelemetry(
        const RequestMessage &/*request*/
    ) -> TelemetryResult {
        return collectTelemetry();
    }

    // notification: "textDocument/didChange"
    auto BaseLspLanguageServer::receiveTextDocument_didChange(
        const NotificationMessage &/*notification*/,
        DidChangeTextDocumentParams &params
    ) -> void {
        const VersionedTextDocumentIdentifier &versionedDocId = params.textDocument;
        const DocumentUri &uri = versionedDocId.uri;
        integer_t version = versionedDocId.version;
        std::shared_ptr<LspTextDocument> textDocument = getDocument(uri);
        textDocument->apply(params.contentChanges, version);
    }

    // notification: "textDocument/didClose"
    auto BaseLspLanguageServer::receiveTextDocument_didClose(
        const NotificationMessage &/*notification*/,
        DidCloseTextDocumentParams &params
    ) -> void {
        const DocumentUri &uri = params.textDocument.uri;
        {
            auto readLock = LSP_READ_LOCK(documentMutex, "documents");
            auto iter = documentsByUri.find(uri);
            if (iter != documentsByUri.end()) {
                readLock.unlock();
                auto writeLock = LSP_WRITE_LOCK(documentMutex, "documents");
                iter = documentsByUri.find(uri);
                if (iter != documentsByUri.end()) {
                    documentsByUri.erase(iter);
                }
            }
        }
    }

    // notification: "textDocument/didSave"
    auto BaseLspLanguageServer::receiveTextDocument_didSave(
        const NotificationMessage &/*notification*/,
        DidSaveTextDocumentParams &/*params*/
    ) -> void {
        // empty
    }

    // notification: $/cancelRequest
    auto BaseLspLanguageServer::receiveCancelRequest(
        const NotificationMessage &/*notification*/,
        CancelParams &params
    ) -> void {
        std::string requestId;
        switch (params.id.type()) {
        case CancelParams_idType::Integer: {
            requestId = std::to_string(params.id.integer());
            break;
        }
        case CancelParams_idType::String: {
            requestId = params.id.string();
            break;
        }
        case CancelParams_idType::Uninitialized: {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidRequest,
                "Missing required attribute for CancelParams: id"
            );
        }
        }
        auto lock = LSP_MUTEX_LOCK(activeMutex, "active-requests");
        auto iter = activeRequests.find(requestId);
        if (iter != activeRequests.end()) {
            *iter->second = false;
        } else {
            activeRequests.emplace_hint(
                iter,
                requestId,
                std::make_shared<std::atomic_bool>(false)
            );
        }
    }

} // namespace LCompilers::LanguageServerProtocol
