#include <algorithm>
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
    auto now = std::chrono::system_clock::now;

    namespace lsr = LCompilers::LanguageServerProtocol::Reporter;

    auto CronComparator::operator()(
        const CronElem &lhs,
        const CronElem &rhs
    ) -> bool {
        return std::get<2>(lhs) > std::get<2>(rhs);
    }

    BaseLspLanguageServer::BaseLspLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        std::size_t numRequestThreads,
        std::size_t numWorkerThreads,
        lsl::Logger &logger,
        const std::string &configSection,
        const std::string &extensionId,
        const std::string &compilerVersion,
        int parentProcessId,
        unsigned int seed,
        std::shared_ptr<lsc::LspConfigTransformer> lspConfigTransformer,
        std::shared_ptr<lsc::LspConfig> workspaceConfig
    ) : LspLanguageServer(
        incomingMessages,
        outgoingMessages,
        logger
      )
      , configSection(configSection)
      , extensionId(extensionId)
      , compilerVersion(compilerVersion)
      , parentProcessId(parentProcessId)
      , requestPool("request", numRequestThreads, logger)
      , workerPool("worker", numWorkerThreads, logger)
      , lspConfigTransformer(std::move(lspConfigTransformer))
      , randomEngine(seed)
      , listener([this, &logger]{
          logger.threadName("BaseLspLanguageServer_listener");
          listen();
      })
      , cron([this, &logger]{
          logger.threadName("BaseLspLanguageServer_cron");
          chronicle();
      })
    {
        documentsByUri.reserve(256);
        configsByUri.reserve(256);
        lspConfigsByUri.reserve(256);
        updateWorkspaceConfig(std::move(workspaceConfig));
        // -----------------------
        // Schedule the cron jobs:
        // -----------------------
        schedule(
            [this](std::shared_ptr<std::atomic_bool> taskIsRunning) {
                checkParentProcessId(std::move(taskIsRunning));
            },
            [this]{ return ttl(1000ms); }
        );
        schedule(
            [this](std::shared_ptr<std::atomic_bool> taskIsRunning) {
                expireCaches(std::move(taskIsRunning));
            },
            [this]{ return ttl(1000ms); }
        );
        schedule(
            [this](std::shared_ptr<std::atomic_bool> taskIsRunning) {
                retryRequests(std::move(taskIsRunning));
            },
            [this]{ return ttl(100ms); }
        );
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

    auto BaseLspLanguageServer::join() -> void
    {
        if (listener.joinable()) {
            listener.join();
            logger.debug()
                << "[LspLanguageServer] Incoming-message listener terminated."
                << std::endl;
        }
        requestPool.join();
        logger.debug()
            << "[LspLanguageServer] Request thread-pool terminated."
            << std::endl;
        workerPool.join();
        logger.debug()
            << "[LspLanguageServer] Worker thread-pool terminated."
            << std::endl;
    }

    auto BaseLspLanguageServer::listen() -> void
    {
        try {
            while (!_exit) {
                const std::string message = incomingMessages.dequeue();
                if (!_exit) {
                    std::size_t sendId = nextSendId();
                    std::shared_ptr<std::atomic_bool> taskIsRunning =
                        requestPool.execute([this, message, sendId](
                            std::shared_ptr<std::atomic_bool> taskIsRunning
                        ) {
                            try {
                                if (*taskIsRunning) {
                                    handle(message, sendId, std::move(taskIsRunning));
                                } else {
                                    logger.debug()
                                        << "Canceled before message could be handled."
                                        << std::endl;
                                }
                            } catch (std::exception &e) {
                                std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
                                logger.error()
                                    << "Failed to handle message: " << message
                                    << std::endl;
                                logger.error()
                                    << "Caught unhandled exception: " << e.what()
                                    << std::endl;
                            }
                            {
                                std::unique_lock<std::mutex> sentLock(sentMutex);
                                sent.wait(sentLock, [this, sendId]{
                                    return (pendingSendId == sendId) || _exit;
                                });
                            }
                            ++pendingSendId;
                            {
                                std::unique_lock<std::mutex> sentLock(sentMutex);
                                sent.notify_all();
                            }
                        });
                }
            }
        } catch (std::exception &e) {
            if (e.what() != lst::DEQUEUE_FAILED_MESSAGE) {
                logger.error()
                    << "[LspLanguageServer] "
                       "Unhandled exception caught: " << e.what()
                    << std::endl;
            } else {
                logger.trace()
                    << "[LspLanguageServer] "
                       "Interrupted while dequeuing messages: " << e.what()
                    << std::endl;
            }
        }
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

    auto BaseLspLanguageServer::checkParentProcessId(
        std::shared_ptr<std::atomic_bool> taskIsRunning
    ) -> void {
        if (*taskIsRunning &&
            (parentProcessId >= 0) &&
            !isProcessRunning(parentProcessId)) {
            logger.error()
                << "Parent process terminated before terminating server."
                << std::endl;
            receiveShutdown();
            receiveExit();
        }
    }

    auto BaseLspLanguageServer::nextCronId() -> std::size_t {
        return ++serialCronId;
    }

    auto BaseLspLanguageServer::schedule(
        lst::Task cronJob,
        CronSchedule schedule
    ) -> std::size_t {
        std::size_t cronId = nextCronId();
        {
            std::unique_lock<std::shared_mutex> writeLock(scheduleMutex);
            cronSchedules.emplace(cronId, schedule);
        }
        time_point_t nextTimePoint = schedule();
        {
            std::unique_lock<std::mutex> cronLock(cronMutex);
            cronJobs.push(std::make_tuple(cronId, cronJob, nextTimePoint));
        }
        return cronId;
    }

    auto BaseLspLanguageServer::unschedule(std::size_t cronId) -> bool {
        std::shared_lock<std::shared_mutex> readLock(scheduleMutex);
        auto iter = cronSchedules.find(cronId);
        if (iter != cronSchedules.end()) {
            readLock.unlock();
            std::unique_lock<std::shared_mutex> writeLock(scheduleMutex);
            iter = cronSchedules.find(cronId);
            if (iter != cronSchedules.end()) {
                cronSchedules.erase(iter);
                return true;
            }
        }
        return false;
    }

    auto BaseLspLanguageServer::chronicle() -> void {
        try {
            while (!_exit) {
                bool changed;
                do {
                    changed = false;
                    std::unique_lock<std::mutex> cronLock(cronMutex);
                    if (cronJobs.size() > 0) {
                        const CronElem &elem = cronJobs.top();
                        if (std::get<2>(elem) < now()) {
                            std::size_t cronId = std::get<0>(elem);
                            lst::Task cronJob = std::get<1>(elem);
                            cronJobs.pop();
                            cronLock.unlock();
                            workerPool.execute([this, cronId, cronJob](
                                std::shared_ptr<std::atomic_bool> taskIsRunning
                            ) {
                                if (!_exit) {
                                    if (*taskIsRunning) {
                                        try {
                                            cronJob(std::move(taskIsRunning));
                                        } catch (const std::exception &e) {
                                            logger.error()
                                                << "Caught unhandled exception while executing cron job with id="
                                                << cronId << ": " << e.what() << std::endl;
                                        }
                                    } else {
                                        logger.debug()
                                            << "Cron job with id=" << cronId << " canceled before execution."
                                            << std::endl;
                                    }

                                    std::shared_lock<std::shared_mutex> readLock(scheduleMutex);
                                    auto iter = cronSchedules.find(cronId);
                                    if (iter != cronSchedules.end()) {
                                        CronSchedule schedule = iter->second;
                                        readLock.unlock();
                                        time_point_t nextTimePoint = schedule();
                                        std::unique_lock<std::mutex> cronLock(cronMutex);
                                        cronJobs.push(std::make_tuple(cronId, cronJob, nextTimePoint));
                                    }
                                }
                            });
                            changed = true;
                        }
                    }
                } while (!_exit && changed);

                // NOTE: Wait a short period of time before running the cron jobs again:
                std::this_thread::sleep_for(100ms);
            }
        } catch (std::exception &e) {
            logger.error()
                << "[LspLanguageServer] "
                   "Unhandled exception caught: " << e.what()
                << std::endl;
        }
    }

    auto BaseLspLanguageServer::send(const RequestMessage &request) -> void {
        int requestId = request.id.integer();
        {
            std::unique_lock<std::mutex> writeLock(requestMutex);
            requestsById.emplace(requestId, request);
        }
        LspLanguageServer::send(request);
        {
            std::unique_lock<std::shared_mutex> retryLock(retryMutex);
            retryAttempts.push(
                std::make_pair(
                    std::make_tuple(
                        requestId,
                        0,  // attempt
                        milliseconds_t(
                            workspaceConfig->retry.minSleepTimeMs
                        )
                    ),
                    ttl(milliseconds_t(workspaceConfig->timeoutMs))
                )
            );
        }
    }

    auto BaseLspLanguageServer::send(
        const std::string &message,
        std::size_t sendId
    ) -> void
    {
        // -------------------------------------------------------------------------
        // NOTE: The LSP spec requires responses to be returned in roughly the same
        // order of receipt of their corresponding requests. Some types of responses
        // may be returned out-of-order, but in order to support those we will need
        // to implement a sort of dependency graph. Without knowledge of their
        // dependencies, we must respond to all requests in order of receipt.
        // -------------------------------------------------------------------------
        {
            std::unique_lock<std::mutex> sentLock(sentMutex);
            sent.wait(sentLock, [this, sendId]{
                return (pendingSendId == sendId) || _exit;
            });
        }
        if ((pendingSendId == sendId) && !_exit) {
            ls::LanguageServer::send(message);
        }
    }

    auto BaseLspLanguageServer::ttl(
        const milliseconds_t &timeout
    ) const -> time_point_t {
        return now() + timeout;
    }

    auto BaseLspLanguageServer::expireCaches(
        std::shared_ptr<std::atomic_bool> taskIsRunning
    ) -> void {
        bool changed = true;
        while (!_exit && *taskIsRunning && changed) {
            changed = false;
            std::shared_lock<std::shared_mutex> readLock(recentMutex);
            if ((recentRequests.size() > 0) && (recentRequests.top().second < now())) {
                readLock.unlock();
                std::unique_lock<std::shared_mutex> writeLock(recentMutex);
                if (recentRequests.size() > 0) {
                    const TTLRecord<std::string> &record = recentRequests.top();
                    if (record.second < now()) {
                        std::string requestId = record.first;
                        recentRequests.pop();
                        writeLock.unlock();
                        {
                            std::unique_lock<std::mutex> requestLock(activeMutex);
                            auto iter = activeRequests.find(requestId);
                            if (iter != activeRequests.end()) {
                                activeRequests.erase(iter);  // timeout
                            }
                        }
                        changed = true;
                    }
                }
            }
        }
    }

    auto BaseLspLanguageServer::randomBetween(
        const milliseconds_t &lower,
        const milliseconds_t &upper
    ) -> milliseconds_t {
        std::uniform_int_distribution<long long> distribution(
            lower.count(),
            upper.count()
        );
        return milliseconds_t(distribution(randomEngine));
    }

    auto BaseLspLanguageServer::retryRequests(
        std::shared_ptr<std::atomic_bool> taskIsRunning
    ) -> void {
        bool changed = true;
        while (!_exit && *taskIsRunning && changed) {
            changed = false;
            std::shared_lock<std::shared_mutex> readLock(retryMutex);
            if ((retryAttempts.size() > 0) && retryAttempts.top().second < now()) {
                readLock.unlock();
                std::unique_lock<std::shared_mutex> writeLock(retryMutex);
                const TTLRecord<RetryRecord> &record = retryAttempts.top();
                if (record.second < now()) {
                    int requestId = std::get<0>(record.first);
                    cancelRequest(requestId);
                    unsigned int attempt = std::get<1>(record.first);
                    retryAttempts.pop();
                    writeLock.unlock();
                    if (attempt < workspaceConfig->retry.maxAttempts) {
                        // NOTE: See the section on "Decorrelated Jitter" in the following article:
                        // https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
                        milliseconds_t lastSleepTimeMs =
                            std::get<2>(record.first);
                        milliseconds_t nextSleepTimeMs =
                            std::min<milliseconds_t>(
                                milliseconds_t(workspaceConfig->retry.maxSleepTimeMs),
                                randomBetween(
                                    milliseconds_t(workspaceConfig->retry.minSleepTimeMs),
                                    lastSleepTimeMs * 3
                                )
                            );
                        // NOTE: Release the worker thread to execute more tasks
                        // while we wait to retry the request. Do not sleep from
                        // the current thread or we may run out of available
                        // workers!
                        schedule([this, requestId, attempt, &nextSleepTimeMs](
                            std::shared_ptr<std::atomic_bool> taskIsRunning
                        ) {
                            if (*taskIsRunning) {
                                std::unique_lock<std::mutex> requestLock(requestMutex);
                                auto iter = requestsById.find(requestId);
                                if (iter != requestsById.end()) {
                                    const RequestMessage &request = iter->second;
                                    LspLanguageServer::send(request);
                                    requestLock.unlock();
                                    std::unique_lock<std::shared_mutex> writeLock(retryMutex);
                                    retryAttempts.push(
                                        std::make_pair(
                                            std::make_tuple(
                                                requestId,
                                                attempt + 1,
                                                nextSleepTimeMs
                                            ),
                                            ttl(milliseconds_t(workspaceConfig->timeoutMs))
                                        )
                                    );
                                }
                            }
                        }, nextSleepTimeMs);
                        logger.trace()
                            << "Request with id=" << requestId
                            << " timed-out. Retrying after "
                            << static_cast<long>(nextSleepTimeMs.count())
                            << " ms." << std::endl;
                    } else {
                        logger.error()
                            << "Request with id=" << requestId
                            << " failed after " << attempt << " attempts."
                            << std::endl;
                    }
                }
                changed = true;
            }
        }
    }

    auto BaseLspLanguageServer::cancelRequest(int requestId) -> void {
        CancelParams params;
        params.id = requestId;
        sendCancelRequest(params);
    }

    auto BaseLspLanguageServer::handle(
        const std::string &incoming,
        std::size_t sendId,
        std::shared_ptr<std::atomic_bool> taskIsRunning
    ) -> void
    {
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
                    if (response.id.type() == ResponseIdType::Null) {
                        throw LSP_EXCEPTION(
                            ErrorCodes::InvalidParams,
                            "Missing request method=\"" + method + "\" attribute: id"
                        );
                    }
                    RequestMessage request =
                        transformer.anyToRequestMessage(*document);
                    std::string requestId;
                    switch (request.id.type()) {
                    case RequestIdType::Integer: {
                        requestId = std::to_string(request.id.integer());
                        break;
                    }
                    case RequestIdType::String: {
                        requestId = request.id.string();
                        break;
                    }
                    case RequestIdType::Uninitialized: {
                        throw LSP_EXCEPTION(
                            ErrorCodes::InvalidRequest,
                            "Missing required attribute for RequestMessage: id"
                        );
                    }
                    }
                    {
                        std::unique_lock<std::mutex> lock(activeMutex);
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
                    {
                        std::unique_lock<std::shared_mutex> writeLock(recentMutex);
                        recentRequests.push(std::make_pair(requestId, ttl(RECENT_REQUEST_TIMEOUT)));
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
                        std::unique_lock<std::mutex> lock(activeMutex);
                        auto iter = activeRequests.find(requestId);
                        if (iter != activeRequests.end()) {
                            activeRequests.erase(iter);
                        }
                    }
                } else if (isIncomingNotification(method)) {
                    if (response.id.type() != ResponseIdType::Null) {
                        throw LSP_EXCEPTION(
                            ErrorCodes::InvalidParams,
                            "Notification method=\"" + method + "\" must not contain the attribute: id"
                        );
                    }
                    NotificationMessage notification =
                        transformer.anyToNotificationMessage(*document);
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
                } else {
                    throw LSP_EXCEPTION(
                        ErrorCodes::InvalidRequest,
                        "Unsupported method: \"" + method + "\""
                    );
                }
            } else if ((iter = object.find("result")) != object.end()) {
                if (*taskIsRunning) {
                    response.result = transformer.copy(*iter->second);
                    dispatch(response, traceId, *document);
                } else {
                    logger.debug()
                        << "Response with result canceled before dispatch."
                        << std::endl;
                }
                return;
            } else if ((iter = object.find("error")) != object.end()) {
                if (*taskIsRunning) {
                    response.error = transformer.anyToResponseError(*iter->second);
                    dispatch(response, traceId, *document);
                } else {
                    logger.debug()
                        << "Response with error canceled before dispatch."
                        << std::endl;
                }
                return;
            } else {
                throw LSP_EXCEPTION(
                    ErrorCodes::InvalidRequest,
                    "Missing required attribute: method"
                );
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
            response.error = std::move(error);
            logger.error() << error.message << std::endl;
        } catch (const std::exception &e) {
            ResponseError error;
            error.code = static_cast<int>(ErrorCodes::InternalError);
            error.message = "Caught unhandled exception: ";
            error.message.append(e.what());
            response.error = std::move(error);
            logger.error() << error.message << std::endl;
        }
        if (*taskIsRunning) {
            LSPAny any = transformer.responseMessageToAny(response);
            logSendResponseTrace(traceId, start, any);
            const std::string outgoing = serializer.serialize(any);
            send(outgoing, sendId);
        } else {
            logger.debug()
                << "Message canceled before sending response."
                << std::endl;
        }
        if (response.error.has_value()) {
            const ResponseError &error = response.error.value();
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
                } catch (const std::exception &e) {
                    logger.error()
                        << "Failed to open issue: " << e.what()
                        << std::endl;
                }
            }
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
                std::unique_lock<std::mutex> requestLock(requestMutex);
                auto iter = requestsById.find(responseId);
                if (iter == requestsById.end()) {
                    logger.error() << "Cannot locate request with id: " << responseId << std::endl;
                    return;
                }
                const RequestMessage &request = iter->second;
                method = std::move(request.method);
                requestsById.erase(iter);
            }
            traceId = method + " - (" + std::to_string(responseId) + ")";
            logReceiveResponseTrace(traceId, document);
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
        if (workspaceConfig->log.prettyPrint) {
            return serializer.pprint(any);
        }
        return serializer.serialize(any);
    }

    auto BaseLspLanguageServer::toJsonString(const LSPArray &array) -> std::string {
        if (workspaceConfig->log.prettyPrint) {
            return serializer.pprint(array);
        }
        return serializer.serialize(array);
    }

    auto BaseLspLanguageServer::toJsonString(const LSPObject &object) -> std::string {
        if (workspaceConfig->log.prettyPrint) {
            return serializer.pprint(object);
        }
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
        std::shared_lock<std::shared_mutex> readLock(lspConfigMutex);
        auto iter = lspConfigsByUri.find(uri);
        if (iter != lspConfigsByUri.end()) {
            return iter->second;
        }

        readLock.unlock();

        std::shared_ptr<LSPAny> config = getConfig(uri, configSection);
        std::shared_ptr<lsc::LspConfig> lspConfig =
            lspConfigTransformer->anyToLspConfig(*config);

        std::unique_lock<std::shared_mutex> writeLock(lspConfigMutex);

        iter = lspConfigsByUri.find(uri);
        if (iter != lspConfigsByUri.end()) {
            return iter->second;
        }

        auto record = lspConfigsByUri.emplace(uri, std::move(lspConfig));
        return record.first->second;
    }

    auto BaseLspLanguageServer::getConfig(
        const DocumentUri &uri,
        const std::string &configSection
    ) -> const std::shared_ptr<LSPAny> {
        std::shared_lock<std::shared_mutex> readLock(configMutex);
        auto configIter = configsByUri.find(uri);
        if (configIter != configsByUri.end()) {
            return configIter->second;
        }

        readLock.unlock();

        ConfigurationItem item;
        item.scopeUri = uri;
        item.section = configSection;

        ConfigurationParams params;
        params.items.push_back(std::move(item));

        std::unique_lock<std::shared_mutex> writeLock(configMutex);
        configIter = configsByUri.find(uri);
        if (configIter != configsByUri.end()) {
            return configIter->second;
        }

        std::shared_future<std::shared_ptr<LSPAny>> future;

        auto pendingIter = pendingConfigsByUri.find(uri);
        if (pendingIter != pendingConfigsByUri.end()) {
            future = pendingIter->second.second;
        } else {
            int requestId = sendWorkspace_configuration(params);
            std::promise<std::shared_ptr<LSPAny>> promise;
            future = promise.get_future().share();
            {
                auto &pairs = pendingConfigsById.emplace(
                    std::piecewise_construct,
                    std::make_tuple(requestId),
                    std::make_tuple()
                ).first->second;
                auto &pair = pairs.emplace_back();
                pair.first = uri;
                pair.second = std::move(promise);
            }
            pendingConfigsByUri.emplace(
                std::piecewise_construct,
                std::make_tuple(uri),
                std::make_tuple(requestId, future)
            );
        }

        writeLock.unlock();

        if (future.valid()) {
            future.wait();
            if (future.valid()) {
                return future.get();
            }
        }

        throw std::runtime_error(
            "Future config became invalid while waiting for it."
        );
    }

    auto BaseLspLanguageServer::invalidateConfigCaches() -> void {
        {
            std::unique_lock<std::shared_mutex> writeLock(configMutex);
            configsByUri.clear();
            logger.debug() << "Invalidated document configuration cache." << std::endl;
        }
        {
            std::unique_lock<std::shared_mutex> writeLock(lspConfigMutex);
            lspConfigsByUri.clear();
            logger.debug() << "Invalidated LSP configuration cache." << std::endl;
        }
    }

    // ================= //
    // Incoming Requests //
    // ================= //

    // request: "initialize"
    auto BaseLspLanguageServer::receiveInitialize(
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
                receiveShutdown();
                receiveExit();
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

    // request: "shutdown"
    auto BaseLspLanguageServer::receiveShutdown() -> ShutdownResult
    {
        bool expected = false;
        if (_shutdown.compare_exchange_strong(expected, true)) {
            logger.info() << "Shutting down server." << std::endl;
        }
        return nullptr;
    }

    // notification: "initialized"
    auto BaseLspLanguageServer::receiveInitialized(
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
    auto BaseLspLanguageServer::receiveSetTrace(SetTraceParams &params) -> void
    {
        trace = params.value;
    }

    // notification: "exit"
    auto BaseLspLanguageServer::receiveExit() -> void
    {
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
            requestPool.stopNow();
            workerPool.stopNow();
        }
    }

    auto BaseLspLanguageServer::receiveClient_registerCapability(
        const RequestId &/*requestId*/,
        Client_RegisterCapabilityResult /*params*/
    ) -> void {
        // empty
    }

    // notification: "workspace/didRenameFiles"
    auto BaseLspLanguageServer::receiveWorkspace_didRenameFiles(
        RenameFilesParams &params
    ) -> void {
        for (const FileRename &param : params.files) {
            const std::string &oldUri = param.oldUri;
            const std::string &newUri = param.newUri;
            {
                std::shared_lock<std::shared_mutex> readLock(documentMutex);
                auto iter = documentsByUri.find(oldUri);
                if (iter != documentsByUri.end()) {
                    readLock.unlock();
                    std::unique_lock<std::shared_mutex> writeLock(documentMutex);
                    std::shared_ptr<LspTextDocument> &textDocument = iter->second;
                    {
                        std::unique_lock<std::shared_mutex> writeLock(textDocument->mutex());
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
        std::unique_lock<std::shared_mutex> writeLock(workspaceMutex);
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
            } catch (std::exception &e) {
                logger.error()
                    << "Caught unhandled exception while updating log level: " << e.what()
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
        DidOpenTextDocumentParams &params
    ) -> void {
        const TextDocumentItem &textDocumentItem = params.textDocument;
        const DocumentUri &uri = textDocumentItem.uri;
        const std::string &languageId = textDocumentItem.languageId;
        int version = textDocumentItem.version;
        const std::string &text = textDocumentItem.text;
        {
            std::unique_lock<std::shared_mutex> writeLock(documentMutex);
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
        const RequestId &requestId,
        Workspace_ConfigurationResult &params
    ) -> void {
        std::unique_lock<std::shared_mutex> writeLock(configMutex);
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
        std::shared_lock<std::shared_mutex> readLock(documentMutex);
        auto iter = documentsByUri.find(uri);
        if (iter != documentsByUri.end()) {
            return iter->second;
        }
        readLock.unlock();
        try {
            std::unique_lock<std::shared_mutex> writeLock(documentMutex);
            iter = documentsByUri.find(uri);
            if (iter != documentsByUri.end()) {
                return iter->second;
            }

            const auto &record = documentsByUri.emplace_hint(
                iter,
                std::piecewise_construct,
                std::make_tuple(uri),
                std::make_tuple(
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

    // request: "$/getDocument"
    auto BaseLspLanguageServer::receiveGetDocument(
        GetDocumentParams &params
    ) -> GetDocumentResult {
        GetDocumentResult result;
        std::shared_ptr<LspTextDocument> document = getDocument(params.uri);
        {
            std::shared_lock<std::shared_mutex> readLock(document->mutex());
            result.uri = document->uri();
            result.version = document->version();
            result.text = document->text();
        }
        return result;
    }

    // notification: "textDocument/didChange"
    auto BaseLspLanguageServer::receiveTextDocument_didChange(
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
        DidCloseTextDocumentParams &params
    ) -> void {
        const DocumentUri &uri = params.textDocument.uri;
        {
            std::shared_lock<std::shared_mutex> readLock(documentMutex);
            auto iter = documentsByUri.find(uri);
            if (iter != documentsByUri.end()) {
                readLock.unlock();
                std::unique_lock<std::shared_mutex> writeLock(documentMutex);
                iter = documentsByUri.find(uri);
                if (iter != documentsByUri.end()) {
                    documentsByUri.erase(iter);
                }
            }
        }
    }

    // notification: "textDocument/didSave"
    auto BaseLspLanguageServer::receiveTextDocument_didSave(
        DidSaveTextDocumentParams &/*params*/
    ) -> void {
        // empty
    }

    // notification: $/cancelRequest
    auto BaseLspLanguageServer::receiveCancelRequest(
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
        std::unique_lock<std::mutex> lock(activeMutex);
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
