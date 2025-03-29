#pragma once

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <functional>
#include <future>
#include <memory>
#include <mutex>
#include <queue>
#include <random>
#include <shared_mutex>
#include <string>
#include <thread>
#include <tuple>
#include <unordered_map>
#include <utility>

#include <server/logger.h>
#include <server/lsp_config.h>
#include <server/lsp_language_server.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

namespace LCompilers::LanguageServerProtocol {
    namespace lsc = LCompilers::LanguageServerProtocol::Config;
    namespace lst = LCompilers::LLanguageServer::Threading;

    using namespace std::chrono_literals;
    typedef std::chrono::system_clock::time_point time_point_t;
    typedef std::chrono::milliseconds milliseconds_t;

    /**
     * Returns the next time_point_t, relative to now, at which to execute the
     * associated cron job.
     */
    typedef std::function<time_point_t()> CronSchedule;

    typedef std::tuple<std::size_t, lst::Task, time_point_t> CronElem;

    struct CronComparator {
        auto operator()(const CronElem &lhs, const CronElem &rhs) -> bool;
    };

    typedef std::priority_queue<
        CronElem,
        std::vector<CronElem>,
        CronComparator
    > CronQueue;

    template <typename T>
    using TTLRecord = std::pair<T, time_point_t>;

    template <typename T>
    struct TTLComparator {
        auto operator()(const TTLRecord<T> &lhs, const TTLRecord<T> &rhs) {
            return lhs.second > rhs.second;
        }
    };

    template <typename T>
    using TTLCache = std::priority_queue<
        TTLRecord<T>,
        std::vector<TTLRecord<T>>,
        TTLComparator<T>
    >;

    typedef std::tuple<
        int,  // requestId
        unsigned int,  // attempt
        milliseconds_t  // last sleep time between attempts
    > RetryRecord;

    class BaseLspLanguageServer : public LspLanguageServer {
    public:
        auto isTerminated() const -> bool override;
    protected:
        BaseLspLanguageServer(
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
        );

        const std::string configSection;
        const std::string extensionId;
        const std::string compilerVersion;
        int parentProcessId;
        lst::ThreadPool requestPool;
        lst::ThreadPool workerPool;
        std::atomic_size_t serialSendId = 0;
        std::atomic_size_t pendingSendId = 0;
        std::condition_variable sent;
        std::mutex sentMutex;
        std::unique_ptr<InitializeParams> _initializeParams;
        std::atomic_bool _initialized = false;
        std::atomic_bool _shutdown = false;
        std::atomic_bool _exit = false;
        std::map<int, RequestMessage> requestsById;
        std::mutex requestMutex;
        std::atomic<TraceValues> trace{TraceValues::Off};
        std::shared_ptr<lsc::LspConfigTransformer> lspConfigTransformer;
        std::unordered_map<DocumentUri, std::shared_ptr<LspTextDocument>> documentsByUri;
        std::shared_mutex documentMutex;
        std::default_random_engine randomEngine;

        std::unordered_map<DocumentUri, std::shared_ptr<LSPAny>> configsByUri;
        std::map<
            DocumentUri,
            std::pair<int, std::shared_future<std::shared_ptr<LSPAny>>>
        > pendingConfigsByUri;
        std::map<
            int,
            std::vector<
                std::pair<
                    DocumentUri,
                    std::promise<std::shared_ptr<LSPAny>>
                >
            >
        > pendingConfigsById;
        std::shared_mutex configMutex;

        std::map<std::string, std::shared_ptr<std::atomic_bool>> activeRequests;
        std::mutex activeMutex;

        const milliseconds_t RECENT_REQUEST_TIMEOUT = 1000ms;
        TTLCache<std::string> recentRequests;
        std::shared_mutex recentMutex;
        auto expireCaches(std::shared_ptr<std::atomic_bool> taskIsRunning) -> void;

        auto ttl(const milliseconds_t &timeout) const -> time_point_t;
        auto randomBetween(
            const milliseconds_t &lower,
            const milliseconds_t &upper
        ) -> milliseconds_t;

        TTLCache<RetryRecord> retryAttempts;
        std::shared_mutex retryMutex;
        auto retryRequests(std::shared_ptr<std::atomic_bool> taskIsRunning) -> void;
        auto cancelRequest(int requestId) -> void;

        std::atomic_size_t serialCronId = 0;
        std::map<std::size_t, CronSchedule> cronSchedules;
        std::shared_mutex scheduleMutex;
        CronQueue cronJobs;
        std::mutex cronMutex;
        auto nextCronId() -> std::size_t;
        auto schedule(lst::Task cronJob, CronSchedule schedule) -> std::size_t;
        auto unschedule(std::size_t cronId) -> bool;
        auto chronicle() -> void;

        auto isProcessRunning(int pid) -> bool;
        auto checkParentProcessId(
            std::shared_ptr<std::atomic_bool> taskIsRunning
        ) -> void;

        template <class Rep, class Period = std::ratio<1>>
        auto schedule(
            lst::Task cronJob,
            std::chrono::duration<Rep, Period> timeout
        ) -> void {
            std::size_t cronId = nextCronId();
            auto now = std::chrono::system_clock::now();
            time_point_t nextTimePoint = now + timeout;
            {
                std::unique_lock<std::mutex> cronLock(cronMutex);
                cronJobs.push(std::make_tuple(cronId, cronJob, nextTimePoint));
            }
        }

        std::shared_ptr<lsc::LspConfig> workspaceConfig;
        std::shared_mutex workspaceMutex;

        std::unordered_map<DocumentUri, std::shared_ptr<lsc::LspConfig>> lspConfigsByUri;
        std::shared_mutex lspConfigMutex;

        std::atomic_bool clientSupportsWorkspaceConfigRequests = false;
        std::atomic_bool clientSupportsWorkspaceConfigChangeNotifications = false;

        // NOTE: By convention and to encourage proper initialization order,
        // move all std::thread declarations to the bottom of the members!
        // See: https://github.com/lfortran/lfortran/issues/6756
        std::thread listener;
        std::thread cron;

        auto nextSendId() -> std::size_t;
        auto isInitialized() const -> bool;
        auto isShutdown() const -> bool;
        auto isRunning() const -> bool;
        auto join() -> void override;
        auto listen() -> void;
        auto to_string(const RequestId &requestId) -> std::string;

        auto toJsonString(const LSPAny &any) -> std::string;
        auto toJsonString(const LSPArray &array) -> std::string;
        auto toJsonString(const LSPObject &object) -> std::string;

        auto initializeParams() const -> const InitializeParams &;
        auto assertInitialized() -> void;
        auto assertRunning() -> void;

        auto prepare(
            std::string &buffer,
            const std::string &response
        ) const -> void override;

        auto send(const RequestMessage &request) -> void override;
        auto send(const std::string &request, std::size_t sendId) -> void;

        auto handle(
            const std::string &incoming,
            std::size_t sendId,
            std::shared_ptr<std::atomic_bool> taskIsRunning
        ) -> void;

        auto dispatch(
            ResponseMessage &response,
            RequestMessage &request
        ) -> void;

        auto dispatch(
            ResponseMessage &response,
            NotificationMessage &notification
        ) -> void;

        auto dispatch(
            ResponseMessage &response,
            std::string &traceId,
            const LSPAny &document
        ) -> void;

        auto logReceiveTrace(
            const std::string &messageType,
            const std::string &traceId,
            const std::optional<MessageParams> &optionalParams
        ) -> void;

        auto logReceiveResponseTrace(
            const std::string &traceId,
            const LSPAny &document
        ) -> void;

        auto logSendResponseTrace(
            const std::string &traceId,
            const std::chrono::time_point<std::chrono::high_resolution_clock> &start,
            const LSPAny &response
        ) -> void;

        auto getDocument(
            const DocumentUri &uri
        ) -> std::shared_ptr<LspTextDocument>;

        auto getConfig(
            const DocumentUri &uri,
            const std::string &configSection
        ) -> const std::shared_ptr<LSPAny>;

        auto getConfig(
            const DocumentUri &uri
        ) -> const std::shared_ptr<lsc::LspConfig>;

        virtual auto invalidateConfigCaches() -> void;
        auto updateWorkspaceConfig(std::shared_ptr<lsc::LspConfig> workspaceConfig) -> void;
        auto updateLogLevel(lsc::LspConfig &workspaceConfig) -> void;
        auto updatePrettyPrintIndentSize(lsc::LspConfig &workspaceConfig) -> void;

        auto sendOpenIssue(
            const std::string &issueTitle,
            const std::string &issueBody
        ) -> void;

        auto receiveInitialize(
            InitializeParams &params
        ) -> InitializeResult override;

        auto receiveSetTrace(SetTraceParams &params) -> void override;

        auto receiveShutdown() -> ShutdownResult override;

        auto receiveClient_registerCapability(
            const RequestId &requestId,
            Client_RegisterCapabilityResult params
        ) -> void override;

        auto receiveInitialized(
            InitializedParams &params
        ) -> void override;

        auto receiveExit() -> void override;

        auto receiveWorkspace_didRenameFiles(
            RenameFilesParams &params
        ) -> void override;

        auto receiveWorkspace_didChangeConfiguration(
            DidChangeConfigurationParams &params
        ) -> void override;

        auto receiveWorkspace_configuration(
            const RequestId &requestId,
            Workspace_ConfigurationResult &params
        ) -> void override;

        auto receiveTextDocument_didOpen(
            DidOpenTextDocumentParams &params
        ) -> void override;

        auto receiveTextDocument_didChange(
            DidChangeTextDocumentParams &params
        ) -> void override;

        auto receiveTextDocument_didClose(
            DidCloseTextDocumentParams &params
        ) -> void override;

        auto receiveTextDocument_didSave(
            DidSaveTextDocumentParams &params
        ) -> void override;

        auto receiveCancelRequest(CancelParams &params) -> void override;

        auto receiveGetDocument(
            GetDocumentParams &params
        ) -> GetDocumentResult override;

    }; // class LspLanguageServer

} // namespace LCompilers::LanguageServerProtocol
