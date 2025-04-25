#pragma once

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <future>
#include <memory>
#include <mutex>
#include <shared_mutex>
#include <string>
#include <thread>
#include <unordered_map>
#include <utility>

#include <server/logger.h>
#include <server/lsp_config.h>
#include <server/lsp_language_server.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>
#include <server/process_usage.h>

namespace LCompilers::LanguageServerProtocol {
    namespace ls = LCompilers::LLanguageServer;
    namespace lsc = LCompilers::LanguageServerProtocol::Config;

    using namespace std::chrono_literals;
    typedef std::chrono::system_clock::time_point time_point_t;
    typedef std::chrono::milliseconds milliseconds_t;

    typedef std::map<
        std::string,
        std::map<std::string, time_point_t>
    > RunningHistogram;

    class BaseLspLanguageServer;

    class RunTracer {
    public:
        RunTracer(BaseLspLanguageServer *server, const std::string &taskType);
        ~RunTracer();
        auto stop() -> void;
    private:
        BaseLspLanguageServer *server;
        const std::string &taskType;
        bool stopped{false};
    }; // class RunTracer

    class BaseLspLanguageServer : public LspLanguageServer {
    public:
        auto isTerminated() const -> bool override;
    protected:
        BaseLspLanguageServer(
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
        );

        lsl::Logger logger;
        const std::string configSection;
        const std::string extensionId;
        const std::string compilerVersion;
        int parentProcessId;
        std::atomic_size_t serialSendId = 0;
        std::atomic_size_t pendingSendId = 0;
        std::unique_ptr<InitializeParams> _initializeParams;
        std::atomic_bool _initialized = false;
        std::atomic_bool _shutdown = false;
        std::atomic_bool _exit = false;
        std::atomic<TraceValues> trace{TraceValues::Off};
        std::shared_ptr<lsc::LspConfigTransformer> lspConfigTransformer;
        std::unordered_map<
            DocumentUri,
            std::shared_ptr<LspTextDocument>
        > documentsByUri;
        std::shared_mutex documentMutex;

        ls::ProcessUsage pu;

        // taskType -> threadName -> startTime
        RunningHistogram runningHistogram;
        std::shared_mutex runningMutex;

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

        auto isProcessRunning(int pid) -> bool;
        auto checkParentProcessId() -> void;

        auto nextSendId() -> std::size_t;
        auto isInitialized() const -> bool;
        auto isShutdown() const -> bool;
        auto isRunning() const -> bool;

        virtual auto listen() -> void = 0;

        auto collectMessageQueueTelemetry(
            const std::string &key,
            ls::MessageQueue &queue
        ) -> LSPAny;
        virtual auto collectTelemetry() -> LSPAny;
        auto sendTelemetry() -> void;

        template <typename V>
        auto toAny(const std::map<std::string, V> &map) const -> LSPAny {
            LSPObject object;
            for (const auto &[key, value] : map) {
                object.emplace(key, std::make_unique<LSPAny>(toAny(value)));
            }
            LSPAny any;
            any = std::move(object);
            return any;
        }

        template <typename V>
        auto toAny(const std::unordered_map<std::string, V> &map) const -> LSPAny {
            LSPObject object;
            for (const auto &[key, value] : map) {
                object.emplace(key, std::make_unique<LSPAny>(toAny(value)));
            }
            LSPAny any;
            any = std::move(object);
            return any;
        }

#ifdef DEBUG
        auto toAny(const ls::OwnerRecord &record) const -> LSPAny;
#endif // DEBUG
        auto toAny(const char *value) const -> LSPAny;
        auto toAny(int value) const -> LSPAny;
        auto toAny(const time_point_t &timePoint) const -> LSPAny;
        auto toAny(const std::string &value) const -> LSPAny;
        auto toAny(const LSPAny &any) const -> LSPAny;
        auto toAny(LSPObject &object) const -> LSPAny;
        auto toAny(LSPArray &array) const -> LSPAny;
        auto toAny(std::size_t value) const -> LSPAny;
        auto toAny(double value) const -> LSPAny;
        auto toAny(bool value) const -> LSPAny;

        auto startRunning(const std::string &taskType) -> RunTracer;

        auto stopRunning(const std::string &taskType) -> void;

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
        virtual auto send(const std::string &request, std::size_t sendId) -> void = 0;

        auto handle(
            const std::string &incoming,
            std::size_t sendId,
            std::shared_ptr<std::atomic_bool> taskIsRunning
        ) -> void;

        auto handleRequest(
            const LSPAny &document,
            ResponseMessage &response,
            const std::string &method,
            std::string &traceId,
            std::shared_ptr<std::atomic_bool> taskIsRunning
        ) -> void;

        auto handleNotification(
            const LSPAny &document,
            ResponseMessage &response,
            const std::string &method,
            std::string &traceId,
            std::shared_ptr<std::atomic_bool> taskIsRunning
        ) -> void;

        virtual auto handleResponse(
            const LSPAny &document,
            ResponseMessage &response,
            std::string &traceId,
            std::shared_ptr<std::atomic_bool> taskIsRunning
        ) -> void;

        virtual auto dispatch(
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

        virtual auto getConfig(
            const DocumentUri &uri,
            const std::string &configSection
        ) -> const std::shared_ptr<LSPAny> = 0;

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
            const RequestMessage &request,
            InitializeParams &params
        ) -> InitializeResult override;

        auto receiveSetTrace(
            const NotificationMessage &notification,
            SetTraceParams &params
        ) -> void override;

        auto shutdown() -> bool;
        auto receiveShutdown(
            const RequestMessage &request
        ) -> ShutdownResult override;

        auto receiveClient_registerCapability(
            const RequestMessage &request,
            const ResponseMessage &response,
            Client_RegisterCapabilityResult params
        ) -> void override;

        auto receiveInitialized(
            const NotificationMessage &notification,
            InitializedParams &params
        ) -> void override;

        virtual auto exit() -> bool;
        auto receiveExit(
            const NotificationMessage &notification
        ) -> void override;

        auto receiveWorkspace_didRenameFiles(
            const NotificationMessage &notification,
            RenameFilesParams &params
        ) -> void override;

        auto receiveWorkspace_didChangeConfiguration(
            const NotificationMessage &notification,
            DidChangeConfigurationParams &params
        ) -> void override;

        auto receiveWorkspace_configuration(
            const RequestMessage &request,
            const ResponseMessage &response,
            Workspace_ConfigurationResult &params
        ) -> void override;

        auto receiveTextDocument_didOpen(
            const NotificationMessage &notification,
            DidOpenTextDocumentParams &params
        ) -> void override;

        auto receiveTextDocument_didChange(
            const NotificationMessage &notification,
            DidChangeTextDocumentParams &params
        ) -> void override;

        auto receiveTextDocument_didClose(
            const NotificationMessage &notification,
            DidCloseTextDocumentParams &params
        ) -> void override;

        auto receiveTextDocument_didSave(
            const NotificationMessage &notification,
            DidSaveTextDocumentParams &params
        ) -> void override;

        auto cancelRequest(int requestId) -> void;
        auto receiveCancelRequest(
            const NotificationMessage &notification,
            CancelParams &params
        ) -> void override;

        auto receiveDocument(
            const RequestMessage &request,
            DocumentParams &params
        ) -> DocumentResult override;

        auto receiveTelemetry(
            const RequestMessage &request
        ) -> TelemetryResult override;

        friend class RunTracer;
    }; // class BaseLspLanguageServer

} // namespace LCompilers::LanguageServerProtocol
