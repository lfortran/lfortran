#pragma once

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <future>
#include <memory>
#include <mutex>
#include <queue>
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
            std::shared_ptr<lsc::LspConfigTransformer> lspConfigTransformer,
            std::shared_ptr<lsc::LspConfig> workspaceConfig
        );

        const std::string configSection;
        const std::string extensionId;
        const std::string compilerVersion;
        std::thread listener;
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
        std::unordered_map<int, std::string> callbacksById;
        std::mutex callbackMutex;
        std::atomic<TraceValues> trace{TraceValues::Off};
        std::shared_ptr<lsc::LspConfigTransformer> lspConfigTransformer;
        std::unordered_map<DocumentUri, std::shared_ptr<LspTextDocument>> documentsByUri;
        std::shared_mutex documentMutex;

        std::unordered_map<DocumentUri, std::shared_ptr<LSPAny>> configsByUri;
        std::unordered_map<
            DocumentUri,
            std::pair<int, std::shared_future<std::shared_ptr<LSPAny>>>
        > pendingConfigsByUri;
        std::queue<
            std::tuple<DocumentUri, int, std::promise<std::shared_ptr<LSPAny>>>
        > pendingConfigs;
        std::shared_mutex configMutex;

        std::shared_ptr<lsc::LspConfig> workspaceConfig;
        std::shared_mutex workspaceMutex;

        std::unordered_map<DocumentUri, std::shared_ptr<lsc::LspConfig>> lspConfigsByUri;
        std::shared_mutex lspConfigMutex;

        std::atomic_bool clientSupportsWorkspaceConfigRequests = false;
        std::atomic_bool clientSupportsWorkspaceConfigChangeNotifications = false;

        auto nextSendId() -> std::size_t;
        auto isInitialized() const -> bool;
        auto isShutdown() const -> bool;
        auto isRunning() const -> bool;
        auto join() -> void override;
        auto listen() -> void;
        auto notifySent() -> void;
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

        auto handle(const std::string &incoming, std::size_t sendId) -> void;

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

    }; // class LspLanguageServer

} // namespace LCompilers::LanguageServerProtocol
