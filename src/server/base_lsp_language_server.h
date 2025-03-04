#pragma once

#include <future>
#include <memory>
#include <queue>
#include <shared_mutex>
#include <string>
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

    class BaseLspLanguageServer : public LspLanguageServer {
    protected:
        BaseLspLanguageServer(
            ls::MessageQueue &incomingMessages,
            ls::MessageQueue &outgoingMessages,
            std::size_t numRequestThreads,
            std::size_t numWorkerThreads,
            lsl::Logger &logger,
            const std::string &configSection,
            std::shared_ptr<lsc::LspConfigTransformer> lspConfigTransformer,
            std::shared_ptr<lsc::LspConfig> workspaceConfig
        );

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

        auto receiveInitialize(
            InitializeParams &params
        ) -> InitializeResult override;

        auto receiveClient_registerCapability(
            Client_RegisterCapabilityResult params
        ) -> void override;

        auto receiveInitialized(
            InitializedParams &params
        ) -> void override;

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
