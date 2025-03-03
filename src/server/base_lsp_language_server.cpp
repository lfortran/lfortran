#include <cctype>
#include <memory>
#include <mutex>
#include <shared_mutex>
#include <stdexcept>
#include <string>

#include <server/base_lsp_language_server.h>
#include <server/lsp_exception.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

namespace LCompilers::LanguageServerProtocol {

    BaseLspLanguageServer::BaseLspLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        std::size_t numRequestThreads,
        std::size_t numWorkerThreads,
        lsl::Logger &logger,
        const std::string &configSection,
        std::shared_ptr<lsc::LspConfigTransformer> lspConfigTransformer,
        std::shared_ptr<lsc::LspConfig> workspaceConfig
    ) : LspLanguageServer(
        incomingMessages,
        outgoingMessages,
        numRequestThreads,
        numWorkerThreads,
        logger,
        configSection
      )
      , lspConfigTransformer(std::move(lspConfigTransformer))
      , workspaceConfig(std::move(workspaceConfig))
    {
        documentsByUri.reserve(256);
        configsByUri.reserve(256);
        pendingConfigsByUri.reserve(256);
        lspConfigsByUri.reserve(256);
        updateLogLevel();
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
            pendingConfigs.emplace(uri, requestId, std::move(promise));
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

    auto BaseLspLanguageServer::invalidateConfigCache() -> void {
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
        InitializeResult result = LspLanguageServer::receiveInitialize(params);

        { // Initialize internal parameters
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

        return result;
    }

    // notification: "initialized"
    auto BaseLspLanguageServer::receiveInitialized(
        InitializedParams &params
    ) -> void {
        LspLanguageServer::receiveInitialized(params);
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

    auto BaseLspLanguageServer::receiveClient_registerCapability(
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
                    documentsByUri.emplace(newUri, std::move(textDocument));
                    documentsByUri.erase(iter);
                }
            }
        }
    }

    auto BaseLspLanguageServer::updateLogLevel() -> void {
        try {
            std::shared_lock<std::shared_mutex> readLock(workspaceMutex);
            if (workspaceConfig) {
                logger.setLevel(workspaceConfig->log.level);
            }
        } catch (std::exception &e) {
            logger.error()
                << "Caught unhandled exception while updating log level: " << e.what()
                << std::endl;
        }
    }

    // notification: "workspace/didChangeConfiguration"
    auto BaseLspLanguageServer::receiveWorkspace_didChangeConfiguration(
        DidChangeConfigurationParams &params
    ) -> void {
        invalidateConfigCache();
        LSPAny &settings = params.settings;
        switch (settings.type()) {
        case LSPAnyType::Object: {
            const LSPObject &object = settings.object();
            auto iter = object.find(configSection);
            if (iter != object.end()) {
                std::shared_ptr<lsc::LspConfig> lspConfig =
                    lspConfigTransformer->anyToLspConfig(*iter->second);
                {
                    std::unique_lock<std::shared_mutex> writeLock(workspaceMutex);
                    workspaceConfig = std::move(lspConfig);
                }
                updateLogLevel();
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
            documentsByUri.emplace(
                uri,
                std::make_shared<LspTextDocument>(uri, languageId, version, text, logger)
            );
        }
    }

    // request: "workspace/configuration"
    auto BaseLspLanguageServer::receiveWorkspace_configuration(
        Workspace_ConfigurationResult &params
    ) -> void {
        for (LSPAny &config : params) {
            std::unique_lock<std::shared_mutex> writeLock(configMutex);
            if (pendingConfigs.empty()) {
                throw LSP_EXCEPTION(
                    ErrorCodes::InternalError,
                    "No record exists of this config being requested. Please file a ticket."
                );
            }
            auto &triple = pendingConfigs.front();
            const DocumentUri &uri = std::get<0>(triple);
            // const int requestId = std::get<1>(triple);
            auto &promise = std::get<2>(triple);
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
            pendingConfigs.pop();
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
        throw LSP_EXCEPTION(
            ErrorCodes::InvalidParams,
            "No document exists with uri=\"" + uri + "\""
        );
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

} // namespace LCompilers::LanguageServerProtocol
