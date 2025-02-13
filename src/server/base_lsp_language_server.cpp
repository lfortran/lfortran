#include <cctype>
#include <mutex>
#include <shared_mutex>
#include <stdexcept>

#include <server/specification.h>
#include <server/lsp_exception.h>
#include <server/base_lsp_language_server.h>

namespace LCompilers::LanguageServerProtocol {

    BaseLspLanguageServer::BaseLspLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        std::size_t numRequestThreads,
        std::size_t numWorkerThreads,
        lsl::Logger &logger,
        const std::string &configSection
    ) : LspLanguageServer(
        incomingMessages,
        outgoingMessages,
        numRequestThreads,
        numWorkerThreads,
        logger,
        configSection
      )
    {
        documentsByUri.reserve(256);
        configsByUri.reserve(256);
        pendingConfigsByUri.reserve(256);
    }

    auto BaseLspLanguageServer::getConfig(
        const DocumentUri &uri,
        const std::string &configSection
    ) -> const LSPAny & {
        std::shared_lock<std::shared_mutex> readLock(configMutex);
        auto configIter = configsByUri.find(uri);
        if (configIter != configsByUri.end()) {
            return *configIter->second;
        }

        readLock.unlock();

        std::unique_ptr<ConfigurationItem> item =
            std::make_unique<ConfigurationItem>();
        item->scopeUri = uri;
        item->section = configSection;

        ConfigurationParams params;
        params.items.push_back(std::move(item));

        std::unique_lock<std::shared_mutex> writeLock(configMutex);
        configIter = configsByUri.find(uri);
        if (configIter != configsByUri.end()) {
            return *configIter->second;
        }

        std::shared_future<std::reference_wrapper<std::unique_ptr<LSPAny>>> future;

        auto pendingIter = pendingConfigsByUri.find(uri);
        if (pendingIter != pendingConfigsByUri.end()) {
            future = pendingIter->second.second;
        } else {
            int requestId = sendWorkspace_configuration(params);
            std::promise<std::reference_wrapper<std::unique_ptr<LSPAny>>> promise;
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
                return *future.get().get();
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
        }
        logger.debug() << "Invalidated document configuration cache." << std::endl;
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
            const ClientCapabilities &capabilities = *params.capabilities;
            if (capabilities.workspace.has_value()) {
                const WorkspaceClientCapabilities &workspace =
                    *capabilities.workspace.value();

                clientSupportsWorkspaceConfigurationRequests =
                    workspace.configuration.has_value()
                    && workspace.configuration.value();

                if (workspace.didChangeConfiguration.has_value()) {
                    const DidChangeConfigurationClientCapabilities &didChangeConfiguration =
                        *workspace.didChangeConfiguration.value();
                    clientSupportsWorkspaceDidChangeConfigurationNotifications =
                        didChangeConfiguration.dynamicRegistration.has_value()
                        && didChangeConfiguration.dynamicRegistration.value();
                }
            }
        }

        if (!result.capabilities) {
            result.capabilities = std::make_unique<ServerCapabilities>();
        }
        ServerCapabilities &capabilities = *result.capabilities;

        {
            // ------------------------- //
            // TextDocument Sync Options //
            // ------------------------- //
            ServerCapabilities_textDocumentSync textDocumentSync;
            std::unique_ptr<TextDocumentSyncOptions> textDocumentSyncOptions =
                std::make_unique<TextDocumentSyncOptions>();
            textDocumentSyncOptions->openClose = true;
            textDocumentSyncOptions->change = TextDocumentSyncKind::INCREMENTAL;
            TextDocumentSyncOptions_save save;
            std::unique_ptr<SaveOptions> saveOptions = std::make_unique<SaveOptions>();
            saveOptions->includeText = false;
            save = std::move(saveOptions);
            textDocumentSyncOptions->save = std::move(save);
            textDocumentSync = std::move(textDocumentSyncOptions);
            capabilities.textDocumentSync = std::move(textDocumentSync);
        }

        return result;
    }

    // ====================== //
    // Incoming Notifications //
    // ====================== //

    // notification: "initialized"
    auto BaseLspLanguageServer::receiveInitialized(
        InitializedParams &params
    ) -> void {
        LspLanguageServer::receiveInitialized(params);
        if (clientSupportsWorkspaceDidChangeConfigurationNotifications) {
            const std::string method = "workspace/didChangeConfiguration";

            LSPObject configOptions;
            configOptions.emplace("section", transformer.stringToAny(configSection));

            std::unique_ptr<LSPAny> options = std::make_unique<LSPAny>();
            (*options) = std::move(configOptions);

            std::unique_ptr<Registration> registration =
                std::make_unique<Registration>();
            registration->id = method;
            registration->method = method;
            registration->registerOptions = std::move(options);

            RegistrationParams params;
            params.registrations.push_back(std::move(registration));

            sendClient_registerCapability(params);
        }
    }

    // notification: "workspace/didRenameFiles"
    auto BaseLspLanguageServer::receiveWorkspace_didRenameFiles(
        RenameFilesParams &params
    ) -> void {
        for (const std::unique_ptr<FileRename> &param : params.files) {
            const std::string &oldUri = param->oldUri;
            const std::string &newUri = param->newUri;
            {
                std::shared_lock<std::shared_mutex> readLock(documentMutex);
                auto iter = documentsByUri.find(oldUri);
                if (iter != documentsByUri.end()) {
                    readLock.unlock();
                    std::unique_lock<std::shared_mutex> writeLock(documentMutex);
                    TextDocument &textDocument = iter->second;
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
                LSPAnyType configType = static_cast<LSPAnyType>(workspaceConfig->index());
                switch (configType) {
                case LSPAnyType::OBJECT_TYPE: {
                    const LSPObject &config = std::get<LSPObject>(*workspaceConfig);
                    auto iter = config.find("log");
                    if (iter != config.end()) {
                        LSPAnyType logType = static_cast<LSPAnyType>(iter->second->index());
                        switch (logType) {
                        case LSPAnyType::OBJECT_TYPE: {
                            const LSPObject &logConfig = std::get<LSPObject>(*iter->second);
                            if ((iter = logConfig.find("level")) != logConfig.end()) {
                                LSPAnyType levelType = static_cast<LSPAnyType>(iter->second->index());
                                switch (levelType) {
                                case LSPAnyType::STRING_TYPE: {
                                    const string_t &value = transformer.anyToString(*iter->second);
                                    lsl::Level level = lsl::levelByValue(value);
                                    logger.setLevel(level);
                                    break;
                                }
                                case LSPAnyType::INTEGER_TYPE: {
                                    integer_t value = transformer.anyToInteger(*iter->second);
                                    lsl::Level level = lsl::levelByValue(value);
                                    logger.setLevel(level);
                                    break;
                                }
                                case LSPAnyType::UINTEGER_TYPE: {
                                    uinteger_t value = transformer.anyToUInteger(*iter->second);
                                    lsl::Level level = lsl::levelByValue(static_cast<int>(value));
                                    logger.setLevel(level);
                                    break;
                                }
                                default: {
                                    logger.error()
                                        << "Unable to update log level of type LSPAnyType::"
                                        << LSPAnyTypeNames.at(levelType)
                                        << std::endl;
                                }
                                }
                            } else {
                                logger.error()
                                    << "Config does not have required section: log.level"
                                    << std::endl;
                            }
                            break;
                        }
                        default: {
                            logger.error()
                                << "Unsupported log configuration type: LSPAnyType::"
                                << LSPAnyTypeNames.at(logType)
                                << std::endl;
                        }
                        }
                    } else {
                        logger.error()
                            << "Config does not have required section: log"
                            << std::endl;
                    }
                    break;
                }
                default: {
                    logger.error()
                        << "Cannot update log level from config of type LSPAnyType::"
                        << LSPAnyTypeNames.at(configType)
                        << std::endl;
                }
                }
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
        LSPAny &settings = *params.settings;
        LSPAnyType settingsType = static_cast<LSPAnyType>(settings.index());
        switch (settingsType) {
        case LSPAnyType::OBJECT_TYPE: {
            LSPObject &object = std::get<LSPObject>(settings);
            auto iter = object.find(configSection);
            if (iter != object.end()) {
                {
                    std::unique_lock<std::shared_mutex> writeLock(workspaceMutex);
                    workspaceConfig = transformer.copy(iter->second);
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
                << LSPAnyTypeNames.at(settingsType)
                << std::endl;
        }
        }
    }

    // notification: "textDocument/didOpen"
    auto BaseLspLanguageServer::receiveTextDocument_didOpen(
        DidOpenTextDocumentParams &params
    ) -> void {
        const TextDocumentItem &textDocumentItem = *params.textDocument;
        const DocumentUri &uri = textDocumentItem.uri;
        const std::string &languageId = textDocumentItem.languageId;
        int version = textDocumentItem.version;
        const std::string &text = textDocumentItem.text;
        {
            std::unique_lock<std::shared_mutex> writeLock(documentMutex);
            documentsByUri.emplace(
                std::piecewise_construct,
                std::forward_as_tuple(uri),
                std::forward_as_tuple(uri, languageId, version, text, logger)
            );
        }
    }

    // request: "workspace/configuration"
    auto BaseLspLanguageServer::receiveWorkspace_configuration(
        WorkspaceConfigurationResult &params
    ) -> void {
        for (std::unique_ptr<LSPAny> &config : params) {
            std::unique_lock<std::shared_mutex> writeLock(configMutex);
            if (pendingConfigs.empty()) {
                throw LSP_EXCEPTION(
                    ErrorCodes::INTERNAL_ERROR,
                    "No record exists of this config being requested. Please file a ticket."
                );
            }
            auto &triple = pendingConfigs.front();
            const DocumentUri &uri = std::get<0>(triple);
            // const int requestId = std::get<1>(triple);
            auto &promise = std::get<2>(triple);
            auto record = configsByUri.emplace(uri, std::move(config));
            promise.set_value(std::ref(record.first->second));
            auto pendingIter = pendingConfigsByUri.find(uri);
            if (pendingIter != pendingConfigsByUri.end()) {
                pendingConfigsByUri.erase(pendingIter);
            }
            pendingConfigs.pop();
        }
    }

    // notification: "textDocument/didChange"
    auto BaseLspLanguageServer::receiveTextDocument_didChange(
        DidChangeTextDocumentParams &params
    ) -> void {
        const VersionedTextDocumentIdentifier &versionedDocId = *params.textDocument;
        const DocumentUri &uri = versionedDocId.uri;
        integer_t version = versionedDocId.version;
        {
            std::shared_lock<std::shared_mutex> readLock(documentMutex);
            TextDocument &textDocument = documentsByUri.at(uri);
            readLock.unlock();
            textDocument.apply(params.contentChanges, version);
        }
    }

    // notification: "textDocument/didClose"
    auto BaseLspLanguageServer::receiveTextDocument_didClose(
        DidCloseTextDocumentParams &params
    ) -> void {
        const DocumentUri &uri = params.textDocument->uri;
        {
            std::shared_lock<std::shared_mutex> readLock(documentMutex);
            auto pos = documentsByUri.find(uri);
            if (pos != documentsByUri.end()) {
                readLock.unlock();
                std::unique_lock<std::shared_mutex> writeLock(documentMutex);
                pos = documentsByUri.find(uri);
                if (pos != documentsByUri.end()) {
                    documentsByUri.erase(pos);
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
