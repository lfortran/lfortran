#include <chrono>
#include <cstring>

#include <bin/cli_utils.h>
#include <bin/lsp_utils.h>
#include <bin/server/lfortran_lsp_language_server.h>

namespace LCompilers::LanguageServerProtocol {
    namespace lcli = LCompilers::CommandLineInterface;

    LFortranLspLanguageServer::LFortranLspLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        std::size_t numRequestThreads,
        std::size_t numWorkerThreads,
        lsl::Logger &logger,
        const std::string &configSection
    ) : BaseLspLanguageServer(
            incomingMessages,
            outgoingMessages,
            numRequestThreads,
            numWorkerThreads,
            logger,
            configSection
        )
    {
        optionsByUri.reserve(256);
    }

    auto LFortranLspLanguageServer::invalidateConfigCache() -> void {
        BaseLspLanguageServer::invalidateConfigCache();
        {
            std::unique_lock<std::shared_mutex> writeLock(optionMutex);
            optionsByUri.clear();
        }
        logger.debug() << "Invalidated compiler options cache." << std::endl;
    }

    auto LFortranLspLanguageServer::getCompilerOptions(
        const DocumentUri &uri
    ) -> const CompilerOptions & {
        std::shared_lock<std::shared_mutex> readLock(optionMutex);
        auto optionIter = optionsByUri.find(uri);
        if (optionIter != optionsByUri.end()) {
            return optionIter->second;
        }

        readLock.unlock();

        CompilerOptions compiler_options;
        compiler_options.continue_compilation = true;

        const std::shared_ptr<LSPAny> config = getConfig(uri);
        switch (config->type()) {
        case LSPAnyType::OBJECT_TYPE: {
            const LSPObject &object = config->object();
            auto iter = object.find("compiler");
            if (iter != object.end()) {
                switch (iter->second->type()) {
                case LSPAnyType::OBJECT_TYPE: {
                    const LSPObject &compiler = iter->second->object();
                    if ((iter = compiler.find("flags")) != compiler.end()) {
                        switch (iter->second->type()) {
                        case LSPAnyType::ARRAY_TYPE: {
                            const LSPArray &flags = iter->second->array();
                            int argc = 1 + flags.size();
                            char **argv = new char*[argc];
                            argv[0] = new char[source.length() + 1];
                            std::strcpy(argv[0], source.c_str());
                            bool success = true;
                            std::size_t i;
                            for (i = 0; (i < flags.size()) && success; ++i) {
                                std::string value;
                                const std::unique_ptr<LSPAny> &flag = flags[i];
                                switch (flag->type()) {
                                case LSPAnyType::OBJECT_TYPE: // fallthrough
                                case LSPAnyType::ARRAY_TYPE: {
                                    value = serializer.serialize(*flag);
                                    break;
                                }
                                case LSPAnyType::STRING_TYPE: {
                                    value = transformer.anyToString(*flag);
                                    break;
                                }
                                case LSPAnyType::INTEGER_TYPE: {
                                    integer_t integer = transformer.anyToInteger(*flag);
                                    value = std::to_string(integer);
                                    break;
                                }
                                case LSPAnyType::UINTEGER_TYPE: {
                                    uinteger_t uinteger = transformer.anyToUInteger(*flag);
                                    value = std::to_string(uinteger);
                                    break;
                                }
                                case LSPAnyType::DECIMAL_TYPE: {
                                    decimal_t decimal = transformer.anyToDecimal(*flag);
                                    value = std::to_string(decimal);
                                    break;
                                }
                                case LSPAnyType::BOOLEAN_TYPE: {
                                    boolean_t boolean = transformer.anyToBoolean(*flag);
                                    value = std::to_string(boolean);
                                    break;
                                }
                                case LSPAnyType::NULL_TYPE: {
                                    value = "";
                                    break;
                                }
                                case LSPAnyType::UNINITIALIZED: {
                                    logger.error()
                                        << "Attempted to copy a command-line argument from an uninitialized value."
                                        << std::endl;
                                    success = false;
                                    break;
                                }
                                }
                                if (success) {
                                    argv[i + 1] = new char[value.size() + 1];
                                    std::strcpy(argv[i + 1], value.c_str());
                                }
                            }
                            if (success) {
                                int exitCode = lcli::init_compiler_options(compiler_options, argc, argv);
                                if (exitCode != 0) {
                                    std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
                                    logger.error()
                                        << "Failed to initialize compiler options for document with uri: " << uri
                                        << std::endl;
                                    logger.error()
                                        << "init_compiler_options(...) returned with status: " << exitCode
                                        << std::endl;
                                }
                            }
                            for (std::size_t j = 0; j < i + 1; ++j) {
                                delete[] argv[j];
                            }
                            delete[] argv;
                            break;
                        }
                        default: {
                            logger.error()
                                << "Unsupported type of compiler flags for uri=\"" << uri << "\": "
                                   "LSPAnyType::" << LSPAnyTypeNames.at(iter->second->type())
                                << std::endl;
                        }
                        }
                    }
                    break;
                }
                default: {
                    logger.error()
                        << "Unsupported type of compiler options for uri=\"" << uri << "\": "
                           "LSPAnyType::" << LSPAnyTypeNames.at(iter->second->type())
                        << std::endl;
                }
                }
            }
            break;
        }
        default: {
            logger.error()
                << "Unsupported type of config for uri=\"" << uri << "\": "
                   "LSPAnyType::" << LSPAnyTypeNames.at(config->type())
                << std::endl;
        }
        }

        std::unique_lock<std::shared_mutex> writeLock(configMutex);
        optionIter = optionsByUri.find(uri);
        if (optionIter != optionsByUri.end()) {
            return optionIter->second;
        }

        auto record = optionsByUri.emplace(uri, std::move(compiler_options));
        return record.first->second;
    }

    auto LFortranLspLanguageServer::validate(
        LspTextDocument &document
    ) -> void {
        workerPool.execute([this, &document](
            const std::string &threadName,
            const std::size_t threadId
        ) {
            try {
                const auto start = std::chrono::high_resolution_clock::now();
                // NOTE: These value may have been updated since the validation was
                // requested, but that's okay because we want to validate the latest
                // version anyway:
                std::unique_lock<std::shared_mutex> readLock(document.mutex());
                const std::string &uri = document.uri();
                const std::string &path = document.path().string();
                const std::string &text = document.text();
                int version = document.version();
                try {
                    CompilerOptions compiler_options = getCompilerOptions(uri);
                    std::vector<LCompilers::error_highlight> highlights =
                        lfortran.showErrors(path, text, compiler_options);

                    std::vector<Diagnostic> diagnostics;
                    for (const LCompilers::error_highlight &highlight : highlights) {
                        Position start;
                        start.line = highlight.first_line - 1;
                        start.character = highlight.first_column - 1;

                        Position end;
                        end.line = highlight.last_line - 1;
                        end.character = highlight.last_column;

                        Range range;
                        range.start = std::move(start);
                        range.end = std::move(end);

                        Diagnostic diagnostic;
                        diagnostic.range = std::move(range);
                        diagnostic.severity =
                            diagnostic_level_to_lsp_severity(highlight.severity);
                        diagnostic.message = highlight.message;
                        diagnostic.source = source;

                        diagnostics.push_back(std::move(diagnostic));
                    }

                    PublishDiagnosticsParams params;
                    params.uri = uri;
                    params.version = version;
                    params.diagnostics = std::move(diagnostics);
                    if (trace >= TraceValues::MESSAGES) {
                      const auto end = std::chrono::high_resolution_clock::now();
                      const auto duration =
                          std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
                      LogTraceParams logTraceParams;
                      logTraceParams.message =
                          "Sending response 'textDocument/publishDiagnostics'. Processing request took " +
                          std::to_string(duration.count()) + "ms";
                      if (trace >= TraceValues::VERBOSE) {
                          LSPAny any = transformer.publishDiagnosticsParamsToAny(params);
                          logTraceParams.verbose = "Result: " + serializer.pprint(any);
                      }
                      sendLogTrace(logTraceParams);
                    }
                    sendTextDocument_publishDiagnostics(params);
                } catch (std::exception &e) {
                    logger.error()
                        << "[" << threadName << "_" << threadId << "] "
                        << "Failed to validate document (uri=\""
                        << uri << "\"): " << e.what()
                        << std::endl;
                }
            } catch (std::exception &e) {
                logger.error()
                    << "[" << threadName << "_" << threadId << "] "
                    << "Failed to read document attributes: " << e.what()
                    << std::endl;
            }
        });
    }

    // request: "initialize"
    auto LFortranLspLanguageServer::receiveInitialize(
        InitializeParams &params
    ) -> InitializeResult {
        InitializeResult result = BaseLspLanguageServer::receiveInitialize(params);
        // add additional function for lfortran
        return result;
    }

    // notification: "workspace/didDeleteFiles"
    auto LFortranLspLanguageServer::receiveWorkspace_didDeleteFiles(
        DeleteFilesParams &/*params*/
    ) -> void {
        std::shared_lock<std::shared_mutex> readLock(documentMutex);
        for (auto &[uri, document] : documentsByUri) {
            validate(document);
        }
    }

    // notification: "workspace/didChangeConfiguration"
    auto LFortranLspLanguageServer::receiveWorkspace_didChangeConfiguration(
        DidChangeConfigurationParams &params
    ) -> void {
        BaseLspLanguageServer::receiveWorkspace_didChangeConfiguration(params);
        std::shared_lock<std::shared_mutex> readLock(documentMutex);
        for (auto &[uri, document] : documentsByUri) {
            validate(document);
        }
    }

    // notification: "textDocument/didOpen"
    auto LFortranLspLanguageServer::receiveTextDocument_didOpen(
        DidOpenTextDocumentParams &params
    ) -> void {
        BaseLspLanguageServer::receiveTextDocument_didOpen(params);
        {
            std::shared_lock<std::shared_mutex> readLock(documentMutex);
            const DocumentUri &uri = params.textDocument.uri;
            LspTextDocument &document = documentsByUri.at(uri);
            validate(document);
        }
    }

    // notification: "textDocument/didChange"
    auto LFortranLspLanguageServer::receiveTextDocument_didChange(
        DidChangeTextDocumentParams &params
    ) -> void {
        BaseLspLanguageServer::receiveTextDocument_didChange(params);
        {
            std::shared_lock<std::shared_mutex> readLock(documentMutex);
            const DocumentUri &uri = params.textDocument.uri;
            LspTextDocument &document = documentsByUri.at(uri);
            validate(document);
        }
    }

    // notification: "workspace/didChangeWatchedFiles"
    auto LFortranLspLanguageServer::receiveWorkspace_didChangeWatchedFiles(
        DidChangeWatchedFilesParams &/*params*/
    ) -> void {
        std::shared_lock<std::shared_mutex> readLock(documentMutex);
        for (auto &[uri, document] : documentsByUri) {
            validate(document);
        }
    }

} // namespace LCompilers::LanguageServerProtocol
