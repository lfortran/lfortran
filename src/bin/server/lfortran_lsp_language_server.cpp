#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>

#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

#include <bin/cli_utils.h>
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

    auto LFortranLspLanguageServer::diagnosticLevelToLspSeverity(
        diag::Level level
    ) const -> DiagnosticSeverity {
        switch (level) {
        case diag::Level::Error:
            return DiagnosticSeverity::ERROR;
        case diag::Level::Warning:
            return DiagnosticSeverity::WARNING;
        case diag::Level::Note:
            return DiagnosticSeverity::INFORMATION;
        case diag::Level::Help:
            return DiagnosticSeverity::HINT;
        default:
            return DiagnosticSeverity::WARNING;
        }
    }

    auto LFortranLspLanguageServer::asrSymbolTypeToLspSymbolKind(
        ASR::symbolType symbolType
    ) const -> SymbolKind {
        switch (symbolType) {
        case ASR::symbolType::Module:
            return SymbolKind::MODULE;
        case ASR::symbolType::Function:
            return SymbolKind::FUNCTION;
        case ASR::symbolType::GenericProcedure:
            return SymbolKind::FUNCTION;
        case ASR::symbolType::CustomOperator:
            return SymbolKind::OPERATOR;
        case ASR::symbolType::Struct:
            return SymbolKind::STRUCT;
        case ASR::symbolType::Enum:
            return SymbolKind::ENUM;
        case ASR::symbolType::Variable:
            return SymbolKind::VARIABLE;
        case ASR::symbolType::Class:
            return SymbolKind::CLASS;
        case ASR::symbolType::ClassProcedure:
            return SymbolKind::METHOD;
        case ASR::symbolType::Template:
            return SymbolKind::TYPE_PARAMETER;
        default:
            return SymbolKind::FUNCTION;
        }
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
        std::shared_ptr<LspTextDocument> document
    ) -> void {
        workerPool.execute([this, document = std::move(document)](
            const std::string &threadName,
            const std::size_t threadId
        ) {
            try {
                const auto start = std::chrono::high_resolution_clock::now();
                // NOTE: These value may have been updated since the validation was
                // requested, but that's okay because we want to validate the latest
                // version anyway:
                std::unique_lock<std::shared_mutex> readLock(document->mutex());
                const std::string &uri = document->uri();
                const std::string &path = document->path().string();
                const std::string &text = document->text();
                int version = document->version();
                try {
                    CompilerOptions compilerOptions = getCompilerOptions(uri);
                    std::vector<LCompilers::error_highlight> highlights =
                        lfortran.showErrors(path, text, compilerOptions);

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
                            diagnosticLevelToLspSeverity(highlight.severity);
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
                            std::chrono::duration_cast<std::chrono::milliseconds>(
                                (end - start)
                            );
                        LogTraceParams logTraceParams;
                        logTraceParams.message =
                            "Sending response 'textDocument/publishDiagnostics'. "
                            "Processing request took " +
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
                           "Failed to validate document (uri=\""
                        << uri << "\"): " << e.what()
                        << std::endl;
                }
            } catch (std::exception &e) {
                logger.error()
                    << "[" << threadName << "_" << threadId << "] "
                       "Failed to read document attributes: " << e.what()
                    << std::endl;
            }
        });
    }

    // request: "initialize"
    auto LFortranLspLanguageServer::receiveInitialize(
        InitializeParams &params
    ) -> InitializeResult {
        InitializeResult result = BaseLspLanguageServer::receiveInitialize(params);

        { // Initialize internal parameters
            const ClientCapabilities &capabilities = params.capabilities;
            if (capabilities.textDocument.has_value()) {
                const TextDocumentClientCapabilities &textDocument =
                    capabilities.textDocument.value();
                if (textDocument.definition.has_value()) {
                    clientSupportsGotoDefinition = true;
                    const DefinitionClientCapabilities &definition =
                        textDocument.definition.value();
                    clientSupportsGotoDefinitionLinks =
                        definition.linkSupport.has_value() &&
                        definition.linkSupport.value();
                }
            }
            logger.debug()
                << "clientSupportsGotoDefinition = "
                << clientSupportsGotoDefinition
                << std::endl;
            logger.debug()
                << "clientSupportsGotoDefinitionLinks = "
                << clientSupportsGotoDefinitionLinks
                << std::endl;
        }

        ServerCapabilities &capabilities = result.capabilities;

        if (clientSupportsGotoDefinition) {
            ServerCapabilities_definitionProvider definitionProvider;
            definitionProvider = true;
            capabilities.definitionProvider = std::move(definitionProvider);
        }

        return result;
    }

    auto LFortranLspLanguageServer::receiveTextDocument_definition(
        DefinitionParams &params
    ) -> TextDocumentDefinitionResult {
        const DocumentUri &uri = params.textDocument.uri;
        const Position &pos = params.position;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        const std::string &path = document->path().string();
        const std::string &text = document->text();
        CompilerOptions compilerOptions = getCompilerOptions(uri);
        compilerOptions.line = std::to_string(pos.line + 1);  // 0-to-1 index
        compilerOptions.column = std::to_string(pos.character + 1);  // 0-to-1 index
        std::vector<LCompilers::document_symbols> symbols =
            lfortran.lookupName(path, text, compilerOptions);
        TextDocumentDefinitionResult result;
        if (symbols.size() > 0) {
            if (clientSupportsGotoDefinitionLinks) {
                std::unique_ptr<std::vector<DefinitionLink>> links =
                    std::make_unique<std::vector<DefinitionLink>>();
                links->reserve(symbols.size());
                for (const auto &symbol : symbols) {
                    DefinitionLink &link = links->emplace_back();
                    link.targetUri = uri;
                    Position &targetRangeStart = link.targetRange.start;
                    Position &targetSelectionRangeStart =
                        link.targetSelectionRange.start;
                    targetRangeStart.line =
                        targetSelectionRangeStart.line =
                        symbol.first_line - 1;  // 1-to-0 index
                    targetRangeStart.character =
                        targetSelectionRangeStart.character =
                        symbol.first_column - 1;  // 1-to-0 index
                    Position &targetRangeEnd = link.targetRange.end;
                    Position &targetSelectionRangeEnd =
                        link.targetSelectionRange.end;
                    targetRangeEnd.line =
                        targetSelectionRangeEnd.line =
                        symbol.last_line - 1;  // 1-to-0 index
                    targetRangeEnd.character =
                        targetSelectionRangeEnd.character =
                        symbol.last_column - 1;  // 1-to-0 index
                }
                result = std::move(links);
            } else {
                std::unique_ptr<std::vector<Location>> locations =
                    std::make_unique<std::vector<Location>>();
                locations->reserve(symbols.size());
                for (const auto &symbol : symbols) {
                    Location &location = locations->emplace_back();
                    location.uri = uri;
                    Position &start = location.range.start;
                    Position &end = location.range.end;
                    start.line = symbol.first_line - 1;  // 1-to-0 index
                    start.character = symbol.first_column - 1;  // 1-to-0 index
                    end.line = symbol.last_line - 1;  // 1-to-0 index
                    end.character = symbol.last_column - 1;  // 1-to-0 index
                }
                result = std::move(locations);
            }
        } else {
            result = nullptr;
        }
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
            validate(getDocument(uri));
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
            validate(getDocument(uri));
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
