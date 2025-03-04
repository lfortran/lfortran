#include <chrono>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <memory>
#include <string>
#include <vector>

#include <server/base_lsp_language_server.h>
#include <server/lsp_exception.h>
#include <server/lsp_specification.h>

#ifndef CLI11_HAS_FILESYSTEM
#define CLI11_HAS_FILESYSTEM 0
#endif // CLI11_HAS_FILESYSTEM
#include <bin/CLI11.hpp>

#include <bin/lfortran_command_line_parser.h>
#include <bin/lfortran_lsp_language_server.h>

namespace LCompilers::LanguageServerProtocol {
    namespace lcli = LCompilers::CommandLineInterface;

    LFortranLspLanguageServer::LFortranLspLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        std::size_t numRequestThreads,
        std::size_t numWorkerThreads,
        lsl::Logger &logger,
        const std::string &configSection,
        std::shared_ptr<lsc::LFortranLspConfig> workspaceConfig
    ) : BaseLspLanguageServer(
        incomingMessages,
        outgoingMessages,
        numRequestThreads,
        numWorkerThreads,
        logger,
        configSection,
        std::make_shared<lsc::LFortranLspConfigTransformer>(
            serializer
        ),
        std::move(workspaceConfig)
      )
    {
        optionsByUri.reserve(256);
    }

    auto LFortranLspLanguageServer::diagnosticLevelToLspSeverity(
        diag::Level level
    ) const -> DiagnosticSeverity {
        switch (level) {
        case diag::Level::Error:
            return DiagnosticSeverity::Error;
        case diag::Level::Warning:
            return DiagnosticSeverity::Warning;
        case diag::Level::Note:
            return DiagnosticSeverity::Information;
        case diag::Level::Help:
            return DiagnosticSeverity::Hint;
        default:
            return DiagnosticSeverity::Warning;
        }
    }

    auto LFortranLspLanguageServer::asrSymbolTypeToLspSymbolKind(
        ASR::symbolType symbolType
    ) const -> SymbolKind {
        switch (symbolType) {
        case ASR::symbolType::Module:
            return SymbolKind::Module;
        case ASR::symbolType::Function:
            return SymbolKind::Function;
        case ASR::symbolType::GenericProcedure:
            return SymbolKind::Function;
        case ASR::symbolType::CustomOperator:
            return SymbolKind::Operator;
        case ASR::symbolType::Struct:
            return SymbolKind::Struct;
        case ASR::symbolType::Enum:
            return SymbolKind::Enum;
        case ASR::symbolType::Variable:
            return SymbolKind::Variable;
        case ASR::symbolType::Class:
            return SymbolKind::Class;
        case ASR::symbolType::ClassProcedure:
            return SymbolKind::Method;
        case ASR::symbolType::Template:
            return SymbolKind::TypeParameter;
        default:
            return SymbolKind::Function;
        }
    }

    auto LFortranLspLanguageServer::invalidateConfigCaches() -> void {
        BaseLspLanguageServer::invalidateConfigCaches();
        {
            std::unique_lock<std::shared_mutex> writeLock(optionMutex);
            optionsByUri.clear();
            logger.debug() << "Invalidated compiler options cache." << std::endl;
        }
    }

    auto LFortranLspLanguageServer::getLFortranConfig(
        const DocumentUri &uri
    ) -> const std::shared_ptr<lsc::LFortranLspConfig> {
        return std::static_pointer_cast<lsc::LFortranLspConfig>(
            BaseLspLanguageServer::getConfig(uri)
        );
    }

    auto LFortranLspLanguageServer::getCompilerOptions(
        const LspTextDocument &document
    ) -> const std::shared_ptr<CompilerOptions> {
        const DocumentUri &uri = document.uri();

        std::shared_lock<std::shared_mutex> readLock(optionMutex);
        auto optionIter = optionsByUri.find(uri);
        if (optionIter != optionsByUri.end()) {
            return optionIter->second;
        }

        readLock.unlock();

        const std::shared_ptr<lsc::LFortranLspConfig> config = getLFortranConfig(uri);
        std::vector<std::string> argv(config->compiler.flags);
        argv.push_back(document.path().string());

        lcli::LFortranCommandLineParser parser(argv);
        try {
            parser.parse();
        } catch (const LCompilers::LCompilersException &e) {
            logger.error()
                << "Failed to initialize compiler options for document with uri=\""
                << uri << "\": " << e.what() << std::endl;
            throw LSP_EXCEPTION(ErrorCodes::InvalidParams, e.what());
        }

        CompilerOptions &compilerOptions = parser.opts.compiler_options;
        compilerOptions.continue_compilation = true;

        std::unique_lock<std::shared_mutex> writeLock(configMutex);
        optionIter = optionsByUri.find(uri);
        if (optionIter != optionsByUri.end()) {
            return optionIter->second;
        }

        auto record = optionsByUri.emplace(
            uri,
            std::make_shared<CompilerOptions>(
                std::move(compilerOptions)
            )
        );
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
                    std::shared_ptr<CompilerOptions> compilerOptions =
                        getCompilerOptions(*document);
                    std::vector<LCompilers::error_highlight> highlights =
                        lfortran.showErrors(path, text, *compilerOptions);

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
                    if (trace >= TraceValues::Messages) {
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
                        if (trace >= TraceValues::Verbose) {
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
    ) -> TextDocument_DefinitionResult {
        const DocumentUri &uri = params.textDocument.uri;
        const Position &pos = params.position;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        const std::string &path = document->path().string();
        const std::string &text = document->text();
        // NOTE: Copy the compiler options since we will modify them.
        CompilerOptions compilerOptions = *getCompilerOptions(*document);
        compilerOptions.line = std::to_string(pos.line + 1);  // 0-to-1 index
        compilerOptions.column = std::to_string(pos.character + 1);  // 0-to-1 index
        std::vector<LCompilers::document_symbols> symbols =
            lfortran.lookupName(path, text, compilerOptions);
        TextDocument_DefinitionResult result;
        if (symbols.size() > 0) {
            if (clientSupportsGotoDefinitionLinks) {
                std::unique_ptr<std::vector<DefinitionLink>> links =
                    std::make_unique<std::vector<DefinitionLink>>();
                links->reserve(symbols.size());
                for (const auto &symbol : symbols) {
                    DefinitionLink &link = links->emplace_back();
                    link.targetUri = "file://" + resolve(
                        symbol.filename,
                        compilerOptions
                    ).string();
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
                    location.uri = "file://" + resolve(
                        symbol.filename,
                        compilerOptions
                    ).string();
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

    auto LFortranLspLanguageServer::resolve(
        const std::string &filename,
        const CompilerOptions &compilerOptions
    ) -> fs::path {
        fs::path path = fs::absolute(filename).lexically_normal();
        if (fs::exists(path)) {
            return path;
        }
        for (const fs::path &includeDir : compilerOptions.po.include_dirs) {
            path = fs::absolute(includeDir / filename).lexically_normal();
            if (fs::exists(path)) {
                return path;
            }
        }
        throw LSP_EXCEPTION(
            ErrorCodes::InvalidParams,
            "File not found: " + filename
        );
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
        const DocumentUri &uri = params.textDocument.uri;
        validate(getDocument(uri));
    }

    // notification: "textDocument/didChange"
    auto LFortranLspLanguageServer::receiveTextDocument_didChange(
        DidChangeTextDocumentParams &params
    ) -> void {
        BaseLspLanguageServer::receiveTextDocument_didChange(params);
        const DocumentUri &uri = params.textDocument.uri;
        validate(getDocument(uri));
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
