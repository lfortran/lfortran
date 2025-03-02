#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <cstring>
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
        std::shared_ptr<lsc::LspConfig> workspaceConfig
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
            return DiagnosticSeverity::ERROR_TYPE;
        case diag::Level::Warning:
            return DiagnosticSeverity::WARNING_TYPE;
        case diag::Level::Note:
            return DiagnosticSeverity::INFORMATION;
        case diag::Level::Help:
            return DiagnosticSeverity::HINT;
        default:
            return DiagnosticSeverity::WARNING_TYPE;
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
    ) -> const CompilerOptions & {
        const DocumentUri &uri = document.uri();

        std::shared_lock<std::shared_mutex> readLock(optionMutex);
        auto optionIter = optionsByUri.find(uri);
        if (optionIter != optionsByUri.end()) {
            return optionIter->second;
        }

        readLock.unlock();

        const std::shared_ptr<lsc::LFortranLspConfig> config = getLFortranConfig(uri);
        std::vector<std::string> argv(config->compiler.flags);
        argv.push_back(document.path());

        lcli::LFortranCommandLineParser parser(argv);
        try {
            parser.parse();
        } catch (const LCompilers::LCompilersException &e) {
            logger.error()
                << "Failed to initialize compiler options for document with uri=\""
                << uri << "\": " << e.what() << std::endl;
            throw LSP_EXCEPTION(ErrorCodes::INVALID_PARAMS, e.what());
        }

        CompilerOptions &compiler_options = parser.opts.compiler_options;
        compiler_options.continue_compilation = true;

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
                    CompilerOptions compiler_options = getCompilerOptions(document);
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
