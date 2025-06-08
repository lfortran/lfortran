#include <cctype>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <memory>
#include <shared_mutex>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include <libasr/exception.h>
#include <libasr/stacktrace.h>

#include <server/base_lsp_language_server.h>
#include <server/lsp_exception.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

#ifndef CLI11_HAS_FILESYSTEM
#define CLI11_HAS_FILESYSTEM 0
#endif // CLI11_HAS_FILESYSTEM
#include <bin/CLI11.hpp>

#include <bin/lfortran_accessor.h>
#include <bin/lfortran_command_line_parser.h>
#include <bin/lfortran_lsp_config.h>
#include <bin/lfortran_lsp_language_server.h>
#include <bin/semantic_highlighter.h>

namespace LCompilers::LanguageServerProtocol {
    namespace lc = LCompilers;
    namespace lcli = LCompilers::CommandLineInterface;

    LFortranLspLanguageServer::LFortranLspLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        lsl::Logger &logger,
        const std::string &configSection,
        const std::string &extensionId,
        const std::string &compilerVersion,
        int parentProcessId,
        std::shared_ptr<lsc::LFortranLspConfig> workspaceConfig,
        std::atomic_bool &start,
        std::condition_variable &startChanged,
        std::mutex &startMutex
    ) : BaseLspLanguageServer(
        incomingMessages,
        outgoingMessages,
        logger,
        configSection,
        extensionId,
        compilerVersion,
        parentProcessId,
        std::make_shared<lsc::LFortranLspConfigTransformer>(
            transformer,
            serializer
        ),
        std::move(workspaceConfig),
        start,
        startChanged,
        startMutex
      )
      , logger(logger.having("LFortranLspLanguageServer"))
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
        case ASR::symbolType::ClassProcedure:
            return SymbolKind::Method;
        case ASR::symbolType::Template:
            return SymbolKind::TypeParameter;
        default:
            return SymbolKind::Function;
        }
    }

    auto LFortranLspLanguageServer::asrSymbolTypeToCompletionItemKind(
        ASR::symbolType symbolType
    ) const -> CompletionItemKind {
        switch (symbolType) {
        case ASR::symbolType::Module:
            return CompletionItemKind::Module;
        case ASR::symbolType::Function:
            return CompletionItemKind::Function;
        case ASR::symbolType::GenericProcedure:
            return CompletionItemKind::Function;
        case ASR::symbolType::CustomOperator:
            return CompletionItemKind::Operator;
        case ASR::symbolType::Struct:
            return CompletionItemKind::Struct;
        case ASR::symbolType::Enum:
            return CompletionItemKind::Enum;
        case ASR::symbolType::Variable:
            return CompletionItemKind::Variable;
        case ASR::symbolType::ClassProcedure:
            return CompletionItemKind::Method;
        case ASR::symbolType::Template:
            return CompletionItemKind::TypeParameter;
        default:
            return CompletionItemKind::Function;
        }
    }

    auto LFortranLspLanguageServer::invalidateConfigCaches() -> void {
        BaseLspLanguageServer::invalidateConfigCaches();
        {
            auto writeLock = LSP_WRITE_LOCK(optionMutex, "compiler-options");
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
        LspTextDocument &document
    ) -> const std::shared_ptr<CompilerOptions> {
        auto documentLock = LSP_READ_LOCK(document.mutex(), "document:" + document.uri());
        const DocumentUri &uri = document.uri();

        auto readLock = LSP_READ_LOCK(optionMutex, "compiler-options");
        auto optionIter = optionsByUri.find(uri);
        if (optionIter != optionsByUri.end()) {
            return optionIter->second;
        }

        readLock.unlock();

        documentLock.unlock();
        const std::shared_ptr<lsc::LFortranLspConfig> config = getLFortranConfig(uri);
        documentLock.lock();

        std::vector<std::string> argv(config->compiler.flags);
        argv.push_back(document.path().string());

        lcli::LFortranCommandLineParser parser(argv);
        try {
            parser.parse();
        } catch (...) {
            const std::string message = formatException(
                ("Failed to initialize compiler options for document with "
                 "uri=\"" + uri + "\""),
                std::current_exception()
            );
            logger.error() << message << std::endl;
            throw LSP_EXCEPTION(ErrorCodes::InvalidParams, message);
        }

        CompilerOptions &compilerOptions = parser.opts.compiler_options;
        compilerOptions.continue_compilation = true;
        compilerOptions.use_colors = false;  // disable ANSI terminal colors

        auto writeLock = LSP_WRITE_LOCK(optionMutex, "compiler-options");
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

    auto LFortranLspLanguageServer::formatException(
        const std::string &heading,
        const std::exception_ptr &exception_ptr
    ) const -> std::string {
        try {
            if (exception_ptr) {
                std::rethrow_exception(exception_ptr);
            } else {
                return std::string{heading}.append(": ").append("unknown");
            }
        } catch (const LCompilers::LCompilersException &e) {
            std::vector<LCompilers::StacktraceItem> stacktrace =
                e.stacktrace_addresses();
            get_local_addresses(stacktrace);
            get_local_info(stacktrace);
            return std::string{heading}.append("\n")
                .append(stacktrace2str(stacktrace, LCompilers::stacktrace_depth, false))
                .append(e.name()).append(": ").append(e.msg());
        } catch (...) {
            return BaseLspLanguageServer::formatException(
                heading,
                std::current_exception()
            );
        }
    }

    auto LFortranLspLanguageServer::validate(
        LspTextDocument &document,
        std::atomic_bool &taskIsRunning
    ) -> void {
        const std::string taskType = "validate";
        auto tracer = startRunning(taskType);
        if (!taskIsRunning) {
            logger.trace()  //<- trace instead of debug because this will happen often
                << "Validation canceled before execution."
                << std::endl;
            return;
        }
        const auto start = std::chrono::high_resolution_clock::now();
        auto readLock = LSP_READ_LOCK(document.mutex(), "document:" + document.uri());
        const std::string &path = document.path().string();
        const std::string &text = document.text();
        const std::string &uri = document.uri();
        try {
            // NOTE: These value may have been updated since the validation was
            // requested, but that's okay because we want to validate the latest
            // version anyway:
            int version = document.version();
            try {
                readLock.unlock();
                std::shared_ptr<CompilerOptions> compilerOptions =
                    getCompilerOptions(document);
                readLock.lock();
                logger.trace()
                    << "Getting diagnostics from LFortran for document with URI="
                    << uri << std::endl;
                // NOTE: Lock the logger to add debug statements to stderr within LFortran.
                // std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
                std::vector<lc::error_highlight> highlights =
                    lfortran.showErrors(path, text, *compilerOptions);
                // loggerLock.unlock();

                logger.trace()
                    << "Collected " << highlights.size()
                    << " diagnostics for document with URI=" << uri
                    << std::endl;

                readLock.unlock();
                const std::shared_ptr<lsc::LFortranLspConfig> config =
                    getLFortranConfig(uri);
                readLock.lock();

                unsigned int numProblems = config->maxNumberOfProblems;
                if (highlights.size() < numProblems) {
                    numProblems = highlights.size();
                }

                std::vector<Diagnostic> diagnostics;
                diagnostics.reserve(numProblems);
                for (unsigned int i = 0; i < numProblems; ++i) {
                    const lc::error_highlight &highlight = highlights[i];

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

                if (taskIsRunning) {
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
                            logTraceParams.verbose = "Result: " + toJsonString(any);
                        }
                        sendLogTrace(logTraceParams);
                    }
                    sendTextDocument_publishDiagnostics(params);
                } else {
                    logger.trace()  //<- trace instead of debug because this will happen often
                        << "Validation canceled before publishing results."
                        << std::endl;
                }
            } catch (...) {
                logger.error()
                    << formatException(
                        ("Failed to validate document (uri=\"" + uri + "\")"),
                        std::current_exception()
                    )
                    << std::endl;
            }
        } catch (...) {
            logger.error()
                << formatException(
                    "Failed to read document attributes",
                    std::current_exception()
                )
                << std::endl;
        }
    }

    // request: "initialize"
    auto LFortranLspLanguageServer::receiveInitialize(
        const RequestMessage &request,
        InitializeParams &params
    ) -> InitializeResult {
        InitializeResult result = BaseLspLanguageServer::receiveInitialize(request, params);

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
                if (textDocument.documentSymbol.has_value()) {
                    clientSupportsDocumentSymbols = true;
                    const DocumentSymbolClientCapabilities &documentSymbols =
                        textDocument.documentSymbol.value();
                    clientSupportsHierarchicalDocumentSymbols =
                        documentSymbols.hierarchicalDocumentSymbolSupport.has_value() &&
                        documentSymbols.hierarchicalDocumentSymbolSupport.value();
                }
                clientSupportsHover = textDocument.hover.has_value();
                clientSupportsHighlight = textDocument.documentHighlight.has_value();
                clientSupportsSemanticHighlight = textDocument.semanticTokens.has_value();
                if (textDocument.completion.has_value()) {
                    clientSupportsCodeCompletion = true;
                    const CompletionClientCapabilities &completion =
                        textDocument.completion.value();
                    clientSupportsCodeCompletionContext =
                        completion.contextSupport.has_value()
                        && completion.contextSupport.value();
                }
                clientSupportsFormatting = textDocument.formatting.has_value();
                clientSupportsRangeFormatting = textDocument.rangeFormatting.has_value();
            }
            logger.debug()
                << "clientSupportsGotoDefinition = "
                << clientSupportsGotoDefinition
                << std::endl;
            logger.debug()
                << "clientSupportsGotoDefinitionLinks = "
                << clientSupportsGotoDefinitionLinks
                << std::endl;
            logger.debug()
                << "clientSupportsDocumentSymbols = "
                << clientSupportsDocumentSymbols
                << std::endl;
            logger.debug()
                << "clientSupportsHierarchicalDocumentSymbols = "
                << clientSupportsHierarchicalDocumentSymbols
                << std::endl;
            logger.debug()
                << "clientSupportsHover = "
                << clientSupportsHover
                << std::endl;
            logger.debug()
                << "clientSupportsHighlight = "
                << clientSupportsHighlight
                << std::endl;
            logger.debug()
                << "clientSupportsSemanticHighlight = "
                << clientSupportsSemanticHighlight
                << std::endl;
            logger.debug()
                << "clientSupportsCodeCompletion = "
                << clientSupportsCodeCompletion
                << std::endl;
            logger.debug()
                << "clientSupportsCodeCompletionContext = "
                << clientSupportsCodeCompletionContext
                << std::endl;
            logger.debug()
                << "clientSupportsFormatting = "
                << clientSupportsFormatting
                << std::endl;
            logger.debug()
                << "clientSupportsRangeFormatting = "
                << clientSupportsRangeFormatting
                << std::endl;
        }

        InitializeResult_serverInfo &serverInfo = result.serverInfo.emplace();
        serverInfo.name = source;
        serverInfo.version = compilerVersion;

        ServerCapabilities &capabilities = result.capabilities;

        if (clientSupportsGotoDefinition) {
            ServerCapabilities_definitionProvider &definitionProvider =
                capabilities.definitionProvider.emplace();
            definitionProvider = true;
        }

        {
            ServerCapabilities_renameProvider &renameProvider =
                capabilities.renameProvider.emplace();
            renameProvider = true;
        }

        if (clientSupportsDocumentSymbols) {
            ServerCapabilities_documentSymbolProvider &documentSymbolProvider =
                capabilities.documentSymbolProvider.emplace();
            documentSymbolProvider = true;
        }

        if (clientSupportsHover) {
            ServerCapabilities_hoverProvider &hoverProvider =
                capabilities.hoverProvider.emplace();
            hoverProvider = true;
        }

        if (clientSupportsHighlight) {
            ServerCapabilities_documentHighlightProvider &documentHighlightProvider =
                capabilities.documentHighlightProvider.emplace();
            documentHighlightProvider = true;
        }

        if (clientSupportsSemanticHighlight) {
            auto semanticTokensOptions = std::make_unique<SemanticTokensOptions>();
            SemanticTokensLegend &legend = semanticTokensOptions->legend;
            legend.tokenTypes = {
                "namespace",
                "type",
                "class",
                "enum",
                "interface",
                "struct",
                "typeParameter",
                "parameter",
                "variable",
                "property",
                "enumMember",
                "event",
                "function",
                "method",
                "macro",
                "keyword",
                "modifier",
                "comment",
                "string",
                "number",
                "regexp",
                "operator",
                "decorator"
            };
            legend.tokenModifiers = {
                "declaration",
                "definition",
                "readonly",
                "static",
                "deprecated",
                "abstract",
                "async",
                "modification",
                "documentation",
                "defaultLibrary"
            };
            SemanticTokensOptions_full &full = semanticTokensOptions->full.emplace();
            full = true;
            ServerCapabilities_semanticTokensProvider &semanticTokensProvider =
                capabilities.semanticTokensProvider.emplace();
            semanticTokensProvider = std::move(semanticTokensOptions);
        }

        if (clientSupportsCodeCompletion) {
            CompletionOptions &completionProvider =
                capabilities.completionProvider.emplace();
            completionProvider.triggerCharacters = {"%"};
            completionProvider.resolveProvider = true;
        }

        if (clientSupportsFormatting) {
            ServerCapabilities_documentFormattingProvider &documentFormattingProvider =
                capabilities.documentFormattingProvider.emplace();
            documentFormattingProvider = true;
        }

        if (clientSupportsRangeFormatting) {
            ServerCapabilities_documentRangeFormattingProvider &documentRangeFormattingProvider =
                capabilities.documentRangeFormattingProvider.emplace();
            documentRangeFormattingProvider = true;
        }

        return result;
    }

    auto LFortranLspLanguageServer::receiveTextDocument_definition(
        const RequestMessage &/*request*/,
        DefinitionParams &params
    ) -> TextDocument_DefinitionResult {
        const DocumentUri &uri = params.textDocument.uri;
        const Position &pos = params.position;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        auto readLock = LSP_READ_LOCK(document->mutex(), "document:" + document->uri());
        const std::string &path = document->path().string();
        const std::string &text = document->text();
        // NOTE: Copy the compiler options since we will modify them.
        readLock.unlock();
        CompilerOptions compilerOptions = *getCompilerOptions(*document);
        readLock.lock();
        compilerOptions.line = std::to_string(pos.line + 1);  // 0-to-1 index
        compilerOptions.column = std::to_string(pos.character + 1);  // 0-to-1 index
        logger.trace()
            << "Looking up symbol information for document with URI=" << uri
            << " on line=" << compilerOptions.line
            << ", column=" << compilerOptions.column
            << std::endl;
        // NOTE: Lock the logger to add debug statements to stderr within LFortran.
        // std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
        std::vector<lc::document_symbols> symbols =
            lfortran.lookupName(path, text, compilerOptions);
        // loggerLock.unlock();
        logger.trace()
            << "Found " << symbols.size() << " symbol(s) matching the query."
            << std::endl;
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

    // request: "textDocument/rename"
    auto LFortranLspLanguageServer::receiveTextDocument_rename(
        const RequestMessage &/*request*/,
        RenameParams &params
    ) -> TextDocument_RenameResult {
        const DocumentUri &uri = params.textDocument.uri;
        const Position &pos = params.position;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        auto readLock = LSP_READ_LOCK(document->mutex(), "document:" + document->uri());
        const std::string &path = document->path().string();
        const std::string &text = document->text();
        // NOTE: Copy the compiler options since we will modify them.
        readLock.unlock();
        CompilerOptions compilerOptions = *getCompilerOptions(*document);
        readLock.lock();
        compilerOptions.line = std::to_string(pos.line + 1);  // 0-to-1 index
        compilerOptions.column = std::to_string(pos.character + 1);  // 0-to-1 index
        logger.trace()
            << "Getting all occurrences of symbol from document with URI=" << uri
            << " on line=" << compilerOptions.line
            << ", column=" << compilerOptions.column
            << std::endl;
        // NOTE: Lock the logger to add debug statements to stderr within LFortran.
        // std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
        std::vector<lc::document_symbols> symbols =
            lfortran.getAllOccurrences(path, text, compilerOptions);
        // loggerLock.unlock();
        logger.trace()
            << "Found " << symbols.size() << " symbol(s) matching the query."
            << std::endl;
        TextDocument_RenameResult result;
        if (symbols.size() > 0) {
            std::unique_ptr<WorkspaceEdit> workspaceEdit =
                std::make_unique<WorkspaceEdit>();
            std::map<DocumentUri, std::vector<TextEdit>> &changes =
                workspaceEdit->changes.emplace();
            for (const auto &symbol : symbols) {
                const std::string symbolUri =
                    "file://" + resolve(symbol.filename, compilerOptions).string();
                std::vector<TextEdit> *edits = nullptr;
                auto iter = changes.find(symbolUri);
                if (iter != changes.end()) {
                    edits = &iter->second;
                } else {
                    edits = &changes.emplace_hint(
                        iter,
                        std::piecewise_construct,
                        std::forward_as_tuple(symbolUri),
                        std::forward_as_tuple()
                    )->second;
                }
                TextEdit &edit = edits->emplace_back();
                Position &start = edit.range.start;
                Position &end = edit.range.end;
                start.line = symbol.first_line - 1;  // 1-to-0 index
                start.character = symbol.first_column - 1;  // 1-to-0 index
                end.line = symbol.last_line - 1;  // 1-to-0 index
                end.character = symbol.last_column - 1;  // 1-to-0 index
                edit.newText = params.newName;
            }
            result = std::move(workspaceEdit);
        } else {
            result = nullptr;
        }
        return result;
    }

    // request: "textDocument/documentSymbol"
    auto LFortranLspLanguageServer::receiveTextDocument_documentSymbol(
        const RequestMessage &/*request*/,
        DocumentSymbolParams &params
    ) -> TextDocument_DocumentSymbolResult {
        const DocumentUri &uri = params.textDocument.uri;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        std::shared_ptr<CompilerOptions> compilerOptions =
            getCompilerOptions(*document);
        auto readLock = LSP_READ_LOCK(document->mutex(), "document:" + document->uri());
        const std::string &path = document->path().string();
        const std::string &text = document->text();
        logger.trace()
            << "Looking up all symbols in document with URI=" << uri
            << std::endl;
        // NOTE: Lock the logger to add debug statements to stderr within LFortran.
        // std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
        std::vector<lc::document_symbols> symbols =
            lfortran.getSymbols(path, text, *compilerOptions);
        // loggerLock.unlock();
        logger.trace()
            << "Found " << symbols.size() << " symbol(s) matching the query."
            << std::endl;
        TextDocument_DocumentSymbolResult result;
        if (clientSupportsHierarchicalDocumentSymbols) {
            std::map<
                const lc::document_symbols *,
                std::vector<const lc::document_symbols *>
            > childrenBySymbol;
            std::vector<const lc::document_symbols *> roots;
            roots.reserve(symbols.size());
            for (const auto &symbol : symbols) {
                // Filter on the current document
                if (document->path() == resolve(symbol.filename, *compilerOptions)) {
                    if (symbol.parent_index >= 0) {
                        const lc::document_symbols &parent = symbols.at(symbol.parent_index);
                        std::vector<const lc::document_symbols *> *children = nullptr;
                        auto iter = childrenBySymbol.find(&parent);
                        if (iter != childrenBySymbol.end()) {
                            children = &iter->second;
                        } else {
                            children = &childrenBySymbol.emplace_hint(
                                iter,
                                std::piecewise_construct,
                                std::forward_as_tuple(&parent),
                                std::forward_as_tuple()
                            )->second;
                        }
#ifdef DEBUG
                        if (children == nullptr) {
                            throw LSP_EXCEPTION(
                                ErrorCodes::InternalError,
                                ("Failed to collect children for symbol=\"" +
                                parent->symbol_name + "\" in document with uri=\"" +
                                uri + "\"")
                            );
                        }
#endif // DEBUG
                        children->push_back(&symbol);
                    } else {
                        roots.push_back(&symbol);
                    }
                }
            }
            std::unique_ptr<std::vector<DocumentSymbol>> documentSymbols =
                std::make_unique<std::vector<DocumentSymbol>>();
            for (const lc::document_symbols *root : roots) {
                DocumentSymbol &symbol = documentSymbols->emplace_back();
                init(symbol, root);
                walk(root, symbol, childrenBySymbol);
            }
            result = std::move(documentSymbols);
        } else {
            std::unique_ptr<std::vector<SymbolInformation>> infos =
                std::make_unique<std::vector<SymbolInformation>>();
            for (const auto &symbol : symbols) {
                SymbolInformation &info = infos->emplace_back();
                Location &location = info.location;
                location.uri = "file://" + resolve(
                    symbol.filename,
                    *compilerOptions
                ).string();
                Range &range = location.range;
                Position &start = range.start;
                Position &end = range.end;
                start.line = symbol.first_line - 1;  // 1-to-0 index
                start.character = symbol.first_column - 1;  // 1-to-0 index
                end.line = symbol.last_line - 1;  // 1-to-0 index
                end.character = symbol.last_column;  // (0-to-1 index) + 1
            }
            result = std::move(infos);
        }
        return result;
    }

    // request: "textDocument/hover"
    auto LFortranLspLanguageServer::receiveTextDocument_hover(
        const RequestMessage &/*request*/,
        HoverParams &params
    ) -> TextDocument_HoverResult {
        const DocumentUri &uri = params.textDocument.uri;
        const Position &pos = params.position;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        auto readLock = LSP_READ_LOCK(document->mutex(), "document:" + document->uri());
        const std::string &path = document->path().string();
        const std::string &text = document->text();
        // NOTE: Copy the compiler options since we will modify them.
        readLock.unlock();
        CompilerOptions compilerOptions = *getCompilerOptions(*document);
        readLock.lock();
        compilerOptions.line = std::to_string(pos.line + 1);  // 0-to-1 index
        compilerOptions.column = std::to_string(pos.character + 1);  // 0-to-1 index
        logger.trace()
            << "Looking up symbol information for preview in document with URI=" << uri
            << " on line=" << compilerOptions.line
            << ", column=" << compilerOptions.column
            << std::endl;
        // NOTE: Lock the logger to add debug statements to stderr within LFortran.
        // std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
        std::vector<std::pair<lc::document_symbols, std::string>> previews =
            lfortran.previewSymbol(path, text, compilerOptions);
        // loggerLock.unlock();
        logger.trace()
            << "Found " << previews.size() << " preview(s) matching the query."
            << std::endl;
        TextDocument_HoverResult result;
        if (previews.size() > 0) {
            std::pair<lc::document_symbols, std::string> &pair = previews.front();
            lc::document_symbols &symbol = pair.first;
            std::string &preview = pair.second;

            fs::path symbolPath = resolve(symbol.filename, compilerOptions);
            std::unique_ptr<Hover> hover = std::make_unique<Hover>();

            MarkupContent content;
            content.kind = MarkupKind::Markdown;
            content.value = "```fortran\n";
            content.value.append(preview);
            content.value.append("\n```");

            Hover_contents &contents = hover->contents;
            contents = std::move(content);

            if (symbolPath == document->path()) {
                Range &range = hover->range.emplace();
                Position &start = range.start;
                Position &end = range.end;
                start.line = symbol.first_line - 1;  // 1-to-0 index
                start.character = symbol.first_column - 1;  // 1-to-0 index
                end.line = symbol.last_line - 1;  // 1-to-0 index
                end.character = symbol.last_column;  // (1-to-0 index) + 1
            }

            result = std::move(hover);
        } else {
            result = nullptr;
        }
        return result;
    }

    auto LFortranLspLanguageServer::init(
        DocumentSymbol &lspSymbol,
        const lc::document_symbols *asrSymbol
    ) -> void {
        lspSymbol.name = asrSymbol->symbol_name;
        lspSymbol.kind = asrSymbolTypeToLspSymbolKind(asrSymbol->symbol_type);
        Position &rangeStart = lspSymbol.range.start;
        Position &selectionRangeStart = lspSymbol.selectionRange.start;
        rangeStart.line =
            selectionRangeStart.line =
            asrSymbol->first_line - 1;  // 1-to-0 index
        rangeStart.character =
            selectionRangeStart.character =
            asrSymbol->first_column - 1;  // 1-to-0 index
        Position &rangeEnd = lspSymbol.range.end;
        Position &selectionRangeEnd = lspSymbol.selectionRange.end;
        rangeEnd.line =
            selectionRangeEnd.line =
            asrSymbol->last_line - 1;  // 1-to-0 index
        rangeEnd.character =
            selectionRangeEnd.character =
            asrSymbol->last_column;  // (0-to-1 index) + 1
    }

    auto LFortranLspLanguageServer::walk(
        const lc::document_symbols *root,
        DocumentSymbol &symbol,
        std::map<
            const lc::document_symbols *,
            std::vector<const lc::document_symbols *>
        > &childrenBySymbol
    ) -> void {
        auto iter = childrenBySymbol.find(root);
        if (iter != childrenBySymbol.end()) {
            std::vector<std::unique_ptr<DocumentSymbol>> &children =
                symbol.children.emplace();
            for (const lc::document_symbols *node : iter->second) {
                std::unique_ptr<DocumentSymbol> &child = children.emplace_back(
                    std::make_unique<DocumentSymbol>()
                );
                init(*child, node);
                walk(node, *child, childrenBySymbol);
            }
        }
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

    // request: "textDocument/documentHighlight"
    auto LFortranLspLanguageServer::receiveTextDocument_documentHighlight(
        const RequestMessage &/*request*/,
        DocumentHighlightParams &params
    ) -> TextDocument_DocumentHighlightResult {
        const DocumentUri &uri = params.textDocument.uri;
        const Position &pos = params.position;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        auto readLock = LSP_READ_LOCK(document->mutex(), "document:" + document->uri());
        const std::string &path = document->path().string();
        const std::string &text = document->text();
        // NOTE: Copy the compiler options since we will modify them.
        readLock.unlock();
        CompilerOptions compilerOptions = *getCompilerOptions(*document);
        readLock.lock();
        compilerOptions.line = std::to_string(pos.line + 1);  // 0-to-1 index
        compilerOptions.column = std::to_string(pos.character + 1);  // 0-to-1 index
        logger.trace()
            << "Finding all occurrences of symbol to highlight in document with URI="
            << uri << std::endl;
        // NOTE: Lock the logger to add debug statements to stderr within LFortran.
        // std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
        std::vector<lc::document_symbols> symbols =
            lfortran.getAllOccurrences(path, text, compilerOptions);
        // loggerLock.unlock();
        logger.trace()
            << "Found " << symbols.size() << " symbol(s) matching the query."
            << std::endl;
        TextDocument_DocumentHighlightResult result;
        if (symbols.size() > 0) {
            std::unique_ptr<std::vector<DocumentHighlight>> highlights =
                std::make_unique<std::vector<DocumentHighlight>>();
            highlights->reserve(symbols.size());
            for (const auto &symbol : symbols) {
                // NOTE: Only highlight symbols from the current document
                if (document->path() == resolve(symbol.filename, compilerOptions)) {
                    DocumentHighlight &highlight = highlights->emplace_back();
                    Range &range = highlight.range;
                    Position &start = range.start;
                    Position &end = range.end;
                    start.line = symbol.first_line - 1;  // 1-to-0 index
                    start.character = symbol.first_column - 1;  // 1-to-0 index
                    end.line = symbol.last_line - 1;  // 1-to-0 index
                    end.character = symbol.last_column - 1;  // 1-to-0 index
                }
            }
            result = std::move(highlights);
        } else {
            result = nullptr;
        }
        return result;
    }

    auto LFortranLspLanguageServer::getHighlights(
        LspTextDocument &document
    ) -> std::shared_ptr<std::pair<std::vector<FortranToken>, int>> {
        auto documentLock = LSP_READ_LOCK(document.mutex(), "document:" + document.uri());
        int version = document.version();
        auto readLock = LSP_READ_LOCK(highlightsMutex, "highlights");
        auto iter = highlightsByDocumentId.find(document.id());
        if ((iter != highlightsByDocumentId.end())
            && (iter->second->second == version)) {
            return iter->second;
        }
        readLock.unlock();
        auto writeLock = LSP_WRITE_LOCK(highlightsMutex, "highlights");
        iter = highlightsByDocumentId.find(document.id());
        if ((iter != highlightsByDocumentId.end())
            && (iter->second->second == version)) {
            return iter->second;
        }
        return highlightsByDocumentId.emplace_hint(
            iter,
            document.id(),
            std::make_shared<std::pair<std::vector<FortranToken>, int>>(
                std::make_pair(semantic_tokenize(document.text()), version)
            )
        )->second;
    }

    auto LFortranLspLanguageServer::encodeHighlights(
        std::vector<unsigned int> &encodings,
        LspTextDocument &document,
        std::vector<FortranToken> &highlights
    ) -> void {
        encodings.reserve(5 * highlights.size());
        auto documentLock = LSP_READ_LOCK(document.mutex(), "document:" + document.uri());
        std::size_t prevLine = 0;
        std::size_t prevColumn = 0;
        for (const FortranToken &token : highlights) {
            std::size_t line, column, deltaLine, deltaStart;
            document.fromPosition(line, column, token.position);
            deltaLine = line - prevLine;
            deltaStart = (line == prevLine)
                ? (column - prevColumn)
                : column;
            unsigned int modifiers = 0x0000;
            for (SemanticTokenModifiers modifier : token.modifiers) {
                modifiers |= (1 << static_cast<unsigned int>(modifier));
            }
            encodings.push_back(deltaLine);
            encodings.push_back(deltaStart);
            encodings.push_back(token.length);
            encodings.push_back(static_cast<unsigned int>(token.type));
            encodings.push_back(modifiers);
            prevLine = line;
            prevColumn = column;
        }
    }

    // request: "textDocument/semanticTokens/full"
    auto LFortranLspLanguageServer::receiveTextDocument_semanticTokens_full(
        const RequestMessage &/*request*/,
        SemanticTokensParams &params
    ) -> TextDocument_SemanticTokens_FullResult {
        TextDocument_SemanticTokens_FullResult result;
        if (clientSupportsSemanticHighlight) {
            const std::string &uri = params.textDocument.uri;
            std::shared_ptr<LspTextDocument> document = getDocument(uri);
            auto highlights = getHighlights(*document);
            auto semanticTokens = std::make_unique<SemanticTokens>();
            encodeHighlights(semanticTokens->data, *document, highlights->first);
            result = std::move(semanticTokens);
        } else {
            result = nullptr;
        }
        return result;
    }

    inline auto startsWith(
        const std::string &term,
        const std::string_view &prefix
    ) -> bool {
        return (term.length() >= prefix.length())
            && (term.compare(0, prefix.size(), prefix) == 0);
    }

    // request: "textDocument/completion"
    auto LFortranLspLanguageServer::receiveTextDocument_completion(
        const RequestMessage &/*request*/,
        CompletionParams &params
    ) -> TextDocument_CompletionResult {
        TextDocument_CompletionResult result;
        if (clientSupportsCodeCompletion) {
            const std::string &uri = params.textDocument.uri;
            std::shared_ptr<LspTextDocument> document = getDocument(uri);
            const std::shared_ptr<CompilerOptions> compilerOptions = getCompilerOptions(*document);
            auto readLock = LSP_READ_LOCK(document->mutex(), "document:" + document->uri());
            const std::string &path = document->path().string();
            const std::string &text = document->text();
            logger.trace()
                << "Looking up all symbols in document with URI=" << uri
                << std::endl;
            std::vector<lc::document_symbols> symbols =
                lfortran.getSymbols(path, text, *compilerOptions);
            logger.trace()
                << "Found " << symbols.size() << " symbol(s) matching the query."
                << std::endl;
            std::string_view query = document->symbolAt(
                params.position.line,
                params.position.character
            );
            auto completionItems = std::make_unique<std::vector<CompletionItem>>();
            completionItems->reserve(symbols.size());
            std::unordered_set<std::string> visited;
            visited.reserve(symbols.size());
            for (const lc::document_symbols &symbol : symbols) {
                if (startsWith(symbol.symbol_name, query)
                    && (visited.find(symbol.symbol_name) == visited.end())) {
                    CompletionItem &item = completionItems->emplace_back();
                    item.label = symbol.symbol_name;
                    item.kind = asrSymbolTypeToCompletionItemKind(symbol.symbol_type);
                    visited.insert(symbol.symbol_name);
                }
            }
            result = std::move(completionItems);
        } else {
            result = nullptr;
        }
        return result;
    }

    // request: "completionItem/resolve"
    auto LFortranLspLanguageServer::receiveCompletionItem_resolve(
        const RequestMessage &/*request*/,
        CompletionItem &params
    ) -> CompletionItem_ResolveResult {
        // NOTE: Use this handler to lazily populate code-completion attributes
        // --------------------------------------------------------------------
        // NOTE: `CompletionItem_ResolveResult` is an alias of `CompletionItem`,
        // so returning the params, directly, is acceptable (making this a sort
        // of identity function).
        CompletionItem_ResolveResult &result = params;
        return result;
    }

    // request: "textDocument/formatting"
    auto LFortranLspLanguageServer::receiveTextDocument_formatting(
        const RequestMessage &/*request*/,
        DocumentFormattingParams &params
    ) -> TextDocument_FormattingResult {
        TextDocument_FormattingResult result;
        if (clientSupportsFormatting) {
            const std::string &uri = params.textDocument.uri;
            std::shared_ptr<LspTextDocument> document = getDocument(uri);
            const std::shared_ptr<CompilerOptions> compilerOptions =
                getCompilerOptions(*document);
            auto readLock = LSP_READ_LOCK(document->mutex(), "document:" + document->uri());
            const std::string &text = document->text();
            const std::string &path = document->path().string();
            auto formatted = lfortran.format(
                path,
                text,
                *compilerOptions,
                false,  //-> color
                params.options.tabSize,
                true  //-> indent_unit
            );
            std::vector<TextEdit> edits;
            if (formatted.ok) {
                // TODO: Specify the reformatted document in terms of a diff
                // between the previous text and the formatted text rather than
                // replacing the whole text with the formatted version:
                TextEdit &edit = edits.emplace_back();
                Range &range = edit.range;
                Position &start = range.start;
                Position &end = range.end;
                start.line = 0;
                start.character = 0;
                end.line = static_cast<uinteger_t>(document->lastLine());
                end.character =
                    static_cast<uinteger_t>(document->lastColumn(end.line));
                edit.newText = formatted.result;
            }
            result = std::move(edits);
        } else {
            result = nullptr;
        }
        return result;
    }

    inline auto isNewline(unsigned char c) -> bool {
        return (c == '\r') || (c == '\n');
    }

    auto correctIndentation(
        std::string &fragment,
        const std::string_view &indentation
    ) -> std::string & {
        for (std::size_t i = 1; i < fragment.length(); ++i) {
            if ((fragment[i] == '\n')
                && ((i + 1) < fragment.length())
                && !isNewline(fragment[i + 1])) {
                fragment.insert(i + 1, indentation);
                i += indentation.length();
                if ((fragment[i + 1] == '\r')
                    && ((i + 2) < fragment.length())
                    && (fragment[i + 2] == '\n')) {
                    ++i;
                }
            }
        }
        return fragment;
    }

    // request: "textDocument/rangeFormatting"
    auto LFortranLspLanguageServer::receiveTextDocument_rangeFormatting(
        const RequestMessage &/*request*/,
        DocumentRangeFormattingParams &params
    ) -> TextDocument_RangeFormattingResult {
        TextDocument_RangeFormattingResult result;
        if (clientSupportsRangeFormatting) {
            const std::string &uri = params.textDocument.uri;
            std::shared_ptr<LspTextDocument> document = getDocument(uri);
            CompilerOptions compilerOptions = *getCompilerOptions(*document);
            compilerOptions.interactive = true;
            auto readLock = LSP_READ_LOCK(document->mutex(), "document:" + document->uri());
            const std::string &path = document->path().string();
            const std::string fragment = document->slice(
                params.range.start.line,
                params.range.start.character,
                params.range.end.line,
                params.range.end.character
            );
            auto formatted = lfortran.format(
                path,
                fragment,
                compilerOptions,
                false,  //-> color
                params.options.tabSize,
                true  //-> indent_unit
            );
            std::vector<TextEdit> edits;
            if (formatted.ok) {
                // TODO: Specify the reformatted document in terms of a diff
                // between the previous text and the formatted text rather than
                // replacing the whole text with the formatted version:
                TextEdit &edit = edits.emplace_back();
                edit.range.start.line = params.range.start.line;
                edit.range.start.character = params.range.start.character;
                edit.range.end.line = params.range.end.line;
                edit.range.end.character = params.range.end.character;
                edit.newText = correctIndentation(
                    formatted.result,
                    document->leadingIndentation(params.range.start.line)
                );
            }
            result = std::move(edits);
        } else {
            result = nullptr;
        }
        return result;
    }

    // notification: "workspace/didDeleteFiles"
    auto LFortranLspLanguageServer::receiveWorkspace_didDeleteFiles(
        const NotificationMessage &/*notification*/,
        DeleteFilesParams &/*params*/
    ) -> void {
        auto readLock = LSP_READ_LOCK(documentMutex, "documents");
        for (auto &[uri, document] : documentsByUri) {
            validate(document);
        }
    }

    // notification: "workspace/didChangeConfiguration"
    auto LFortranLspLanguageServer::receiveWorkspace_didChangeConfiguration(
        const NotificationMessage &notification,
        DidChangeConfigurationParams &params
    ) -> void {
        BaseLspLanguageServer::receiveWorkspace_didChangeConfiguration(notification, params);
        auto readLock = LSP_READ_LOCK(documentMutex, "documents");
        for (auto &[uri, document] : documentsByUri) {
            validate(document);
        }
    }

    // notification: "textDocument/didOpen"
    auto LFortranLspLanguageServer::receiveTextDocument_didOpen(
        const NotificationMessage &notification,
        DidOpenTextDocumentParams &params
    ) -> void {
        BaseLspLanguageServer::receiveTextDocument_didOpen(notification, params);
        const DocumentUri &uri = params.textDocument.uri;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        validate(document);
        updateHighlights(document);
    }

    // notification: "textDocument/didChange"
    auto LFortranLspLanguageServer::receiveTextDocument_didChange(
        const NotificationMessage &notification,
        DidChangeTextDocumentParams &params
    ) -> void {
        BaseLspLanguageServer::receiveTextDocument_didChange(notification, params);
        const DocumentUri &uri = params.textDocument.uri;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        validate(document);
        updateHighlights(document);
    }

    // notification: "textDocument/didClose"
    auto LFortranLspLanguageServer::receiveTextDocument_didClose(
        const NotificationMessage &notification,
        DidCloseTextDocumentParams &params
    ) -> void {
        const DocumentUri &uri = params.textDocument.uri;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        {
            auto highlightsLock = LSP_WRITE_LOCK(highlightsMutex, "highlights");
            auto iter = highlightsByDocumentId.find(document->id());
            if (iter != highlightsByDocumentId.end()) {
                highlightsByDocumentId.erase(iter);
            }
        }
        BaseLspLanguageServer::receiveTextDocument_didClose(notification, params);
    }

    // notification: "workspace/didChangeWatchedFiles"
    auto LFortranLspLanguageServer::receiveWorkspace_didChangeWatchedFiles(
        const NotificationMessage &/*notification*/,
        DidChangeWatchedFilesParams &/*params*/
    ) -> void {
        auto readLock = LSP_READ_LOCK(documentMutex, "documents");
        for (auto &[uri, document] : documentsByUri) {
            validate(document);
        }
    }

} // namespace LCompilers::LanguageServerProtocol
