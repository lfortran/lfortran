#include <chrono>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <memory>
#include <shared_mutex>
#include <string>
#include <vector>

#include <server/base_lsp_language_server.h>
#include <server/lsp_exception.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

#ifndef CLI11_HAS_FILESYSTEM
#define CLI11_HAS_FILESYSTEM 0
#endif // CLI11_HAS_FILESYSTEM
#include <bin/CLI11.hpp>

#include <bin/lfortran_command_line_parser.h>
#include <bin/lfortran_lsp_config.h>
#include <bin/lfortran_lsp_language_server.h>

namespace LCompilers::LanguageServerProtocol {
    namespace lc = LCompilers;
    namespace lcli = LCompilers::CommandLineInterface;

    LFortranLspLanguageServer::LFortranLspLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        std::size_t numRequestThreads,
        std::size_t numWorkerThreads,
        lsl::Logger &logger,
        const std::string &configSection,
        const std::string &extensionId,
        const std::string &compilerVersion,
        int parentProcessId,
        unsigned int seed,
        std::shared_ptr<lsc::LFortranLspConfig> workspaceConfig
    ) : BaseLspLanguageServer(
        incomingMessages,
        outgoingMessages,
        numRequestThreads,
        numWorkerThreads,
        logger,
        configSection,
        extensionId,
        compilerVersion,
        parentProcessId,
        seed,
        std::make_shared<lsc::LFortranLspConfigTransformer>(
            transformer,
            serializer
        ),
        std::move(workspaceConfig)
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
        } catch (const lc::LCompilersException &e) {
            logger.error()
                << "Failed to initialize compiler options for document with uri=\""
                << uri << "\": " << e.what() << std::endl;
            throw LSP_EXCEPTION(ErrorCodes::InvalidParams, e.what());
        }

        CompilerOptions &compilerOptions = parser.opts.compiler_options;
        compilerOptions.continue_compilation = true;
        compilerOptions.use_colors = false;  // disable ANSI terminal colors

        std::unique_lock<std::shared_mutex> writeLock(optionMutex);
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
        const std::string &uri = document->uri();
        std::unique_lock<std::shared_mutex> writeLock(validationMutex);
        auto iter = validationsByUri.find(uri);
        if (iter != validationsByUri.end()) {
            *iter->second = false;
        }
        std::shared_ptr<std::atomic_bool> taskIsRunning =
            workerPool.execute([this, &uri, document = std::move(document)](
                std::shared_ptr<std::atomic_bool> taskIsRunning
            ) {
                if (!*taskIsRunning) {
                    logger.trace()  //<- trace instead of debug because this will happen often
                        << "Validation canceled before execution."
                        << std::endl;
                    return;
                }
                try {
                    const auto start = std::chrono::high_resolution_clock::now();
                    // NOTE: These value may have been updated since the validation was
                    // requested, but that's okay because we want to validate the latest
                    // version anyway:
                    std::shared_lock<std::shared_mutex> readLock(document->mutex());
                    const std::string &path = document->path().string();
                    const std::string &text = document->text();
                    int version = document->version();
                    try {
                        std::shared_ptr<CompilerOptions> compilerOptions =
                            getCompilerOptions(*document);
                        std::vector<lc::error_highlight> highlights =
                            lfortran.showErrors(path, text, *compilerOptions);
                        readLock.unlock();

                        const std::shared_ptr<lsc::LFortranLspConfig> config =
                            getLFortranConfig(uri);
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

                        if (*taskIsRunning) {
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
                    } catch (std::exception &e) {
                        logger.error()
                            << "Failed to validate document (uri=\""
                            << uri << "\"): " << e.what()
                            << std::endl;
                    }
                } catch (std::exception &e) {
                    logger.error()
                        << "Failed to read document attributes: " << e.what()
                        << std::endl;
                }

                std::unique_lock<std::shared_mutex> writeLock(validationMutex);
                auto iter = validationsByUri.find(uri);
                if (iter != validationsByUri.end()) {
                    validationsByUri.erase(iter);
                }
            });
        validationsByUri.insert_or_assign(iter, uri, taskIsRunning);
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
        std::vector<lc::document_symbols> symbols =
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

    // request: "textDocument/rename"
    auto LFortranLspLanguageServer::receiveTextDocument_rename(
        RenameParams &params
    ) -> TextDocument_RenameResult {
        const DocumentUri &uri = params.textDocument.uri;
        const Position &pos = params.position;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        std::shared_lock<std::shared_mutex> documentLock(document->mutex());
        const std::string &path = document->path().string();
        const std::string &text = document->text();
        // NOTE: Copy the compiler options since we will modify them.
        CompilerOptions compilerOptions = *getCompilerOptions(*document);
        documentLock.unlock();
        compilerOptions.line = std::to_string(pos.line + 1);  // 0-to-1 index
        compilerOptions.column = std::to_string(pos.character + 1);  // 0-to-1 index
        std::vector<lc::document_symbols> symbols =
            lfortran.getAllOccurrences(path, text, compilerOptions);
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
                        std::make_tuple(symbolUri),
                        std::make_tuple()
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
        DocumentSymbolParams &params
    ) -> TextDocument_DocumentSymbolResult {
        const DocumentUri &uri = params.textDocument.uri;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        const std::string &path = document->path().string();
        const std::string &text = document->text();
        std::shared_ptr<CompilerOptions> compilerOptions =
            getCompilerOptions(*document);
        std::vector<lc::document_symbols> symbols =
            lfortran.getSymbols(path, text, *compilerOptions);
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
                                std::make_tuple(&parent),
                                std::make_tuple()
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

    auto LFortranLspLanguageServer::receiveTextDocument_hover(
        HoverParams &params
    ) -> TextDocument_HoverResult {
        const DocumentUri &uri = params.textDocument.uri;
        const Position &pos = params.position;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        const std::string &path = document->path().string();
        const std::string &text = document->text();
        // NOTE: Copy the compiler options since we will modify them.
        CompilerOptions compilerOptions = *getCompilerOptions(*document);
        compilerOptions.line = std::to_string(pos.line + 1);  // 0-to-1 index
        compilerOptions.column = std::to_string(pos.character + 1);  // 0-to-1 index
        std::vector<lc::document_symbols> symbols =
            lfortran.lookupName(path, text, compilerOptions);
        TextDocument_HoverResult result;
        if (symbols.size() > 0) {
            lc::document_symbols &symbol = symbols.front();
            fs::path symbolPath = resolve(symbol.filename, compilerOptions);
            std::shared_ptr<LspTextDocument> symbolDocument =
                getDocument("file://" + symbolPath.string());
            const std::string &symbolText = symbolDocument->text();
            std::unique_ptr<Hover> hover = std::make_unique<Hover>();

            std::shared_ptr<lsc::LFortranLspConfig> config = getLFortranConfig(uri);

            MarkupContent content;
            content.kind = MarkupKind::Markdown;
            content.value = "```fortran\n";
            uint32_t first_pos = symbolDocument->toPosition(
                symbol.first_line - 1,
                symbol.first_column - 1
            );
            uint32_t last_pos = symbolDocument->toPosition(
                symbol.last_line - 1,
                symbol.last_column
            );
            uint32_t length = (last_pos - first_pos) + 1;
            std::string preview = symbolText.substr(first_pos, length);
            lc::Result<std::string> formatted = lfortran.format(
                path,
                preview,
                compilerOptions,
                false,  //<- apply ANSI colors
                config->indentSize,
                true  //<- indent units like module bodies
            );
            if (formatted.ok) {
                content.value.append(formatted.result);
            } else {
                content.value.append(preview);
            }
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
        DocumentHighlightParams &params
    ) -> TextDocument_DocumentHighlightResult {
        const DocumentUri &uri = params.textDocument.uri;
        const Position &pos = params.position;
        std::shared_ptr<LspTextDocument> document = getDocument(uri);
        const std::string &path = document->path().string();
        const std::string &text = document->text();
        // NOTE: Copy the compiler options since we will modify them.
        CompilerOptions compilerOptions = *getCompilerOptions(*document);
        compilerOptions.line = std::to_string(pos.line + 1);  // 0-to-1 index
        compilerOptions.column = std::to_string(pos.character + 1);  // 0-to-1 index
        std::vector<lc::document_symbols> symbols =
            lfortran.getAllOccurrences(path, text, compilerOptions);
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
