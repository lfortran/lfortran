#pragma once

#include <atomic>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <shared_mutex>
#include <string>
#include <unordered_map>
#include <utility>

#include <libasr/asr.h>
#include <libasr/diagnostics.h>
#include <libasr/utils.h>

#include <server/base_lsp_language_server.h>
#include <server/logger.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

#include <bin/lfortran_accessor.h>
#include <bin/lfortran_lsp_config.h>
#include <bin/semantic_highlighter.h>

namespace LCompilers::LanguageServerProtocol {
    namespace lc = LCompilers;
    namespace ls = LCompilers::LLanguageServer;
    namespace lsl = LCompilers::LLanguageServer::Logging;
    namespace lsc = LCompilers::LanguageServerProtocol::Config;

    class LFortranLspLanguageServer : virtual public BaseLspLanguageServer {
    protected:
        LFortranLspLanguageServer(
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
        );

        const std::string source = "lfortran";
        lsl::Logger logger;
        ls::LFortranAccessor lfortran;
        std::unordered_map<
            DocumentUri,
            std::shared_ptr<CompilerOptions>
        > optionsByUri;
        std::shared_mutex optionMutex;
        std::map <
            DocumentUri,
            std::shared_ptr<std::atomic_bool>
        > validationsByUri;
        std::shared_mutex validationMutex;

        std::unordered_map<
            std::size_t,
            std::shared_ptr<std::pair<std::vector<FortranToken>, int>>
        > highlightsByDocumentId;
        std::shared_mutex highlightsMutex;

        std::atomic_bool clientSupportsGotoDefinition = false;
        std::atomic_bool clientSupportsGotoDefinitionLinks = false;
        std::atomic_bool clientSupportsDocumentSymbols = false;
        std::atomic_bool clientSupportsHierarchicalDocumentSymbols = false;
        std::atomic_bool clientSupportsHover = false;
        std::atomic_bool clientSupportsHighlight = false;
        std::atomic_bool clientSupportsSemanticHighlight = false;
        std::atomic_bool clientSupportsCodeCompletion = false;
        std::atomic_bool clientSupportsCodeCompletionContext = false;

        auto formatException(
            const std::string &heading,
            const std::exception_ptr &exception_ptr
        ) const -> std::string override;

        virtual auto validate(
            std::shared_ptr<LspTextDocument> document
        ) -> void = 0;

        auto validate(
            LspTextDocument &document,
            std::atomic_bool &taskIsRunning
        ) -> void;

        auto getCompilerOptions(
            LspTextDocument &document
        ) -> const std::shared_ptr<CompilerOptions>;

        auto diagnosticLevelToLspSeverity(
            diag::Level level
        ) const -> DiagnosticSeverity;

        auto asrSymbolTypeToLspSymbolKind(
            ASR::symbolType symbol_type
        ) const -> SymbolKind;

        auto asrSymbolTypeToCompletionItemKind(
            ASR::symbolType symbol_type
        ) const -> CompletionItemKind;

        auto encodeHighlights(
            std::vector<unsigned int> &encodings,
            LspTextDocument &document,
            std::vector<FortranToken> &highlights
        ) -> void;

        auto getHighlights(
            LspTextDocument &document
        ) -> std::shared_ptr<std::pair<std::vector<FortranToken>, int>>;

        virtual auto updateHighlights(
            std::shared_ptr<LspTextDocument> document
        ) -> void = 0;

        auto getLFortranConfig(
            const DocumentUri &uri
        ) -> const std::shared_ptr<lsc::LFortranLspConfig>;

        auto resolve(
            const std::string &filename,
            const CompilerOptions &compilerOptions
        ) -> fs::path;

        auto init(
            DocumentSymbol &lspSymbol,
            const lc::document_symbols *asrSymbol
        ) -> void;

        auto walk(
            const lc::document_symbols *root,
            DocumentSymbol &symbol,
            std::map<
                const lc::document_symbols *,
                std::vector<const lc::document_symbols *>
            > &childrenBySymbol
        ) -> void;

        auto invalidateConfigCaches() -> void override;

        // ================= //
        // Incoming Requests //
        // ================= //

        auto receiveInitialize(
            const RequestMessage &request,
            InitializeParams &params
        ) -> InitializeResult override;

        auto receiveTextDocument_definition(
            const RequestMessage &request,
            DefinitionParams &params
        ) -> TextDocument_DefinitionResult override;

        auto receiveTextDocument_rename(
            const RequestMessage &request,
            RenameParams &params
        ) -> TextDocument_RenameResult override;

        auto receiveTextDocument_documentSymbol(
            const RequestMessage &request,
            DocumentSymbolParams &params
        ) -> TextDocument_DocumentSymbolResult override;

        auto receiveTextDocument_hover(
            const RequestMessage &request,
            HoverParams &params
        ) -> TextDocument_HoverResult override;

        auto receiveTextDocument_documentHighlight(
            const RequestMessage &request,
            DocumentHighlightParams &params
        ) -> TextDocument_DocumentHighlightResult override;

        auto receiveTextDocument_semanticTokens_full(
            const RequestMessage &request,
            SemanticTokensParams &params
        ) -> TextDocument_SemanticTokens_FullResult override;

        auto receiveTextDocument_completion(
            const RequestMessage &request,
            CompletionParams &params
        ) -> TextDocument_CompletionResult override;

        auto receiveCompletionItem_resolve(
            const RequestMessage &request,
            CompletionItem &params
        ) -> CompletionItem_ResolveResult override;

        // ====================== //
        // Incoming Notifications //
        // ====================== //

        auto receiveWorkspace_didDeleteFiles(
            const NotificationMessage &notification,
            DeleteFilesParams &params
        ) -> void override;

        auto receiveWorkspace_didChangeConfiguration(
            const NotificationMessage &notification,
            DidChangeConfigurationParams &params
        ) -> void override;

        auto receiveTextDocument_didOpen(
            const NotificationMessage &notification,
            DidOpenTextDocumentParams &params
        ) -> void override;

        auto receiveTextDocument_didChange(
            const NotificationMessage &notification,
            DidChangeTextDocumentParams &params
        ) -> void override;

        auto receiveTextDocument_didClose(
            const NotificationMessage &notification,
            DidCloseTextDocumentParams &params
        ) -> void override;

        auto receiveWorkspace_didChangeWatchedFiles(
            const NotificationMessage &notification,
            DidChangeWatchedFilesParams &params
        ) -> void override;
    };

} // namespace LCompilers::LanguageServerProtocol
