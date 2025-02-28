#pragma once

#include <memory>
#include <shared_mutex>
#include <string>
#include <unordered_map>

#include <libasr/asr.h>
#include <libasr/diagnostics.h>
#include <libasr/utils.h>

#include <server/base_lsp_language_server.h>
#include <server/logger.h>
#include <server/lsp_specification.h>

#include <bin/lfortran_accessor.h>
#include <bin/server/lfortran_lsp_config.h>

namespace LCompilers::LanguageServerProtocol {
    namespace ls = LCompilers::LLanguageServer;
    namespace lsl = LCompilers::LLanguageServer::Logging;
    namespace lsc = LCompilers::LanguageServerProtocol::Config;

    class LFortranLspLanguageServer : public BaseLspLanguageServer {
    public:
        LFortranLspLanguageServer(
            ls::MessageQueue &incomingMessages,
            ls::MessageQueue &outgoingMessages,
            std::size_t numRequestThreads,
            std::size_t numWorkerThreads,
            lsl::Logger &logger,
            const std::string &configSection,
            std::shared_ptr<lsc::LspConfig> workspaceConfig
        );
    protected:

        auto invalidateConfigCache() -> void override;

        // ================= //
        // Incoming Requests //
        // ================= //

        InitializeResult receiveInitialize(
            InitializeParams &params
        ) override;

        // ====================== //
        // Incoming Notifications //
        // ====================== //

        void receiveWorkspace_didDeleteFiles(
            DeleteFilesParams &params
        ) override;

        void receiveWorkspace_didChangeConfiguration(
            DidChangeConfigurationParams &params
        ) override;

        void receiveTextDocument_didOpen(
            DidOpenTextDocumentParams &params
        ) override;

        void receiveTextDocument_didChange(
            DidChangeTextDocumentParams &params
        ) override;

        void receiveWorkspace_didChangeWatchedFiles(
            DidChangeWatchedFilesParams &params
        ) override;

    private:
        const std::string source = "lfortran";
        ls::LFortranAccessor lfortran;
        std::unordered_map<DocumentUri, CompilerOptions> optionsByUri;
        std::shared_mutex optionMutex;

        auto validate(LspTextDocument &document) -> void;
        auto getCompilerOptions(const DocumentUri &uri) -> const CompilerOptions &;

        auto diagnosticLevelToLspSeverity(diag::Level level) const -> DiagnosticSeverity;

        auto asrSymbolTypeToLspSymbolKind(ASR::symbolType symbol_type) const -> SymbolKind;

        auto getLFortranConfig(
            const DocumentUri &uri
        ) -> const std::shared_ptr<lsc::LFortranLspConfig>;
    };

} // namespace LCompilers::LanguageServerProtocol
