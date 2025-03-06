#pragma once

#include <filesystem>
#include <memory>

#include <server/logger.h>
#include <server/lsp_specification.h>
#include <server/lsp_transformer.h>

namespace LCompilers::LanguageServerProtocol::Config {
    namespace fs = std::filesystem;

    namespace lsl = LCompilers::LLanguageServer::Logging;
    namespace lsp = LCompilers::LanguageServerProtocol;

    struct LspConfig_trace {
        TraceValues server;
    };

    struct LspConfig_log {
        fs::path path;
        lsl::Level level;
        bool prettyPrint;
        unsigned int indentSize;
    };

    struct LspConfig {
        virtual ~LspConfig() = default;
        bool openIssueReporterOnError;
        LspConfig_trace trace;
        LspConfig_log log;
    };

    class LspConfigTransformer {
    public:
        LspConfigTransformer(lsp::LspTransformer &transformer);
        virtual ~LspConfigTransformer() = default;

        auto anyToLspConfig_trace(
            const lsp::LSPAny &any
        ) const -> LspConfig_trace;

        auto lspConfig_traceToAny(
            const LspConfig_trace &trace
        ) const -> LSPAny;

        auto anyToLspConfig_log(
            const lsp::LSPAny &any
        ) const -> LspConfig_log;

        auto lspConfig_logToAny(
            const LspConfig_log &log
        ) const -> LSPAny;

        virtual auto anyToLspConfig(
            const lsp::LSPAny &any
        ) const -> std::shared_ptr<LspConfig>;

        virtual auto lspConfigToAny(
            const LspConfig &config
        ) const -> LSPAny;

    protected:
        lsp::LspTransformer &transformer;

        virtual auto makeConfig() const -> std::shared_ptr<LspConfig>;
    };

} // namespace LCompilers::LanguageServerProtocol::Config
