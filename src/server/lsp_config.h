#pragma once

#include <filesystem>
#include <memory>

#include <server/logger.h>
#include <server/lsp_specification.h>

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
        LspConfig_trace trace;
        LspConfig_log log;
    };

    class LspConfigTransformer {
    public:
        LspConfigTransformer() = default;
        virtual ~LspConfigTransformer() = default;

        auto anyToLspConfig_trace(
            const lsp::LSPAny &any
        ) const -> LspConfig_trace;

        auto anyToLspConfig_log(
            const lsp::LSPAny &any
        ) const -> LspConfig_log;

        virtual auto anyToLspConfig(
            const lsp::LSPAny &any
        ) const -> std::shared_ptr<LspConfig>;

    protected:
        virtual auto makeConfig() const -> std::shared_ptr<LspConfig>;
    };

} // namespace LCompilers::LanguageServerProtocol::Config
