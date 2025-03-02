#pragma once

#include <filesystem>
#include <string>
#include <vector>

#include <server/lsp_config.h>
#include <server/lsp_json_serializer.h>
#include <server/lsp_specification.h>
#include <server/lsp_transformer.h>

namespace LCompilers::LanguageServerProtocol::Config {
    namespace fs = std::filesystem;

    namespace lsp = LCompilers::LanguageServerProtocol;

    struct LFortranLspConfig_compiler {
        fs::path path;
        std::vector<std::string> flags;
    };

    struct LFortranLspConfig : public LspConfig {
        bool openIssueReporterOnError;
        unsigned int maxNumberOfProblems;
        LFortranLspConfig_compiler compiler;
    };

    class LFortranLspConfigTransformer : public LspConfigTransformer {
    public:
        LFortranLspConfigTransformer(
            lsp::LspJsonSerializer &serializer
        );

        auto anyToLFortranLspConfig_compiler(
            const lsp::LSPAny &any
        ) const -> LFortranLspConfig_compiler;

        auto anyToLspConfig(
            const lsp::LSPAny &any
        ) const -> std::shared_ptr<LspConfig> override;
    private:
        lsp::LspJsonSerializer &serializer;

        auto makeConfig() const -> std::shared_ptr<LspConfig> override;
    };

} // namespace LCompilers::LanguageServerProtocol::Config
