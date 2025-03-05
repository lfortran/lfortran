#pragma once

#include <map>
#include <string>

#include <server/lsp_config.h>
#include <server/lsp_json_serializer.h>
#include <server/lsp_specification.h>
#include <server/lsp_transformer.h>

namespace LCompilers::LanguageServerProtocol::Reporter {
    namespace lsc = LCompilers::LanguageServerProtocol::Config;

    // NOTE: IssueType is an enum from the VSCode sources for the issue dialog.
    enum class IssueType {
        Bug = 0,
        PerformanceIssue = 1,
        FeatureRequest = 2,
    };

    extern const std::map<IssueType, std::string> IssueTypeNames;

    // NOTE: IssueSource is an enum from the VSCode sources for the issue
    // dialog.
    enum class IssueSource {
        VSCode,
        Extension,
        Marketplace,
    };

    extern const std::map<IssueSource, std::string> IssueSourceNames;
    extern const std::map<IssueSource, std::string> IssueSourceValues;

    class LspIssueReporter {
    public:
        LspIssueReporter(
            LspJsonSerializer &serializer,
            LspTransformer &transformer
        );
        virtual ~LspIssueReporter() = default;
        virtual auto title() const -> std::string = 0;
        virtual auto body() const -> std::string = 0;
    protected:
        LspJsonSerializer &serializer;
        LspTransformer &transformer;

        auto escape(std::string &buffer, const std::string &text) const -> void;
    };

    class InternalErrorReporter : public LspIssueReporter {
    public:
        InternalErrorReporter(
            LspJsonSerializer &serializer,
            LspTransformer &transformer,
            const std::string &extensionId,
            const InitializeParams &initializeParams,
            const lsc::LspConfig &config,
            lsc::LspConfigTransformer &configTransformer,
            const std::string &message,
            const std::string &traceId,
            const ResponseError &error
        );

        auto title() const -> std::string override;
        auto body() const -> std::string override;
    private:
        const std::string &extensionId;
        const InitializeParams &initializeParams;
        const lsc::LspConfig &config;
        lsc::LspConfigTransformer &configTransformer;
        const std::string &message;
        const std::string &traceId;
        const ResponseError &error;
    };

} // namespace LCompilers::LanguageServerProtocol::Reporter
