#include <server/lsp_issue_reporter.h>

namespace LCompilers::LanguageServerProtocol::Reporter {

    const std::map<IssueType, std::string> IssueTypeNames = {
        {IssueType::Bug, "Bug"},
        {IssueType::PerformanceIssue, "PerformanceIssue"},
        {IssueType::FeatureRequest, "FeatureRequest"},
    };

    const std::map<IssueSource, std::string> IssueSourceNames = {
        {IssueSource::VSCode, "VSCode"},
        {IssueSource::Extension, "Extension"},
        {IssueSource::Marketplace, "Marketplace"},
    };

    const std::map<IssueSource, std::string> IssueSourceValues = {
        {IssueSource::VSCode, "vscode"},
        {IssueSource::Extension, "extension"},
        {IssueSource::Marketplace, "marketplace"},
    };

    LspIssueReporter::LspIssueReporter(
        LspJsonSerializer &serializer,
        LspTransformer &transformer
        ) : serializer(serializer)
          , transformer(transformer)
    {
        // empty
    }

    auto LspIssueReporter::escape(
        std::string &buffer,
        const std::string &text
    ) const -> void {
        for (const unsigned char c : text) {
            switch (c) {
            case '\\': {
                buffer.append("\\\\");
                break;
            }
            case '\n': {
                buffer.append("\\n");
                break;
            }
            case '\t': {
                buffer.append("\\t");
                break;
            }
            case '\b': {
                buffer.append("\\b");
                break;
            }
            case '\r': {
                buffer.append("\\r");
                break;
            }
            case '\f': {
                buffer.append("\\f");
                break;
            }
            default: {
                buffer.push_back(c);
            }
            }
        }
    }

    InternalErrorReporter::InternalErrorReporter(
        LspJsonSerializer &serializer,
        LspTransformer &transformer,
        const std::string &extensionId,
        const std::string &compilerVersion,
        const InitializeParams &initializeParams,
        const lsc::LspConfig &config,
        lsc::LspConfigTransformer &configTransformer,
        const std::string &message,
        const std::string &traceId,
        const ResponseError &error
    ) : LspIssueReporter(serializer, transformer)
      , extensionId(extensionId)
      , compilerVersion(compilerVersion)
      , initializeParams(initializeParams)
      , config(config)
      , configTransformer(configTransformer)
      , message(message)
      , traceId(traceId)
      , error(error)
    {
        // empty
    }

    auto InternalErrorReporter::title() const -> std::string {
        if (traceId.length() > 0) {
            return "[Generated] Internal error while handling " + traceId + ": " + error.message;
        }
        return "[Generated] Internal error: " + error.message;
    }

    auto InternalErrorReporter::body() const -> std::string {
        std::string body{""};
        if (traceId.length() > 0) {
            body.append("Caught internal error while handing: ");
            body.append(traceId);
            body.append("\n");
            body.append("Error Message: ");
            body.append(error.message);
        } else {
            body.append("Caught internal error: ");
            body.append(error.message);
        }
        body.append("\n\n");
        body.append("### LSP/JSON-RPC Message:");
        body.append("\n\n");
        escape(body, message);
        body.append("\n\n");
        body.append("### Additional Information:");
        body.append("\n\n");
        body.append("Please describe what you were doing when the error occurred.");
        if (initializeParams.clientInfo.has_value()) {
            const _InitializeParams_clientInfo &clientInfo =
                initializeParams.clientInfo.value();
            body.append("\n\n");
            body.append("### Client Details:");
            body.append("\n\n");
            body.append("Name: ");
            body.append(clientInfo.name);
            if (clientInfo.version.has_value()) {
                body.append("\n");
                body.append("Version: ");
                body.append(clientInfo.version.value());
            }
        }
        body.append("\n\n");
        body.append("### Extension Details:");
        body.append("\n\n");
        body.append("Compiler Version: ");
        body.append(compilerVersion);
        body.append("\n");
        body.append("Extension Id: ");
        body.append(extensionId);
        body.append("\n\n");
        body.append("### Initialize Parameters:");
        body.append("\n\n");
        body.append(
            serializer.serialize(
                transformer.initializeParamsToAny(initializeParams)
            )
        );
        body.append("\n\n");
        body.append("### Workspace Config:");
        body.append("\n\n");
        body.append(
            serializer.serialize(
                configTransformer.lspConfigToAny(config)
            )
        );
        body.append("\n");
        return body;
    }

} // namespace LCompilers::LanguageServerProtocol::Reporter
