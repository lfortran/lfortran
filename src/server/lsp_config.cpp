#include <memory>

#include <server/logger.h>
#include <server/lsp_config.h>
#include <server/lsp_exception.h>
#include <server/lsp_specification.h>

namespace LCompilers::LanguageServerProtocol::Config {

    LspConfigTransformer::LspConfigTransformer(lsp::LspTransformer &transformer)
        : transformer(transformer)
    {
        // empty
    }

    auto LspConfigTransformer::anyToLspConfig_trace(
        const lsp::LSPAny &any
    ) const -> LspConfig_trace {
        if (any.type() != LSPAnyType::Object) {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                ("LSPAnyType for a "
                 "LspConfig_trace"
                 " must be of type LSPAnyType::Object"
                 " but received LSPAnyType::" + LSPAnyTypeNames.at(any.type()))
            );
        }

        LspConfig_trace trace{};

        const LSPObject &object = any.object();
        LSPObject::const_iterator iter;

        if ((iter = object.find("server")) != object.end()) {
            const std::string &server = iter->second->string();
            try {
                trace.server = traceValuesByValue(server);
            } catch (std::exception &e) {
                throw LSP_EXCEPTION(ErrorCodes::InvalidParams, e.what());
            }
        }

        return trace;
    }

    auto LspConfigTransformer::lspConfig_traceToAny(
        const LspConfig_trace &trace
    ) const -> LSPAny {
        LSPAny any;
        LSPObject object;
        object.emplace(
            "server",
            std::make_unique<LSPAny>(
                transformer.traceValuesToAny(trace.server)
            )
        );
        any = std::make_unique<LSPObject>(std::move(object));
        return any;
    }

    auto LspConfigTransformer::anyToLspConfig_log(
        const lsp::LSPAny &any
    ) const -> LspConfig_log {
        if (any.type() != LSPAnyType::Object) {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                ("LSPAnyType for a "
                 "LspConfig_log"
                 " must be of type LSPAnyType::Object"
                 " but received LSPAnyType::" + LSPAnyTypeNames.at(any.type()))
            );
        }

        LspConfig_log log{};

        const LSPObject &object = any.object();
        LSPObject::const_iterator iter;

        if ((iter = object.find("path")) != object.end()) {
            const std::string &path = iter->second->string();
            try {
                log.path = fs::absolute(path).lexically_normal();
            } catch (std::exception &e) {
                throw LSP_EXCEPTION(ErrorCodes::InvalidParams, e.what());
            }
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                "Missing required LspConfig_log attribute: path"
            );
        }

        if ((iter = object.find("level")) != object.end()) {
            const std::string &level = iter->second->string();
            try {
                log.level = lsl::levelByValue(level);
            } catch (std::exception &e) {
                throw LSP_EXCEPTION(ErrorCodes::InvalidParams, e.what());
            }
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                "Missing required LspConfig_log attribute: level"
            );
        }

        if ((iter = object.find("prettyPrint")) != object.end()) {
            log.prettyPrint = iter->second->boolean();
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                "Missing required LspConfig_log attribute: prettyPrint"
            );
        }

        return log;
    }

    auto LspConfigTransformer::lspConfig_logToAny(
        const LspConfig_log &log
    ) const -> LSPAny {
        LSPAny any;
        LSPObject object;
        object.emplace(
            "path",
            std::make_unique<LSPAny>(
                transformer.stringToAny(log.path.string())
            )
        );
        object.emplace(
            "level",
            std::make_unique<LSPAny>(
                transformer.stringToAny(
                    lsl::LevelValues.at(log.level)
                )
            )
        );
        object.emplace(
            "prettyPrint",
            std::make_unique<LSPAny>(
                transformer.booleanToAny(log.prettyPrint)
            )
        );
        any = std::make_unique<LSPObject>(std::move(object));
        return any;
    }

    auto LspConfigTransformer::anyToLspConfig(
        const lsp::LSPAny &any
    ) const -> std::shared_ptr<LspConfig> {
        if (any.type() != LSPAnyType::Object) {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                ("LSPAnyType for an LspConfig must be of type LSPAnyType::Object"
                 " but received LSPAnyType::" + LSPAnyTypeNames.at(any.type()))
            );
        }

        std::shared_ptr<LspConfig> config = makeConfig();

        const LSPObject &object = any.object();
        LSPObject::const_iterator iter;

        if ((iter = object.find("openIssueReporterOnError")) != object.end()) {
            config->openIssueReporterOnError = iter->second->boolean();
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                ("Missing required LFortranLspConfig attribute: "
                 "openIssueReporterOnError")
            );
        }

        if ((iter = object.find("indentSize")) != object.end()) {
            config->indentSize = iter->second->uinteger();
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                "Missing required LspConfig_log attribute: indentSize"
            );
        }

        if ((iter = object.find("trace")) != object.end()) {
            config->trace = anyToLspConfig_trace(*iter->second);
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                "Missing required LspConfig attribute: trace"
            );
        }

        if ((iter = object.find("log")) != object.end()) {
            config->log = anyToLspConfig_log(*iter->second);
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                "Missing required LspConfig attribute: log"
            );
        }

        return config;
    }

    auto LspConfigTransformer::lspConfigToAny(
        const LspConfig &config
    ) const -> LSPAny {
        LSPAny any;
        LSPObject object;
        object.emplace(
            "openIssueReporterOnError",
            std::make_unique<LSPAny>(
                transformer.booleanToAny(config.openIssueReporterOnError)
            )
        );
        object.emplace(
            "trace",
            std::make_unique<LSPAny>(
                lspConfig_traceToAny(config.trace)
            )
        );
        object.emplace(
            "log",
            std::make_unique<LSPAny>(
                lspConfig_logToAny(config.log)
            )
        );
        any = std::make_unique<LSPObject>(std::move(object));
        return any;
    }

    auto LspConfigTransformer::makeConfig() const
        -> std::shared_ptr<LspConfig> {
        return std::make_shared<LspConfig>();
    }

} // namespace LCompilers::LanguageServerProtocol::Config
