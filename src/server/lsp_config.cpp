#include <server/logger.h>
#include <server/lsp_config.h>
#include <server/lsp_exception.h>
#include <server/lsp_specification.h>

namespace LCompilers::LanguageServerProtocol::Config {

    auto LspConfigTransformer::anyToLspConfig_trace(
        const lsp::LSPAny &any
    ) const -> LspConfig_trace {
        if (any.type() != LSPAnyType::OBJECT_TYPE) {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                ("LSPAnyType for a "
                 "LspConfig_trace"
                 " must be of type LSPAnyType::OBJECT_TYPE"
                 " but received LSPAnyType::" + LSPAnyTypeNames.at(any.type()))
            );
        }

        LspConfig_trace trace;

        const LSPObject &object = any.object();
        LSPObject::const_iterator iter;

        if ((iter = object.find("server")) != object.end()) {
            const std::string &server = iter->second->string();
            try {
                trace.server = traceValuesByValue(server);
            } catch (std::exception &e) {
                throw LSP_EXCEPTION(ErrorCodes::INVALID_PARAMS, e.what());
            }
        }

        return trace;
    }

    auto LspConfigTransformer::anyToLspConfig_log(
        const lsp::LSPAny &any
    ) const -> LspConfig_log {
        if (any.type() != LSPAnyType::OBJECT_TYPE) {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                ("LSPAnyType for a "
                 "LspConfig_log"
                 " must be of type LSPAnyType::OBJECT_TYPE"
                 " but received LSPAnyType::" + LSPAnyTypeNames.at(any.type()))
            );
        }

        LspConfig_log log;

        const LSPObject &object = any.object();
        LSPObject::const_iterator iter;

        if ((iter = object.find("path")) != object.end()) {
            const std::string &path = iter->second->string();
            try {
                log.path = fs::absolute(path).lexically_normal();
            } catch (std::exception &e) {
                throw LSP_EXCEPTION(ErrorCodes::INVALID_PARAMS, e.what());
            }
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                "Missing required LspConfig_log attribute: path"
            );
        }

        if ((iter = object.find("level")) != object.end()) {
            const std::string &level = iter->second->string();
            try {
                log.level = lsl::levelByValue(level);
            } catch (std::exception &e) {
                throw LSP_EXCEPTION(ErrorCodes::INVALID_PARAMS, e.what());
            }
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                "Missing required LspConfig_log attribute: level"
            );
        }

        if ((iter = object.find("prettyPrint")) != object.end()) {
            log.prettyPrint = iter->second->boolean();
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                "Missing required LspConfig_log attribute: prettyPrint"
            );
        }

        if ((iter = object.find("indentSize")) != object.end()) {
            log.indentSize = iter->second->uinteger();
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                "Missing required LspConfig_log attribute: indentSize"
            );
        }

        return log;
    }

    auto LspConfigTransformer::anyToLspConfig(
        const lsp::LSPAny &any
    ) const -> std::shared_ptr<LspConfig> {
        if (any.type() != LSPAnyType::OBJECT_TYPE) {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                ("LSPAnyType for a "
                 "LspConfig"
                 " must be of type LSPAnyType::OBJECT_TYPE"
                 " but received LSPAnyType::" + LSPAnyTypeNames.at(any.type()))
            );
        }

        std::shared_ptr<LspConfig> config = makeConfig();

        const LSPObject &object = any.object();
        LSPObject::const_iterator iter;

        if ((iter = object.find("trace")) != object.end()) {
            config->trace = anyToLspConfig_trace(*iter->second);
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                "Missing required LspConfig attribute: trace"
            );
        }

        if ((iter = object.find("log")) != object.end()) {
            config->log = anyToLspConfig_log(*iter->second);
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                "Missing required LspConfig attribute: log"
            );
        }

        return config;
    }

    auto LspConfigTransformer::makeConfig() const
        -> std::shared_ptr<LspConfig> {
        return std::make_shared<LspConfig>();
    }

} // namespace LCompilers::LanguageServerProtocol::Config
