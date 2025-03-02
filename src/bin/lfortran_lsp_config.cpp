#include <memory>

#include <server/base_lsp_language_server.h>
#include <server/lsp_exception.h>
#include <server/lsp_exception.h>
#include <server/lsp_specification.h>

#include <bin/lfortran_lsp_config.h>

namespace LCompilers::LanguageServerProtocol::Config {

    LFortranLspConfigTransformer::LFortranLspConfigTransformer(
        lsp::LspJsonSerializer &serializer
    ) : LspConfigTransformer()
      , serializer(serializer)
    {
        // empty
    }

    auto LFortranLspConfigTransformer::anyToLFortranLspConfig_compiler(
        const lsp::LSPAny &any
    ) const -> LFortranLspConfig_compiler {
        if (any.type() != LSPAnyType::OBJECT_TYPE) {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                ("LSPAnyType for a "
                 "LFortranLspConfig_compiler"
                 " must be of type LSPAnyType::OBJECT_TYPE"
                 " but received LSPAnyType::" + LSPAnyTypeNames.at(any.type()))
            );
        }

        LFortranLspConfig_compiler compiler{};

        const LSPObject &object = any.object();
        LSPObject::const_iterator iter;

        if ((iter = object.find("path")) != object.end()) {
            const std::string &path = iter->second->string();
            try {
                compiler.path = fs::absolute(path).lexically_normal();
            } catch (std::exception &e) {
                throw LSP_EXCEPTION(ErrorCodes::INVALID_PARAMS, e.what());
            }
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                "Missing required LFortranLspConfig_compiler attribute: path"
            );
        }

        if ((iter = object.find("flags")) != object.end()) {
            for (const auto &flag : iter->second->array()) {
                switch (flag->type()) {
                case LSPAnyType::OBJECT_TYPE: // fallthrough
                case LSPAnyType::ARRAY_TYPE: {
                    compiler.flags.push_back(
                        serializer.serialize(*flag)
                    );
                    break;
                }
                case LSPAnyType::STRING_TYPE: {
                    compiler.flags.push_back(flag->string());
                    break;
                }
                case LSPAnyType::INTEGER_TYPE: {
                    compiler.flags.push_back(
                        std::to_string(flag->integer())
                    );
                    break;
                }
                case LSPAnyType::UINTEGER_TYPE: {
                    compiler.flags.push_back(
                        std::to_string(flag->uinteger())
                    );
                    break;
                }
                case LSPAnyType::DECIMAL_TYPE: {
                    compiler.flags.push_back(
                        std::to_string(flag->decimal())
                    );
                    break;
                }
                case LSPAnyType::BOOLEAN_TYPE: {
                    compiler.flags.push_back(
                        std::to_string(flag->boolean())
                    );
                    break;
                }
                case LSPAnyType::NULL_TYPE: {
                    compiler.flags.push_back("");
                    break;
                }
                case LSPAnyType::UNINITIALIZED: {
                    throw LSP_EXCEPTION(
                        ErrorCodes::INVALID_PARAMS,
                        ("Attempted to copy a command-line argument from an "
                         "uninitialized value.")
                    );
                    break;
                }
                }
            }
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                "Missing required LFortranLspConfig_compiler attribute: flags"
            );
        }

        return compiler;
    }

    auto LFortranLspConfigTransformer::anyToLspConfig(
        const lsp::LSPAny &any
    ) const -> std::shared_ptr<LspConfig> {
        std::shared_ptr<LFortranLspConfig> config =
            std::static_pointer_cast<LFortranLspConfig>(
                LspConfigTransformer::anyToLspConfig(any)
            );

        const LSPObject &object = any.object();
        LSPObject::const_iterator iter;

        if ((iter = object.find("openIssueReporterOnError")) != object.end()) {
            config->openIssueReporterOnError = iter->second->boolean();
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                ("Missing required LFortranLspConfig attribute: "
                 "openIssueReporterOnError")
            );
        }

        if ((iter = object.find("maxNumberOfProblems")) != object.end()) {
            config->maxNumberOfProblems = iter->second->uinteger();
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                ("Missing required LFortranLspConfig attribute: "
                 "maxNumberOfProblems")
            );
        }

        if ((iter = object.find("compiler")) != object.end()) {
            config->compiler = anyToLFortranLspConfig_compiler(*iter->second);
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                "Missing required LFortranLspConfig attribute: compiler"
            );
        }

        return config;
    }

    auto LFortranLspConfigTransformer::makeConfig() const
        -> std::shared_ptr<LspConfig> {
        return std::make_shared<LFortranLspConfig>();
    }

} // namespace LCompilers::LanguageServerProtocol::Config
