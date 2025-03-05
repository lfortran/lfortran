#include <memory>

#include <server/base_lsp_language_server.h>
#include <server/lsp_config.h>
#include <server/lsp_exception.h>
#include <server/lsp_exception.h>
#include <server/lsp_specification.h>

#include <bin/lfortran_lsp_config.h>

namespace LCompilers::LanguageServerProtocol::Config {

    LFortranLspConfigTransformer::LFortranLspConfigTransformer(
        lsp::LspTransformer &transformer,
        lsp::LspJsonSerializer &serializer
    ) : LspConfigTransformer(transformer)
      , serializer(serializer)
    {
        // empty
    }

    auto LFortranLspConfigTransformer::anyToLFortranLspConfig_compiler(
        const lsp::LSPAny &any
    ) const -> LFortranLspConfig_compiler {
        if (any.type() != LSPAnyType::Object) {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                ("LSPAnyType for a "
                 "LFortranLspConfig_compiler"
                 " must be of type LSPAnyType::OBJECT"
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
                throw LSP_EXCEPTION(ErrorCodes::InvalidParams, e.what());
            }
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                "Missing required LFortranLspConfig_compiler attribute: path"
            );
        }

        if ((iter = object.find("flags")) != object.end()) {
            for (const auto &flag : iter->second->array()) {
                switch (flag->type()) {
                case LSPAnyType::Object: // fallthrough
                case LSPAnyType::Array: {
                    compiler.flags.push_back(
                        serializer.serialize(*flag)
                    );
                    break;
                }
                case LSPAnyType::String: {
                    compiler.flags.push_back(flag->string());
                    break;
                }
                case LSPAnyType::Integer: {
                    compiler.flags.push_back(
                        std::to_string(flag->integer())
                    );
                    break;
                }
                case LSPAnyType::UInteger: {
                    compiler.flags.push_back(
                        std::to_string(flag->uinteger())
                    );
                    break;
                }
                case LSPAnyType::Decimal: {
                    compiler.flags.push_back(
                        std::to_string(flag->decimal())
                    );
                    break;
                }
                case LSPAnyType::Boolean: {
                    compiler.flags.push_back(
                        std::to_string(flag->boolean())
                    );
                    break;
                }
                case LSPAnyType::Null: {
                    compiler.flags.push_back("");
                    break;
                }
                case LSPAnyType::Uninitialized: {
                    throw LSP_EXCEPTION(
                        ErrorCodes::InvalidParams,
                        ("Attempted to copy a command-line argument from an "
                         "uninitialized value.")
                    );
                    break;
                }
                }
            }
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                "Missing required LFortranLspConfig_compiler attribute: flags"
            );
        }

        return compiler;
    }

    auto LFortranLspConfigTransformer::lfortranLspConfig_compilerToAny(
        const LFortranLspConfig_compiler &compiler
    ) const -> LSPAny {
        LSPAny any;
        LSPObject object;
        object.emplace(
            "path",
            std::make_unique<LSPAny>(
                transformer.stringToAny(compiler.path.string())
            )
        );
        {
            LSPArray array;
            array.reserve(compiler.flags.size());
            for (const std::string &flag : compiler.flags) {
                array.push_back(
                    std::make_unique<LSPAny>(
                        transformer.stringToAny(flag)
                    )
                );
            }
            object.emplace(
                "flags",
                std::make_unique<LSPAny>(
                    transformer.lspArrayToAny(array)
                )
            );
        }
        any = std::make_unique<LSPObject>(std::move(object));
        return any;
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

        if ((iter = object.find("maxNumberOfProblems")) != object.end()) {
            config->maxNumberOfProblems = iter->second->uinteger();
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                ("Missing required LFortranLspConfig attribute: "
                 "maxNumberOfProblems")
            );
        }

        if ((iter = object.find("compiler")) != object.end()) {
            config->compiler = anyToLFortranLspConfig_compiler(*iter->second);
        } else {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                "Missing required LFortranLspConfig attribute: compiler"
            );
        }

        return config;
    }

    auto LFortranLspConfigTransformer::lspConfigToAny(
        const LspConfig &config
    ) const -> LSPAny {
        LSPAny any = LspConfigTransformer::lspConfigToAny(config);
        const LFortranLspConfig &lfortran =
            static_cast<const LFortranLspConfig &>(config);
        LSPObject &object = const_cast<LSPObject &>(any.object());
        object.emplace(
            "maxNumberOfProblems",
            std::make_unique<LSPAny>(
                transformer.uintegerToAny(lfortran.maxNumberOfProblems)
            )
        );
        object.emplace(
            "compiler",
            std::make_unique<LSPAny>(
                lfortranLspConfig_compilerToAny(lfortran.compiler)
            )
        );
        return any;
    }

    auto LFortranLspConfigTransformer::makeConfig() const
        -> std::shared_ptr<LspConfig> {
        return std::make_shared<LFortranLspConfig>();
    }

} // namespace LCompilers::LanguageServerProtocol::Config
