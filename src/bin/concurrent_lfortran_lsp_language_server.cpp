#include <chrono>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <string>

#include <server/lsp_exception.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

#include <bin/lfortran_command_line_parser.h>
#include <bin/lfortran_lsp_config.h>
#include <bin/concurrent_lfortran_lsp_language_server.h>

namespace LCompilers::LanguageServerProtocol {

    ConcurrentLFortranLspLanguageServer::ConcurrentLFortranLspLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        lsl::Logger &logger,
        const std::string &configSection,
        const std::string &extensionId,
        const std::string &compilerVersion,
        int parentProcessId,
        std::shared_ptr<lsc::LFortranLspConfig> workspaceConfig
    ) : BaseLspLanguageServer(
        incomingMessages,
        outgoingMessages,
        logger,
        configSection,
        extensionId,
        compilerVersion,
        parentProcessId,
        std::make_shared<lsc::LFortranLspConfigTransformer>(
            transformer,
            serializer
        ),
        std::move(workspaceConfig)
      )
      , ConcurrentLspLanguageServer(
        incomingMessages,
        outgoingMessages,
        logger,
        configSection,
        extensionId,
        compilerVersion,
        parentProcessId,
        nullptr, /*std::make_shared<lsc::LFortranLspConfigTransformer>(
            transformer,
            serializer
        ),*/
        nullptr //-> workspaceConfig
      )
      , LFortranLspLanguageServer(
        incomingMessages,
        outgoingMessages,
        logger,
        configSection,
        extensionId,
        compilerVersion,
        parentProcessId,
        nullptr //-> workspaceConfig
      )
      , logger(logger.having("ConcurrentLFortranLspLanguageServer"))
    {
        // empty
    }

    auto ConcurrentLFortranLspLanguageServer::validate(
        std::shared_ptr<LspTextDocument> document
    ) -> void {
        static std::atomic_bool taskIsRunning(true);
        LFortranLspLanguageServer::validate(*document, taskIsRunning);
    }

} // namespace LCompilers::LanguageServerProtocol
