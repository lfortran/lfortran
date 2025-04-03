#include <cstdlib>
#include <cstring>
#include <memory>
#include <shared_mutex>
#include <string>

#include <server/lsp_exception.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

#include <bin/lfortran_command_line_parser.h>
#include <bin/lfortran_lsp_config.h>
#include <bin/parallel_lfortran_lsp_language_server.h>

namespace LCompilers::LanguageServerProtocol {

    ParallelLFortranLspLanguageServer::ParallelLFortranLspLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        std::size_t numRequestThreads,
        std::size_t numWorkerThreads,
        lsl::Logger &logger,
        const std::string &configSection,
        const std::string &extensionId,
        const std::string &compilerVersion,
        int parentProcessId,
        unsigned int seed,
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
      , ParallelLspLanguageServer(
        incomingMessages,
        outgoingMessages,
        numRequestThreads,
        numWorkerThreads,
        logger,
        configSection,
        extensionId,
        compilerVersion,
        parentProcessId,
        seed,
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
        nullptr //->workspaceConfig
      )
      , logger(logger.having("ParallelLFortranLspLanguageServer"))
    {
        // empty
    }

    auto ParallelLFortranLspLanguageServer::validate(
        std::shared_ptr<LspTextDocument> document
    ) -> void {
        std::shared_lock<std::shared_mutex> readLock(document->mutex());
        const std::string &uri = document->uri();
        std::unique_lock<std::shared_mutex> writeLock(validationMutex);
        auto iter = validationsByUri.find(uri);
        if (iter != validationsByUri.end()) {
            // If an older version of the document is being validated, cancel it
            // so only the latest version will be validated:
            *iter->second = false;
        }
        std::shared_ptr<std::atomic_bool> taskIsRunning =
            workerPool.execute([this, document = std::move(document)](
                std::shared_ptr<std::atomic_bool> taskIsRunning
            ) {
                std::shared_lock<std::shared_mutex> readLock(document->mutex());
                const std::string &uri = document->uri();
                LFortranLspLanguageServer::validate(*document, *taskIsRunning);
                std::unique_lock<std::shared_mutex> writeLock(validationMutex);
                auto iter = validationsByUri.find(uri);
                if (iter != validationsByUri.end()) {
                    validationsByUri.erase(iter);
                }
            });
        validationsByUri.insert_or_assign(iter, uri, taskIsRunning);
    }

} // namespace LCompilers::LanguageServerProtocol
