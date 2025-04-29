#include "bin/semantic_highlighter.h"
#include <cstdlib>
#include <cstring>
#include <memory>
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
        std::shared_ptr<lsc::LFortranLspConfig> workspaceConfig,
        std::atomic_bool &start,
        std::condition_variable &startChanged,
        std::mutex &startMutex
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
        std::move(workspaceConfig),
        start,
        startChanged,
        startMutex
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
        nullptr, //-> workspaceConfig
        start,
        startChanged,
        startMutex
      )
      , LFortranLspLanguageServer(
        incomingMessages,
        outgoingMessages,
        logger,
        configSection,
        extensionId,
        compilerVersion,
        parentProcessId,
        nullptr, //->workspaceConfig
        start,
        startChanged,
        startMutex
      )
      , logger(logger.having("ParallelLFortranLspLanguageServer"))
    {
        // empty
    }

    auto ParallelLFortranLspLanguageServer::validate(
        std::shared_ptr<LspTextDocument> document
    ) -> void {
        auto readLock = LSP_READ_LOCK(document->mutex(), "document:" + document->uri());
        const std::string &uri = document->uri();
        auto writeLock = LSP_WRITE_LOCK(validationMutex, "validation");
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
                auto readLock = LSP_READ_LOCK(document->mutex(), "document:" + document->uri());
                const std::string &uri = document->uri();
                readLock.unlock();
                LFortranLspLanguageServer::validate(*document, *taskIsRunning);
                readLock.lock();
                auto writeLock = LSP_WRITE_LOCK(validationMutex, "validation");
                auto iter = validationsByUri.find(uri);
                if (iter != validationsByUri.end()) {
                    validationsByUri.erase(iter);
                }
            });
        validationsByUri.insert_or_assign(iter, uri, taskIsRunning);
    }

    auto ParallelLFortranLspLanguageServer::updateHighlights(
        std::shared_ptr<LspTextDocument> document
    ) -> void {
        std::shared_ptr<std::atomic_bool> taskIsRunning =
            workerPool.execute([this, document = std::move(document)](
                std::shared_ptr<std::atomic_bool> taskIsRunning
            ) {
                if (!*taskIsRunning) {
                    return;
                }
                auto documentLock = LSP_READ_LOCK(
                    document->mutex(),
                    "document:" + document->uri()
                );
                if (!*taskIsRunning) {
                    return;
                }
                int version = document->version();
                auto highlightsLock = LSP_WRITE_LOCK(highlightsMutex, "highlights");
                if (!*taskIsRunning) {
                    return;
                }
                auto iter = highlightsByDocumentId.find(document->id());
                if (iter != highlightsByDocumentId.end()) {
                    if (iter->second->second < version) {
                        iter->second = std::make_unique<std::pair<std::vector<FortranToken>, int>>(
                            std::make_pair(
                                semantic_tokenize(document->text()),
                                version
                            )
                        );
                    }
                } else {
                    highlightsByDocumentId.emplace_hint(
                        iter,
                        document->id(),
                        std::make_unique<std::pair<std::vector<FortranToken>, int>>(
                            std::make_pair(
                                semantic_tokenize(document->text()),
                                version
                            )
                        )
                    );
                }
            });
    }

} // namespace LCompilers::LanguageServerProtocol
