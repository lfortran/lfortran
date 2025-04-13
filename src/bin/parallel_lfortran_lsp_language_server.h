#pragma once

#include <atomic>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <string>

#include <libasr/asr.h>
#include <libasr/diagnostics.h>
#include <libasr/utils.h>

#include <server/logger.h>
#include <server/lsp_specification.h>
#include <server/parallel_lsp_language_server.h>

#include <bin/lfortran_accessor.h>
#include <bin/lfortran_lsp_config.h>
#include <bin/lfortran_lsp_language_server.h>

namespace LCompilers::LanguageServerProtocol {
    namespace ls = LCompilers::LLanguageServer;
    namespace lsl = LCompilers::LLanguageServer::Logging;
    namespace lsc = LCompilers::LanguageServerProtocol::Config;

    class ParallelLFortranLspLanguageServer
        : public ParallelLspLanguageServer
        , public LFortranLspLanguageServer
    {
    public:
        ParallelLFortranLspLanguageServer(
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
        );

    protected:
        lsl::Logger logger;

        auto validate(
            std::shared_ptr<LspTextDocument> document
        ) -> void override;

        auto updateHighlights(
            std::shared_ptr<LspTextDocument> document
        ) -> void override;
    }; // class ParallelLFortranLspLanguageServer

} // namespace LCompilers::LanguageServerProtocol
