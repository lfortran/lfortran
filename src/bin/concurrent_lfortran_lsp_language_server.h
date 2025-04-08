#pragma once

#include <atomic>
#include <condition_variable>
#include <memory>
#include <mutex>

#include <libasr/asr.h>
#include <libasr/diagnostics.h>
#include <libasr/utils.h>

#include <server/concurrent_lsp_language_server.h>
#include <server/logger.h>
#include <server/lsp_specification.h>

#include <bin/lfortran_accessor.h>
#include <bin/lfortran_lsp_config.h>
#include <bin/lfortran_lsp_language_server.h>

namespace LCompilers::LanguageServerProtocol {
    namespace lsl = LCompilers::LLanguageServer::Logging;

    class ConcurrentLFortranLspLanguageServer
        : public ConcurrentLspLanguageServer
        , public LFortranLspLanguageServer
    {
    public:
        ConcurrentLFortranLspLanguageServer(
            ls::MessageQueue &incomingMessages,
            ls::MessageQueue &outgoingMessages,
            lsl::Logger &logger,
            const std::string &configSection,
            const std::string &extensionId,
            const std::string &compilerVersion,
            int parentProcessId,
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
    }; // class ConcurrentLFortranLspLanguageServer

} // namespace LCompilers::LanguageServerProtocol
