#pragma once

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <queue>
#include <string>
#include <utility>

#include <server/base_lsp_language_server.h>
#include <server/logger.h>
#include <server/lsp_config.h>
#include <server/lsp_language_server.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

namespace LCompilers::LanguageServerProtocol {
    namespace lsc = LCompilers::LanguageServerProtocol::Config;

    using namespace std::chrono_literals;
    typedef std::chrono::system_clock::time_point time_point_t;
    typedef std::chrono::milliseconds milliseconds_t;

    typedef std::pair<std::size_t, std::optional<std::string>> PendingMessage;

    struct PendingMessageComparator {
        auto operator()(const PendingMessage &lhs, const PendingMessage &rhs) -> bool;
    };

    typedef std::priority_queue<
        PendingMessage,
        std::vector<PendingMessage>,
        PendingMessageComparator
    > PendingMessageQueue;

    class ConcurrentLspLanguageServer : virtual public BaseLspLanguageServer {
    protected:
        ConcurrentLspLanguageServer(
            ls::MessageQueue &incomingMessages,
            ls::MessageQueue &outgoingMessages,
            lsl::Logger &logger,
            const std::string &configSection,
            const std::string &extensionId,
            const std::string &compilerVersion,
            int parentProcessId,
            std::shared_ptr<lsc::LspConfigTransformer> lspConfigTransformer,
            std::shared_ptr<lsc::LspConfig> workspaceConfig,
            std::atomic_bool &start,
            std::condition_variable &startChanged,
            std::mutex &startMutex
        );

        lsl::Logger logger;
        std::map<int, ResponseMessage> responsesById;

        PendingMessageQueue pendingMessages;

        auto sendAllReady() -> void;

        auto listen() -> void override;

        auto handle(const std::string &message) -> void;

        auto handleResponse(
            const LSPAny &document,
            ResponseMessage &response,
            std::string &traceId,
            std::shared_ptr<std::atomic_bool> taskIsRunning
        ) -> void override;

        auto send(const std::string &request, std::size_t sendId) -> void override;

        auto awaitResponse(int requestId) -> const ResponseMessage &;

        auto getConfig(
            const DocumentUri &uri,
            const std::string &configSection
        ) -> const std::shared_ptr<LSPAny> override;

    }; // class ConcurrentLspLanguageServer

} // namespace LCompilers::LanguageServerProtocol
