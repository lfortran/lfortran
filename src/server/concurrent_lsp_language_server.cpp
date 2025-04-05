#include <atomic>
#include <chrono>
#include <cctype>
#include <exception>
#include <memory>
#include <stdexcept>
#include <string>

#include <server/concurrent_lsp_language_server.h>
#include <server/lsp_exception.h>
#include <server/lsp_issue_reporter.h>
#include <server/lsp_json_parser.h>
#include <server/lsp_language_server.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

namespace LCompilers::LanguageServerProtocol {
    using namespace std::chrono_literals;

    namespace lst = LCompilers::LLanguageServer::Threading;

    auto PendingMessageComparator::operator()(
        const PendingMessage &lhs,
        const PendingMessage &rhs
    ) -> bool {
        return lhs.first > rhs.first;
    }

    ConcurrentLspLanguageServer::ConcurrentLspLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        lsl::Logger &logger,
        const std::string &configSection,
        const std::string &extensionId,
        const std::string &compilerVersion,
        int parentProcessId,
        std::shared_ptr<lsc::LspConfigTransformer> lspConfigTransformer,
        std::shared_ptr<lsc::LspConfig> workspaceConfig
    ) : BaseLspLanguageServer(
        incomingMessages,
        outgoingMessages,
        logger,
        configSection,
        extensionId,
        compilerVersion,
        parentProcessId,
        lspConfigTransformer,
        workspaceConfig
      )
      , logger(logger.having("ConcurrentLspLanguageServer"))
    {
        // empty
    }

    auto ConcurrentLspLanguageServer::listen() -> void {
        try {
            while (!_exit) {
                const std::string message = incomingMessages.dequeue();
                handle(message);
            }
        } catch (std::exception &e) {
            if (e.what() != lst::DEQUEUE_FAILED_MESSAGE) {
                logger.error()
                    << "Unhandled exception caught: " << e.what()
                    << std::endl;
            } else {
                logger.trace()
                    << "Interrupted while dequeuing messages: " << e.what()
                    << std::endl;
            }
        } catch (...) {
            logger.error()
                << "Unhandled exception caught: unknown"
                << std::endl;
        }
    }

    auto ConcurrentLspLanguageServer::handle(
        const std::string &message
    ) -> void {
        if (!_exit) {
            std::size_t sendId = nextSendId();
            try {
                BaseLspLanguageServer::handle(
                    message,
                    sendId,
                    std::make_shared<std::atomic_bool>(true)
                );
            } catch (std::exception &e) {
                logger.error()
                    << "Failed to handle message: " << message
                    << std::endl;
                logger.error()
                    << "Caught unhandled exception: " << e.what()
                    << std::endl;
            }
            if (pendingSendId <= sendId) {
                pendingMessages.push(PendingMessage(sendId, {}));
                sendAllReady();
            }
        }
    }

    auto ConcurrentLspLanguageServer::sendAllReady() -> void {
        while ((pendingMessages.size() > 0) && !_exit) {
            const PendingMessage &pendingMessage = pendingMessages.top();
            std::size_t sendId = pendingMessage.first;
            if (pendingSendId >= sendId) {
                const std::optional<std::string> &message =
                    pendingMessage.second;
                if (message.has_value()) {
                    ls::LanguageServer::send(message.value());
                }
                pendingMessages.pop();
                if (pendingSendId == sendId) {
                    ++pendingSendId;
                }
            } else {
                break;
            }
        }
    }

    auto ConcurrentLspLanguageServer::handleResponse(
        const LSPAny &document,
        ResponseMessage &response,
        std::string &traceId,
        std::shared_ptr<std::atomic_bool> taskIsRunning
    ) -> void {
        BaseLspLanguageServer::handleResponse(
            document,
            response,
            traceId,
            std::move(taskIsRunning)
        );
        if (response.id.type() == ResponseIdType::Integer) {
            int requestId = response.id.integer();
            responsesById.emplace(requestId, std::move(response));
        }
    }

    auto ConcurrentLspLanguageServer::send(
        const std::string &message,
        std::size_t sendId
    ) -> void {
        // -------------------------------------------------------------------------
        // NOTE: The LSP spec requires responses to be returned in roughly the same
        // order of receipt of their corresponding requests. Some types of responses
        // may be returned out-of-order, but in order to support those we will need
        // to implement a sort of dependency graph. Without knowledge of their
        // dependencies, we must respond to all requests in order of receipt.
        // -------------------------------------------------------------------------
        if (!_exit) {
            if (pendingSendId == sendId) {
                ls::LanguageServer::send(message);
                ++pendingSendId;
            } else {
                pendingMessages.emplace(sendId, message);
            }
            sendAllReady();
        }
    }

    auto ConcurrentLspLanguageServer::awaitResponse(
        int requestId
    ) -> const ResponseMessage & {
        auto iter = responsesById.find(requestId);
        if (iter != responsesById.end()) {
            return iter->second;
        }
        while (!_exit) {
            const std::string message = incomingMessages.dequeue();
            try {
                handle(message);
                if ((iter = responsesById.find(requestId)) != responsesById.end()) {
                    return iter->second;
                }
            } catch (std::exception &e) {
                if (e.what() != lst::DEQUEUE_FAILED_MESSAGE) {
                    logger.error()
                        << "Unhandled exception caught: " << e.what()
                        << std::endl;
                } else {
                    logger.trace()
                        << "Interrupted while dequeuing messages: " << e.what()
                        << std::endl;
                }
            } catch (...) {
                logger.error()
                    << "Unhandled exception caught: unknown"
                    << std::endl;
            }
        }
        throw std::runtime_error(
            "Server terminated while awaiting response to request with Id="
            + std::to_string(requestId)
        );
    }

    auto ConcurrentLspLanguageServer::getConfig(
        const DocumentUri &uri,
        const std::string &configSection
    ) -> const std::shared_ptr<LSPAny> {
        auto configIter = configsByUri.find(uri);
        if (configIter != configsByUri.end()) {
            return configIter->second;
        }

        ConfigurationItem item;
        item.scopeUri = uri;
        item.section = configSection;

        ConfigurationParams params;
        params.items.push_back(std::move(item));

        int requestId;

        std::shared_future<std::shared_ptr<LSPAny>> future;

        auto pendingIter = pendingConfigsByUri.find(uri);
        if (pendingIter != pendingConfigsByUri.end()) {
            requestId = pendingIter->second.first;
            future = pendingIter->second.second;
        } else {
            requestId = sendWorkspace_configuration(params);
            std::promise<std::shared_ptr<LSPAny>> promise;
            future = promise.get_future().share();
            {
                auto &pairs = pendingConfigsById.emplace(
                    std::piecewise_construct,
                    std::make_tuple(requestId),
                    std::make_tuple()
                ).first->second;
                auto &pair = pairs.emplace_back();
                pair.first = uri;
                pair.second = std::move(promise);
            }
            pendingConfigsByUri.emplace(
                std::piecewise_construct,
                std::make_tuple(uri),
                std::make_tuple(requestId, future)
            );
        }

        /*const ResponseMessage &response =*/
            awaitResponse(requestId);

        if (future.valid() && (future.wait_for(0ms) == std::future_status::ready)) {
            return future.get();
        }

        throw std::runtime_error(
            "Failed to get config for document with URI="
            + uri + ", section=" + configSection
        );
    }

} // namespace LCompilers::LanguageServerProtocol
