#pragma once

#include <string>

#include <server/logger.h>
#include <server/message_queue.h>
#include <server/thread_pool.h>

namespace LCompilers::LLanguageServer {
    namespace lsl = LCompilers::LLanguageServer::Logging;

    class LanguageServer {
    public:
        LanguageServer(
            MessageQueue &incomingMessages,
            MessageQueue &outgoingMessages,
            lsl::Logger &logger
        );
        virtual bool isTerminated() const = 0;
        virtual void join() = 0;
    protected:
        MessageQueue &incomingMessages;
        MessageQueue &outgoingMessages;
        lsl::Logger &logger;
        std::string buffer;

        auto send(const std::string &request) -> void;

        virtual void prepare(
            std::string &buffer,
            const std::string &message
        ) const = 0;
    };

} // namespace LCompilers::LLanguageServer
