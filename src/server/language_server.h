#pragma once

#include <string>

#include <server/logger.h>
#include <server/queue.hpp>
#include <server/thread_pool.h>

namespace LCompilers::LLanguageServer {
    namespace lsl = LCompilers::LLanguageServer::Logging;
    namespace lst = LCompilers::LLanguageServer::Threading;

    const std::size_t MESSAGE_QUEUE_CAPACITY = 64;

    template class lst::Queue<std::string, MESSAGE_QUEUE_CAPACITY>;

    typedef lst::Queue<std::string, MESSAGE_QUEUE_CAPACITY> MessageQueue;

    class LanguageServer {
    public:
        LanguageServer(
            MessageQueue &incomingMessages,
            MessageQueue &outgoingMessages,
            lsl::Logger &logger
        );
        virtual ~LanguageServer();
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
