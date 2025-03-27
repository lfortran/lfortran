#pragma once

#include <atomic>
#include <thread>

#include <server/language_server.h>
#include <server/logger.h>
#include <server/message_stream.h>

namespace LCompilers::LLanguageServer {
    namespace lsl = LCompilers::LLanguageServer::Logging;

    class CommunicationProtocol {
    public:
        CommunicationProtocol(
            LanguageServer &languageServer,
            MessageStream &messageStream,
            MessageQueue &incomingMessages,
            MessageQueue &outgoingMessages,
            lsl::Logger &logger);
        ~CommunicationProtocol();
        auto serve() -> void;
    private:
        std::streambuf* cout_sbuf;
        int stdout_fd;
        // FILE *stdout_fp;
        LanguageServer &languageServer;
        MessageStream &messageStream;
        MessageQueue &incomingMessages;
        MessageQueue &outgoingMessages;
        lsl::Logger &logger;
        std::thread listener;
        std::atomic_bool running = true;

        auto listen() -> void;
    };

} // namespace LCompilers::LLanguageServer
