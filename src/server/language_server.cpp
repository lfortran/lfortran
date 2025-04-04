#include <ostream>
#include <server/language_server.h>

namespace LCompilers::LLanguageServer {

    LanguageServer::LanguageServer(
        MessageQueue &incomingMessages,
        MessageQueue &outgoingMessages,
        lsl::Logger &logger
    ) : incomingMessages(incomingMessages)
      , outgoingMessages(outgoingMessages)
      , logger(logger.having("LanguageServer"))
    {
        // empty
    }

    LanguageServer::~LanguageServer() {
        // empty
    }

    auto LanguageServer::join() -> void {
        // empty
    }

    auto LanguageServer::send(const std::string &message) -> void {
        static thread_local std::string buffer;
        buffer.clear();
        prepare(buffer, message);
        if (outgoingMessages.enqueue(buffer) == nullptr) {
            logger.error() << "Failed to enqueue message:" << std::endl
                           << buffer << std::endl;
        }
    }

} // namespace LCompilers::LLanguageServer
