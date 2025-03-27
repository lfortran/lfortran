#include <ostream>
#include <server/language_server.h>

namespace LCompilers::LLanguageServer {

    LanguageServer::LanguageServer(
        MessageQueue &incomingMessages,
        MessageQueue &outgoingMessages,
        lsl::Logger &logger
    ) : incomingMessages(incomingMessages)
      , outgoingMessages(outgoingMessages)
      , logger(logger)
    {
        // empty
    }

    LanguageServer::~LanguageServer() {
        // empty
    }

    auto LanguageServer::send(const std::string &message) -> void {
        buffer.clear();
        prepare(buffer, message);
        if (outgoingMessages.enqueue(buffer) == nullptr) {
            logger.error() << "Failed to enqueue message:" << std::endl
                           << buffer << std::endl;
        }
    }

} // namespace LCompilers::LLanguageServer
