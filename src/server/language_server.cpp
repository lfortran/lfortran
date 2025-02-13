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

    auto LanguageServer::send(const std::string &message) -> void {
        buffer.clear();
        prepare(buffer, message);
        outgoingMessages.enqueue(buffer);
    }

} // namespace LCompilers::LLanguageServer
