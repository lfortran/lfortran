#include <cctype>
#include <cstdio>
#include <iostream>
#include <string>

#include <server/communication_protocol.h>

namespace LCompilers::LLanguageServer {

    CommunicationProtocol::CommunicationProtocol(
        LanguageServer &languageServer,
        MessageStream &messageStream,
        MessageQueue &incomingMessages,
        MessageQueue &outgoingMessages,
        lsl::Logger &logger
    ) : languageServer(languageServer)
      , messageStream(messageStream)
      , incomingMessages(incomingMessages)
      , outgoingMessages(outgoingMessages)
      , logger(logger)
      , listener([this]() {
          listen();
      })
    {
        // empty
    }

    auto CommunicationProtocol::listen() -> void {
        try {
            do {
                const std::string message = incomingMessages.dequeue();
                logger.trace()
                    << "Sending:" << std::endl
                    << message << std::endl;
                std::cout << message << std::flush;
            } while (running);
        } catch (std::exception &e) {
            if (e.what() != lst::DEQUEUE_FAILED_MESSAGE) {
                logger.error()
                    << "[CommunicationProtocol] "
                    "Unhandled exception caught: " << e.what()
                    << std::endl;
            } else {
                logger.trace()
                    << "[CommunicationProtocol] "
                    "Interrupted while dequeuing messages: " << e.what()
                    << std::endl;
            }
        }
        logger.debug()
            << "[CommunicationProtocol] Incoming-message listener terminated."
            << std::endl;
    }

    void CommunicationProtocol::serve() {
        logger.info()
            << "[CommunicationProtocol] Serving requests."
            << std::endl;
        try {
            while (!languageServer.isTerminated()) {
                std::string message = messageStream.next();
                if (message.length() > 0) {
                    outgoingMessages.enqueue(message);
                } else {
                    logger.warn()
                        << "[CommunicationProtocol] "
                        "Cannot parse an empty request body."
                        << std::endl;
                }
            }
        } catch (std::exception &e) {
            logger.error()
                << "[CommunicationProtocol] "
                "Caught unhandled exception while serving requests: "
                << e.what()
                << std::endl;
        }
        running = false;
        incomingMessages.stopNow();
        languageServer.join();
        logger.debug()
            << "[CommunicationProtocol] Language server terminated."
            << std::endl;
        if (listener.joinable()) {
            listener.join();
            logger.debug()
                << "[CommunicationProtocol] "
                "Incoming-message listener terminated."
                << std::endl;
        }
    }

} // namespace LCompilers::LLanguageServer
