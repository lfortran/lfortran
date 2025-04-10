#include <cctype>
#include <cerrno>
#include <cstring>
#include <iostream>
#include <ostream>
#include <string>

#include <server/communication_protocol.h>

namespace LCompilers::LLanguageServer {

    CommunicationProtocol::CommunicationProtocol(
        LanguageServer &languageServer,
        MessageStream &messageStream,
        MessageQueue &incomingMessages,
        MessageQueue &outgoingMessages,
        lsl::Logger &logger,
        std::atomic_bool &start,
        std::condition_variable &startChanged,
        std::mutex &startMutex
    ) : languageServer(languageServer)
      , messageStream(messageStream)
      , incomingMessages(incomingMessages)
      , outgoingMessages(outgoingMessages)
      , logger(logger.having("CommunicationProtocol"))
      , listener([this, &logger, &start, &startChanged, &startMutex]() {
          logger.threadName("CommunicationProtocol_listener");
          if (!start) {
              std::unique_lock<std::mutex> startLock(startMutex);
              startChanged.wait(startLock, [&start]{
                  return start.load();
              });
          }
          listen();
      })
    {
        // Decouple stdin from stdout
        std::ios::sync_with_stdio(false);
        std::cin.tie(nullptr);
    }

    CommunicationProtocol::~CommunicationProtocol() {
        // Re-couple stdin with stdout
        std::ios::sync_with_stdio(true);
        std::cin.tie(&std::cout);
    }

    auto CommunicationProtocol::listen() -> void {
        try {
            do {
                const std::string message = incomingMessages.dequeue();
                logger.trace() << "Sending:" << std::endl << message << std::endl;
                std::cout << message << std::flush;
            } while (running);
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
        logger.debug() << "Incoming-message listener terminated." << std::endl;
    }

    void CommunicationProtocol::serve() {
        logger.info() << "Serving requests." << std::endl;
        try {
            bool exit = false;
            while (!languageServer.isTerminated() && !exit) {
                logger.trace() << "Awaiting next message ..." << std::endl;
                std::string message = messageStream.next(exit);
                if (message.length() > 0) {
                    outgoingMessages.enqueue(message);
                } else {
                    logger.warn()
                        << "Cannot parse an empty request body."
                        << std::endl;
                }
            }
        } catch (std::exception &e) {
            logger.error()
                << "Caught unhandled exception while serving requests: "
                << e.what()
                << std::endl;
        }
        running = false;
        incomingMessages.stopNow();
        languageServer.join();
        logger.debug() << "Language server terminated." << std::endl;
        if (listener.joinable()) {
            listener.join();
            logger.debug()
                << "Incoming-message listener terminated."
                << std::endl;
        }
    }

} // namespace LCompilers::LLanguageServer
