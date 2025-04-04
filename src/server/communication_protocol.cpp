#include <cctype>
#include <chrono>
#include <cstdio>
#include <cerrno>
#include <cstring>
#include <iostream>
#include <ostream>
#include <string>

#include <server/communication_protocol.h>

namespace LCompilers::LLanguageServer {
    using namespace std::chrono_literals;

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
      , logger(logger.having("CommunicationProtocol"))
    {
        // Decouple stdin from stdout
        std::ios::sync_with_stdio(false);
        std::cin.tie(nullptr);
      
        listener = std::thread([this, &logger]() {
            logger.threadName("CommunicationProtocol_listener");
            listen();
        });
      
    }

    CommunicationProtocol::~CommunicationProtocol() {
        // Re-couple stdin with stdout
        std::ios::sync_with_stdio(true);
        std::cin.tie(&std::cout);
        // Restore stdout
        std::cout << std::flush;
        std::cout.rdbuf(cout_sbuf);
        fflush(stdout);
#ifdef _WIN32
        if (_dup2(stdout_fd, _fileno(stdout)) == -1) {
#else
        if (dup2(stdout_fd, fileno(stdout)) == -1) {
#endif // _WIN32
            logger.debug() << "Failed to restore stdout." << std::endl;
        }
        close(stdout_fd);
        // fclose(stdout_fp);
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
