#include <cctype>
#include <cstdio>
#include <iostream>
#include <ostream>
#include <string>

#ifdef _WIN32
#include <io.h>
#else
#include <unistd.h>
#endif // _WIN32

#include <server/communication_protocol.h>

namespace LCompilers::LLanguageServer {

    CommunicationProtocol::CommunicationProtocol(
        LanguageServer &languageServer,
        MessageStream &messageStream,
        MessageQueue &incomingMessages,
        MessageQueue &outgoingMessages,
        lsl::Logger &logger
    ) : cout_sbuf(std::cout.rdbuf())  // reference to stdout
      , languageServer(languageServer)
      , messageStream(messageStream)
      , incomingMessages(incomingMessages)
      , outgoingMessages(outgoingMessages)
      , logger(logger)
      , listener([this, &logger]() {
          logger.threadName("CommunicationProtocol_listener");
          listen();
      })
    {
        // Redirect stdout to stderr
        std::cout << std::flush;
        std::cout.rdbuf(std::cerr.rdbuf());
        fflush(stdout);
#ifdef _WIN32
        if ((stdout_fd = _dup(_fileno(stdout))) == -1) {
#else
        if ((stdout_fd = dup(fileno(stdout))) == -1) {
#endif // _WIN32
            logger.error() << "Failed to copy stdout for restoration." << std::endl;
#ifdef _WIN32
        } else if (_dup2(_fileno(stderr), _fileno(stdout)) == -1) {
#else
        } else if (dup2(fileno(stderr), fileno(stdout)) == -1) {
#endif // _WIN32
            logger.error() << "Failed to copy stdout for restoration." << std::endl;
        }
    }

    CommunicationProtocol::~CommunicationProtocol() {
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
    }

    auto CommunicationProtocol::listen() -> void {
        try {
            do {
                const std::string message = incomingMessages.dequeue();
                logger.trace()
                    << "Sending:" << std::endl
                    << message << std::endl;
#ifdef _WIN32
                if (_write(stdout_fd, message.c_str(), message.length()) == -1) {
#else
                if (write(stdout_fd, message.c_str(), message.length()) == -1) {
#endif // _WIN32
                    logger.error() << "Failed to write message to stdout:" << std::endl
                                   << message << std::endl;
                }
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
