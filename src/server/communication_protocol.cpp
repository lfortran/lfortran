#include <cctype>
#include <cerrno>
#include <chrono>
#include <cstdio>
#include <cstring>
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
    using namespace std::chrono_literals;

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
      , logger(logger.having("CommunicationProtocol"))
      , listener([this, &logger]() {
          logger.threadName("CommunicationProtocol_listener");
          listen();
      })
    {
        // Decouple stdin from stdout
        std::ios::sync_with_stdio(false);
        std::cin.tie(nullptr);
        // Redirect stdout to stderr
        std::cout << std::flush;
        std::cout.rdbuf(std::cerr.rdbuf());
        if (fflush(stdout) == EOF) {
            logger.error()
                << "Failed to flush stdout: fflush returned "
                << errno << ":" << strerror(errno) << std::endl;
        }
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
        } else if ((stdout_fp = fdopen(stdout_fd, "w")) == nullptr) {
            logger.error() << "Failed to open FILE to stdout." << std::endl;
        }
    }

    CommunicationProtocol::~CommunicationProtocol() {
        // Re-couple stdin with stdout
        std::ios::sync_with_stdio(true);
        std::cin.tie(&std::cout);
        // Restore stdout
        std::cout << std::flush;
        std::cout.rdbuf(cout_sbuf);
        if (fflush(stdout) == EOF) {
            logger.error()
                << "Failed to flush stdout: fflush returned "
                << errno << ":" << strerror(errno) << std::endl;
        }
#ifdef _WIN32
        if (_dup2(stdout_fd, _fileno(stdout)) == -1) {
#else
        if (dup2(stdout_fd, fileno(stdout)) == -1) {
#endif // _WIN32
            logger.debug() << "Failed to restore stdout." << std::endl;
        }
    }

    auto CommunicationProtocol::listen() -> void {
        try {
            do {
                const std::string message = incomingMessages.dequeue();
                logger.trace() << "Sending:" << std::endl << message << std::endl;
#ifdef _WIN32
                if (_write(stdout_fd, message.c_str(), message.length()) == -1) {
#else
                if (write(stdout_fd, message.c_str(), message.length()) == -1) {
#endif // _WIN32
                    logger.error() << "Failed to write message to stdout:" << std::endl
                                   << message << std::endl;
                }
                if (fflush(stdout_fp) == EOF) {
                    logger.error()
                        << "Failed to flush stdout_fp: fflush returned "
                        << errno << ":" << strerror(errno) << std::endl;
                }
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
