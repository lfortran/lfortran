#include <cctype>
#include <cstddef>
#include <mutex>
#include <regex>
#include <stdexcept>
#include <string_view>

#include <server/lsp_exception.h>
#include <server/lsp_message_stream.h>

namespace LCompilers::LanguageServerProtocol {

    LspMessageStream::LspMessageStream(std::istream &istream, lsl::Logger &logger)
        : RE_IS_CONTENT_LENGTH("^Content-Length$", std::regex_constants::icase)
        , RE_IS_EXIT(
            "^\\{\\s*(?:"
                "\"method\"\\s*:\\s*\"exit\"\\s*,\\s*\"jsonrpc\"\\s*:\\s*\"2\\.0\""
                "|"
                "\"jsonrpc\"\\s*:\\s*\"2\\.0\"\\s*,\\s*\"method\"\\s*:\\s*\"exit\""
            ")\\s*\\}$"
        )
        , istream(istream)
        , logger(logger.having("LspMessageStream"))
    {
        message.reserve(16384);
        position = 0;
    }

    auto LspMessageStream::nextChar() -> char {
        char c;
        istream.get(c);
        message.push_back(c);
        ++position;
        return c;
    }

    auto LspMessageStream::logEscaped(char c) -> void {
        switch (c) {
        case '\n': {
            logger << "\\n";
            break;
        }
        case '\t': {
            logger << "\\t";
            break;
        }
        case '\b': {
            logger << "\\b";
            break;
        }
        case '\r': {
            logger << "\\r";
            break;
        }
        case '\f': {
            logger << "\\f";
            break;
        }
        default: {
            logger << c;
        }
        }
    }

    auto LspMessageStream::logEscapedMessage() -> void {
        for (char c : message) {
            logEscaped(c);
        }
    }

    auto LspMessageStream::next(bool &exit) -> std::string {
        std::size_t numBytes = 0;
        bool hasContentLength = false;
        std::size_t start = 0;
        std::size_t length = 0;
        std::string_view view;
        char c = '\0';
    parse_header_name:
        start = position;
        do {
            switch (nextChar()) {
            case '\r': {
                length = (position - start) - 1;
                view = std::string_view(message.data() + start, length);
                c = nextChar();
                if (c != '\n') {
                    if (logger.isWarnEnabled()) {
                        std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
                        if (logger.isWarnEnabled()) {
                            logger.warn() << "Expected \\r to be followed by \\n, not '";
                            logEscaped(c);
                            logger << "\':" << std::endl;
                            logEscapedMessage();
                            logger << std::endl;
                        }
                    }
                    goto parse_header_name;
                }
                if ((view.length() == 0) && hasContentLength) {
                    goto parse_body;
                }
                if (logger.isWarnEnabled()) {
                    std::unique_lock<std::recursive_mutex> lock(logger.mutex());
                    if (logger.isWarnEnabled()) {
                        logger.warn() << "Reached out-of-sequence newline while parsing header name:" << std::endl;
                        logEscapedMessage();
                        logger << std::endl;
                    }
                }
                goto parse_header_name;
            }
            case '\n': {
                length = (position - start) - 1;
                view = std::string_view(message.data() + start, length);
                if (logger.isWarnEnabled()) {
                    std::unique_lock<std::recursive_mutex> lock(logger.mutex());
                    if (logger.isWarnEnabled()) {
                        logger.warn() << "Reached out-of-sequence newline while parsing header name:" << std::endl;
                        logEscapedMessage();
                        logger << std::endl;
                    }
                }
                goto parse_header_name;
            }
            case ':': {
                length = (position - start) - 1;
                view = std::string_view(message.data() + start, length);
                hasContentLength = std::regex_match(
                    view.begin(),
                    view.end(),
                    RE_IS_CONTENT_LENGTH
                );
                goto parse_header_value;
            }
            default: {
                // empty
            }
            }
        } while (true);
    parse_header_value:
        start = position;
        c = nextChar();
        switch (c) { // ignore leading whitespace
        case ' ': // fallthrough
        case '\t': {
            goto parse_header_value;
        }
        }
        do {
            switch (c) {
            case '\r': {
                length = (position - start) - 1;
                view = std::string_view(message.data() + start, length);
                c = nextChar();
                if (c != '\n') {
                    if (logger.isWarnEnabled()) {
                        std::unique_lock<std::recursive_mutex> lock(logger.mutex());
                        if (logger.isWarnEnabled()) {
                            logger.warn() << "Expected \\r to be followed by \\n, not '";
                            logEscaped(c);
                            logger << "\': " << view << ":\n";
                            logEscapedMessage();
                            logger << std::endl;
                        }
                    }
                    goto parse_header_name;
                }
                if (hasContentLength) {
                    numBytes = std::stoull(std::string(view));
                }
                goto parse_header_name;
            }
            case '\n': {
                length = (position - start) - 1;
                view = std::string_view(message.data() + start, length);
                if (logger.isWarnEnabled()) {
                    std::unique_lock<std::recursive_mutex> lock(logger.mutex());
                    if (logger.isWarnEnabled()) {
                        logger.warn()
                            << "Reached out-of-sequence newline while parsing header value:"
                            << std::endl;
                        logEscapedMessage();
                        logger << std::endl;
                    }
                }
                goto parse_header_name;
            }
            default: {
                // empty
            }
            }
            c = nextChar();
        } while (true);
    parse_body:
        start = position;
        for (std::size_t readBytes = 0; readBytes < numBytes; ++readBytes) {
            nextChar();
        }
        length = (position - start);
        std::string body = message.substr(start, length);
        logger.trace() << "Receiving:" << std::endl << message << std::endl;
        message.clear();
        position = 0;
        exit = std::regex_match(body, RE_IS_EXIT);
        return body;
    }

} // namespace LCompilers::LanguageServerProtocol
