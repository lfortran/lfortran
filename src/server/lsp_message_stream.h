#pragma once

#include <istream>
#include <regex>

#include <server/logger.h>
#include <server/message_stream.h>

namespace LCompilers::LanguageServerProtocol {
    namespace ls = LCompilers::LLanguageServer;
    namespace lsl = LCompilers::LLanguageServer::Logging;

    class LspMessageStream : public ls::MessageStream {
    public:
        LspMessageStream(std::istream &istream, lsl::Logger &logger);
        std::string next(bool &exit) override;
    private:
        const std::regex RE_IS_CONTENT_LENGTH;
        const std::regex RE_IS_EXIT;

        std::istream &istream;
        lsl::Logger logger;
        std::string message;
        std::size_t position;

        auto nextChar() -> char;
        auto logEscaped(char c) -> void;
        auto logEscapedMessage() -> void;
    };

} // namespace LCompilers::LanguageServerProtocol
