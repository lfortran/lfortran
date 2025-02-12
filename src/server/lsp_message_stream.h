#pragma once

#include <istream>

#include <server/logger.h>
#include <server/message_stream.h>

namespace LCompilers::LanguageServerProtocol {
  namespace ls = LCompilers::LLanguageServer;
  namespace lsl = LCompilers::LLanguageServer::Logging;

  class LspMessageStream : public ls::MessageStream {
  public:
    LspMessageStream(std::istream &istream, lsl::Logger &logger);
    std::string next() override;
  private:
    std::istream &istream;
    lsl::Logger &logger;
    std::string message;
    std::size_t position;

    auto nextChar() -> char;
    auto nextUpper() -> char;
    auto logEscaped(char c) -> void;
  };

} // namespace LCompilers::LanguageServerProtocol
