#pragma once

#include <cstddef>
#include <memory>
#include <string>

#include <server/specification.h>

namespace LCompilers::LanguageServerProtocol {

  class LspJsonParser {
  public:
    LspJsonParser(const std::string &message);
    auto parse() -> std::unique_ptr<LSPAny>;
  private:
    const std::string message;
    std::size_t index = 0;
    std::string buffer;

    auto dropWhitespace() -> void;
    auto parseObject() -> LSPObject;
    auto parseArray() -> LSPArray;
    auto parseValue() -> std::unique_ptr<LSPAny>;
    auto parseString() -> string_t;
    auto parseNumber() -> std::unique_ptr<LSPAny>;
    auto parseFraction() -> std::unique_ptr<LSPAny>;
    auto parseExponent() -> std::unique_ptr<LSPAny>;
    auto parseTrue() -> std::unique_ptr<LSPAny>;
    auto parseFalse() -> std::unique_ptr<LSPAny>;
    auto parseNull() -> std::unique_ptr<LSPAny>;

    auto escapeAndBuffer(unsigned char c) -> void;

    inline auto advance() -> void {
      ++index;
    }

    inline auto nextChar() -> unsigned char {
      return message[index++];
    }

    inline auto peekChar() -> unsigned char {
      return message[index];
    }

    inline auto hasNext() const -> bool {
      return index < message.length();
    }
  };

} // namespace LCompilers::LanguageServerProtocol
