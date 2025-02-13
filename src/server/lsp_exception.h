#pragma once

#include <stdexcept>
#include <string>
#include <variant>

#include <server/specification.h>

namespace LCompilers::LanguageServerProtocol {

    enum class ErrorCodeType {
        ERROR_CODES,
        LSP_ERROR_CODES,
    };

    typedef std::variant<
        ErrorCodes,
        LSPErrorCodes
    > ErrorCode;

    class LspException : public std::logic_error {
    public:
        LspException(
            ErrorCode code,
            const std::string &message,
            const char *file,
            int line
        );
        auto code() const -> const ErrorCode &;
        auto file() const -> const char *;
        auto line() const -> int;
    protected:
        ErrorCode _code;
        const char *_file;
        int _line;
    };

#define LSP_EXCEPTION(code, message) \
    LspException((code), (message), __FILE__, __LINE__)

} // namespace LCompilers::LanguageServerProtocol
