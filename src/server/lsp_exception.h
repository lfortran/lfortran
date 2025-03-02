#pragma once

#include <stdexcept>
#include <string>

#include <server/lsp_specification.h>

namespace LCompilers::LanguageServerProtocol {

    enum class ErrorCodeType {
        ErrorCodes,
        LspErrorCodes,
    };

    struct ErrorCode {
        ErrorCodeType type;
        union {
            ErrorCodes errorCodes;
            LSPErrorCodes lspErrorCodes;
        };
    };

    class LspException : public std::logic_error {
    public:
        LspException(
            ErrorCodes code,
            const std::string &message,
            const char *file,
            int line
        );
        LspException(
            LSPErrorCodes code,
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
