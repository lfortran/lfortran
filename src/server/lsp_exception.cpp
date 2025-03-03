#include <server/lsp_exception.h>

namespace LCompilers::LanguageServerProtocol {

    LspException::LspException(
        ErrorCodes code,
        const std::string &message,
        const char *file,
        int line
    ) : std::logic_error(message)
      , _file(file)
      , _line(line)
    {
        _code.type = ErrorCodeType::ErrorCodes;
        _code.errorCodes = code;
    }

    LspException::LspException(
        LSPErrorCodes code,
        const std::string &message,
        const char *file,
        int line
    ) : std::logic_error(message)
      , _file(file)
      , _line(line)
    {
        _code.type = ErrorCodeType::LspErrorCodes;
        _code.lspErrorCodes = code;
    }

    auto LspException::code() const -> const ErrorCode & {
        return _code;
    }

    auto LspException::file() const -> const char * {
        return _file;
    }

    auto LspException::line() const -> int {
        return _line;
    }

} // namespace LCompilers::LanguageServerProtocol
