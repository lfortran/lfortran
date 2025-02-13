#include <server/lsp_exception.h>

namespace LCompilers::LanguageServerProtocol {

    LspException::LspException(
        ErrorCode code,
        const std::string &message,
        const char *file,
        int line
    ) : std::logic_error(message)
      , _code(code)
      , _file(file)
      , _line(line)
    {
        // empty
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
