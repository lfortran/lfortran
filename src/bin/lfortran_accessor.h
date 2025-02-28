#pragma once

#include <string>
#include <vector>

#include <libasr/lsp_interface.h>
#include <libasr/utils.h>

namespace LCompilers::LLanguageServer {

    inline bool is_id_chr(unsigned char c) {
        return std::isalnum(c) || (c == '_');
    }

    class LFortranAccessor {
    public:

        auto showErrors(
            const std::string &filename,
            const std::string &text,
            const CompilerOptions &compiler_options
        ) -> std::vector<LCompilers::error_highlight>;

        auto lookupName(
            const std::string &filename,
            const std::string &text,
            const CompilerOptions &compiler_options
        ) -> std::vector<LCompilers::document_symbols>;
    };

} // namespace LCompilers::LLanguageServer
