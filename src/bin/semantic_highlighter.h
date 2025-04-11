#pragma once

#include <cstddef>
#include <regex>
#include <vector>

#include <server/lsp_specification.h>

namespace LCompilers::LanguageServerProtocol {
    extern const std::regex RE_FORTRAN_IDENTIFIER;
    extern const std::regex RE_FORTRAN_KEYWORD;

    struct FortranToken {
        std::size_t position;
        std::size_t length;
        SemanticTokenTypes type;
        std::vector<SemanticTokenModifiers> modifiers;
    };

    auto semantic_tokenize(const std::string &text) -> std::vector<FortranToken>;
} // namespace LCompilers::LanguageServerProtocol
