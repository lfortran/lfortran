#pragma once

#include <atomic>
#include <cstddef>
#include <regex>
#include <shared_mutex>
#include <vector>

#include <server/lsp_specification.h>

namespace LCompilers::LanguageServerProtocol {
    extern const std::regex RE_F90_IDENTIFIER;
    extern const std::regex RE_F90_KEYWORD;
    extern const std::regex RE_F90_OPERATOR;
    extern const std::regex RE_F90_STRING;
    extern const std::regex RE_F90_COMMENT;
    extern const std::regex RE_F90_NUMBER;

    struct F90Token {
        std::size_t position;
        std::size_t length;
        SemanticTokenTypes type;
        std::vector<SemanticTokenModifiers> modifiers;
    };

    class SemanticHighlighter {
    public:
        SemanticHighlighter(const std::string &text);
        auto begin() const -> std::vector<F90Token>::const_iterator;
        auto end() const -> std::vector<F90Token>::const_iterator;
        auto size() const -> std::size_t;
        auto mutex() -> std::shared_mutex &;
    private:
        std::vector<F90Token> m_tokens;
        std::shared_mutex m_mutex;

        auto tokenize(const std::string &text) -> void;
    };
} // namespace LCompilers::LanguageServerProtocol
