#include <algorithm>
#include <mutex>

#include <libasr/exception.h>

#include <bin/semantic_highlighter.h>

namespace LCompilers::LanguageServerProtocol {
    const std::regex RE_F90_IDENTIFIER{R"([a-zA-Z][a-zA-Z0-9_]*)"};
    const std::regex RE_F90_KEYWORD{
        R"(\b(?:program|integer|real|double\s*precision|complex|character|logical|if|then|else|endif|do|while|enddo|continue|stop|pause|write|read|print|open|close|inquire|format|call|return|end|subroutine|function|implicit|none|data|parameter|allocate|deallocate|module|use|interface|type|public|private|contains|where|forall)\b)",
        std::regex::icase
    };
    const std::regex RE_F90_OPERATOR{
        R"(\+|-|\*|\/|\*\*|\.eq\.|\.ne\.|\.lt\.|\.le\.|\.gt\.|\.ge\.|\.and\.|\.or\.|\.not\.|\.eqv\.|\.neqv\.)"
    };
    const std::regex RE_F90_STRING{R"('([^']|'')*'|\"([^"]|"")*\")"};
    const std::regex RE_F90_COMMENT{R"(!.*$)"};
    const std::regex RE_F90_NUMBER{R"(\d+\.?\d*|\.\d+)"};

    SemanticHighlighter::SemanticHighlighter(const std::string &text) {
        tokenize(text);
    }

    auto SemanticHighlighter::begin() const -> std::vector<F90Token>::const_iterator {
        return m_tokens.begin();
    }

    auto SemanticHighlighter::end() const -> std::vector<F90Token>::const_iterator {
        return m_tokens.end();
    }

    auto SemanticHighlighter::size() const -> std::size_t {
        return m_tokens.size();
    }

    auto SemanticHighlighter::mutex() -> std::shared_mutex & {
        return m_mutex;
    }

    auto SemanticHighlighter::tokenize(const std::string &text) -> void {
        std::unique_lock<std::shared_mutex> writeLock(m_mutex);
        std::vector<F90Token *> v1, v2;
        std::vector<F90Token *>::iterator viter;
        std::sregex_iterator riter, rend;
        std::size_t lower, upper;
        F90Token *outerToken, *innerToken;

        // Tokenize strings:
        // ------------------
        riter = std::sregex_iterator(
            text.begin(),
            text.end(),
            RE_F90_STRING
        );
        while (riter != rend) {
            innerToken = &m_tokens.emplace_back();
            innerToken->position = riter->position();
            innerToken->length = riter->length();
            innerToken->type = SemanticTokenTypes::String;
            v1.push_back(innerToken);
            ++riter;
        }

        // Tokenize comments:
        // -----------------
        lower = 0;
        for (viter = v1.begin(); viter != v1.end(); ++viter) {
            outerToken = *viter;
            upper = outerToken->position;
            riter = std::sregex_iterator(
                text.begin() + lower,
                text.begin() + upper,
                RE_F90_COMMENT
            );
            while (riter != rend) {
                innerToken = &m_tokens.emplace_back();
                innerToken->position = riter->position();
                innerToken->length = riter->length();
                innerToken->type = SemanticTokenTypes::Comment;
                v2.push_back(innerToken);
                ++riter;
            }
            lower = upper + outerToken->length;
            v2.push_back(outerToken);
        }

        riter = std::sregex_iterator(
            text.begin() + lower,
            text.end(),
            RE_F90_STRING
        );
        while (riter != rend) {
            innerToken = &m_tokens.emplace_back();
            innerToken->position = riter->position();
            innerToken->length = riter->length();
            innerToken->type = SemanticTokenTypes::Comment;
            v2.push_back(innerToken);
            ++riter;
        }

        // Tokenize operators:
        // -------------------
        v1.clear();
        lower = 0;
        for (viter = v2.begin(); viter != v2.end(); ++viter) {
            outerToken = *viter;
            upper = outerToken->position;
            riter = std::sregex_iterator(
                text.begin() + lower,
                text.begin() + upper,
                RE_F90_OPERATOR
            );
            while (riter != rend) {
                innerToken = &m_tokens.emplace_back();
                innerToken->position = riter->position();
                innerToken->length = riter->length();
                innerToken->type = SemanticTokenTypes::Operator;
                v1.push_back(innerToken);
                ++riter;
            }
            lower = upper + outerToken->length;
            v1.push_back(outerToken);
        }

        riter = std::sregex_iterator(
            text.begin() + lower,
            text.end(),
            RE_F90_OPERATOR
        );
        while (riter != rend) {
            innerToken = &m_tokens.emplace_back();
            innerToken->position = riter->position();
            innerToken->length = riter->length();
            innerToken->type = SemanticTokenTypes::Operator;
            v1.push_back(innerToken);
            ++riter;
        }

        // Tokenize identifiers:
        // ---------------------
        v2.clear();
        lower = 0;
        for (viter = v1.begin(); viter != v1.end(); ++viter) {
            outerToken = *viter;
            upper = outerToken->position;
            riter = std::sregex_iterator(
                text.begin() + lower,
                text.begin() + upper,
                RE_F90_IDENTIFIER
            );
            while (riter != rend) {
                innerToken = &m_tokens.emplace_back();
                innerToken->position = riter->position();
                innerToken->length = riter->length();
                if (std::regex_match(riter->str(), RE_F90_KEYWORD)) {
                    innerToken->type = SemanticTokenTypes::Keyword;
                } else {
                    innerToken->type = SemanticTokenTypes::Variable;
                }
                v2.push_back(innerToken);
                ++riter;
            }
            lower = upper + outerToken->length;
            v2.push_back(outerToken);
        }

        riter = std::sregex_iterator(
            text.begin() + lower,
            text.end(),
            RE_F90_IDENTIFIER
        );
        while (riter != rend) {
            innerToken = &m_tokens.emplace_back();
            innerToken->position = riter->position();
            innerToken->length = riter->length();
            if (std::regex_match(riter->str(), RE_F90_KEYWORD)) {
                innerToken->type = SemanticTokenTypes::Keyword;
            } else {
                innerToken->type = SemanticTokenTypes::Variable;
            }
            v2.push_back(innerToken);
            ++riter;
        }

        // // Tokenize numbers:
        // // -----------------
        // lower = 0;
        // for (viter = v2.begin(); viter != v2.end(); ++viter) {
        //     outerToken = *viter;
        //     upper = outerToken->position;
        //     riter = std::sregex_iterator(
        //         text.begin() + lower,
        //         text.begin() + upper,
        //         RE_F90_NUMBER
        //     );
        //     while (riter != rend) {
        //         innerToken = &m_tokens.emplace_back();
        //         innerToken->position = riter->position();
        //         innerToken->length = riter->length();
        //         innerToken->type = SemanticTokenTypes::Number;
        //         ++riter;
        //     }
        //     lower = upper + outerToken->length;
        // }

        // riter = std::sregex_iterator(
        //     text.begin() + lower,
        //     text.end(),
        //     RE_F90_NUMBER
        // );
        // while (riter != rend) {
        //     innerToken = &m_tokens.emplace_back();
        //     innerToken->position = riter->position();
        //     innerToken->length = riter->length();
        //     innerToken->type = SemanticTokenTypes::Number;
        //     ++riter;
        // }

        // Sort the tokens in order of position
        std::sort(
            m_tokens.begin(),
            m_tokens.end(),
            [](const F90Token &lhs, const F90Token &rhs) {
                return lhs.position < rhs.position;
            }
        );
    }
} // namespace LCompilers::LanguageServerProtocol
