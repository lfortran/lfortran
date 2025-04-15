#include <algorithm>

#include <libasr/exception.h>

#include <bin/semantic_highlighter.h>

namespace LCompilers::LanguageServerProtocol {
    const std::regex RE_FORTRAN_IDENTIFIER{R"([a-z][a-z0-9_]*)", std::regex::icase};
    const std::regex RE_FORTRAN_KEYWORD{
        R"(\b(?:program|integer|real|double\s*precision|complex|character|logical|if|then|else|endif|do|while|enddo|continue|stop|pause|write|read|print|open|close|inquire|format|call|return|end|subroutine|function|implicit|none|data|parameter|allocate|deallocate|module|use|interface|type|public|private|contains|where|forall)\b)",
        std::regex::icase
    };

    auto semantic_tokenize(const std::string &text) -> std::vector<FortranToken> {
        std::vector<FortranToken> tokens;

        std::sregex_iterator riter = std::sregex_iterator(
            text.begin(),
            text.end(),
            RE_FORTRAN_IDENTIFIER
        );
        std::sregex_iterator rend;
        while (riter != rend) {
            FortranToken *innerToken = &tokens.emplace_back();
            innerToken->position = riter->position();
            innerToken->length = riter->length();
            if (std::regex_match(riter->str(), RE_FORTRAN_KEYWORD)) {
                innerToken->type = SemanticTokenTypes::Keyword;
            } else {
                innerToken->type = SemanticTokenTypes::Variable;
            }
            ++riter;
        }

        return tokens;
    }
} // namespace LCompilers::LanguageServerProtocol
