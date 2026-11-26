#ifndef LFORTRAN_PARSER_LL_PARSER_H
#define LFORTRAN_PARSER_LL_PARSER_H

#include <vector>
#include <lfortran/parser/parser_stype.h>
#include <libasr/diagnostics.h>
#include <libasr/utils.h>

namespace LCompilers::LFortran {

// Thrown inside the parser at a token that a rule does not expect
struct LLUnsupported {
    int line; // line of the grammar (or of ll_parser.cpp) that gave up
    int token; // the kind of the current token
    size_t pos; // its position in the source
};

// The tokens of a whole file. The arrays are reused between files.
struct LLTokens {
    std::vector<int> kind;
    std::vector<YYSTYPE> value;
    std::vector<Location> loc;
    size_t n = 0;
    // The source that the locations refer to: the input, or `input` below
    // if a newline had to be added at its end
    const std::string *source = nullptr;
    std::string input;
};

void ll_tokenize(Allocator &al, const std::string &input,
    diag::Diagnostics &diagnostics, LLTokens &tokens, bool openmp=false,
    uint32_t loc_offset=0, bool continue_compilation=false);

// Parses free-form Fortran (already prescanned) to AST with the LL parser
Result<AST::TranslationUnit_t*> ll_parse(Allocator &al,
    const std::string &input, diag::Diagnostics &diagnostics,
    LLTokens &tokens, const CompilerOptions &co, uint32_t loc_offset=0);

} // namespace LCompilers::LFortran

#endif
