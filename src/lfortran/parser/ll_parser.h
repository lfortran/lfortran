#ifndef LFORTRAN_PARSER_LL_PARSER_H
#define LFORTRAN_PARSER_LL_PARSER_H

#include <vector>
#include <lfortran/parser/parser_stype.h>
#include <libasr/diagnostics.h>

namespace LCompilers::LFortran {

// Thrown for a construct outside the subset that the prototype parses
struct LLUnsupported {
    int line; // line in ll_parser.cpp that gave up
    int token; // the kind of the current token
    size_t pos; // its position in the source
};

// The tokens of a whole file. The arrays are reused between files.
struct LLTokens {
    std::vector<int> kind;
    std::vector<YYSTYPE> value;
    std::vector<Location> loc;
    size_t n = 0;
    std::string input; // a copy of the input, if a newline had to be added
};

void ll_tokenize(Allocator &al, const std::string &input,
    diag::Diagnostics &diagnostics, LLTokens &tokens, bool openmp=false,
    uint32_t loc_offset=0);

// Parses free-form Fortran (already prescanned) with the prototype LL parser.
// Throws LLUnsupported if it cannot parse the input (it reports no syntax
// errors itself).
AST::TranslationUnit_t* ll_parse(Allocator &al, const std::string &input,
    diag::Diagnostics &diagnostics, LLTokens &tokens, bool openmp=false,
    uint32_t loc_offset=0);

// The same, but returns nullptr (and adds no diagnostics) if the LL parser
// cannot parse the input, so that the caller can use the Bison parser.
AST::TranslationUnit_t* ll_parse_or_null(Allocator &al,
    const std::string &input, diag::Diagnostics &diagnostics, bool openmp,
    uint32_t loc_offset);

} // namespace LCompilers::LFortran

#endif
