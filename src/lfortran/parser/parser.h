#ifndef LFORTRAN_PARSER_PARSER_H
#define LFORTRAN_PARSER_PARSER_H

#include <filesystem>

#include <libasr/utils.h>
#include <libasr/containers.h>
#include <libasr/diagnostics.h>
#include <lfortran/parser/tokenizer.h>
#include <lfortran/parser/fixedform_tokenizer.h>


namespace LCompilers::LFortran {

// Parses Fortran code to AST
// `loc_offset` is added to every location the parse produces. Interactive
// mode gives each cell a range of its own that way; everything else parses a
// whole file and leaves it at 0.
Result<AST::TranslationUnit_t*> parse(Allocator &al,
    const std::string &s,
    diag::Diagnostics &diagnostics,
    const CompilerOptions &co,
    uint32_t loc_offset=0);

// Tokenizes the `input` and return a list of tokens
Result<std::vector<int>> tokens(Allocator &al, const std::string &input,
        diag::Diagnostics &diagnostics,
        std::vector<YYSTYPE> *stypes,
        std::vector<Location> *locations,
        bool fixed_form,
        bool continue_compilation = false);

// Converts token number to text
std::string token2text(const int token);

// With `openmp`, free-form `!$` conditional compilation lines are compiled
Result<std::string> prescan(const std::string &s, LocationManager &lm,
        bool fixed_form, bool openmp,
        std::vector<std::filesystem::path> &include_dirs,
        diag::Diagnostics &diagnostics);

} // namespace LCompilers::LFortran

#endif
