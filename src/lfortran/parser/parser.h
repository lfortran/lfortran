#ifndef LFORTRAN_PARSER_PARSER_H
#define LFORTRAN_PARSER_PARSER_H

#include <fstream>
#include <algorithm>
#include <memory>

#include <libasr/containers.h>
#include <libasr/diagnostics.h>
#include <lfortran/parser/tokenizer.h>
#include <lfortran/parser/fixedform_tokenizer.h>


namespace LFortran
{

class Parser
{
public:
    std::string inp;

public:
    diag::Diagnostics &diag;
    Allocator &m_a;
    Tokenizer m_tokenizer;
    FixedFormTokenizer f_tokenizer;
    Vec<AST::ast_t*> result;
    bool fixed_form;

    Parser(Allocator &al, diag::Diagnostics &diagnostics, const bool &fixed_form=false)
            : diag{diagnostics}, m_a{al}, fixed_form{fixed_form}{
        result.reserve(al, 32);
    }

    bool parse(const std::string &input);
    void handle_yyerror(const Location &loc, const std::string &msg);
};


// Parses Fortran code to AST
Result<AST::TranslationUnit_t*> parse(Allocator &al,
    const std::string &s,
    diag::Diagnostics &diagnostics,
    const bool &fixed_form=false);

// Tokenizes the `input` and return a list of tokens
Result<std::vector<int>> tokens(Allocator &al, const std::string &input,
        diag::Diagnostics &diagnostics,
        std::vector<YYSTYPE> *stypes,
        std::vector<Location> *locations,
        bool fixed_form);

// Converts token number to text
std::string token2text(const int token);

std::string prescan(const std::string &s, LocationManager &lm,
        bool fixed_form, const std::string &root_dir);

} // namespace LFortran

#endif
