#ifndef LFORTRAN_SRC_PARSER_FIXEDFORM_TOKENIZER_H
#define LFORTRAN_SRC_PARSER_FIXEDFORM_TOKENIZER_H

#include <libasr/exception.h>
#include <lfortran/parser/parser_stype.h>

namespace LCompilers::LFortran {

class FixedFormTokenizer
{
public:
    unsigned char *cur;
    unsigned char *tok;
    unsigned char *cur_line;
    unsigned int line_num;
    unsigned char *string_start;

    int last_token=-1;

    std::vector<uint64_t> enddo_label_stack = {0};
    bool enddo_newline_process = false;
    int enddo_state = 0;
    int enddo_insert_count = 0;

    std::vector<int> tokens;
    std::vector<YYSTYPE> stypes;
    std::vector<Location> locations;

    bool tokenized = false;
    unsigned int token_pos = 0;

public:
    // Set the string to tokenize. The caller must ensure `str` will stay valid
    // as long as `lex` is being called.
    void set_string(const std::string &str);

    // Tokenizes the whole input and saves all tokens into an internal array
    // The lex function then just iterates on this array and returns the next
    // token
    // Returns True if successful, otherwise there will be errors in
    // `diagnostics`
    bool tokenize_input(diag::Diagnostics &diagnostics, Allocator &al);

    // Get next token. Token ID is returned as function result, the semantic
    // value is put into `yylval`.
    int lex(Allocator &al, YYSTYPE &yylval, Location &loc, diag::Diagnostics &diagnostics);

    // Return the current token as std::string
    std::string token() const
    {
        return std::string((char *)tok, cur - tok);
    }

    // Return the string of the token at the given location `loc`
    std::string token_at_loc(const Location &loc) {
        return std::string((char *)(string_start + loc.first),
            loc.last - loc.first + 1);
    }

    // Return the current token as YYSTYPE::Str
    void token(Str &s) const
    {
        s.p = (char*) tok;
        s.n = cur-tok;
    }

    // Return the current token as YYSTYPE::Str, strips first and last character
    void token_str(Str &s) const
    {
        s.p = (char*) tok + 1;
        s.n = cur-tok-2;
    }

    // Return the current token's location
    void token_loc(Location &loc) const
    {
        loc.first = tok-string_start;
        loc.last = cur-string_start-1;
    }
    void add_rel_warning(diag::Diagnostics &diagnostics, int rel_token) const;
};

bool lex_int(const unsigned char *s, const unsigned char *e, uint64_t &u,
    Str &suffix);

} // namespace LCompilers::LFortran

#endif
