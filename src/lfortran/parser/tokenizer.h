#ifndef LFORTRAN_SRC_PARSER_TOKENIZER_H
#define LFORTRAN_SRC_PARSER_TOKENIZER_H

#include <libasr/exception.h>
#include <lfortran/parser/parser_stype.h>

namespace LCompilers::LFortran {

class Tokenizer
{
public:
    unsigned char *cur;
    unsigned char *tok;
    unsigned char *cur_line;
    unsigned int line_num;
    unsigned char *string_start;
    bool fixed_form=false;

    int last_token=-1;

    std::vector<uint64_t> enddo_label_stack = {0};
    bool enddo_newline_process = false;
    int enddo_state = 0;
    int enddo_insert_count = 0;

public:
    // Set the string to tokenize. The caller must ensure `str` will stay valid
    // as long as `lex` is being called.
    void set_string(const std::string &str);

    // Get next token. Token ID is returned as function result, the semantic
    // value is put into `yylval`.
    int lex(Allocator &al, YYSTYPE &yylval, Location &loc, diag::Diagnostics &diagnostics, bool continue_compilation);

    // Return the current token as std::string
    std::string token() const
    {
        return std::string((char *)tok, cur - tok);
    }

    // Return the current token as YYSTYPE::Str
    void token(Str &s) const
    {
        s.p = (char*) tok;
        s.n = cur-tok;
    }

    // Return the current token as YYSTYPE::Str, strips first and last character
    void token_str(Allocator &al, Str &s, char ch) const
    {
        s.p = (char*) tok + 1;
        s.n = cur-tok-2;
        s.p = str_unescape_fortran(al, s, ch);
        s.n = strlen(s.p);
    }

    // Parse string literal with optional kind prefix (e.g., tfc_"#" or "#")
    // Extracts both the string content and the kind prefix (if present)
    void lex_string(Allocator &al, StrSuffix &str_suffix, char ch) const
    {
        // Check for kind prefix: kind_"..." or kind_'...'
        unsigned char *p = tok;
        str_suffix.str_kind.p = nullptr;
        str_suffix.str_kind.n = 0;
        
        while (p < cur && *p != ch) {
            if (*p == '_' && p + 1 < cur && *(p + 1) == ch) {
                // Found kind prefix
                str_suffix.str_kind.p = (char*)tok;
                str_suffix.str_kind.n = p - tok;
                
                // String starts after '_' and opening quote
                str_suffix.str_s.p = (char*)(p + 2);
                str_suffix.str_s.n = cur - (p + 2) - 1; // -1 for closing quote
                str_suffix.str_s.p = str_unescape_fortran(al, str_suffix.str_s, ch);
                str_suffix.str_s.n = strlen(str_suffix.str_s.p);
                return;
            }
            p++;
        }
        
        // No kind prefix found - just extract string
        str_suffix.str_s.p = (char*) tok + 1;
        str_suffix.str_s.n = cur-tok-2;
        str_suffix.str_s.p = str_unescape_fortran(al, str_suffix.str_s, ch);
        str_suffix.str_s.n = strlen(str_suffix.str_s.p);
    }

    // Return the current token's location
    void token_loc(Location &loc) const
    {
        loc.first = tok-string_start;
        loc.last = cur-string_start-1;
    }
    void add_rel_warning(diag::Diagnostics &diagnostics, bool fixed_form, int rel_token) const;
};

bool lex_int(const unsigned char *s, const unsigned char *e, uint64_t &u,
    Str &suffix);
void lex_int_large(Allocator &al, const unsigned char *s,
    const unsigned char *e, BigInt::BigInt &u, Str &suffix);
void lex_format(unsigned char *&cur, Location &loc,
        unsigned char *&start, diag::Diagnostics &diagnostics, bool continue_compilation, unsigned char *&string_start);


} // namespace LCompilers::LFortran

#endif
