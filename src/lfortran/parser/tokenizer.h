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
    bool openmp_enabled=false;

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
    // The kind prefix is allocated in Arena as a Str* to keep StrPrefix at 24 bytes
    void lex_string(Allocator &al, StrPrefix &str_prefix, char ch) const
    {
        // The tokenizer has already identified this as a string with or without prefix
        // tok points to the start, cur points past the closing quote
        // Format: [kind_]"content" or [kind_]'content'
        
        unsigned char *p = tok;
        str_prefix.str_kind = nullptr;
        
        // Find the opening quote (first occurrence of ch)
        while (p < cur && *p != ch) {
            p++;
        }
        
        // Check if there's a kind prefix (underscore immediately before opening quote)
        if (p > tok && *(p - 1) == '_' && p - 1 > tok) {
            // Found kind prefix: everything from tok to (p-1) excluding the '_'
            str_prefix.str_kind = al.make_new<Str>();
            str_prefix.str_kind->p = (char*)tok;
            str_prefix.str_kind->n = (p - 1) - tok;
            
            // String content starts after opening quote, ends before closing quote
            str_prefix.str_s.p = (char*)(p + 1);
            str_prefix.str_s.n = cur - p - 2; // cur is past closing quote
            str_prefix.str_s.p = str_unescape_fortran(al, str_prefix.str_s, ch);
            str_prefix.str_s.n = strlen(str_prefix.str_s.p);
        } else {
            // No kind prefix - just extract string between quotes
            str_prefix.str_s.p = (char*) tok + 1;
            str_prefix.str_s.n = cur - tok - 2;
            str_prefix.str_s.p = str_unescape_fortran(al, str_prefix.str_s, ch);
            str_prefix.str_s.n = strlen(str_prefix.str_s.p);
        }
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
