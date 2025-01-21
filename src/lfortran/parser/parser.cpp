#include <iostream>
#include <string>
#include <cctype>

#include <lfortran/parser/parser.h>
#include <lfortran/parser/parser.tab.hh>
#include <libasr/diagnostics.h>
#include <libasr/string_utils.h>
#include <lfortran/parser/parser_exception.h>
#include <lfortran/parser/fixedform_tokenizer.h>
#include <lfortran/utils.h>

#include <lfortran/pickle.h>

namespace LCompilers::LFortran {

bool is_program_needed(AST::TranslationUnit_t &ast) {
    for (size_t i = 0; i < ast.n_items; i++) {
        if (!AST::is_a<AST::mod_t>(*ast.m_items[i])
            && !AST::is_a<AST::program_unit_t>(*ast.m_items[i])) {
            return true;
        }
    }
    return false;
}

bool is_program_end(AST::Name_t* name) {
    // "end program" should not be a name. Ideally, it should be a special entity.
    // It is used as a Name_t here, so that `end`, `endprogram` and `end program`
    // can all be handled together and this simplifies the code logic.
    return (to_lower(name->m_id) == "end" || to_lower(name->m_id) == "endprogram"
            || to_lower(name->m_id) == "end program");
}

void fix_program_without_program_line(Allocator &al, AST::TranslationUnit_t &ast) {
    Vec<AST::ast_t*> global_items; global_items.reserve(al, 0);
    Vec<AST::unit_decl1_t*> use; use.reserve(al, 0);
    Vec<AST::implicit_statement_t*> implicit; implicit.reserve(al, 0);
    Vec<AST::unit_decl2_t*> decl; decl.reserve(al, 0);
    Vec<AST::stmt_t*> body; body.reserve(al, 0);
    Vec<AST::program_unit_t*> contains_body; contains_body.reserve(al, 0);
    bool contains = false, program_added = false;
    for (size_t i = 0; i < ast.n_items; i++) {
        if (program_added) {
            if (ast.m_items[i]->type == AST::astType::program_unit
                || ast.m_items[i]->type == AST::astType::mod) {
                global_items.push_back(al, ast.m_items[i]);
            } else {
                throw parser_local::ParserError("Only function, subroutine, procedure, module, submodule or"
                    " block data allowed in global scope in non-interactive mode", ast.m_items[i]->loc);
            }
        } else if (contains) {
            if (ast.m_items[i]->type == AST::astType::program_unit) {
                contains_body.push_back(al, AST::down_cast<AST::program_unit_t>(ast.m_items[i]));
            } else if (ast.m_items[i]->type == AST::astType::expr) {
                AST::expr_t* expr = AST::down_cast<AST::expr_t>(ast.m_items[i]);
                if (AST::is_a<AST::Name_t>(*expr)) {
                    AST::Name_t* name = AST::down_cast<AST::Name_t>(expr);
                    if (is_program_end(name)) {
                        AST::ast_t* program_ast = AST::make_Program_t(al, ast.base.base.loc, s2c(al, "__xx_main"), nullptr,
                            use.p, use.size(), implicit.p, implicit.size(), decl.p, decl.size(),
                            body.p, body.size(), contains_body.p, contains_body.size());

                        global_items.push_back(al, program_ast);
                        program_added = true;
                    } else {
                        throw parser_local::ParserError("Expected function, subroutine, procedure in program contains", name->base.base.loc);
                    }
                } else {
                    throw parser_local::ParserError("Expected function, subroutine, procedure in program contains", expr->base.loc);
                }
            } else {
                throw parser_local::ParserError("Expected function, subroutine, procedure in program contains", ast.m_items[i]->loc);
            }
        } else if (ast.m_items[i]->type == AST::astType::stmt) {
            body.push_back(al, AST::down_cast<AST::stmt_t>(ast.m_items[i]));
        } else if (ast.m_items[i]->type == AST::astType::unit_decl1) {
            // use module_name
            use.push_back(al, AST::down_cast<AST::unit_decl1_t>(ast.m_items[i]));
        } else if (ast.m_items[i]->type == AST::astType::implicit_statement) {
            implicit.push_back(al, AST::down_cast<AST::implicit_statement_t>(ast.m_items[i]));
        } else if (ast.m_items[i]->type == AST::astType::unit_decl2) {
            // Declaration, Interface, DerivedType, Template, Enum, Instantiate, Requirement, Requires
            decl.push_back(al, AST::down_cast<AST::unit_decl2_t>(ast.m_items[i]));
        } else if (ast.m_items[i]->type == AST::astType::expr) {
            AST::expr_t* expr = AST::down_cast<AST::expr_t>(ast.m_items[i]);
            if (AST::is_a<AST::Name_t>(*expr)) {
                AST::Name_t* name = AST::down_cast<AST::Name_t>(expr);
                if (to_lower(name->m_id) == "stop") {
                    AST::ast_t* stop_ast = AST::make_Stop_t(al, name->base.base.loc, 0, nullptr, nullptr, nullptr);
                    body.push_back(al, AST::down_cast<AST::stmt_t>(stop_ast));
                } else if (to_lower(name->m_id) == "return") {
                    AST::ast_t* return_ast = AST::make_Return_t(al, name->base.base.loc, 0, nullptr, nullptr);
                    body.push_back(al, AST::down_cast<AST::stmt_t>(return_ast));
                } else if (to_lower(name->m_id) == "exit") {
                    AST::ast_t* exit_ast = AST::make_Exit_t(al, name->base.base.loc, 0, name->m_id, nullptr);
                    body.push_back(al, AST::down_cast<AST::stmt_t>(exit_ast));
                } else if (to_lower(name->m_id) == "cycle") {
                    AST::ast_t* cycle_ast = AST::make_Cycle_t(al, name->base.base.loc, 0, name->m_id, nullptr);
                    body.push_back(al, AST::down_cast<AST::stmt_t>(cycle_ast));
                } else if (to_lower(name->m_id) == "continue") {
                    AST::ast_t* continue_ast = AST::make_Continue_t(al, name->base.base.loc, 0, nullptr);
                    body.push_back(al, AST::down_cast<AST::stmt_t>(continue_ast));
                } else if (is_program_end(name)) {
                    AST::ast_t* program_ast = AST::make_Program_t(al, ast.base.base.loc, s2c(al, "__xx_main"), nullptr,
                    use.p, use.size(), implicit.p, implicit.size(), decl.p, decl.size(),
                    body.p, body.size(), contains_body.p, contains_body.size());

                    global_items.push_back(al, program_ast);
                    program_added = true;
                } else if (to_lower(name->m_id) == "contains") {
                    contains = true;
                } else {
                    throw parser_local::ParserError("Statement or Declaration expected inside program, found Variable name", ast.m_items[i]->loc);
                }
            } else if (AST::is_a<AST::FuncCallOrArray_t>(*expr)) {
                AST::FuncCallOrArray_t* func_call_or_array = AST::down_cast<AST::FuncCallOrArray_t>(expr);
                if (to_lower(func_call_or_array->m_func) == "allocate") {
                    AST::ast_t* allocate_ast = AST::make_Allocate_t(al,
                                                    func_call_or_array->base.base.loc,
                                                    0,
                                                    func_call_or_array->m_args,
                                                    func_call_or_array->n_args,
                                                    func_call_or_array->m_keywords,
                                                    func_call_or_array->n_keywords,
                                                    nullptr);

                    body.push_back(al, AST::down_cast<AST::stmt_t>(allocate_ast));
                } else if (to_lower(func_call_or_array->m_func) == "deallocate") {
                    AST::ast_t* deallocate_ast = AST::make_Deallocate_t(al,
                                                    func_call_or_array->base.base.loc,
                                                    0,
                                                    func_call_or_array->m_args,
                                                    func_call_or_array->n_args,
                                                    func_call_or_array->m_keywords,
                                                    func_call_or_array->n_keywords,
                                                    nullptr);

                    body.push_back(al, AST::down_cast<AST::stmt_t>(deallocate_ast));
                } else if (to_lower(func_call_or_array->m_func) == "open") {
                    Vec<AST::expr_t*> args; args.reserve(al, func_call_or_array->n_args);
                    for (size_t j = 0; j < func_call_or_array->n_args; j++) {
                        args.push_back(al, func_call_or_array->m_args[j].m_end);
                    }
                    AST::ast_t* open_ast = AST::make_Open_t(al,
                                                    func_call_or_array->base.base.loc,
                                                    0,
                                                    args.p,
                                                    args.n,
                                                    func_call_or_array->m_keywords,
                                                    func_call_or_array->n_keywords,
                                                    nullptr);

                    body.push_back(al, AST::down_cast<AST::stmt_t>(open_ast));
                } else if (to_lower(func_call_or_array->m_func) == "close") {
                    Vec<AST::expr_t*> args; args.reserve(al, func_call_or_array->n_args);
                    for (size_t j = 0; j < func_call_or_array->n_args; j++) {
                        args.push_back(al, func_call_or_array->m_args[j].m_end);
                    }
                    AST::ast_t* close_ast = AST::make_Close_t(al,
                                                    func_call_or_array->base.base.loc,
                                                    0,
                                                    args.p,
                                                    args.n,
                                                    func_call_or_array->m_keywords,
                                                    func_call_or_array->n_keywords,
                                                    nullptr);

                    body.push_back(al, AST::down_cast<AST::stmt_t>(close_ast));
                } else if (to_lower(func_call_or_array->m_func) == "nullify") {
                    Vec<AST::expr_t*> args; args.reserve(al, func_call_or_array->n_args);
                    for (size_t j = 0; j < func_call_or_array->n_args; j++) {
                        args.push_back(al, func_call_or_array->m_args[j].m_end);
                    }
                    AST::ast_t* nullify_ast = AST::make_Nullify_t(al,
                                                    func_call_or_array->base.base.loc,
                                                    0,
                                                    args.p,
                                                    args.n,
                                                    func_call_or_array->m_keywords,
                                                    func_call_or_array->n_keywords,
                                                    nullptr);

                    body.push_back(al, AST::down_cast<AST::stmt_t>(nullify_ast));
                } else if (to_lower(func_call_or_array->m_func) == "flush") {
                    Vec<AST::expr_t*> args; args.reserve(al, func_call_or_array->n_args);
                    for (size_t j = 0; j < func_call_or_array->n_args; j++) {
                        args.push_back(al, func_call_or_array->m_args[j].m_end);
                    }
                    AST::ast_t* flush_ast = AST::make_Flush_t(al,
                                                    func_call_or_array->base.base.loc,
                                                    0,
                                                    args.p,
                                                    args.n,
                                                    func_call_or_array->m_keywords,
                                                    func_call_or_array->n_keywords,
                                                    nullptr);

                    body.push_back(al, AST::down_cast<AST::stmt_t>(flush_ast));
                }
            } else {
                throw parser_local::ParserError("Statement or Declaration expected inside program, found Expression", ast.m_items[i]->loc);
            }
        } else {
            global_items.push_back(al, ast.m_items[i]);
        }
    }
    if (!program_added) {
        throw parser_local::ParserError("Expected program end", ast.base.base.loc);
    }
    ast.m_items = global_items.p;
    ast.n_items = global_items.size();
}

Result<AST::TranslationUnit_t*> parse(Allocator &al, const std::string &s,
        diag::Diagnostics &diagnostics, const CompilerOptions &co)
{
    Parser p(al, diagnostics, co.fixed_form, co.continue_compilation);
    try {
        if (!p.parse(s)) {
            if (!co.continue_compilation) {
                return Error();
            }
        };
    } catch (const parser_local::TokenizerError &e) {
        Error error;
        diagnostics.diagnostics.push_back(e.d);
        return error;
    } catch (const parser_local::ParserError &e) {
        Error error;
        diagnostics.diagnostics.push_back(e.d);
        return error;
    }
    Location l;
    if (p.result.size() == 0) {
        l.first=0;
        l.last=0;
    } else {
        l.first=p.result[0]->loc.first;
        l.last=p.result[p.result.size()-1]->loc.last;
    }
    AST::TranslationUnit_t* ast = (AST::TranslationUnit_t*)AST::make_TranslationUnit_t(al, l,
        p.result.p, p.result.size());
    if (!co.interactive && !co.fixed_form && is_program_needed(*ast)) {
        try {
            fix_program_without_program_line(al, *ast);
        } catch (const parser_local::ParserError &e) {
            Error error;
            diagnostics.diagnostics.push_back(e.d);
            if (!co.continue_compilation) {
                return error;
            }
        }
    }
    return ast;
}

bool Parser::parse(const std::string &input)
{
    inp = input;
    if (inp.size() > 0) {
        if (inp[inp.size()-1] != '\n') inp.append("\n");
    } else {
        inp.append("\n");
    }
    if (!fixed_form) {
        m_tokenizer.set_string(inp);
        try {
            if (yyparse(*this) == 0) {
                if (diag.has_error())
                    return false;
                return true;
            }
        } catch (const parser_local::TokenizerAbort &e) {
            return false;
        }
    } else {
        f_tokenizer.set_string(inp);
        if (!f_tokenizer.tokenize_input(diag, m_a)) return false;
        if (yyparse(*this) == 0) {
            if (diag.has_error())
                return false;
            return true;
        }
    }

    if (!diag.has_error()) {
        if (this->continue_compilation) {
            diag.add(parser_local::ParserError("Parsing unsuccessful (internal compiler error)").d);
        } else {
            throw parser_local::ParserError("Parsing unsuccessful (internal compiler error)");
        }
    }
    return false;
}

Result<std::vector<int>> tokens(Allocator &al, const std::string &input,
        diag::Diagnostics &diagnostics,
        std::vector<YYSTYPE> *stypes,
        std::vector<Location> *locations,
        bool fixed_form,
        bool continue_compilation)
{
    if (fixed_form) {
        FixedFormTokenizer t;
        t.set_string(input);
        try {
            if (t.tokenize_input(diagnostics, al)) {
                LCOMPILERS_ASSERT(t.tokens.size() == t.stypes.size())
                if (stypes) {
                    for(const auto & el : t.stypes) {
                        stypes->push_back(el);
                    }
                }
                if (locations) {
                    for(const auto & el : t.locations) {
                        locations->push_back(el);
                    }
                }
            } else {
                return Error();
            };
        } catch (const parser_local::TokenizerAbort &e) {
            if (!continue_compilation) {
                return Error();
            }
        }
        return t.tokens;
    } else {
        Tokenizer t;
        t.set_string(input);
        std::vector<int> tst;
        int token = yytokentype::END_OF_FILE + 1; // Something different from EOF
        while (token != yytokentype::END_OF_FILE) {
            YYSTYPE y;
            Location l;
            try {
                token = t.lex(al, y, l, diagnostics, continue_compilation);
            } catch (const parser_local::TokenizerError &e) {
                diagnostics.diagnostics.push_back(e.d);
                return Error();
            } catch (const parser_local::TokenizerAbort &e) {
                return Error();
            }
            tst.push_back(token);
            if (stypes) stypes->push_back(y);
            if (locations) locations->push_back(l);
        }
        return tst;
    }
}

char previous_nonspace_character(const std::string &s, size_t pos) {
    while (pos > 0) {
        --pos;
        if (s[pos] != ' ' && s[pos] != '\t') {
            return s[pos];
        }
    }
    return '\0';
}

char next_nonspace_character(const std::string &s, size_t pos) {
    pos++;
    while (pos < s.size()) {
        if (s[pos] != ' ' && s[pos] != '\t') {
            return s[pos];
        }
        ++pos;
    }
    return '\0';
}


void is_within_string(
    const std::string &s,
    size_t pos,
    char &quote,
    const bool in_comment,
    bool &in_string
) {
    // when in a comment, as everything is ignored, wearen't in string
    if (in_comment) {
        return;
    }
    if ((s[pos] == '\'' || s[pos] == '"')) {
        if (quote == '\0') {
            in_string = true;
            quote = s[pos];
        } else if (quote == s[pos]) {
            in_string = false;
            quote = '\0';
        }
    }
}

void cont1(
    const std::string &s,
    size_t &pos,
    bool &in_string,
    char &quote,
    bool &ws_or_comment
) {
    ws_or_comment = true;
    bool in_comment = false;
    while (s[pos] != '\n') {
        // when in a comment, as everything is ignore, wearen't in string
        is_within_string(s, pos, quote, in_comment, in_string);
        // in a string if '&!' appear together then it isn't a comment
        if (s[pos] == '!' && (!in_string || previous_nonspace_character(s, pos) != '&')) {
            in_comment = true;
        }
        if (!in_comment) {
            if (s[pos] != ' ' && s[pos] != '\t') {
                ws_or_comment = false;
                return;
            }
        }
        pos++;
    }
    pos++;
}

bool is_digit(unsigned char ch) {
    return (ch >= '0' && ch <= '9');
}

enum LineType {
    Comment, Statement, LabeledStatement, Continuation, EndOfFile,
    ContinuationTab, StatementTab, Include,
};

// Determines the type of line in the fixed-form prescanner
// `pos` points to the first character (column) of the line
// The line ends with either `\n` or `\0`.  Only used for fixed-form
LineType determine_line_type(const unsigned char *pos)
{
    int col=1;
    if (*pos == '\n') {
        // Empty line => classified as comment
        return LineType::Comment;
    } else if (*pos == '*' || *pos == 'c' || *pos == 'C' || *pos == '!') {
        // Comment
        return LineType::Comment;
    } else if (*pos == '\0') {
        return LineType::EndOfFile;
    } else if (*pos == '\t') {
        pos++;
        if (*pos == '\0') {
            return LineType::EndOfFile;
        } else {
            if (is_digit(*pos)) {
                // A continuation line after a tab
                return LineType::ContinuationTab;
            } else {
                // A statement line after a tab
                return LineType::StatementTab;
            }

        }
    } else {
        while (*pos == ' ') {
            pos++;
            col+=1;
        }
        if (*pos == '\n' || *pos == '\0' || (*pos == '\r' && *(pos+1) == '\n')
	    || col > 72)
	  return LineType::Comment;
        if (*pos == '!' && col != 6) return LineType::Comment;
        if (col == 6) {
            if (*pos == ' ' || *pos == '0') {
                return LineType::Statement;
            } else {
                return LineType::Continuation;
            }
        }
        if (col <= 6) {
            return LineType::LabeledStatement;
        } else if (str_compare(pos, "include")) {
            return LineType::Include;
        } else {
            return LineType::Statement;
        }
    }
}

void skip_rest_of_line(const std::string &s, size_t &pos)
{
    while (pos < s.size() && s[pos] != '\n') {
        pos++;
    }
    pos++; // Skip the last '\n'
}

// Parses string, including possible continuation lines
void parse_string(std::string &out, const std::string &s, size_t &pos,
    bool fixed_form, int &col)
{
    char quote = s[pos];
    LCOMPILERS_ASSERT(quote == '"' || quote == '\'');
    out += s[pos];
    pos++;
    col++;

    while (pos < s.size()) {
        if (fixed_form) {
	    if (col > 72) {
		skip_rest_of_line(s, pos);
		col = 7;
		pos += 6;
		continue;
	    } else if (s[pos] == quote && (col == 72 || s[pos+1] != quote)) {
		break;
	    }
        } else {
	    if (s[pos] == quote && s[pos+1] != quote) break;
	}
        if (s[pos] == '\n') {
            pos++;
            if (fixed_form) {
                col = 7;
                pos += 6;
            } else {
                col = 1;
            }
            continue;
        }
        if (s[pos] == quote && s[pos+1] == quote && (!fixed_form || col < 72)) {
	    // Emit a doubled quote
            out += s[pos];
            pos++;
            col++;
        }
        out += s[pos];
        pos++;
        col++;
    }
    if (pos < s.size()) {
	out += s[pos]; // Copy the last quote
	pos++;
	col++;
    }
}

bool is_num(char c)
{
    return '0' <= c && c <= '9';
}

void copy_label(std::string &out, const std::string &s, size_t &pos)
{
    size_t col = 1;
    while (pos < s.size() && s[pos] != '\n' && col <= 6) {
        out += s[pos];
        pos++;
        col++;
    }
}

// Only used in fixed-form
void copy_rest_of_line(std::string &out, const std::string &s, size_t &pos,
		       LocationManager &lm, int &col)
{
    while (pos < s.size() && s[pos] != '\n') {
        if (col > 72) {
            skip_rest_of_line(s, pos);
            out += '\n';
            return;
        }
        if (s[pos] == '"' || s[pos] == '\'') {
            parse_string(out, s, pos, true, col);
        } else if (s[pos] == '!') {
            skip_rest_of_line(s, pos);
            out += '\n';
            return;
        } else if (s[pos] == ' ') {
            // Skip white space in a fixed-form parser
            pos++;
            col++;
            lm.files.back().out_start.push_back(out.size());
            lm.files.back().in_start.push_back(pos);
        } else if (s[pos] == '\r') {
            // Skip CR in a fixed-form parser
            pos++;
            // Don't advance the column count here
            lm.files.back().out_start.push_back(out.size());
            lm.files.back().in_start.push_back(pos);
        } else {
            // Copy the character, but covert to lowercase
            out += tolower(s[pos]);
            pos++;
            col++;
        }
    }
    // not always a program end's with '\n', but when it does, copy it
    if (s[pos] == '\n') {
        out += s[pos];
        pos++;
    }
}

// Checks that newlines are computed correctly
bool check_newlines(const std::string &s, const std::vector<uint32_t> &newlines) {
    std::vector<uint32_t> newlines2;
    for (uint32_t pos=0; pos < s.size(); pos++) {
        if (s[pos] == '\n') newlines2.push_back(pos);
    }
    if (newlines2.size() != newlines.size()) return false;
    for (size_t i=0; i < newlines2.size(); i++) {
        if (newlines2[i] != newlines[i]) return false;
    }
    return true;
}

void process_include(std::string& out, const std::string& s,
                     LocationManager& lm, size_t& pos, bool fixed_form,
                     std::vector<std::filesystem::path> &include_dirs,
                     int &col)
{
    std::string include_filename;
    parse_string(include_filename, s, pos, fixed_form, col);
    include_filename = include_filename.substr(1, include_filename.size() - 2);

    bool file_found = false;
    std::string include = "";
    if (is_relative_path(include_filename)) {
        for (auto &path:include_dirs) {
            std::string filepath = join_paths({path.generic_string(), include_filename});
            file_found = read_file(filepath, include);
            if (file_found) {
                include_filename = filepath;
                break;
            }
        }
    } else {
        file_found = read_file(include_filename, include);
    }

    if (!file_found) {
        throw LCompilersException("Include file '" + include_filename
            + "' not found. If an include path "
            "is available, please use the `-I` option to specify it.");
    }

    LocationManager lm_tmp;
    {
        LocationManager::FileLocations fl;
        fl.in_filename = include_filename;
        lm_tmp.files.push_back(fl);
    }
    include = prescan(include, lm_tmp, fixed_form, include_dirs);

    // Possible it goes here
    // lm.files.back().out_start.push_back(out.size());
    out += include;
    while (pos < s.size() && s[pos] != '\n') pos++;
    lm.files.back().out_start.push_back(out.size());
    lm.files.back().in_start.push_back(pos);
}

bool is_include(const std::string &s, uint32_t pos) {
    while (pos < s.size() && s[pos] == ' ') pos++;
    if (pos + 6 < s.size() && s.substr(pos, 7) == "include") {
        pos += 7;
        while (pos < s.size() && s[pos] == ' ') pos++;
        if (pos < s.size() && ((s[pos] == '"') || (s[pos] == '\''))) {
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
}

/*
The prescan phase includes:
- Removal of whitespace (fixed-form only)
- Joining of continuation lines
- Removal of comments and empty lines
- Handling of include statements
- Conversion to lowercase (fixed-form only)
- Handling of fixed-form column rules (columns 1–6 for labels/comments)
*/
std::string prescan(const std::string &s, LocationManager &lm,
        bool fixed_form, std::vector<std::filesystem::path> &include_dirs)
{
    if (fixed_form) {
        // `pos` is the position in the original code `s`
        // `out` is the final code (outcome)
        lm.get_newlines(s, lm.files.back().in_newlines);
        lm.files.back().out_start.push_back(0);
        lm.files.back().in_start.push_back(0);
        std::string out;
        size_t pos = 0;
        /* Note:
         * This is a fixed-form prescanner, which:
         *
         *   * Removes all whitespace
         *   * Joins continuation lines
         *   * Removes comments and empty lines
         *   * Handles the first 6 columns
         *   * Converts to lowercase
         *   * Removes all CR characters
         *
         * features which are currently not yet implemented:
         *
         *   * Continuation lines after comment(s) or empty lines (they will be
         *     appended to the previous comment, and thus skipped)
         *
         * After the prescanner, the tokenizer is itself a recursive descent
         * parser that correctly identifies tokens so that the Bison
         * parser can parse it correctly.
         */
        while (true) {
            const char *p = &s[pos];
            int col = 7;  // Valid after p is advanced to code begin
            LineType lt = determine_line_type((const unsigned char*)p);
            switch (lt) {
                case LineType::Comment : {
                    // Skip
                    skip_rest_of_line(s, pos);
                    lm.files.back().out_start.push_back(out.size());
                    lm.files.back().in_start.push_back(pos);
                    break;
                }
                case LineType::Statement : {
                    // Copy from column 7
                    pos += 6;
                    lm.files.back().out_start.push_back(out.size());
                    lm.files.back().in_start.push_back(pos);
                    copy_rest_of_line(out, s, pos, lm, col);
                    break;
                }
                case LineType::StatementTab : {
                    // Copy from column 2
                    pos += 1;
                    lm.files.back().out_start.push_back(out.size());
                    lm.files.back().in_start.push_back(pos);
                    copy_rest_of_line(out, s, pos, lm, col);
                    break;
                }
                case LineType::LabeledStatement : {
                    // Copy the label
                    copy_label(out, s, pos);
                    // Copy from column 7
                    lm.files.back().out_start.push_back(out.size());
                    lm.files.back().in_start.push_back(pos);
                    copy_rest_of_line(out, s, pos, lm, col);
                    break;
                }
                case LineType::Continuation : {
                    // Append from column 7 to previous line
                    out = out.substr(0, out.size()-1); // Remove the last '\n'
                    pos += 6;
                    lm.files.back().out_start.push_back(out.size());
                    lm.files.back().in_start.push_back(pos);
                    copy_rest_of_line(out, s, pos, lm, col);
                    break;
                }
                case LineType::ContinuationTab : {
                    // Append from column 3 to previous line
                    out = out.substr(0, out.size()-1); // Remove the last '\n'
                    pos += 2;
                    lm.files.back().out_start.push_back(out.size());
                    lm.files.back().in_start.push_back(pos);
                    copy_rest_of_line(out, s, pos, lm, col);
                    break;
                }
                case LineType::Include: {
                    while (pos < s.size() && s[pos] == ' ') pos++;
                    LCOMPILERS_ASSERT(s.substr(pos, 7) == "include");
                    pos += 7;
                    while (pos < s.size() && s[pos] == ' ') pos++;
                    if ((s[pos] == '"') || (s[pos] == '\'')) {
                        process_include(out, s, lm, pos, fixed_form,
                            include_dirs, col);
                    }
                    break;
                }
                case LineType::EndOfFile : {
                    break;
                }
            };
            if (lt == LineType::EndOfFile) break;
        }
        lm.files.back().in_start.push_back(pos);
        lm.files.back().out_start.push_back(out.size());
        return out;
    } else {
         // `pos` is the position in the original code `s`
        // `out` is the final code (outcome)
        lm.files.back().out_start.push_back(0);
        lm.files.back().in_start.push_back(0);
        std::string out;
        size_t pos = 0;
        bool in_comment = false, newline = true;
        // keeps track of whether we're in a string or not
        bool in_string = false;
        // if `in_string` is true, keeps track of the quote
        // used for that string
        char quote = '\0';
        while (pos < s.size()) {
            is_within_string(s, pos, quote, in_comment, in_string);
            if (newline && is_include(s, pos)) {
                int col = 0; // doesn't matter
                while (pos < s.size() && s[pos] == ' ') pos++;
                LCOMPILERS_ASSERT(pos + 6 < s.size() && s.substr(pos, 7) == "include")
                pos += 7;
                while (pos < s.size() && s[pos] == ' ') pos++;
                LCOMPILERS_ASSERT(pos < s.size() && ((s[pos] == '"') || (s[pos] == '\'')));
                process_include(out, s, lm, pos, fixed_form, include_dirs, col);
            }
            newline = false;
            if (s[pos] == '!' && !in_string) in_comment = true;
            if (in_comment && s[pos] == '\n') in_comment = false;
            if (!in_comment && s[pos] == '&' &&(next_nonspace_character(s,pos) == '\n' || next_nonspace_character(s,pos) == '!')) {
                size_t pos2=pos+1;
                bool ws_or_comment = false;
                cont1(s, pos2, in_string, quote, ws_or_comment);
                if (ws_or_comment) lm.files.back().in_newlines.push_back(pos2-1);
                if (ws_or_comment) {
                    while (ws_or_comment) {
                        cont1(s, pos2, in_string, quote, ws_or_comment);
                        if (ws_or_comment) lm.files.back().in_newlines.push_back(pos2-1);
                    }}
                    // `pos` will move by more than 1, close the old interval
                    //lm.in_size.push_back(pos-lm.in_start[lm.in_start.size()-1]);
                    // Move `pos`
                    pos = pos2;
                    // Start a new interval (just the starts, the size will be
                    // filled in later)
                    lm.files.back().out_start.push_back(out.size());
                    lm.files.back().in_start.push_back(pos);
                
            } else {
                if (s[pos] == '\n') {
                    lm.files.back().in_newlines.push_back(pos);
                    newline = true;
                }
            }
            if (!(s[pos] == '&' &&  previous_nonspace_character(s, pos) == '\n')){
                out += s[pos];
            }
            pos++;
        }
        // set the size of the last interval
    //    lm.in_size.push_back(pos-lm.in_start[lm.in_start.size()-1]);

        LCOMPILERS_ASSERT(check_newlines(s, lm.files.back().in_newlines))

        // Add the position of EOF as the last \n, whether or not the original
        // file has it
        lm.files.back().in_start.push_back(pos);
        lm.files.back().out_start.push_back(out.size());
        return out;
    }
}



#define T(tk, name) case (yytokentype::tk) : return name;

std::string token2text(const int token)
{
    if (0 < token && token < 256) {
        char t = token;
        return std::string(&t, 1);
    }
    switch (token) {
        T(END_OF_FILE, "end of file")
        T(TK_NEWLINE, "newline")
        T(TK_NAME, "identifier")
        T(TK_DEF_OP, "defined operator")
        T(TK_INTEGER, "integer")
        T(TK_REAL, "real")
        T(TK_BOZ_CONSTANT, "BOZ constant")

        T(TK_PLUS, "+")
        T(TK_MINUS, "-")
        T(TK_STAR, "*")
        T(TK_SLASH, "/")
        T(TK_COLON, ":")
        T(TK_SEMICOLON, ";")
        T(TK_COMMA, ",")
        T(TK_EQUAL, "=")
        T(TK_LPAREN, "(")
        T(TK_RPAREN, ")")
        T(TK_LBRACKET, "[")
        T(TK_RBRACKET, "]")
        T(TK_LBRACE, "{")
        T(TK_RBRACE, "}")
        T(TK_RBRACKET_OLD, "/)")
        T(TK_PERCENT, "%")
        T(TK_VBAR, "|")

        T(TK_STRING, "string")
        T(TK_COMMENT, "comment")
        T(TK_EOLCOMMENT, "end of line comment")
        T(TK_LABEL, "label")
        T(TK_PRAGMA_DECL, "pragma declare")
        T(TK_OMP, "pragma")
        T(TK_OMP_END, "pragma end")

        T(TK_DBL_DOT, "..")
        T(TK_DBL_COLON, "::")
        T(TK_POW, "**")
        T(TK_CONCAT, "//")
        T(TK_ARROW, "=>")

        T(TK_EQ, "==")
        T(TK_NE, "!=")
        T(TK_LT, "<")
        T(TK_LE, "<=")
        T(TK_GT, ">")
        T(TK_GE, ">=")

        T(TK_NOT, ".not.")
        T(TK_AND, ".and.")
        T(TK_OR, ".or.")
        T(TK_EQV, ".eqv.")
        T(TK_NEQV, ".neqv.")

        T(TK_TRUE, ".true.")
        T(TK_FALSE, ".false.")

        T(TK_FORMAT, "format")

        T(KW_ABSTRACT, "abstract")
        T(KW_ALL, "all")
        T(KW_ALLOCATABLE, "allocatable")
        T(KW_ALLOCATE, "allocate")
        T(KW_ASSIGN, "assign")
        T(KW_ASSIGNMENT, "assignment")
        T(KW_ASSOCIATE, "associate")
        T(KW_ASYNCHRONOUS, "asynchronous")
        T(KW_BACKSPACE, "backspace")
        T(KW_BIND, "bind")
        T(KW_BLOCK, "block")
        T(KW_CALL, "call")
        T(KW_CASE, "case")
        T(KW_CHANGE, "change")
        T(KW_CHANGE_TEAM, "changeteam")
        T(KW_CHARACTER, "character")
        T(KW_CLASS, "class")
        T(KW_CLOSE, "close")
        T(KW_CODIMENSION, "codimension")
        T(KW_COMMON, "common")
        T(KW_COMPLEX, "complex")
        T(KW_CONCURRENT, "concurrent")
        T(KW_CONTAINS, "contains")
        T(KW_CONTIGUOUS, "contiguous")
        T(KW_CONTINUE, "continue")
        T(KW_CRITICAL, "critical")
        T(KW_CYCLE, "cycle")
        T(KW_DATA, "data")
        T(KW_DEALLOCATE, "deallocate")
        T(KW_DEFAULT, "default")
        T(KW_DEFERRED, "deferred")
        T(KW_DIMENSION, "dimension")
        T(KW_DO, "do")
        T(KW_DOWHILE, "dowhile")
        T(KW_DOUBLE, "double")
        T(KW_DOUBLE_PRECISION, "doubleprecision")
        T(KW_ELEMENTAL, "elemental")
        T(KW_ELSE, "else")
        T(KW_ELSEIF, "elseif")
        T(KW_ELSEWHERE, "elsewhere")

        T(KW_END, "end")
        T(KW_END_DO, "end do")
        T(KW_ENDDO, "enddo")
        T(KW_END_IF, "end if")
        T(KW_ENDIF, "endif")
        T(KW_END_INTERFACE, "end interface")
        T(KW_ENDINTERFACE, "endinterface")
        T(KW_END_TYPE, "end type")
        T(KW_ENDTYPE, "endtype")
        T(KW_END_PROGRAM, "end program")
        T(KW_ENDPROGRAM, "endprogram")
        T(KW_END_MODULE, "end module")
        T(KW_ENDMODULE, "endmodule")
        T(KW_END_SUBMODULE, "end submodule")
        T(KW_ENDSUBMODULE, "endsubmodule")
        T(KW_END_BLOCK, "end block")
        T(KW_ENDBLOCK, "endblock")
        T(KW_END_BLOCK_DATA, "end block data")
        T(KW_ENDBLOCKDATA, "endblockdata")
        T(KW_END_SUBROUTINE, "end subroutine")
        T(KW_ENDSUBROUTINE, "endsubroutine")
        T(KW_END_FUNCTION, "end function")
        T(KW_ENDFUNCTION, "endfunction")
        T(KW_END_PROCEDURE, "end procedure")
        T(KW_ENDPROCEDURE, "endprocedure")
        T(KW_END_ENUM, "end enum")
        T(KW_ENDENUM, "endenum")
        T(KW_END_SELECT, "end select")
        T(KW_ENDSELECT, "endselect")
        T(KW_END_ASSOCIATE, "end associate")
        T(KW_ENDASSOCIATE, "endassociate")
        T(KW_END_FORALL, "end forall")
        T(KW_ENDFORALL, "endforall")
        T(KW_END_WHERE, "end where")
        T(KW_ENDWHERE, "endwhere")
        T(KW_END_CRITICAL, "end critical")
        T(KW_ENDCRITICAL, "endcritical")
        T(KW_END_FILE, "end file")
        T(KW_ENDFILE, "endfile")
        T(KW_END_TEAM, "end team")
        T(KW_ENDTEAM, "endteam")

        T(KW_ENTRY, "entry")
        T(KW_ENUM, "enum")
        T(KW_ENUMERATOR, "enumerator")
        T(KW_EQUIVALENCE, "equivalence")
        T(KW_ERRMSG, "errmsg")
        T(KW_ERROR, "error")
        T(KW_EVENT, "event")
        T(KW_EXIT, "exit")
        T(KW_EXTENDS, "extends")
        T(KW_EXTERNAL, "external")
        T(KW_FILE, "file")
        T(KW_FINAL, "final")
        T(KW_FLUSH, "flush")
        T(KW_FORALL, "forall")
        T(KW_FORMATTED, "formatted")
        T(KW_FORM, "form")
        T(KW_FORM_TEAM, "formteam")
        T(KW_FUNCTION, "function")
        T(KW_GENERIC, "generic")
        T(KW_GO, "go")
        T(KW_GOTO, "goto")
        T(KW_IF, "if")
        T(KW_IMAGES, "images")
        T(KW_IMPLICIT, "implicit")
        T(KW_IMPORT, "import")
        T(KW_IMPURE, "impure")
        T(KW_IN, "in")
        T(KW_INCLUDE, "include")
        T(KW_INOUT, "inout")
        T(KW_INQUIRE, "inquire")
        T(KW_INSTANTIATE, "instantiate")
        T(KW_INTEGER, "integer")
        T(KW_INTENT, "intent")
        T(KW_INTERFACE, "interface")
        T(KW_INTRINSIC, "intrinsic")
        T(KW_IS, "is")
        T(KW_KIND, "kind")
        T(KW_LEN, "len")
        T(KW_LOCAL, "local")
        T(KW_LOCAL_INIT, "local_init")
        T(KW_LOGICAL, "logical")
        T(KW_MEMORY, "memory")
        T(KW_MODULE, "module")
        T(KW_MOLD, "mold")
        T(KW_NAME, "name")
        T(KW_NAMELIST, "namelist")
        T(KW_NEW_INDEX, "new_index")
        T(KW_NOPASS, "nopass")
        T(KW_NON_INTRINSIC, "non_intrinsic")
        T(KW_NON_OVERRIDABLE, "non_overridable")
        T(KW_NON_RECURSIVE, "non_recursive")
        T(KW_NONE, "none")
        T(KW_NULLIFY, "nullify")
        T(KW_ONLY, "only")
        T(KW_OPEN, "open")
        T(KW_OPERATOR, "operator")
        T(KW_OPTIONAL, "optional")
        T(KW_OUT, "out")
        T(KW_PARAMETER, "parameter")
        T(KW_PASS, "pass")
        T(KW_POINTER, "pointer")
        T(KW_POST, "post")
        T(KW_PRECISION, "precision")
        T(KW_PRINT, "print")
        T(KW_PRIVATE, "private")
        T(KW_PROCEDURE, "procedure")
        T(KW_PROGRAM, "program")
        T(KW_PROTECTED, "protected")
        T(KW_PUBLIC, "public")
        T(KW_PURE, "pure")
        T(KW_QUIET, "quiet")
        T(KW_RANK, "rank")
        T(KW_READ, "read")
        T(KW_REAL, "real")
        T(KW_RECURSIVE, "recursive")
        T(KW_REDUCE, "reduce")
        T(KW_REQUIREMENT, "requirement")
        T(KW_REQUIRE, "require")
        T(KW_RESULT, "result")
        T(KW_RETURN, "return")
        T(KW_REWIND, "rewind")
        T(KW_SAVE, "save")
        T(KW_SELECT, "select")
        T(KW_SELECT_CASE, "selectcase")
        T(KW_SELECT_RANK, "selectrank")
        T(KW_SELECT_TYPE, "selecttype")
        T(KW_SEQUENCE, "sequence")
        T(KW_SHARED, "shared")
        T(KW_SOURCE, "source")
        T(KW_STAT, "stat")
        T(KW_STOP, "stop")
        T(KW_SUBMODULE, "submodule")
        T(KW_SUBROUTINE, "subroutine")
        T(KW_SYNC, "sync")
        T(KW_SYNC_ALL, "syncall")
        T(KW_SYNC_IMAGES, "synimages")
        T(KW_SYNC_MEMORY, "syncmemory")
        T(KW_SYNC_TEAM, "syncteam")
        T(KW_TARGET, "target")
        T(KW_TEAM, "team")
        T(KW_TEAM_NUMBER, "team_number")
        T(KW_TEMPLATE, "template")
        T(KW_THEN, "then")
        T(KW_TO, "to")
        T(KW_TYPE, "type")
        T(KW_UNFORMATTED, "unformatted")
        T(KW_USE, "use")
        T(KW_VALUE, "value")
        T(KW_VOLATILE, "volatile")
        T(KW_WAIT, "wait")
        T(KW_WHERE, "where")
        T(KW_WHILE, "while")
        T(KW_WRITE, "write")
        default : {
            std::cout << "TOKEN: " << token << std::endl;
            throw LCompilersException("Token conversion not implemented yet.");
        }
    }
}

void Parser::handle_yyerror(const Location &loc, const std::string &msg)
{
    std::string message;
    if (msg == "syntax is ambiguous") {
        message = "Internal Compiler Error: syntax is ambiguous in the parser";
    } else if (msg == "syntax error") {
        int token;
        std::string token_str;
        // Determine the unexpected token's type:
        if (this->fixed_form) {
            unsigned int invalid_token = this->f_tokenizer.token_pos;
            if (invalid_token == 0 || invalid_token > f_tokenizer.tokens.size()) {
                message = "unknown error";
                if (this->continue_compilation) {
                    diag.add(parser_local::ParserError(message, loc).d);
                } else {
                    throw parser_local::ParserError(message, loc);
                }
            }
            invalid_token--;
            LCOMPILERS_ASSERT(invalid_token < f_tokenizer.tokens.size())
            LCOMPILERS_ASSERT(invalid_token < f_tokenizer.locations.size())
            token = f_tokenizer.tokens[invalid_token];
            Location loc = f_tokenizer.locations[invalid_token];
            token_str = f_tokenizer.token_at_loc(loc);
        } else {
            LFortran::YYSTYPE yylval_;
            YYLTYPE yyloc_;
            this->m_tokenizer.cur = this->m_tokenizer.tok;
            token = this->m_tokenizer.lex(this->m_a, yylval_, yyloc_, diag, this->continue_compilation);
            token_str = this->m_tokenizer.token();
        }
        // Create a nice error message
        if (token == yytokentype::END_OF_FILE) {
            message =  "End of file is unexpected here";
        } else if (token == yytokentype::TK_NEWLINE) {
            message =  "Newline is unexpected here";
        } else {
            std::string token_type = token2text(token);
            if (token_str == token_type || token_str.size() == 0) {
                message =  "Token '" + token_type + "' is unexpected here";
            } else {
                message =  "Token '" + token_str + "' (of type '" + token2text(token) + "') is unexpected here";
            }
        }
    } else {
        message = "Internal Compiler Error: parser returned unknown error";
    }
    if (this->continue_compilation) {
        diag.add(parser_local::ParserError(message, loc).d);
    } else {
        throw parser_local::ParserError(message, loc);
    }
}

} // namespace LCompilers::LFortran
