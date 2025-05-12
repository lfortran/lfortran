#include <iostream>
#include <map>

#include <lfortran/parser/preprocessor.h>
#include <libasr/assert.h>
#include <lfortran/utils.h>
#include <libasr/string_utils.h>

namespace LCompilers::LFortran {

// This exception is only used internally for the preprocessor, nowhere else.

class PreprocessorError
{
public:
    diag::Diagnostic d;
public:
    PreprocessorError(const std::string &msg, const Location &loc)
        : d{diag::Diagnostic(msg, diag::Level::Error, diag::Stage::CPreprocessor, {
            diag::Label("", {loc})
        })}
    { }

    PreprocessorError(const diag::Diagnostic &d) : d{d} { }
};


CPreprocessor::CPreprocessor(CompilerOptions &compiler_options)
    : compiler_options{compiler_options} {
    CPPMacro md;
    md.expansion = "1";
    macro_definitions["__LFORTRAN__"] = md;
    md.expansion = "\"" + std::string(LFORTRAN_VERSION) + "\"";
    macro_definitions["__VERSION__"] = md;
    md.expansion = std::to_string(LFORTRAN_MAJOR);
    macro_definitions["__LFORTRAN_MAJOR__"] = md;
    md.expansion = std::to_string(LFORTRAN_MINOR);
    macro_definitions["__LFORTRAN_MINOR__"] = md;
    md.expansion = std::to_string(LFORTRAN_PATCHLEVEL);
    macro_definitions["__LFORTRAN_PATCHLEVEL__"] = md;
    if (compiler_options.platform == Platform::Windows) {
        md.expansion = "1";
        macro_definitions["_WIN32"] = md;
    } else if (compiler_options.platform == Platform::macOS_ARM
        || compiler_options.platform == Platform::macOS_Intel) {
        md.expansion = "1";
        macro_definitions["__APPLE__"] = md;
        if (compiler_options.platform == Platform::macOS_ARM) {
            md.expansion = "1";
            macro_definitions["__aarch64__"] = md;
        } else {
            md.expansion = "1";
            macro_definitions["__x86_64__"] = md;
        }
    } else if (compiler_options.platform == Platform::FreeBSD) {
        md.expansion = "1";
        macro_definitions["__FreeBSD__"] = md;
    } else if (compiler_options.platform == Platform::OpenBSD) {
        md.expansion = "1";
        macro_definitions["__OpenBSD__"] = md;
    } else {
        md.expansion = "1";
        macro_definitions["__linux__"] = md;
    }
#ifdef __ELF__
    md.expansion = std::to_string(__ELF__);
    macro_definitions["__ELF__"] = md;
#endif
#ifdef __SIZEOF_POINTER__
    md.expansion = std::to_string(__SIZEOF_POINTER__);
    macro_definitions["__SIZEOF_POINTER__"] = md;
#endif
#ifdef __SIZEOF_SIZE_T__
    md.expansion = std::to_string(__SIZEOF_SIZE_T__);
    macro_definitions["__SIZEOF_SIZE_T__"] = md;
#endif
#ifdef __linux__
#ifdef __x86_64__
    md.expansion = std::to_string(__x86_64__);
    macro_definitions["__x86_64__"] = md;
#endif
#ifdef __i386__
    md.expansion = std::to_string(__i386__);
    macro_definitions["__i386__"] = md;
#endif
#endif
    for (auto &d : compiler_options.c_preprocessor_defines) {
        std::size_t idx = d.find("=");
        if (idx != std::string::npos) {
            md.expansion = d.substr(idx+1);
            d = d.substr(0, idx);
        } else {
            md.expansion = "1";
        }
        macro_definitions[d] = md;
    }

    md.expansion = "\"\"";
    macro_definitions["__FILE__"] = md;
    md.expansion = "0";
    macro_definitions["__LINE__"] = md;
}
std::string CPreprocessor::token(unsigned char *tok, unsigned char* cur) const
{
    return std::string((char *)tok, cur - tok);
}

void handle_continuation_lines(std::string &s, unsigned char *&cur);

std::string parse_continuation_lines(unsigned char *&cur) {
    std::string output;
    while (*cur != '\n') {
        output += *cur;
        cur++;
    }
    cur++;
    handle_continuation_lines(output, cur);
    return output;
}

void handle_continuation_lines(std::string &s, unsigned char *&cur) {
    if (s.size() > 0 && s[s.size()-1] == '\\') {
        s = s.substr(0, s.size()-1);
        s += parse_continuation_lines(cur);
    }
}

// Parse a macro declaration argument, e.g. in:
// f(a,b, c,  d  )
std::string parse_argument(unsigned char *string_start, unsigned char *old_cur, unsigned char *&cur) {
    std::string arg;
    while (*cur == ' ' && *cur != '\0') cur++;
    while (*cur != ')' && *cur != ',' && *cur != ' ') {
        if (*cur == '\0') {
            Location loc;
            loc.first = old_cur - string_start;
            loc.last = loc.first;
            throw PreprocessorError("Argument list is not closed with ')'", loc);
        }
        arg += *cur;
        cur++;
    }
    while (*cur == ' ' && *cur != '\0') cur++;
    if (*cur == '\0') {
        Location loc;
        loc.first = old_cur - string_start;
        loc.last = loc.first;
        throw PreprocessorError("Argument list is not closed with ')'", loc);
    }
    return arg;
}

std::string match_parentheses(unsigned char *string_start, unsigned char *&cur) {
    LCOMPILERS_ASSERT(*cur == '(')
    unsigned char *old_cur = cur;
    std::string arg;
    arg += *cur;
    cur++;
    while (*cur != ')') {
        if (*cur == '\0') {
            Location loc;
            loc.first = old_cur - string_start;
            loc.last = loc.first;
            throw PreprocessorError("unmatched parentheses", loc);
        }
        if (*cur == '(') {
            arg += match_parentheses(string_start, cur);
            LCOMPILERS_ASSERT(*cur == ')')
        } else {
            arg += *cur;
        }
        cur++;
    }
    arg += *cur;
    return arg;
}

// Parse a macro call argument, e.g. in:
// ASSERT(fn(3, 5))
std::string parse_argument2(unsigned char *string_start, unsigned char *old_cur, unsigned char *&cur) {
    std::string arg;
    while (*cur != ')' && *cur != ',') {
        if (*cur == '\0') {
            Location loc;
            loc.first = old_cur - string_start;
            loc.last = loc.first;
            throw PreprocessorError("Argument list is not closed with ')'", loc);
        }
        if (*cur == '(') {
            arg += match_parentheses(string_start, cur);
            LCOMPILERS_ASSERT(*cur == ')')
        } else {
            arg += *cur;
        }
        cur++;
    }
    return arg;
}

std::vector<std::string> parse_arguments(unsigned char *string_start, unsigned char *&cur, bool skip_spaces) {
    std::vector<std::string> args;
    LCOMPILERS_ASSERT(*cur == '(');
    unsigned char *old_cur = cur;
    cur++;
    while (*cur != ')') {
        if (skip_spaces) {
            args.push_back(parse_argument(string_start, old_cur, cur));
        } else {
            args.push_back(parse_argument2(string_start, old_cur, cur));
        }
        if (*cur == ',') cur++;
    }
    return args;
}

void interval_end(LocationManager &lm, size_t output_len,
                size_t input_len, size_t input_interval_len,
                uint32_t interval_type) {
    lm.files.back().out_start0.push_back(output_len);
    lm.files.back().in_start0.push_back(input_len);
    lm.files.back().in_size0.push_back(input_interval_len);
    lm.files.back().interval_type0.push_back(interval_type);
}

void interval_end_type_0(LocationManager &lm, size_t output_len,
                size_t input_len) {
    size_t input_interval_len = output_len - lm.files.back().out_start0.back();
    interval_end(lm, output_len, input_len, input_interval_len, 0);
}

enum class DirectiveType {
    If,
    Ifdef,
    Ifndef
};

inline std::string to_string(DirectiveType type) {
    switch (type) {
        case DirectiveType::If:
            return "if";
        case DirectiveType::Ifdef:
            return "ifdef";
        case DirectiveType::Ifndef:
            return "ifndef";
        default:
            return "unknown";
    }
}

struct ConditionalDirective {
    // The conditional directive is active, meaning one of its branches might
    // get executed
    // Inactive conditional directive is in a dead branch of another conditional directive
    bool active=true;
    // The current branch of conditional directive is active
    bool branch_enabled=true;
    // ConditionalDirective's enabled branch has been executed, now we just need to process
    // and skip all `elif` and `else`.
    bool enabled_branch_executed=false;
    DirectiveType type;
    Location loc;
};

namespace {

int parse_bexpr(unsigned char *string_start, unsigned char *&cur, const cpp_symtab &macro_definitions);

}

Result<std::string> CPreprocessor::run(const std::string &input, LocationManager &lm,
        cpp_symtab &macro_definitions, diag::Diagnostics &diagnostics) const {
    LCOMPILERS_ASSERT(input[input.size()] == '\0');
    unsigned char *string_start=(unsigned char*)(&input[0]);
    unsigned char *cur = string_start;
    std::string output;
    lm.files.back().preprocessor = true;
    lm.get_newlines(input, lm.files.back().in_newlines0);
    lm.files.back().out_start0.push_back(0);
    lm.files.back().in_start0.push_back(0);
    std::vector<ConditionalDirective> ConditionalDirective_stack;
    bool branch_enabled = true;
    macro_definitions["__FILE__"].expansion = "\"" + lm.files.back().in_filename + "\"";
    try {
    for (;;) {
        unsigned char *tok = cur;
        unsigned char *mar;
        unsigned char *t1, *t2, *t3, *t4;
        /*!stags:re2c format = 'unsigned char *@@;\n'; */
        /*!re2c
            re2c:define:YYCURSOR = cur;
            re2c:define:YYMARKER = mar;
            re2c:yyfill:enable = 0;
            re2c:flags:tags = 1;
            re2c:define:YYCTYPE = "unsigned char";

            end = "\x00";
            newline = "\n";
            single_line_comment = "//" [^\n\x00]*;
            multi_line_comment = "/*" ([^*\x00] | "*"[^/\x00])* "*/";
            unterminated_multi_line_comment = "/*" ([^*\x00] | "*"[^/\x00])* "\x00";
            comment = (single_line_comment | multi_line_comment);
            whitespace = [ \t\v\r]+;
            digit = [0-9];
            digits = digit+;
            int_oct = "0"[oO]([0-7] | "_" [0-7])+;
            int_bin = "0"[bB]([01] | "_" [01])+;
            int_hex = "0"[xX]([0-9a-fA-F] | "_" [0-9a-fA-F])+;
            int = digits | int_oct | int_bin | int_hex;
            char =  [a-zA-Z_];
            name = char (char | digit)*;

            * {
                if (!branch_enabled) continue;
                output.append(token(tok, cur));
                continue;
            }
            end {
                if (ConditionalDirective_stack.size() > 0) {
                    ConditionalDirective directive = ConditionalDirective_stack[ConditionalDirective_stack.size() - 1];
                    std::string unterminated_directive = "#" + to_string(directive.type);
                    throw PreprocessorError("Unterminated " + unterminated_directive, directive.loc);
                }
                break;
            }

            unterminated_multi_line_comment {
                if (!branch_enabled) continue;
                Location loc;
                loc.first = tok - string_start;
                loc.last = loc.first;
                throw PreprocessorError("Unterminated comment", loc);
            }
            "!" [^\n\x00]* newline {
                if (!branch_enabled) continue;
                output.append(token(tok, cur));
                continue;
            }
            "#" whitespace? "define" whitespace @t1 name @t2 (whitespace? | whitespace @t3 [^\n\x00]* @t4 ) newline  {
                if (!branch_enabled) continue;
                std::string macro_name = token(t1, t2), macro_subs;
                if (t3 != nullptr) {
                    LCOMPILERS_ASSERT(t4 != nullptr);
                    macro_subs = token(t3, t4);
                    handle_continuation_lines(macro_subs, cur);
                }
                CPPMacro fn;
                fn.expansion = macro_subs;
                macro_definitions[macro_name] = fn;

                interval_end_type_0(lm, output.size(), cur-string_start);
                continue;
            }
            "#" whitespace? "define" whitespace @t1 name @t2 '(' whitespace? name whitespace? (',' whitespace? name whitespace?)* ')' (whitespace @t3 [^\n\x00]* @t4)? newline  {
                if (!branch_enabled) continue;
                std::string macro_name = token(t1, t2),
                        macro_subs = token(t3, t4);
                handle_continuation_lines(macro_subs, cur);
                std::vector<std::string> args = parse_arguments(string_start, t2, true);
                CPPMacro fn;
                fn.function_like = true;
                fn.args = args;
                fn.expansion = macro_subs;
                macro_definitions[macro_name] = fn;

                interval_end_type_0(lm, output.size(), cur-string_start);
                continue;
            }
            "#" whitespace? "undef" whitespace @t1 name @t2 whitespace? newline  {
                if (!branch_enabled) continue;
                std::string macro_name = token(t1, t2);
                auto search = macro_definitions.find(macro_name);
                if (search != macro_definitions.end()) {
                    macro_definitions.erase(search);
                }

                interval_end_type_0(lm, output.size(), cur-string_start);
                continue;
            }
            "#" whitespace? "ifdef" whitespace @t1 name @t2 whitespace? newline {
                ConditionalDirective ifdef;
                ifdef.active = branch_enabled;
                ifdef.type = DirectiveType::Ifdef;
                Location loc;
                loc.first = tok - string_start;
                loc.last = loc.first;
                ifdef.loc = loc;
                if (ifdef.active) {
                    std::string macro_name = token(t1, t2);
                    if (macro_definitions.find(macro_name) != macro_definitions.end()) {
                        ifdef.branch_enabled = true;
                        ifdef.enabled_branch_executed = true;
                    } else {
                        ifdef.branch_enabled = false;
                    }
                    branch_enabled = ifdef.branch_enabled;
                } else {
                    ifdef.branch_enabled = false;
                }
                ConditionalDirective_stack.push_back(ifdef);
                if (!ifdef.active) continue;

                interval_end_type_0(lm, output.size(), cur-string_start);
                continue;
            }
            "#" whitespace? "ifndef" whitespace @t1 name @t2 whitespace? newline {
                ConditionalDirective ifndef;
                ifndef.active = branch_enabled;
                ifndef.type = DirectiveType::Ifndef;
                Location loc;
                loc.first = tok - string_start;
                loc.last = loc.first;
                ifndef.loc = loc;
                if (ifndef.active) {
                    std::string macro_name = token(t1, t2);
                    if (macro_definitions.find(macro_name) != macro_definitions.end()) {
                        ifndef.branch_enabled = false;
                    } else {
                        ifndef.branch_enabled = true;
                        ifndef.enabled_branch_executed = true;
                    }
                    branch_enabled = ifndef.branch_enabled;
                } else {
                    ifndef.branch_enabled = false;
                }
                ConditionalDirective_stack.push_back(ifndef);
                if (!ifndef.active) continue;

                interval_end_type_0(lm, output.size(), cur-string_start);
                continue;
            }
            "#" whitespace? "if" whitespace @t1 [^\n\x00]* @t2 newline {
                ConditionalDirective if_directive;
                if_directive.active = branch_enabled;
                if_directive.type = DirectiveType::If;
                Location loc;
                loc.first = tok - string_start;
                loc.last = loc.first;
                if_directive.loc = loc;
                if (if_directive.active) {
                    bool test_true = parse_bexpr(string_start, t1, macro_definitions) > 0;
                    cur = t1;
                    if (test_true) {
                        if_directive.branch_enabled = true;
                        if_directive.enabled_branch_executed = true;
                    } else {
                        if_directive.branch_enabled = false;
                    }
                    branch_enabled = if_directive.branch_enabled;
                } else {
                    if_directive.branch_enabled = false;
                }
                ConditionalDirective_stack.push_back(if_directive);
                if (!if_directive.active) continue;

                interval_end_type_0(lm, output.size(), cur-string_start);
                continue;
            }
            "#" whitespace? "else" whitespace? comment? newline  {
                if (ConditionalDirective_stack.size() == 0) {
                    Location loc;
                    loc.first = cur - string_start;
                    loc.last = loc.first;
                    throw PreprocessorError("#else encountered outside of #ifdef or #ifndef", loc);
                }
                ConditionalDirective ifdef = ConditionalDirective_stack[ConditionalDirective_stack.size()-1];
                if (ifdef.active) {
                    if (!ifdef.branch_enabled && !ifdef.enabled_branch_executed) {
                        ifdef.branch_enabled = true;
                        ifdef.enabled_branch_executed = true;
                    } else {
                        ifdef.branch_enabled = false;
                    }
                    ConditionalDirective_stack[ConditionalDirective_stack.size()-1] = ifdef;
                    branch_enabled = ifdef.branch_enabled;
                } else {
                    continue;
                }

                interval_end_type_0(lm, output.size(), cur-string_start);
                continue;
            }
            "#" whitespace? "elif" whitespace @t1 [^\n\x00]* @t2 newline  {
                if (ConditionalDirective_stack.size() == 0) {
                    Location loc;
                    loc.first = cur - string_start;
                    loc.last = loc.first;
                    throw PreprocessorError("#elif encountered outside of #ifdef or #ifndef", loc);
                }
                ConditionalDirective ifdef = ConditionalDirective_stack[ConditionalDirective_stack.size()-1];
                if (ifdef.active) {
                    if (!ifdef.branch_enabled && !ifdef.enabled_branch_executed) {
                        bool test_true = parse_bexpr(string_start, t1, macro_definitions) > 0;
                        cur = t1;
                        if (test_true) {
                            ifdef.branch_enabled = true;
                            ifdef.enabled_branch_executed = true;
                        } else {
                            ifdef.branch_enabled = false;
                        }
                    } else {
                        ifdef.branch_enabled = false;
                    }
                    branch_enabled = ifdef.branch_enabled;
                    ConditionalDirective_stack[ConditionalDirective_stack.size()-1] = ifdef;
                } else {
                    continue;
                }

                interval_end_type_0(lm, output.size(), cur-string_start);
                continue;
            }
            "#" whitespace? "endif" whitespace? comment? newline  {
                if (ConditionalDirective_stack.size() == 0) {
                    Location loc;
                    loc.first = cur - string_start;
                    loc.last = loc.first;
                    throw PreprocessorError("#endif encountered outside of #ifdef or #ifndef", loc);
                }
                ConditionalDirective ifdef = ConditionalDirective_stack[ConditionalDirective_stack.size()-1];
                ConditionalDirective_stack.pop_back();
                if (ifdef.active) {
                    branch_enabled = true;
                } else {
                    continue;
                }

                interval_end_type_0(lm, output.size(), cur-string_start);
                continue;
            }
            "#" whitespace? "include" whitespace ["<] @t1 [^">\x00]* @t2 [">] [^\n\x00]* newline {
                if (!branch_enabled) continue;
                std::string filename = token(t1, t2);
                std::vector<std::filesystem::path> include_dirs;
                include_dirs.push_back(parent_path(lm.files.back().in_filename));
                include_dirs.insert(include_dirs.end(),
                                    compiler_options.po.include_dirs.begin(),
                                    compiler_options.po.include_dirs.end());
                bool file_found = false;
                std::string include = "";
                if (is_relative_path(filename)) {
                    for (auto &path:include_dirs) {
                        std::string filepath = join_paths({path.generic_string(), filename});
                        file_found = read_file(filepath, include);
                        if (file_found) {
                            filename = filepath;
                            break;
                        }
                    }
                } else {
                    file_found = read_file(filename, include);
                }

                if (!file_found) {
                    Location loc;
                    loc.first = t1 - string_start;
                    loc.last = t2-1 - string_start;
                    throw PreprocessorError("Include file '" + filename + "' not found. If an include path is available, please use the `-I` option to specify it.", loc);
                }

                LocationManager lm_tmp = lm; // Make a copy
                if (include.size() == 0 || include[include.size()-1] != '\n') {
                    include.append("\n");
                }
                Result<std::string> res = run(include, lm_tmp, macro_definitions, diagnostics);
                if (res.ok) {
                    include = res.result;
                } else {
                    return res.error;
                }

                // Prepare the start of the interval
                interval_end_type_0(lm, output.size(), tok-string_start);

                // Include
                output.append(include);

                // Prepare the end of the interval
                interval_end(lm, output.size(), cur-string_start,
                    token(tok, cur).size()-1, 1);
                continue;
            }
            name {
                if (!branch_enabled) continue;
                std::string t = token(tok, cur);
                if (macro_definitions.find(t) != macro_definitions.end()) {
                    // Prepare the start of the interval
                    interval_end_type_0(lm, output.size(), tok-string_start);

                    // Expand the macro once
                    std::string expansion;
                    if (macro_definitions[t].function_like) {
                        if (*cur != '(') {
                            Location loc;
                            loc.first = cur - string_start;
                            loc.last = loc.first;
                            throw PreprocessorError("function-like macro invocation must have argument list", loc);
                        }
                        std::vector<std::string> args;
                        args = parse_arguments(string_start, cur, false);
                        if (*cur != ')') {
                            Location loc;
                            loc.first = cur - string_start;
                            loc.last = loc.first;
                            throw PreprocessorError("expected ')'", loc);
                        }
                        cur++;
                        expansion = function_like_macro_expansion(
                            macro_definitions[t].args,
                            macro_definitions[t].expansion,
                            args);
                    } else {
                        if (t == "__LINE__") {
                            uint32_t line;
                            if (lm.files.back().current_line == 0) {
                                uint32_t pos = cur-string_start;
                                uint32_t col;
                                std::string filename;
                                lm.pos_to_linecol(pos, line, col, filename);
                            } else {
                                line = lm.files.back().current_line;
                            }
                            expansion = std::to_string(line);
                        } else {
                            expansion = macro_definitions[t].expansion;
                        }
                    }

                    // Recursively expand the expansion
                    std::string expansion2;
                    int i = 0;
                    while (expansion2 != expansion) {
                        expansion2 = expansion;
                        LocationManager lm_tmp = lm; // Make a copy

                        uint32_t pos = cur-string_start;
                        uint32_t line, col;
                        std::string filename;
                        lm.pos_to_linecol(pos, line, col, filename);
                        lm_tmp.files.back().current_line = line;

                        Result<std::string> res = run(expansion2, lm_tmp, macro_definitions, diagnostics);
                        if (res.ok) {
                            expansion = res.result;
                        } else {
                            return res.error;
                        }
                        i++;
                        if (i == 40) {
                            Location loc;
                            loc.first = cur - string_start;
                            loc.last = loc.first;
                            throw PreprocessorError("maximum recursion limit reached", loc);
                        }
                    }

                    // Output the final recursively expanded macro
                    output.append(expansion);

                    // Prepare the end of the interval
                    interval_end(lm, output.size(), cur-string_start,
                        t.size(), 1);
                } else {
                    output.append(t);
                }
                continue;
            }
            '"' ('""'|[^"\x00])* '"' {
                if (!branch_enabled) continue;
                output.append(token(tok, cur));
                continue;
            }
            "'" ("''"|[^'\x00])* "'" {
                if (!branch_enabled) continue;
                output.append(token(tok, cur));
                continue;
            }
            "/*" {
                if (!branch_enabled) continue;
                cur++;
                while (!(*cur == '/' && *(cur - 1) == '*')) {
                    cur++;
                }
                cur++;
                interval_end_type_0(lm, output.size(), cur-string_start);
                continue;
            }
        */
    }
    } catch (const LFortran::PreprocessorError &e) {
        diagnostics.add(e.d);
        lm.init_simple(input);
        lm.files.back().preprocessor = false;
        return Error();
    }
    lm.files.back().out_start0.push_back(output.size());
    lm.files.back().in_start0.push_back(input.size());
    // The just created interval ID:
    size_t N = lm.files.back().out_start0.size()-2;
    lm.files.back().in_size0.push_back(
        lm.files.back().out_start0[N+1] - lm.files.back().out_start0[N]);
    lm.files.back().interval_type0.push_back(0);

    // Uncomment for debugging
    /*
    std::cout << "in_start0: ";
    for (auto A : lm.files.back().in_start0) { std::cout << A << " "; }
    std::cout << std::endl;
    std::cout << "in_size0: ";
    for (auto A : lm.files.back().in_size0) { std::cout << A << " "; }
    std::cout << std::endl;
    std::cout << "interval_type0: ";
    for (auto A : lm.files.back().interval_type0) { std::cout << A << " "; }
    std::cout << std::endl;
    std::cout << "out_start0: ";
    for (auto A : lm.files.back().out_start0) { std::cout << A << " "; }
    std::cout << std::endl;
    */

    return output;
}

namespace {

std::string token(unsigned char *tok, unsigned char* cur)
{
    return std::string((char *)tok, cur - tok);
}

}

std::string function_like_macro_expansion(
            std::vector<std::string> &def_args,
            std::string &expansion,
            std::vector<std::string> &call_args) {
    LCOMPILERS_ASSERT(expansion[expansion.size()] == '\0');
    unsigned char *string_start=(unsigned char*)(&expansion[0]);
    unsigned char *cur = string_start;
    std::string output;
    for (;;) {
        unsigned char *tok = cur;
        unsigned char *mar;
        /*!re2c
            re2c:define:YYCURSOR = cur;
            re2c:define:YYMARKER = mar;
            re2c:yyfill:enable = 0;
            re2c:define:YYCTYPE = "unsigned char";

            * {
                output.append(token(tok, cur));
                continue;
            }
            end {
                break;
            }
            name {
                std::string t = token(tok, cur);
                auto search = std::find(def_args.begin(), def_args.end(), t);
                if (search != def_args.end()) {
                    size_t i = std::distance(def_args.begin(), search);
                    output.append(call_args[i]);
                } else {
                    output.append(t);
                }
                continue;
            }
            '"' ('""'|[^"\x00])* '"' {
                output.append(token(tok, cur));
                continue;
            }
            "'" ("''"|[^'\x00])* "'" {
                output.append(token(tok, cur));
                continue;
            }
        */
    }
    return output;
}

enum CPPTokenType {
    TK_EOF, TK_NAME, TK_INTEGER, TK_STRING, TK_AND, TK_OR, TK_NEG,
    TK_LPAREN, TK_RPAREN, TK_LT, TK_GT, TK_LTE, TK_GTE, TK_NE, TK_EQ,
    TK_PLUS, TK_MINUS, TK_MUL, TK_DIV, TK_PERCENT, TK_LSHIFT, TK_RSHIFT,
    TK_BITAND, TK_BITOR
};

std::string token_type_to_string(CPPTokenType type) {
    switch (type) {
        case (TK_EOF)     : return "end of line";
        case (TK_NAME)    : return "name";
        case (TK_INTEGER) : return "integer";
        case (TK_STRING)  : return "string";
        case (TK_AND)     : return "&&";
        case (TK_OR)      : return "||";
        case (TK_NEG)     : return "!";
        case (TK_LPAREN)  : return "(";
        case (TK_RPAREN)  : return ")";
        case (TK_LT)      : return "<";
        case (TK_GT)      : return ">";
        case (TK_LTE)     : return "<=";
        case (TK_GTE)     : return ">=";
        case (TK_NE)      : return "/=";
        case (TK_EQ)      : return "==";
        case (TK_PLUS)    : return "+";
        case (TK_MINUS)   : return "-";
        case (TK_MUL)     : return "*";
        case (TK_DIV)     : return "/";
        case (TK_PERCENT) : return "%";
        case (TK_LSHIFT)  : return "<<";
        case (TK_RSHIFT)  : return ">>";
        case (TK_BITAND)  : return "&";
        case (TK_BITOR)   : return "|";
    }
    return "";
}

void get_next_token(unsigned char *string_start, unsigned char *&cur, CPPTokenType &type, std::string &str) {
    std::string output;
    for (;;) {
        unsigned char *tok = cur;
        unsigned char *mar;
        /*!re2c
            re2c:define:YYCURSOR = cur;
            re2c:define:YYMARKER = mar;
            re2c:yyfill:enable = 0;
            re2c:define:YYCTYPE = "unsigned char";

            * {
                std::string t = token(tok, cur);
                Location loc;
                loc.first = tok - string_start;
                loc.last = cur-1 - string_start;
                throw PreprocessorError("Unknown token '" + t + "'", loc);
            }
            end { type = CPPTokenType::TK_EOF; return; }
            newline { type = CPPTokenType::TK_EOF; return; }
            whitespace { continue; }
            "\\" whitespace? newline { continue; }
            "+" { type = CPPTokenType::TK_PLUS; return; }
            "-" { type = CPPTokenType::TK_MINUS; return; }
            "*" { type = CPPTokenType::TK_MUL; return; }
            "/" { type = CPPTokenType::TK_DIV; return; }
            "%" { type = CPPTokenType::TK_PERCENT; return; }
            "<<" { type = CPPTokenType::TK_LSHIFT; return; }
            ">>" { type = CPPTokenType::TK_RSHIFT; return; }
            "&" { type = CPPTokenType::TK_BITAND; return; }
            "|" { type = CPPTokenType::TK_BITOR; return; }
            "&&" { type = CPPTokenType::TK_AND; return; }
            "||" { type = CPPTokenType::TK_OR; return; }
            "!" { type = CPPTokenType::TK_NEG; return; }
            "(" { type = CPPTokenType::TK_LPAREN; return; }
            ")" { type = CPPTokenType::TK_RPAREN; return; }
            "<" { type = CPPTokenType::TK_LT; return; }
            ">" { type = CPPTokenType::TK_GT; return; }
            "<=" { type = CPPTokenType::TK_LTE; return; }
            ">=" { type = CPPTokenType::TK_GTE; return; }
            "/=" { type = CPPTokenType::TK_NE; return; }
            "!=" { type = CPPTokenType::TK_NE; return; }
            "==" { type = CPPTokenType::TK_EQ; return; }
            int {
                str = token(tok, cur);
                type = CPPTokenType::TK_INTEGER;
                if (str.size() > 2 && isalpha(str[1])) {
                    char ch = str[1];
                    str = str.substr(2);
                    if (ch == 'x' || ch == 'X') {
                        int64_t hex_int = std::stoll(str, nullptr, 16);
                        str = std::to_string(hex_int);
                    } else if (ch == 'o' || ch == 'O') {
                        int64_t oct_int = std::stoll(str, nullptr, 8);
                        str = std::to_string(oct_int);
                    } else {
                        int64_t bin_int = std::stoll(str, nullptr, 2);
                        str = std::to_string(bin_int);
                    }
                }
                return;
            }
            name {
                str = token(tok, cur);
                type = CPPTokenType::TK_NAME;
                return;
            }
        */
    }
}

namespace {

void accept(unsigned char *string_start, unsigned char *&cur, CPPTokenType type_expected) {
    CPPTokenType type;
    std::string str;
    unsigned char *old_cur = cur;
    get_next_token(string_start, cur, type, str);
    if (type != type_expected) {
        Location loc;
        loc.first = old_cur - string_start;
        loc.last = cur - string_start;
        throw PreprocessorError("Unexpected token '" + token_type_to_string(type) + "', expected '" + token_type_to_string(type_expected) + "'", loc);
    }
}

std::string accept_name(unsigned char *string_start, unsigned char *&cur) {
    CPPTokenType type;
    std::string str;
    unsigned char *old_cur = cur;
    get_next_token(string_start, cur, type, str);
    if (type != CPPTokenType::TK_NAME) {
        Location loc;
        loc.first = old_cur - string_start;
        loc.last = cur - string_start;
        throw PreprocessorError("Unexpected token '" + token_type_to_string(type) + "', expected '" + token_type_to_string(TK_NAME) + "'", loc);
    }
    return str;
}

int parse_term(unsigned char *string_star, unsigned char *&cur, const cpp_symtab &macro_definitions);
int parse_factor(unsigned char *string_start, unsigned char *&cur, const cpp_symtab &macro_definitions);
int parse_bfactor(unsigned char *string_start, unsigned char *&cur, const cpp_symtab &macro_definitions);

/*
b-expr
    = b-factor (("&&"|"||") b-factor)*
*/
int parse_bexpr(unsigned char *string_start, unsigned char *&cur, const cpp_symtab &macro_definitions) {
    int tmp = parse_bfactor(string_start, cur, macro_definitions);

    CPPTokenType type;
    std::string str;
    unsigned char *old_cur = cur;
    get_next_token(string_start, cur, type, str);
    while (type == CPPTokenType::TK_AND || type == CPPTokenType::TK_OR || type == CPPTokenType::TK_BITOR
        || type == CPPTokenType::TK_BITAND) {
        bool factor = parse_bfactor(string_start, cur, macro_definitions) > 0;
        if (type == CPPTokenType::TK_AND) {
            tmp = (int)( (tmp > 0) && (factor > 0) );
        } else if (type == CPPTokenType::TK_BITOR) {
            tmp = (int)( (tmp > 0) | (factor > 0) );
        } else if (type == CPPTokenType::TK_BITAND) {
            tmp = (int)( (tmp > 0) & (factor > 0) );
        } else {
            tmp = (int)( (tmp > 0) || (factor > 0) );
        }
        old_cur = cur;
        get_next_token(string_start, cur, type, str);
    }
    cur = old_cur; // Revert the last token, as we will not consume it
    return tmp;
}

/*
expr
    = term ((+,-) term)*
*/
int parse_expr(unsigned char *string_start, unsigned char *&cur, const cpp_symtab &macro_definitions) {
    int tmp;
    tmp = parse_term(string_start, cur, macro_definitions);

    CPPTokenType type;
    std::string str;
    unsigned char *old_cur = cur;
    get_next_token(string_start, cur, type, str);
    while (type == CPPTokenType::TK_PLUS || type == CPPTokenType::TK_MINUS) {
        int term = parse_term(string_start, cur, macro_definitions);
        if (type == CPPTokenType::TK_PLUS) {
            tmp = tmp + term;
        } else {
            tmp = tmp - term;
        }
        old_cur = cur;
        get_next_token(string_start, cur, type, str);
    }
    cur = old_cur; // Revert the last token, as we will not consume it
    return tmp;
}

/*
term
    = factor ((*,/,%,<<,>>,&,|) factor)*
*/
int parse_term(unsigned char *string_start, unsigned char *&cur, const cpp_symtab &macro_definitions) {
    int tmp;
    tmp = parse_factor(string_start, cur, macro_definitions);

    CPPTokenType type;
    std::string str;
    unsigned char *old_cur = cur;
    get_next_token(string_start, cur, type, str);
    while (type == CPPTokenType::TK_MUL ||
           type == CPPTokenType::TK_DIV ||
           type == CPPTokenType::TK_PERCENT ||
           type == CPPTokenType::TK_LSHIFT ||
           type == CPPTokenType::TK_RSHIFT ||
           type == CPPTokenType::TK_BITAND ||
           type == CPPTokenType::TK_BITOR) {
        int term = parse_factor(string_start, cur, macro_definitions);
        if (type == CPPTokenType::TK_MUL) {
            tmp = tmp * term;
        } else if (type == CPPTokenType::TK_DIV) {
            tmp = tmp / term;
        } else if (type == CPPTokenType::TK_PERCENT) {
            tmp = tmp % term;
        } else if (type == CPPTokenType::TK_LSHIFT) {
            tmp = tmp << term;
        } else if (type == CPPTokenType::TK_RSHIFT) {
            tmp = tmp >> term;
        } else if (type == CPPTokenType::TK_BITAND) {
            tmp = tmp & term;
        } else if (type == CPPTokenType::TK_BITOR) {
            tmp = tmp | term;
        } else {
            Location loc;
            loc.first = old_cur - string_start;
            loc.last = cur - string_start;
            throw PreprocessorError("Unknown operator '" + token_type_to_string(type) + "'", loc);
        }
        old_cur = cur;
        get_next_token(string_start, cur, type, str);
    }
    cur = old_cur; // Revert the last token, as we will not consume it
    return tmp;
}

/*
factor
    = TK_INTEGER
    | TK_NAME
    | (-,+) factor
    | "(" b-expr ")"
*/
int parse_factor(unsigned char *string_start, unsigned char *&cur, const cpp_symtab &macro_definitions) {
    CPPTokenType type;
    std::string str;
    unsigned char *old_cur = cur;
    get_next_token(string_start, cur, type, str);
    if (type == CPPTokenType::TK_NAME) {
        if (macro_definitions.find(str) != macro_definitions.end()) {
            std::string v;
            if (macro_definitions.at(str).function_like) {
                if (*cur != '(') {
                    Location loc;
                    loc.first = cur - string_start;
                    loc.last = loc.first;
                    throw PreprocessorError("function-like macro invocation must have argument list", loc);
                }
                std::vector<std::string> args;
                args = parse_arguments(string_start, cur, false);
                if (*cur != ')') {
                    Location loc;
                    loc.first = cur - string_start;
                    loc.last = loc.first;
                    throw PreprocessorError("expected ')'", loc);
                }
                cur++;
                std::vector<std::string> margs = macro_definitions.at(str).args;
                std::string mexpansion = macro_definitions.at(str).expansion;
                v = function_like_macro_expansion(
                    margs,
                    mexpansion,
                    args);
            } else {
                v = macro_definitions.at(str).expansion;
            }
            unsigned char *cur2 = (unsigned char*)(&v[0]);
            int i = parse_expr(string_start, cur2, macro_definitions);
            return i;
        } else {
            // If the variable/macro is not defined, we evaluate it as 0
            return 0;
        }
    } else if (type == CPPTokenType::TK_INTEGER) {
        int i = std::stoi(str);
        return i;
    } else if (type == CPPTokenType::TK_MINUS) {
        int result = parse_factor(string_start, cur, macro_definitions);
        return -result;
    } else if (type == CPPTokenType::TK_PLUS) {
        int result = parse_factor(string_start, cur, macro_definitions);
        return +result;
    } else if (type == CPPTokenType::TK_LPAREN) {
        int result = parse_bexpr(string_start, cur, macro_definitions);
        accept(string_start, cur, CPPTokenType::TK_RPAREN);
        return result;

    // This is the only place where we can get unexpected tokens. Let us
    // handle them here:
    } else if (type == CPPTokenType::TK_EOF) {
        // EOF means the expression is not complete
        Location loc;
        loc.first = old_cur - string_start;
        loc.last = loc.first;
        throw PreprocessorError("factor(): The expression is not complete, expecting integer, name, +, - or (", loc);
    } else {
        Location loc;
        loc.first = old_cur - string_start;
        loc.last = cur - string_start;
        throw PreprocessorError("Unexpected token '" + token_type_to_string(type) + "' in factor()", loc);
    }
}

/*
relation
    = expr
    | expr (<,>,>=,<=,/=,!=,==) expr
*/
int parse_relation(unsigned char *string_start, unsigned char *&cur, const cpp_symtab &macro_definitions) {
    int lhs;
    lhs = parse_expr(string_start, cur, macro_definitions);
    unsigned char *old_cur = cur;

    CPPTokenType type;
    std::string str;
    get_next_token(string_start, cur, type, str);
    if (type >= CPPTokenType::TK_LT && type <= CPPTokenType::TK_EQ) {
        int rhs = parse_expr(string_start, cur, macro_definitions);
        if (type == CPPTokenType::TK_LT) {
            return lhs < rhs;
        } else if (type == CPPTokenType::TK_GT) {
            return lhs > rhs;
        } else if (type == CPPTokenType::TK_LTE) {
            return lhs <= rhs;
        } else if (type == CPPTokenType::TK_GTE) {
            return lhs >= rhs;
        } else if (type == CPPTokenType::TK_NE) {
            return lhs != rhs;
        } else if (type == CPPTokenType::TK_EQ) {
            return lhs == rhs;
        } else {
            Location loc;
            loc.first = cur - string_start;
            loc.last = loc.first;
            throw PreprocessorError("Unknown operator '" + token_type_to_string(type) + "' in if", loc);
        }
    } else {
        cur = old_cur; // Revert the last token, as we will not consume it
        return lhs;
    }
}

/*
b-factor
    = "defined(" TK_NAME ")"
    | "defined" TK_NAME
    | "!" b-factor
    | relation
*/
int parse_bfactor(unsigned char *string_start, unsigned char *&cur, const cpp_symtab &macro_definitions) {
    CPPTokenType type;
    std::string str;
    unsigned char *old_cur = cur;
    get_next_token(string_start, cur, type, str);
    if (type == CPPTokenType::TK_NAME && str == "defined") {
        std::string macro_name;
        get_next_token(string_start, cur, type, str);
        if (type == CPPTokenType::TK_LPAREN) {
            macro_name = accept_name(string_start, cur);
            accept(string_start, cur, CPPTokenType::TK_RPAREN);
        } else if (type == CPPTokenType::TK_NAME) {
            macro_name = str;
        } else {
            Location loc;
            loc.first = old_cur - string_start;
            loc.last = loc.first;
            throw PreprocessorError("Unexpected token "
                + token_type_to_string(type)
                + ", expected '(' or 'name'", loc);
        }
        if (macro_definitions.find(macro_name) != macro_definitions.end()) {
            return true;
        } else {
            return false;
        }
    } else if (type == CPPTokenType::TK_NEG) {
        bool bresult = parse_bfactor(string_start, cur, macro_definitions) > 0;
        bresult = !bresult; // Apply "!"
        return (int) bresult;
    } else {
        // For everything else we commit to relation and handle any potential errors there:
        cur = old_cur; // Restore the token
        int result = parse_relation(string_start, cur, macro_definitions);
        return result;
    }
}

}

} // namespace LCompilers::LFortran
