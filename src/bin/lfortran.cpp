#include "libasr/utils.h"
#include <chrono>
#include <iostream>
#include <stdlib.h>
#include <filesystem>
#include <random>
#ifndef CLI11_HAS_FILESYSTEM
#define CLI11_HAS_FILESYSTEM 0
#endif // CLI11_HAS_FILESYSTEM
#include <bin/CLI11.hpp>

#include <libasr/stacktrace.h>
#include <lfortran/parser/parser.h>
#include <lfortran/parser/preprocessor.h>
#include <lfortran/pickle.h>
#include <libasr/pickle.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <lfortran/mod_to_asr.h>
#include <libasr/codegen/asr_to_llvm.h>
#include <libasr/codegen/asr_to_cpp.h>
#include <libasr/codegen/asr_to_py.h>
#include <libasr/codegen/asr_to_x86.h>
#include <libasr/codegen/asr_to_wasm.h>
#include <lfortran/ast_to_src.h>
#include <lfortran/fortran_evaluator.h>
#include <libasr/codegen/evaluator.h>
#include <libasr/pass/pass_manager.h>
#include <libasr/pass/replace_do_loops.h>
#include <libasr/pass/replace_for_all.h>
#include <libasr/pass/wrap_global_stmts.h>
#include <libasr/pass/replace_implied_do_loops.h>
#include <libasr/pass/replace_array_op.h>
#include <libasr/pass/replace_class_constructor.h>
#include <libasr/pass/replace_arr_slice.h>
#include <libasr/pass/replace_print_arr.h>
#include <libasr/pass/replace_where.h>
#include <libasr/pass/unused_functions.h>
#include <libasr/pass/replace_flip_sign.h>
#include <libasr/pass/replace_div_to_mul.h>
#include <libasr/pass/replace_fma.h>
#include <libasr/pass/loop_unroll.h>
#include <libasr/pass/inline_function_calls.h>
#include <libasr/pass/dead_code_removal.h>
#include <libasr/pass/replace_sign_from_value.h>
#include <libasr/pass/unique_symbols.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/modfile.h>
#include <libasr/config.h>
#include <lfortran/fortran_kernel.h>
#include <libasr/string_utils.h>
#include <lfortran/utils.h>
#include <lfortran/parser/parser.tab.hh>
#include <string>
#include <sstream>

#include <cpp-terminal/terminal.h>
#include <cpp-terminal/prompt0.h>

#include <bin/lfortran_accessor.h>
#include <bin/lfortran_command_line_parser.h>
#include <bin/lsp_cli.h>

#ifdef WITH_LSP
#include <bin/language_server_interface.h>
#endif // WITH_LSP

#ifdef HAVE_BUILD_TO_WASM
    #include <emscripten/emscripten.h>
#endif

// ANSI color codes
#define RESET   "\033[0m"
#define BLUE    "\033[34m"   // Blue for non-[PASS] entries
#define GREEN   "\033[32m"   // Green for 'Total time'
#define YELLOW  "\033[33m"   // Yellow for 'Allocator usage of last chunk (MB)'
#define CYAN    "\033[36m"   // Cyan for 'Allocator chunks'
#define MAGENTA "\033[35m"   // Magenta for 'File reading' and 'Src -> ASR'
#define RED        "\033[31m"   // Red for 'Time taken by pass' and 'ASR -> ASR passes'

extern std::string lcompilers_unique_ID;
extern std::string lcompilers_commandline_options;

namespace {

using LCompilers::endswith;
using LCompilers::CompilerOptions;

namespace lcli = LCompilers::CommandLineInterface;

#ifdef WITH_LSP
namespace lsi = LCompilers::LLanguageServer::Interface;
#endif

enum Backend {
    llvm, c, cpp, x86, wasm, fortran, mlir
};

std::string get_unique_ID() {
    static std::random_device dev;
    static std::mt19937 rng(dev());
    std::uniform_int_distribution<int> dist(0, 61);
    const std::string v =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    std::string res;
    for (int i = 0; i < 22; i++) {
        res += v[dist(rng)];
    }
    return res;
}

void print_one_component(std::string component) {
    std::istringstream ss(component);
    std::string component_name;
    std::string time_value;

    // Extract component name and time
    std::getline(ss, component_name, ':');
    ss >> time_value;

    // Trim whitespace
    component_name.erase(component_name.find_last_not_of(" \t\n\r") + 1);
    component_name.erase(0, component_name.find_first_not_of(" \t\n\r"));

    bool is_pass = false;

    // Detect `[PASS]` and remove it
    if (component_name.find("[PASS]") == 0) {
        component_name = component_name.substr(6); // Remove '[PASS]'
        is_pass = true;
    }

    // Apply colors for key entries
    if (component_name == "Total time") {
        std::cout << std::string(60, '-') << '\n';  // Add dashed line before 'Total time'
        std::cout << GREEN;
    } else if (component_name == "Allocator usage of last chunk (MB)") {
        std::cout << YELLOW;
    } else if (component_name == "Allocator chunks") {
        std::cout << CYAN;
    } else if (component_name == "File reading" || component_name == "Src -> ASR") {
        std::cout << MAGENTA;
    } else if (component_name == "Time taken by pass" || component_name == "ASR -> ASR passes") {
        std::cout << RED;
    } else if (!is_pass) {
        std::cout << BLUE;  // Default blue for non-[PASS] entries
    }

    // Print in formatted table
    int indent_width = is_pass ? 4 : 0;  // Indent `[PASS]` components
    int name_width = 50 - indent_width;  // Adjust name column width

    if (time_value.empty()) {
        std::cout << std::string(indent_width, ' ')  // Print indentation
                  << std::left << component_name << RESET << '\n';
    } else {
        float time_float = std::stof(time_value);
        int time_width = 10; 

        std::cout << std::string(indent_width, ' ')  // Print indentation
                  << std::left << std::setw(name_width) << component_name << RESET
                  << std::right << std::setw(time_width)
                  << std::fixed << std::setprecision(3) << time_float
                  << '\n';
    }
}


// Note: this function is case sensitive to the input string
void print_time_report(const std::vector<std::string>& vector_of_time_report) {
    for (const auto& entry : vector_of_time_report) {
        // check if `Allocator usage of last chunk (MB)` or `Allocator chunks` is present
        if (entry.find("Allocator usage of last chunk (MB)") != std::string::npos ||
            entry.find("Allocator chunks") != std::string::npos) {
            print_one_component(entry);
        }
    }

    // Header
    std::cout << std::string(60, '-') << '\n';
    std::cout << std::left << std::setw(50) << "Component name"
              << std::right << std::setw(10) << "Time (ms)" << '\n';
    std::cout << std::string(60, '-') << '\n';

    for (const auto& entry : vector_of_time_report) {
        if (entry.find("Allocator usage of last chunk (MB)") == std::string::npos &&
            entry.find("Allocator chunks") == std::string::npos) {
            print_one_component(entry);
        }
    }

    std::cout << std::string(60, '-') << '\n';
}

std::string read_file(const std::string &filename)
{
    std::ifstream ifs(filename.c_str(), std::ios::in | std::ios::binary
            | std::ios::ate);

    std::ifstream::pos_type filesize = ifs.tellg();
    if (filesize < 0) return std::string();

    ifs.seekg(0, std::ios::beg);

    std::vector<char> bytes(filesize);
    ifs.read(&bytes[0], filesize);

    return std::string(&bytes[0], filesize);
}

#ifdef HAVE_LFORTRAN_LLVM

void section(const std::string &s)
{
    std::cout << color(LCompilers::style::bold) << color(LCompilers::fg::blue) << s << color(LCompilers::style::reset) << color(LCompilers::fg::reset) << std::endl;
}

int emit_tokens2(const std::string &input, std::vector<std::string>
    &tok_strings, std::vector<int> &toks, std::vector<LCompilers::LFortran::YYSTYPE>
    &stypes)
{
    // Overload for the case where we want all the token information to use
    // elsewhere
    // Src -> Tokens
    Allocator al(64*1024*1024);
    LCompilers::diag::Diagnostics diagnostics;
    auto res = LCompilers::LFortran::tokens(al, input, diagnostics, &stypes, nullptr, false);
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = "input";
        // `input` is only used for error rendering
        std::ofstream out("input");
        out << input;
        lm.files.push_back(fl);
        lm.init_simple(input);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::CompilerOptions cu;
    std::cerr << diagnostics.render(lm, cu);
    if (res.ok) {
        toks = res.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }
    for (size_t i=0; i < toks.size(); i++) {
        tok_strings.push_back(LCompilers::LFortran::pickle(toks[i], stypes[i]));
        //std::cout << LFortran::pickle(toks[i], stypes[i]) << std::endl;
    }
    return 0;
}

bool determine_completeness(std::string command)
{
    // Determine if the statement is complete
    // Get the tokens
    bool complete;
    std::vector<int> toks;
    std::vector<LCompilers::LFortran::YYSTYPE> stypes;
    std::vector<std::string> token_strings;
    int tok_ret = emit_tokens2(command, token_strings, toks, stypes);
    // The token enumerators are in parser.tab.hh
    int do_blnc = 0;
    if (std::find(toks.begin(), toks.end(), KW_DO)!=toks.end()
        || std::find(toks.begin(), toks.end(), KW_DOWHILE)!=toks.end()) {
        // Statement contains do loop
        for (size_t i = 0; i < toks.size(); i++) {
            if (toks[i] == KW_DO || toks[i] == KW_DOWHILE) {
                do_blnc++;
            } else if (toks[i] == KW_END_DO || toks[i] == KW_ENDDO) {
                do_blnc--;
            }
        }
    }
    int sr_blnc = 0;
    if (std::find(toks.begin(), toks.end(), KW_SUBROUTINE)!=toks.end()) {
        // Statement contains subroutine
        for (size_t i = 0; i < toks.size(); i++) {
            if (toks[i] == KW_SUBROUTINE) {
                sr_blnc++;
            } else if (toks[i] == KW_END_SUBROUTINE
                    || toks[i] == KW_ENDSUBROUTINE) {
                sr_blnc--;
            }
        }
    }
    int fn_blnc = 0;
    if (std::find(toks.begin(), toks.end(), KW_FUNCTION)!=toks.end()) {
        // Statement contains function
        for (size_t i = 0; i < toks.size(); i++) {
            if (toks[i] == KW_FUNCTION) {
                fn_blnc++;
            } else if (toks[i] == KW_END_FUNCTION
                    || toks[i] == KW_ENDFUNCTION) {
                fn_blnc--;
            }
        }
    }
    int if_blnc = 0;
    size_t endif_loc = toks.size() - 1;
    // If statements need more involved checks due to the potential for deep
    // nesting of block and logical if's.
    if (std::find(toks.begin(), toks.end(), KW_IF)!=toks.end()) {
        // Statement contains if
        for (size_t i = 0; i < toks.size(); i++) {
            if (toks[i] == KW_IF) {
                if_blnc++;
                // Determine if this is a logical or block if by checking for
                // then token.
                // Block if balance is decremented on an end if token.
                // An apparent logical if is decremented arbitrarily since we
                // aren't checking syntax here.
                if (std::find(toks.begin() + i, toks.begin() + endif_loc,
                            KW_THEN)!=toks.begin() + endif_loc) {
                    // The statement contains block ifs, check for end if
                    for (size_t j = i+1; j < endif_loc; j++) {
                        if (toks[j] == KW_THEN) {
                            bool then_found = true;
                            for (size_t k = endif_loc; k > j; k--) {
                                if (toks[k] == KW_ENDIF || toks[k] ==
                                        KW_END_IF) {
                                    if_blnc--;
                                    // Make sure to not double count this end
                                    // if by not looping to this far again
                                    endif_loc = k - 1;
                                 }
                            }
                            if (then_found) {
                                // We hit a then and no end if so this must be
                                // incomplete
                                break;
                            }
                        } else if (toks[j] == KW_IF) {
                            // We've encountered another if before a then, so
                            // this appears to be a logical if in a statement
                            // with other block ifs.
                            if_blnc--;
                            break;
                        }
                    }
                } else {
                    // No associated then, assume logical if
                    if_blnc--;
                }
            }
        }
    }
    if (do_blnc > 0 || sr_blnc > 0 || fn_blnc > 0 || if_blnc > 0) {
        complete = false;
    } else {
        // If there are excess end statements just return error eventually
        complete = true;
    }
    if (tok_ret == 1) {
        // Tokenizer error, assume complete and return error eventually
        complete = true;
    }
    return complete;
}

int prompt(bool verbose, CompilerOptions &cu)
{
    Terminal term(true, false);
    std::cout << "Interactive Fortran. Experimental prototype, not ready for end users." << std::endl;
    std::string version = LFORTRAN_VERSION;
    std::cout << "LFortran version: " << version << std::endl;
    std::cout << "  * Use Ctrl-D to exit" << std::endl;
    std::cout << "  * Use Enter to submit" << std::endl;
    std::cout << "  * Use Alt-Enter or Ctrl-N to make a new line" << std::endl;
    std::cout << "    - Editing (Keys: Left, Right, Home, End, Backspace, Delete)" << std::endl;
    std::cout << "    - History (Keys: Up, Down)" << std::endl;

    Allocator al(64*1024*1024);
    cu.interactive = true;
    LCompilers::FortranEvaluator e(cu);

    std::vector<std::string> history;
    std::function<bool(std::string)> iscomplete = determine_completeness;
    while (true) {
        std::string input = prompt0(term, ">>> ", history, iscomplete);
        if (input.size() == 1 && input[0] == CTRL_KEY('d')) {
            std::cout << std::endl;
            std::cout << "Exiting." << std::endl;
            return 0;
        }

        if (verbose) {
            section("Input:");
            std::cout << input << std::endl;
        }

        LCompilers::FortranEvaluator::EvalResult r;
        LCompilers::diag::Diagnostics diagnostics;

        try {
            LCompilers::LocationManager lm;
            LCompilers::PassManager lpm;
            lpm.use_default_passes();
            {
                LCompilers::LocationManager::FileLocations fl;
                fl.in_filename = "input";
                // `input` is only used for error rendering
                std::ofstream out("input");
                out << input;
                lm.files.push_back(fl);
                lm.file_ends.push_back(input.size());
            }
            LCompilers::Result<LCompilers::FortranEvaluator::EvalResult>
            res = e.evaluate(input, verbose, lm, lpm, diagnostics);
            if (res.ok) {
                r = res.result;
            } else {
                LCOMPILERS_ASSERT(diagnostics.has_error())
                std::cerr << diagnostics.render(lm, cu);
                diagnostics.clear();
                continue;
            }
        } catch (const LCompilers::LCompilersException &e) {
            std::cerr << "Internal Compiler Error: Unhandled exception" << std::endl;
            std::vector<LCompilers::StacktraceItem> d = e.stacktrace_addresses();
            get_local_addresses(d);
            get_local_info(d);
            std::cerr << stacktrace2str(d, LCompilers::stacktrace_depth);
            std::cerr << e.name() + ": " << e.msg() << std::endl;
            continue;
        }

        if (verbose) {
            section("AST:");
            std::cout << r.ast  << std::endl;
            section("ASR:");
            std::cout << r.asr << std::endl;
            section("LLVM IR:");
            std::cout << r.llvm_ir << std::endl;
        }

        switch (r.type) {
            case (LCompilers::FortranEvaluator::EvalResult::integer4) : {
                if (verbose) std::cout << "Return type: integer" << std::endl;
                if (verbose) section("Result:");
                std::cout << r.i32 << std::endl;
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::integer8) : {
                if (verbose) std::cout << "Return type: integer(8)" << std::endl;
                if (verbose) section("Result:");
                std::cout << r.i64 << std::endl;
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::real4) : {
                if (verbose) std::cout << "Return type: real" << std::endl;
                if (verbose) section("Result:");
                std::cout << std::setprecision(8) << r.f32 << std::endl;
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::real8) : {
                if (verbose) std::cout << "Return type: real(8)" << std::endl;
                if (verbose) section("Result:");
                std::cout << std::setprecision(17) << r.f64 << std::endl;
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::complex4) : {
                if (verbose) std::cout << "Return type: complex" << std::endl;
                if (verbose) section("Result:");
                std::cout << std::setprecision(8) << "(" << r.c32.re << ", " << r.c32.im << ")" << std::endl;
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::complex8) : {
                if (verbose) std::cout << "Return type: complex(8)" << std::endl;
                if (verbose) section("Result:");
                std::cout << std::setprecision(17) << "(" << r.c64.re << ", " << r.c64.im << ")" << std::endl;
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::boolean) : {
                if (verbose) std::cout << "Return type: logical" << std::endl;
                if (verbose) section("Result:");
                std::cout << (r.b ? "True" : "False") << std::endl;
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::statement) : {
                if (verbose) {
                    std::cout << "Return type: none" << std::endl;
                    section("Result:");
                    std::cout << "(statement)" << std::endl;
                }
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::none) : {
                if (verbose) {
                    std::cout << "Return type: none" << std::endl;
                    section("Result:");
                    std::cout << "(nothing to execute)" << std::endl;
                }
                break;
            }
            default : throw LCompilers::LCompilersException("Return type not supported");
        }
    }
    return 0;
}
#endif

int emit_prescan(const std::string &infile, CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }

    std::vector<std::filesystem::path> include_dirs;
    include_dirs.push_back(LCompilers::parent_path(lm.files.back().in_filename));
    include_dirs.insert(include_dirs.end(),
                          compiler_options.po.include_dirs.begin(),
                          compiler_options.po.include_dirs.end());
    std::string prescan = LCompilers::LFortran::prescan(input, lm,
        compiler_options.fixed_form, include_dirs);
    std::cout << prescan << std::endl;
    return 0;
}

int emit_tokens(const std::string &infile, bool line_numbers, const CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);
    // Src -> Tokens
    Allocator al(64*1024*1024);
    std::vector<int> toks;
    std::vector<LCompilers::LFortran::YYSTYPE> stypes;
    std::vector<LCompilers::Location> locations;
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
    }
    if (compiler_options.prescan || compiler_options.fixed_form) {
        std::vector<std::filesystem::path> include_dirs;
        include_dirs.push_back(LCompilers::parent_path(lm.files.back().in_filename));
        include_dirs.insert(include_dirs.end(),
                            compiler_options.po.include_dirs.begin(),
                            compiler_options.po.include_dirs.end());
        input = LCompilers::LFortran::prescan(input, lm,
            compiler_options.fixed_form, include_dirs);
    }
    auto res = LCompilers::LFortran::tokens(al, input, diagnostics, &stypes, &locations,
        compiler_options.fixed_form, compiler_options.continue_compilation);
    lm.init_simple(input);
    lm.file_ends.push_back(input.size());
    std::cerr << diagnostics.render(lm, compiler_options);
    if (res.ok) {
        toks = res.result;
        LCOMPILERS_ASSERT(toks.size() == stypes.size())
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }
    for (size_t i=0; i < toks.size(); i++) {
        std::cout << LCompilers::LFortran::pickle(toks[i], stypes[i]);
        if (line_numbers) {
            std::cout << " " << locations[i].first << ":" << locations[i].last;
        }
        std::cout << std::endl;
    }
    return 0;
}

int emit_ast(const std::string &infile, CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    LCompilers::diag::Diagnostics diagnostics;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::Result<std::string> r = fe.get_ast(input, lm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (r.ok) {
        if (compiler_options.po.visualize) {
            return visualize_json(r.result, compiler_options.platform);
        }
        std::cout << r.result << std::endl;
        return 0;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 2;
    }
}

int emit_ast_f90(const std::string &infile, CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);
    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    LCompilers::diag::Diagnostics diagnostics;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::Result<LCompilers::LFortran::AST::TranslationUnit_t*> r
            = fe.get_ast2(input, lm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (r.ok) {
        std::cout << LCompilers::LFortran::ast_to_src(*r.result,
            compiler_options.use_colors);
        return 0;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 2;
    }
}

int format(const std::string &infile, bool inplace, bool color, int indent,
    bool indent_unit, CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);
    LCompilers::LLanguageServer::LFortranAccessor lfortran_accessor;

    if (inplace) color = false;
    LCompilers::Result<std::string> result = lfortran_accessor.format(
        infile, input, compiler_options, color, indent, indent_unit
    );

    if (!result.ok) {
        return 2;
    }

    std::string &source = result.result;
    if (inplace) {
        std::ofstream out;
        out.open(infile);
        out << source;
    } else {
        std::cout << source;
    }

    return 0;
}

int python_wrapper(const std::string &infile, std::string array_order,
    CompilerOptions &compiler_options)
{

    bool c_order = (0==array_order.compare("c"));

    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::ASR::TranslationUnit_t* asr;

    // Src -> AST -> ASR
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
        result = fe.get_asr2(input, lm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (result.ok) {
        asr = result.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }

    // figure out pyx and pxd filenames
    auto prefix = infile.substr(0,infile.rfind('.'));
    auto chdr_fname = prefix + ".h";
    auto pxd_fname = prefix  + "_pxd.pxd"; // the "_pxd" is an ugly hack, see comment in asr_to_py.cpp
    auto pyx_fname = prefix  + ".pyx";

    // The ASR to Python converter needs to know the name of the .h file that will be written,
    // but needs all path information stripped off - just the filename.
    auto chdr_fname_forcodegen = chdr_fname;
    {
        // Find last ocurrence of \ or /, and delete everything up to that point.
        auto pos_windows = chdr_fname_forcodegen.rfind('\\');
        auto pos_other = chdr_fname_forcodegen.rfind('/');
        auto lastpos = std::max( (pos_windows == std::string::npos ? 0 : pos_windows) ,
                                 (pos_other   == std::string::npos ? 0 : pos_other) );
        if (lastpos > 0UL) chdr_fname_forcodegen.erase(0,lastpos+1);
    }

    // ASR -> (C header file, Cython pxd file, Cython pyx file)
    std::string c_h, pxd, pyx;
    std::tie(c_h, pxd, pyx) = LCompilers::asr_to_py(*asr, c_order, chdr_fname_forcodegen);


    // save generated outputs to files.
    std::ofstream(chdr_fname) << c_h;
    std::ofstream(pxd_fname)  << pxd;
    std::ofstream(pyx_fname)  << pyx;

    return 0;
}

[[maybe_unused]] int run_parser_and_semantics(const std::string &infile,
    CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
        r = fe.get_asr2(input, lm, diagnostics);
    bool has_error_w_cc = compiler_options.continue_compilation && diagnostics.has_error();
    std::cerr << diagnostics.render(lm, compiler_options);
    if (!r.ok) {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 2;
    }
    return has_error_w_cc;
}

[[maybe_unused]] int emit_asr(const std::string &infile,
    LCompilers::PassManager& pass_manager,
    CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
        r = fe.get_asr2(input, lm, diagnostics);
    bool has_error_w_cc = compiler_options.continue_compilation && diagnostics.has_error();
    std::cerr << diagnostics.render(lm, compiler_options);
    if (!r.ok) {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 2;
    }
    if ( compiler_options.lookup_name ) {
        // TODO: output in any format we want, right now just print normal ASR.
        // convert string to uint16_t
        uint16_t l = std::stoi(compiler_options.line);
        uint16_t c = std::stoi(compiler_options.column);
        uint64_t input_pos = lm.linecol_to_pos(l, c);
        uint64_t output_pos = lm.input_to_output_pos(input_pos, false);
        LCompilers::ASR::asr_t* asr = fe.handle_lookup_name(r.result, output_pos);
        std::cout << LCompilers::pickle(*asr, compiler_options.use_colors, compiler_options.indent,
                compiler_options.po.with_intrinsic_mods) << std::endl;
        return 0;
    }
    LCompilers::ASR::TranslationUnit_t* asr = r.result;

    Allocator al(64*1024*1024);
    compiler_options.po.always_run = true;
    compiler_options.po.run_fun = "f";

    pass_manager.apply_passes(al, asr, compiler_options.po, diagnostics);
    if (compiler_options.po.tree) {
        std::cout << LCompilers::pickle_tree(*asr,
            compiler_options.use_colors) << std::endl;
    } else if (compiler_options.po.json) {
        std::cout << LCompilers::pickle_json(*asr, lm, compiler_options.po.no_loc, compiler_options.po.with_intrinsic_mods) << std::endl;
    } else if (compiler_options.po.visualize) {
        std::string astr_data_json = LCompilers::pickle_json(*asr, lm, compiler_options.po.no_loc, compiler_options.po.with_intrinsic_mods);
        return visualize_json(astr_data_json, compiler_options.platform);
    } else {
        std::cout << LCompilers::pickle(*asr, compiler_options.use_colors, compiler_options.indent,
                compiler_options.po.with_intrinsic_mods) << std::endl;
    }
    return has_error_w_cc;
}

int emit_cpp(const std::string &infile, CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    LCompilers::diag::Diagnostics diagnostics;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::Result<std::string> cpp = fe.get_cpp(input, lm, diagnostics, 1);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (cpp.ok) {
        std::cout << cpp.result;
        return 0;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }
}

int emit_c(const std::string &infile,
    LCompilers::PassManager& pass_manager, CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    LCompilers::diag::Diagnostics diagnostics;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
        r = fe.get_asr2(input, lm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (!r.ok) {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 2;
    }
    diagnostics.diagnostics.clear();
    LCompilers::ASR::TranslationUnit_t* asr = r.result;

    LCompilers::Result<std::string> c_result = fe.get_c3(*asr, diagnostics,
                                                pass_manager, 1);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (c_result.ok) {
        std::cout << c_result.result;
        return 0;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }
}

int emit_julia(const std::string &infile, CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    LCompilers::diag::Diagnostics diagnostics;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::Result<std::string> julia = fe.get_julia(input, lm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (julia.ok) {
        std::cout << julia.result;
        return 0;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }
}

int emit_fortran(const std::string &infile, CompilerOptions &compiler_options) {
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    LCompilers::diag::Diagnostics diagnostics;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::Result<std::string> src = fe.get_fortran(input, lm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (src.ok) {
        std::cout << src.result;
        return compiler_options.continue_compilation && diagnostics.has_error();
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }
}

int dump_all_passes(const std::string &infile, CompilerOptions &compiler_options,
                        LCompilers::PassManager &pass_manager) {
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    LCompilers::diag::Diagnostics diagnostics;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }

    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*> asr = fe.get_asr2(input, lm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (asr.ok) {
        Allocator al(64*1024*1024);
        compiler_options.po.always_run = true;
        compiler_options.po.run_fun = "f";
        pass_manager.dump_all_passes(al, asr.result, compiler_options.po, diagnostics, lm);
        std::cerr << diagnostics.render(lm, compiler_options);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }
    return 0;
}

int save_mod_files(const LCompilers::ASR::TranslationUnit_t &u,
    const LCompilers::CompilerOptions &compiler_options,
    LCompilers::LocationManager lm)
{
    for (auto &item : u.m_symtab->get_scope()) {
        if (LCompilers::ASR::is_a<LCompilers::ASR::Module_t>(*item.second)) {
            LCompilers::ASR::Module_t *m = LCompilers::ASR::down_cast<LCompilers::ASR::Module_t>(item.second);

            // Do not save modfiles for modules that were already loaded
            // from modfiles (as full ASR)
            if (m->m_loaded_from_mod) continue;

            Allocator al(4*1024);
            LCompilers::SymbolTable *symtab =
                al.make_new<LCompilers::SymbolTable>(nullptr);
            symtab->add_symbol(std::string(m->m_name), item.second);
            LCompilers::SymbolTable *orig_symtab = m->m_symtab->parent;
            m->m_symtab->parent = symtab;

            LCompilers::Location loc;
            LCompilers::ASR::asr_t *asr = LCompilers::ASR::make_TranslationUnit_t(al, loc,
                symtab, nullptr, 0);
            LCompilers::ASR::TranslationUnit_t *tu =
                LCompilers::ASR::down_cast2<LCompilers::ASR::TranslationUnit_t>(asr);
            LCompilers::diag::Diagnostics diagnostics;
            LCOMPILERS_ASSERT(LCompilers::asr_verify(*tu, true, diagnostics));

            std::string modfile_binary = LCompilers::save_modfile(*tu, lm);

            m->m_symtab->parent = orig_symtab;

            LCOMPILERS_ASSERT(LCompilers::asr_verify(u, true, diagnostics));

	    std::filesystem::path filename { std::string(m->m_name) + ".mod" };
            std::filesystem::path fullpath = compiler_options.po.mod_files_dir / filename;
            {
                std::ofstream out;
		out.open(fullpath, std::ofstream::out | std::ofstream::binary);
                out << modfile_binary;
            }
        }
    }
    return 0;
}

#ifdef HAVE_LFORTRAN_MLIR
int handle_mlir(const std::string &infile,
        const std::string &outfile,
        CompilerOptions &compiler_options,
        bool emit_mlir, bool emit_llvm) {
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::ASR::TranslationUnit_t* asr;

    // Src -> AST -> ASR
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
        result = fe.get_asr2(input, lm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (result.ok) {
        asr = result.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }

    // ASR -> MLIR -> LLVM
    LCompilers::LLVMEvaluator e(compiler_options.target);
    std::unique_ptr<LCompilers::MLIRModule> m;
    diagnostics.diagnostics.clear();
    LCompilers::Result<std::unique_ptr<LCompilers::MLIRModule>>
        res = fe.get_mlir(*(LCompilers::ASR::asr_t *)asr, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (res.ok) {
        m = std::move(res.result);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 2;
    }
    if (emit_mlir) {
        std::cout << m->mlir_str();
    } else if (emit_llvm) {
        std::cout << m->llvm_str();
    } else {
        // LLVM -> Machine code (saves to an object file)
        e.save_object_file(*(m->llvm_m), outfile);
    }
    return 0;
}
#endif // HAVE_LFORTRAN_MLIR

#ifdef HAVE_LFORTRAN_LLVM

int emit_llvm(const std::string &infile, LCompilers::PassManager& pass_manager,
              CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::Result<std::string> llvm
        = fe.get_llvm(input, lm, pass_manager, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (llvm.ok) {
        std::cout << llvm.result;
        return compiler_options.continue_compilation && diagnostics.has_error();
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }
}

int emit_asm(const std::string &infile, CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    // TODO: Remove this and accept pass manager in emit_asm
    LCompilers::PassManager lpm;
    LCompilers::diag::Diagnostics diagnostics;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::Result<std::string> r = fe.get_asm(input, lm, lpm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (r.ok) {
        std::cout << r.result;
        return 0;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }
}

int compile_src_to_object_file(const std::string &infile,
        const std::string &outfile,
        bool time_report,
        bool assembly,
        CompilerOptions &compiler_options,
        LCompilers::PassManager& lpm)
{
    int time_file_read=0;
    int time_src_to_asr=0;
    int time_save_mod=0;
    int time_opt=0;
    int time_llvm_to_bin=0;

    auto t1 = std::chrono::high_resolution_clock::now();
    std::string input = read_file(infile);
    auto t2 = std::chrono::high_resolution_clock::now();
    time_file_read = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count();

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::ASR::TranslationUnit_t* asr;


    // Src -> AST -> ASR
    LCompilers::LocationManager lm;

    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::diag::Diagnostics diagnostics;
    t1 = std::chrono::high_resolution_clock::now();
    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
        result = fe.get_asr2(input, lm, diagnostics);
    t2 = std::chrono::high_resolution_clock::now();
    time_src_to_asr = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count();
    bool has_error_w_cc = compiler_options.continue_compilation && diagnostics.has_error();
    std::cerr << diagnostics.render(lm, compiler_options);
    if (result.ok) {
        asr = result.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }

    // Save .mod files
    {
        t1 = std::chrono::high_resolution_clock::now();
        int err = save_mod_files(*asr, compiler_options, lm);
        t2 = std::chrono::high_resolution_clock::now();
        time_save_mod = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count();
        if (err) return err;
    }

    // ASR -> LLVM
    LCompilers::LLVMEvaluator e(compiler_options.target);

    if (!compiler_options.generate_object_code && !LCompilers::ASRUtils::main_program_present(*asr)
        && !LCompilers::ASRUtils::global_function_present(*asr)) {
        // Create an empty object file (things will be actually
        // compiled and linked when the main program is present):
        e.create_empty_object_file(outfile);
        return 0;
    }

    std::unique_ptr<LCompilers::LLVMModule> m;
    diagnostics.diagnostics.clear();
    if (compiler_options.emit_debug_info) {
#ifndef HAVE_RUNTIME_STACKTRACE
        diagnostics.add(LCompilers::diag::Diagnostic(
            "The `runtime stacktrace` is not enabled. To get the stack traces "
            "or debugging information, please re-build LFortran with "
            "`-DWITH_RUNTIME_STACKTRACE=yes`",
            LCompilers::diag::Level::Error,
            LCompilers::diag::Stage::Semantic, {})
        );
        std::cerr << diagnostics.render(lm, compiler_options);
        return 1;
#endif
    }
    LCompilers::Result<std::unique_ptr<LCompilers::LLVMModule>>
        res = fe.get_llvm3(*asr, lpm, diagnostics, infile);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (res.ok) {
        m = std::move(res.result);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 5;
    }

    if (compiler_options.po.fast) {
        t1 = std::chrono::high_resolution_clock::now();
        e.opt(*m->m_m);
        t2 = std::chrono::high_resolution_clock::now();
        time_opt = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count();
    }

    // LLVM -> Machine code (saves to an object file)
    if (assembly) {
        e.save_asm_file(*(m->m_m), outfile);
    } else {
        t1 = std::chrono::high_resolution_clock::now();
        e.save_object_file(*(m->m_m), outfile);
        t2 = std::chrono::high_resolution_clock::now();
        time_llvm_to_bin = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count();
    }

    if(compiler_options.po.enable_gpu_offloading) {
#ifdef HAVE_LFORTRAN_MLIR
        for (auto &item : asr->m_symtab->get_scope()) {
            if (LCompilers::ASR::is_a<LCompilers::ASR::Module_t>(*item.second) &&
                    item.first.find("_lcompilers_mlir_gpu_offloading")
                    != std::string::npos) {
                LCompilers::ASR::Module_t &mod = *LCompilers::ASR::down_cast
                    <LCompilers::ASR::Module_t>(item.second);
                LCompilers::Result<std::unique_ptr<LCompilers::MLIRModule>>
                    mlir_res = fe.get_mlir((LCompilers::ASR::asr_t &)mod, diagnostics);

                std::cerr << diagnostics.render(lm, compiler_options);
                if (mlir_res.ok) {
                    mlir_res.result->mlir_to_llvm(*mlir_res.result->llvm_ctx);
                    std::string mlir_tmp_o{std::filesystem::path(infile).
                        replace_extension(".mlir.tmp.o").string()};
                    e.save_object_file(*(mlir_res.result->llvm_m), mlir_tmp_o);
                } else {
                    LCOMPILERS_ASSERT(diagnostics.has_error())
                    return 1;
                }
            }
        }
#endif
    }

    if (time_report) {
        std::string message = "";
        message = "Allocator usage of last chunk (MB): " +
            std::to_string(fe.get_al().size_current() / (1024. * 1024));
        compiler_options.po.vector_of_time_report.push_back(message);
        message = "Allocator chunks: " + std::to_string(fe.get_al().num_chunks());
        compiler_options.po.vector_of_time_report.push_back(message);
        message = "File reading: " + std::to_string(time_file_read / 1000) + "." + std::to_string(time_file_read % 1000) + " ms";
        compiler_options.po.vector_of_time_report.push_back(message);
        message = "Src -> ASR:  " + std::to_string(time_src_to_asr / 1000) + "." + std::to_string(time_src_to_asr % 1000) + " ms";
        compiler_options.po.vector_of_time_report.push_back(message);
        message = "Time taken by pass: ";
        compiler_options.po.vector_of_time_report.push_back(message);
        for (auto it: fe.compiler_options.po.vector_of_time_report) {
            compiler_options.po.vector_of_time_report.push_back(it);
        }
        message = "ASR -> mod:  " + std::to_string(time_save_mod / 1000) + "." + std::to_string(time_save_mod % 1000) + " ms";
        compiler_options.po.vector_of_time_report.push_back(message);
        message = "LLVM opt:    " + std::to_string(time_opt / 1000) + "." + std::to_string(time_opt % 1000) + " ms";
        compiler_options.po.vector_of_time_report.push_back(message);
        message = "LLVM -> BIN: " + std::to_string(time_llvm_to_bin / 1000) + "." + std::to_string(time_llvm_to_bin % 1000) + " ms";
        compiler_options.po.vector_of_time_report.push_back(message);
    }

    return has_error_w_cc;
}

int compile_llvm_to_object_file(const std::string& infile,
                                const std::string& outfile,
                                CompilerOptions& compiler_options)
{
    std::string input = read_file(infile);
    LCompilers::LLVMEvaluator e(compiler_options.target);

    std::unique_ptr<LCompilers::LLVMModule> m = e.parse_module2(input, infile);
    e.save_object_file(*(m->m_m), outfile);

    return 0;
}

int compile_to_assembly_file(const std::string &infile,
    const std::string &outfile, bool time_report, CompilerOptions &compiler_options,
    LCompilers::PassManager& lpm)
{
    return compile_src_to_object_file(infile, outfile, time_report, true, compiler_options, lpm);
}
#endif // HAVE_LFORTRAN_LLVM


int emit_wat(const std::string &infile, CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    LCompilers::diag::Diagnostics diagnostics;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::Result<std::string> r = fe.get_wat(input, lm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (r.ok) {
        std::cout << r.result;
        return 0;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }
}

int compile_to_binary_x86(const std::string &infile, const std::string &outfile,
        bool time_report,
        CompilerOptions &compiler_options)
{
    int time_file_read=0;
    int time_src_to_ast=0;
    int time_ast_to_asr=0;
    int time_asr_to_x86=0;

    std::string input;
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::FortranEvaluator fe(compiler_options);
    Allocator al(64*1024*1024); // Allocate 64 MB
    LCompilers::LFortran::AST::TranslationUnit_t* ast;
    LCompilers::ASR::TranslationUnit_t* asr;

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        input = read_file(infile);
        auto t2 = std::chrono::high_resolution_clock::now();
        time_file_read = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();
    }

    // Src -> AST
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    {
        auto t1 = std::chrono::high_resolution_clock::now();
        LCompilers::Result<LCompilers::LFortran::AST::TranslationUnit_t*>
            result = fe.get_ast2(input, lm, diagnostics);
        auto t2 = std::chrono::high_resolution_clock::now();
        time_src_to_ast = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();

        std::cerr << diagnostics.render(lm, compiler_options);
        if (result.ok) {
            ast = result.result;
        } else {
            LCOMPILERS_ASSERT(diagnostics.has_error())
            return 1;
        }
    }

    // AST -> ASR
    {
        diagnostics.diagnostics.clear();
        auto t1 = std::chrono::high_resolution_clock::now();
        LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
            result = fe.get_asr3(*ast, diagnostics, lm);
        auto t2 = std::chrono::high_resolution_clock::now();
        time_ast_to_asr = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();

        std::cerr << diagnostics.render(lm, compiler_options);
        if (result.ok) {
            asr = result.result;
        } else {
            LCOMPILERS_ASSERT(diagnostics.has_error())
            return 2;
        }
    }

    // ASR -> x86 machine code
    {
        diagnostics.diagnostics.clear();
        auto t1 = std::chrono::high_resolution_clock::now();
        LCompilers::Result<int>
            result = LCompilers::asr_to_x86(*asr, al, outfile, time_report, diagnostics);
        auto t2 = std::chrono::high_resolution_clock::now();
        time_asr_to_x86 = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();

        std::cerr << diagnostics.render(lm, compiler_options);
        if (result.ok) {
            // pass
        } else {
            LCOMPILERS_ASSERT(diagnostics.has_error())
            return 3;
        }
    }

    if (time_report) {
        std::cout << "Allocator usage of last chunk (MB): "
            << al.size_current() / (1024. * 1024) << std::endl;
        std::cout << "Allocator chunks: " << al.num_chunks() << std::endl;
        std::cout << std::endl;
        std::cout << "Time report:" << std::endl;
        std::cout << "File reading:" << std::setw(5) << time_file_read << std::endl;
        std::cout << "Src -> AST:  " << std::setw(5) << time_src_to_ast << std::endl;
        std::cout << "AST -> ASR:  " << std::setw(5) << time_ast_to_asr << std::endl;
        std::cout << "ASR -> x86:  " << std::setw(5) << time_asr_to_x86 << std::endl;
        int total = time_file_read + time_src_to_ast + time_ast_to_asr
                + time_asr_to_x86;
        std::cout << "Total:       " << std::setw(5) << total << std::endl;
    }

    return 0;
}

int compile_to_binary_wasm(const std::string &infile, const std::string &outfile,
        bool time_report,
        CompilerOptions &compiler_options)
{
    int time_file_read=0;
    int time_src_to_ast=0;
    int time_ast_to_asr=0;
    int time_asr_to_wasm=0;

    std::string input;
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::FortranEvaluator fe(compiler_options);
    Allocator al(64*1024*1024); // Allocate 64 MB
    LCompilers::LFortran::AST::TranslationUnit_t* ast;
    LCompilers::ASR::TranslationUnit_t* asr;

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        input = read_file(infile);
        auto t2 = std::chrono::high_resolution_clock::now();
        time_file_read = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count();
    }

    // Src -> AST
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    {
        auto t1 = std::chrono::high_resolution_clock::now();
        LCompilers::Result<LCompilers::LFortran::AST::TranslationUnit_t*>
            result = fe.get_ast2(input, lm, diagnostics);
        auto t2 = std::chrono::high_resolution_clock::now();
        time_src_to_ast = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();

        std::cerr << diagnostics.render(lm, compiler_options);
        if (result.ok) {
            ast = result.result;
        } else {
            LCOMPILERS_ASSERT(diagnostics.has_error())
            return 1;
        }
    }

    // AST -> ASR
    {
        diagnostics.diagnostics.clear();
        auto t1 = std::chrono::high_resolution_clock::now();
        LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
            result = fe.get_asr3(*ast, diagnostics, lm);
        auto t2 = std::chrono::high_resolution_clock::now();
        time_ast_to_asr = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();

        std::cerr << diagnostics.render(lm, compiler_options);
        if (result.ok) {
            asr = result.result;
        } else {
            LCOMPILERS_ASSERT(diagnostics.has_error())
            return 2;
        }
    }

    // ASR -> wasm machine code
    {
        diagnostics.diagnostics.clear();
        auto t1 = std::chrono::high_resolution_clock::now();
        LCompilers::Result<int>
            result = LCompilers::asr_to_wasm(*asr, al, outfile, time_report, diagnostics, compiler_options);
        auto t2 = std::chrono::high_resolution_clock::now();
        time_asr_to_wasm = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();

        std::cerr << diagnostics.render(lm, compiler_options);
        if (result.ok) {
            // pass
        } else {
            LCOMPILERS_ASSERT(diagnostics.has_error())
            return 3;
        }
    }

    if (time_report) {
        std::cout << "Allocator usage of last chunk (MB): "
            << al.size_current() / (1024. * 1024) << std::endl;
        std::cout << "Allocator chunks: " << al.num_chunks() << std::endl;
        std::cout << std::endl;
        std::cout << "Time report:" << std::endl;
        std::cout << "File reading:" << std::setw(5) << time_file_read << std::endl;
        std::cout << "Src -> AST:  " << std::setw(5) << time_src_to_ast << std::endl;
        std::cout << "AST -> ASR:  " << std::setw(5) << time_ast_to_asr << std::endl;
        std::cout << "ASR -> wasm:  " << std::setw(5) << time_asr_to_wasm << std::endl;
        int total = time_file_read + time_src_to_ast + time_ast_to_asr
                + time_asr_to_wasm;
        std::cout << "Total:       " << std::setw(5) << total << std::endl;
    }

    return 0;
}


int compile_to_object_file_cpp(const std::string &infile,
        const std::string &outfile, bool verbose,
        bool assembly, bool kokkos, const std::string &rtlib_header_dir,
        CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::ASR::TranslationUnit_t* asr;

    // Src -> AST -> ASR
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
        result = fe.get_asr2(input, lm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (result.ok) {
        asr = result.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }

    // Save .mod files
    {
        int err = save_mod_files(*asr, compiler_options, lm);
        if (err) return err;
    }

    if (!LCompilers::ASRUtils::main_program_present(*asr)) {
        // Create an empty object file (things will be actually
        // compiled and linked when the main program is present):
        if (compiler_options.platform == LCompilers::Platform::Windows) {
            {
                std::ofstream out;
                out.open(outfile);
                out << " ";
            }
        } else {
            std::string outfile_empty = outfile + ".empty.c";
            {
                std::ofstream out;
                out.open(outfile_empty);
                out << " ";
            }
	    std::string CC = "cc";
            char *env_CC = std::getenv("LFORTRAN_CC");
            if (env_CC) CC = env_CC;
            std::string cmd = CC + " -c '" + outfile_empty + "' -o '" + outfile + "'";
            int err = system(cmd.c_str());
            if (err) {
                std::cout << "The command '" + cmd + "' failed." << std::endl;
                return 11;
            }
        }
        return 0;
    }

    // ASR -> C++
    std::string src;
    diagnostics.diagnostics.clear();
    LCompilers::Result<std::string> res
        = fe.get_cpp2(*asr, diagnostics, 1);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (res.ok) {
        src = res.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 5;
    }

    // C++ -> Machine code (saves to an object file)
    if (assembly) {
        throw LCompilers::LCompilersException("Not implemented");
    } else {
        std::string cppfile = outfile + ".tmp.cpp";
        {
            std::ofstream out;
            out.open(cppfile);
            out << src;
        }

        std::string CXX = "g++";
        std::string options;
        if (compiler_options.openmp) {
            options += "-fopenmp ";
        }
        if (kokkos) {
            std::string kokkos_includedir = LCompilers::LFortran::get_kokkos_includedir();
            options += "-std=c++17 -I" + kokkos_includedir;
        }
        options += " -I" + rtlib_header_dir;
        std::string cmd = CXX + " " + options + " -o " + outfile + " -c " + cppfile;
        if (verbose) {
            std::cout << cmd << std::endl;
        }
        int err = system(cmd.c_str());
        if (err) {
            std::cout << "The command '" + cmd + "' failed." << std::endl;
            return 11;
        }
    }

    return 0;
}

int compile_to_object_file_c(const std::string &infile,
        const std::string &outfile, bool verbose,
        bool assembly, const std::string &rtlib_header_dir,
        LCompilers::PassManager pass_manager,
        CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    // Src -> AST -> ASR
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
        r = fe.get_asr2(input, lm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (!r.ok) {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 2;
    }
    diagnostics.diagnostics.clear();
    LCompilers::ASR::TranslationUnit_t* asr = r.result;

    // Save .mod files
    {
        int err = save_mod_files(*asr, compiler_options, lm);
        if (err) return err;
    }

    if (!LCompilers::ASRUtils::main_program_present(*asr)) {
        // Create an empty object file (things will be actually
        // compiled and linked when the main program is present):
        if (compiler_options.platform == LCompilers::Platform::Windows) {
            {
                std::ofstream out;
                out.open(outfile);
                out << " ";
            }
        } else {
            std::string outfile_empty = outfile + ".empty.c";
            {
                std::ofstream out;
                out.open(outfile_empty);
                out << " ";
            }
	    std::string CC = "cc";
            char *env_CC = std::getenv("LFORTRAN_CC");
            if (env_CC) CC = env_CC;
            std::string cmd = CC + " -c '" + outfile_empty + "' -o '" + outfile + "'";
            int err = system(cmd.c_str());
            if (err) {
                std::cout << "The command '" + cmd + "' failed." << std::endl;
                return 11;
            }
        }
        return 0;
    }

    // ASR -> C
    std::string src;
    diagnostics.diagnostics.clear();
    LCompilers::Result<std::string> res
        = fe.get_c3(*asr, diagnostics, pass_manager, 1);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (res.ok) {
        src = res.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 5;
    }

    // C -> Machine code (saves to an object file)
    if (assembly) {
        throw LCompilers::LCompilersException("Not implemented");
    } else {
        std::string cfile = outfile + ".tmp.c";
        {
            std::ofstream out;
            out.open(cfile);
            out << src;
        }

        std::string CXX = "gcc";
        std::string options = " -I" + rtlib_header_dir;
        std::string cmd = CXX + " " + options + " -o " + outfile + " -c " + cfile;
        if (verbose) {
            std::cout << cmd << std::endl;
        }
        int err = system(cmd.c_str());
        if (err) {
            std::cout << "The command '" + cmd + "' failed." << std::endl;
            return 11;
        }
    }

    return 0;
}

int compile_to_binary_fortran(const std::string &infile,
        const std::string &outfile,
        CompilerOptions &compiler_options) {
    std::string input = read_file(infile);

    LCompilers::FortranEvaluator fe(compiler_options);
    LCompilers::LocationManager lm;
    LCompilers::diag::Diagnostics diagnostics;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::Result<std::string> src = fe.get_fortran(input, lm, diagnostics);
    std::cerr << diagnostics.render(lm, compiler_options);
    if (!src.ok) {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return 1;
    }

    std::string in_file = outfile + ".tmp.f90";
    {
        std::ofstream out;
        out.open(in_file);
        out << src.result;
    }

    std::string cmd = "gfortran -fno-backtrace -o " + outfile + " -c " + in_file;
    int err = system(cmd.c_str());
    if (err) {
        std::cout << "The command '" + cmd + "' failed." << std::endl;
        return 11;
    }
    return 0;
}

// infile is an object file
// outfile will become the executable
int link_executable(const std::vector<std::string> &infiles,
    const std::string &outfile,
    bool time_report,
    const std::string &runtime_library_dir, Backend backend,
    bool static_executable, bool shared_executable,
    std::string linker, std::string linker_path, bool kokkos,
    bool verbose, const std::vector<std::string> &lib_dirs,
    const std::vector<std::string> &libraries,
    const std::vector<std::string> &linker_flags,
    CompilerOptions &compiler_options)
{
    /*
    The `gcc` line for dynamic linking that is constructed below:

    gcc -o $outfile $infile \
        -Lsrc/runtime -Wl,-rpath=src/runtime -llfortran_runtime

    is equivalent to the following:

    ld -o $outfile $infile \
        -Lsrc/runtime -rpath=src/runtime -llfortran_runtime \
        -dynamic-linker /lib64/ld-linux-x86-64.so.2  \
        /usr/lib/x86_64-linux-gnu/Scrt1.o /usr/lib/x86_64-linux-gnu/libc.so

    and this for static linking:

    gcc -static -o $outfile $infile \
        -Lsrc/runtime -Wl,-rpath=src/runtime -llfortran_runtime_static

    is equivalent to:

    ld -o $outfile $infile \
        -Lsrc/runtime -rpath=src/runtime -llfortran_runtime_static \
        /usr/lib/x86_64-linux-gnu/crt1.o /usr/lib/x86_64-linux-gnu/crti.o \
        /usr/lib/x86_64-linux-gnu/libc.a \
        /usr/lib/gcc/x86_64-linux-gnu/7/libgcc_eh.a \
        /usr/lib/x86_64-linux-gnu/libc.a \
        /usr/lib/gcc/x86_64-linux-gnu/7/libgcc.a \
        /usr/lib/x86_64-linux-gnu/crtn.o

    This was tested on Ubuntu 18.04.

    The `gcc` and `ld` approaches are equivalent except:

    1. The `gcc` command knows how to find and link the `libc` library,
       while in `ld` we must do that manually
    2. For dynamic linking, we must also specify the dynamic linker for `ld`

    Notes:

    * We can use `lld` to do the linking via the `ld` approach, so `ld` is
      preferable if we can mitigate the issues 1. and 2.
    * If we ship our own libc (such as musl), then we know how to find it
      and link it, which mitigates the issue 1.
    * If we link `musl` statically, then issue 2. does not apply.
    * If we link `musl` dynamically, then we have to find the dynamic
      linker (doable), which mitigates the issue 2.

    One way to find the default dynamic linker is by:

        $ readelf -e /bin/bash | grep ld-linux
            [Requesting program interpreter: /lib64/ld-linux-x86-64.so.2]

    There are probably simpler ways.
    */

    auto t1 = std::chrono::high_resolution_clock::now();
#ifdef HAVE_LFORTRAN_LLVM
    std::string t = (compiler_options.target == "") ? LCompilers::LLVMEvaluator::get_default_target_triple() : compiler_options.target;
#else
    std::string t = (compiler_options.platform == LCompilers::Platform::Windows) ? "x86_64-pc-windows-msvc" : compiler_options.target;
#endif
    size_t dot_index = outfile.find_last_of(".");
    std::string file_name = outfile.substr(0, dot_index);
    std::string extra_linker_flags;
    if (!linker_flags.empty()) {
        for (auto &s: linker_flags) {
            extra_linker_flags += " -W" + s;
        }
    }
    std::string extra_library_flags;
    if (!lib_dirs.empty()) {
        for (auto &s: lib_dirs) {
            extra_library_flags += " -L" + s;
        }
    }
    if (!libraries.empty()) {
        for (auto &s: libraries) {
            extra_library_flags += " -l" + s;
        }
    }
    if(static_executable && shared_executable) {
        std::cout << "Cannot use static_executable and shared_executable together" << std::endl;
        return 10;
    }
    if (backend == Backend::llvm || backend == Backend::mlir) {
        std::string run_cmd = "", compile_cmd = "";
        if (t == "x86_64-pc-windows-msvc") {
            compile_cmd = "link /NOLOGO /OUT:" + outfile + " ";
            for (auto &s : infiles) {
                compile_cmd += s + " ";
            }
            compile_cmd += runtime_library_dir + "\\lfortran_runtime_static.lib";
            run_cmd = outfile;
        } else if (LCompilers::startswith(t, "wasm")) {
            std::string CC, options, runtime_lib;
            if (LCompilers::endswith(t, "wasi")) {
                char* wasi_sdk_path = std::getenv("WASI_SDK_PATH");
                if (wasi_sdk_path == nullptr) {
                    std::cerr << "WASI_SDK_PATH must be defined to use llvm->wasm\n";
                    return 11;
                }
                CC = std::string(wasi_sdk_path) + "/bin/clang";
                options = " --target=wasm32-wasi -nostartfiles -Wl,--entry=_start -Wl,-lwasi-emulated-process-clocks";
                runtime_lib = "lfortran_runtime_wasm_wasi.o";
                compile_cmd = CC + options + " -o " + outfile + " ";
            } else if (LCompilers::endswith(t, "emscripten")) {
                char* emsdk_path = std::getenv("EMSDK_PATH");
                if (emsdk_path == nullptr) {
                    std::cerr << "EMSDK_PATH must be defined to use llvm->wasm\n";
                    return 11;
                }
                CC = std::string(emsdk_path) + "/upstream/emscripten/emcc";
                options = " --target=wasm32-unknown-emscripten -sSTACK_SIZE=50mb -sINITIAL_MEMORY=256mb";
                if (!compiler_options.emcc_embed.empty()) {
                    options += " --embed-file " + compiler_options.emcc_embed;
                }
                runtime_lib = "lfortran_runtime_wasm_emcc.o";
                compile_cmd = CC + options + " -o " + outfile +
                     (compiler_options.wasm_html ? ".html " : " ");
            } else {
                std::cerr << "Unsupported target: " << t << std::endl;
                return 10;
            }
            for (auto &s : infiles) {
                compile_cmd += s + " ";
            }
            compile_cmd += runtime_library_dir + "/" + runtime_lib;
            compile_cmd +=  extra_linker_flags;
        } else {
            std::string CC{""};
            std::string base_path = "\"" + runtime_library_dir + "\"";
            std::string options;
            std::string runtime_lib = "lfortran_runtime";

            if (!linker_path.empty()) {
                CC = linker_path;
            } else if (char *env_path = std::getenv("LFORTRAN_LINKER_PATH")) {
                CC = env_path;
            }

            if (!CC.empty() && CC.back() != '/') {
                // TODO: Fix the path usage for Windows
                CC += "/";
            }

            if (!linker.empty()) {
                CC += linker;
            } else if (char *env_linker = std::getenv("LFORTRAN_LINKER")) {
                CC += env_linker;
            } else {
                // TODO: Add support for msvc linker for Windows
                // TODO: Add support for lld linker
                // Default linker to be used
                CC += "clang";
            }

            if (compiler_options.target != "" &&
                    CC.find("clang" ) != std::string::npos) {
                options = " -target " + compiler_options.target;
            }

            if (static_executable) {
                if (compiler_options.platform != LCompilers::Platform::macOS_Intel
                && compiler_options.platform != LCompilers::Platform::macOS_ARM) {
                    options += " -static ";
                }
                runtime_lib = "lfortran_runtime_static";
            }
            if (shared_executable) {
                options += " -shared ";
            }
            compile_cmd = CC + options + " -o " + outfile + " ";
            for (auto &s : infiles) {
                compile_cmd += s + " ";
                if (backend == Backend::llvm &&
                        compiler_options.po.enable_gpu_offloading &&
                        LCompilers::endswith(s, ".tmp.o")) {
                    std::string mlir_tmp_o{s.substr(0, s.size() - 6) +
                        ".mlir.tmp.o"};
                    compile_cmd += mlir_tmp_o + " ";
                }
            }
            if(!extra_library_flags.empty()) {
                compile_cmd += extra_library_flags + " ";
            }
            compile_cmd += + " -L"
                + base_path + " -Wl,-rpath," + base_path;
            if (!extra_linker_flags.empty()) {
                compile_cmd += extra_linker_flags;
            }
            compile_cmd += " -l" + runtime_lib + " -lm";
            if (compiler_options.openmp) {
                std::string openmp_shared_library = compiler_options.openmp_lib_dir;
                std::string omp_cmd =  " -L" + openmp_shared_library + " -Wl,-rpath," + openmp_shared_library + " -lomp";
                if (!openmp_shared_library.empty()) {
                    compile_cmd += omp_cmd;
                }
            }
            run_cmd = "./" + outfile;
        }
        if (verbose) {
            compile_cmd += " -v";
            std::cout << compile_cmd << std::endl;
        }
        int err = system(compile_cmd.c_str());
        if (err) {
            std::cerr << "The command '" + compile_cmd + "' failed." << std::endl;
            std::cerr << "Tip: If there is a linker issue, switch the linker "
                "using --linker=<CC> option or create an environment "
                "variable `export LFORTRAN_LINKER=<CC>`, where CC is "
                "clang or gcc" << std::endl;
            std::cerr << "Also, if required use --linker-path=<PATH>, "
                "where PATH has location to look for the linker "
                "execuatable" << std::endl;
            return 10;
        }

#ifdef HAVE_RUNTIME_STACKTRACE
        if (compiler_options.emit_debug_info) {
            // TODO: Replace the following hardcoded part
            std::string cmd = "";
#ifdef HAVE_LFORTRAN_MACHO
            cmd += "dsymutil " + file_name + ".out && llvm-dwarfdump --debug-line "
                + file_name + ".out.dSYM > ";
#else
            cmd += "llvm-dwarfdump --debug-line " + file_name + ".out > ";
#endif
            std::string dwarf_scripts_path = LCompilers::LFortran::get_dwarf_scripts_dir();
            cmd += file_name + "_ldd.txt && (" + dwarf_scripts_path + "/dwarf_convert.py "
                + file_name + "_ldd.txt " + file_name + "_lines.txt "
                + file_name + "_lines.dat && " + dwarf_scripts_path + "/dat_convert.py "
                + file_name + "_lines.dat)";
            int status = system(cmd.c_str());
            if ( status != 0 ) {
                std::cerr << "Error in creating the files used to generate "
                    "the debug information. This might be caused because either"
                    " `llvm-dwarfdump` or `Python` are not available. "
                    "Please activate the CONDA environment and compile again.\n";
                return status;
            }
        }
#endif
    } else if (backend == Backend::c) {
        std::string CXX = "gcc";
        std::string cmd = CXX + " -o " + outfile + " ";
        std::string base_path = "\"" + runtime_library_dir + "\"";
        std::string runtime_lib = "lfortran_runtime";
        for (auto &s : infiles) {
            cmd += s + " ";
        }
        if(!extra_library_flags.empty()) {
            cmd += extra_library_flags + " ";
        }
        cmd += " -L" + base_path
            + " -Wl,-rpath," + base_path;
        if (!extra_linker_flags.empty()) {
            cmd += extra_linker_flags;
        }
        cmd += " -l" + runtime_lib + " -lm";
        if (verbose) {
            std::cout << cmd << std::endl;
        }
        int err = system(cmd.c_str());
        if (err) {
            std::cout << "The command '" + cmd + "' failed." << std::endl;
            return 10;
        }
    } else if (backend == Backend::cpp) {
        std::string CXX = "g++";
        std::string options, post_options;
        if (static_executable) {
            options += " -static ";
        }
        if (shared_executable) {
            options += " -shared ";
        }
        if (compiler_options.openmp) {
            options += " -fopenmp ";
        }
        if (kokkos) {
            std::string kokkos_libdir = LCompilers::LFortran::get_kokkos_libdir();
            post_options += "-L" + kokkos_libdir + " -lkokkoscontainers "
                + "-lkokkoscore -ldl" + " -Wl,-rpath," + kokkos_libdir;
        }
        std::string cmd = CXX + options + " -o " + outfile + " ";
        for (auto &s : infiles) {
            cmd += s + " ";
        }
        if(!extra_library_flags.empty()) {
            cmd += extra_library_flags + " ";
        }
        cmd += " " + post_options + " -lm";
        if (verbose) {
            std::cout << cmd << std::endl;
        }
        int err = system(cmd.c_str());
        if (err) {
            std::cout << "The command '" + cmd + "' failed." << std::endl;
            return 10;
        }
    } else if (backend == Backend::x86) {
        std::string cmd = "cp " + infiles[0] + " " + outfile;
        int err = system(cmd.c_str());
        if (err) {
            std::cout << "The command '" + cmd + "' failed." << std::endl;
            return 10;
        }
    } else if (backend == Backend::wasm) {
        // do nothing
    } else if (backend == Backend::fortran) {
        std::string cmd = "gfortran -o " + outfile + " ";
        std::string base_path = "\"" + runtime_library_dir + "\"";
        std::string runtime_lib = "lfortran_runtime";
        for (auto &s : infiles) {
            cmd += s + " ";
        }
        if(!extra_library_flags.empty()) {
            cmd += extra_library_flags + " ";
        }
        cmd += " -L" + base_path
            + " -Wl,-rpath," + base_path;
        cmd += " -l" + runtime_lib + " -lm";
        if (verbose) {
            std::cout << cmd << std::endl;
        }
        int err = system(cmd.c_str());
        if (err) {
            std::cout << "The command '" + cmd + "' failed." << std::endl;
            return 10;
        }
    } else {
        LCOMPILERS_ASSERT(false);
        return 1;
    }

    if ( compiler_options.arg_o != "" ) {
        return 0;
    }

    std::string run_cmd = "";
    if (backend == Backend::wasm) {
        // for node version less than 16, we need to also provide flag --experimental-wasm-bigint
        run_cmd = "node --experimental-wasi-unstable-preview1 " + outfile + ".js";
    } else if (t == "x86_64-pc-windows-msvc") {
        run_cmd = outfile;
    } else if (LCompilers::startswith(t, "wasm")) {
        if (LCompilers::endswith(t, "wasi")) {
            run_cmd = "wasmtime " + outfile + " --dir=.";
        } else if (LCompilers::endswith(t, "emscripten")) {
            run_cmd = "node " + outfile +
                (compiler_options.wasm_html ? ".js" : "");
        }
    } else {
        run_cmd = "./" + outfile;
    }
    int err = system(run_cmd.c_str());
    if (err != 0) {
        if (0 < err && err < 256) {
            return err;
        } else {
            return LCompilers::LFortran::get_exit_status(err);
        }
    }

    auto t2 = std::chrono::high_resolution_clock::now();
    int time_total = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count();
    if (time_report) {
        std::string message = "Linking time:  " + std::to_string(time_total / 1000) + "." + std::to_string(time_total % 1000) + " ms";
        compiler_options.po.vector_of_time_report.push_back(message);
    }
    return 0;
}

int emit_c_preprocessor(const std::string &infile, CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);

    LCompilers::LFortran::CPreprocessor cpp(compiler_options);
    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::Result<std::string> res = cpp.run(input, lm, cpp.macro_definitions, diagnostics);
    std::string s;
    if (res.ok) {
        s = res.result;
    } else {
        s = diagnostics.render(lm, compiler_options);
    }
    if(!compiler_options.arg_o.empty()) {
        std::ofstream fout(compiler_options.arg_o);
        fout << s;
    } else {
        std::cout << s;
    }
    return 0;
}

} // anonymous namespace

#ifdef HAVE_BUILD_TO_WASM

namespace wasm {

#define INITIALIZE_VARS CompilerOptions compiler_options; \
                        compiler_options.use_colors = true; \
                        compiler_options.indent = true; \
                        compiler_options.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir(); \
                        LCompilers::FortranEvaluator fe(compiler_options); \
                        LCompilers::LocationManager lm; \
                        LCompilers::diag::Diagnostics diagnostics; \
                        { \
                            LCompilers::LocationManager::FileLocations fl; \
                            fl.in_filename = "input"; \
                            std::ofstream out("input"); \
                            out << input; \
                            lm.files.push_back(fl); \
                            lm.file_ends.push_back(strlen(input)); \
                        }



std::string out;

extern "C" { // using extern "C" to prevent function name mangling

EMSCRIPTEN_KEEPALIVE char* emit_ast_from_source(char *input) {
    INITIALIZE_VARS;
    LCompilers::Result<std::string> r = fe.get_ast(input, lm, diagnostics);
    out = diagnostics.render(lm, compiler_options);
    if (r.ok) { out += r.result; }
    return &out[0];
}

EMSCRIPTEN_KEEPALIVE char* emit_ast_json_from_source(char *input) {
    INITIALIZE_VARS;
    compiler_options.po.json = true;
    LCompilers::FortranEvaluator fe2(compiler_options);
    LCompilers::Result<std::string> r = fe2.get_ast(input, lm, diagnostics);
    out = diagnostics.render(lm, compiler_options);
    if (r.ok) { out += r.result; }
    return &out[0];
}

EMSCRIPTEN_KEEPALIVE char* emit_asr_from_source(char *input) {
    INITIALIZE_VARS;
    LCompilers::Result<std::string> r = fe.get_asr(input, lm, diagnostics);
    out = diagnostics.render(lm, compiler_options);
    if (r.ok) { out += r.result; }
    return &out[0];
}

EMSCRIPTEN_KEEPALIVE char* emit_asr_json_from_source(char *input) {
    INITIALIZE_VARS;
    compiler_options.po.json = true;
    LCompilers::FortranEvaluator fe2(compiler_options);
    LCompilers::Result<std::string> r = fe2.get_asr(input, lm, diagnostics);
    out = diagnostics.render(lm, compiler_options);
    if (r.ok) { out += r.result; }
    return &out[0];
}

EMSCRIPTEN_KEEPALIVE char* emit_wat_from_source(char *input) {
    INITIALIZE_VARS;
    LCompilers::Result<std::string> r = fe.get_wat(input, lm, diagnostics);
    out = diagnostics.render(lm, compiler_options);
    if (r.ok) { out += r.result; }
    return &out[0];
}

EMSCRIPTEN_KEEPALIVE char* emit_cpp_from_source(char *input) {
    INITIALIZE_VARS;
    LCompilers::Result<std::string> r = fe.get_cpp(input, lm, diagnostics, 1);
    out = diagnostics.render(lm, compiler_options);
    if (r.ok) { out += r.result; }
    return &out[0];
}

EMSCRIPTEN_KEEPALIVE char* emit_c_from_source(char *input) {
    INITIALIZE_VARS;
    LCompilers::Result<std::string> r = fe.get_c(input, lm, diagnostics, 1);
    out = diagnostics.render(lm, compiler_options);
    if (r.ok) { out += r.result; }
    return &out[0];
}

// EMSCRIPTEN_KEEPALIVE char* emit_py_from_source(char *input) {
//     INITIALIZE_VARS;
//     LFortran::Result<std::string> r = fe.get_py(input, lm, diagnostics);
//     out = diagnostics.render(lm, compiler_options);
//     if (r.ok) { out += r.result; }
//     return &out[0];
// }

EMSCRIPTEN_KEEPALIVE char* emit_wasm_from_source(char *input) {
    INITIALIZE_VARS;
    LCompilers::Result<LCompilers::Vec<uint8_t>> r = fe.get_wasm(input, lm, diagnostics);
    if(r.ok){
        out = "0"; // exit code
        for (size_t i = 0; i < r.result.size(); i++) {
            out += "," + std::to_string(r.result[i]);
        }
    }
    else{
        out = "1"; // non-zero exit code
        out += "," + diagnostics.render(lm, compiler_options);
    }
    return &out[0];
}

}

} // namespace wasm

#endif

int main_app(int argc, char *argv[]) {
    int dirname_length;
    auto start_time = std::chrono::high_resolution_clock::now();
    LCompilers::LFortran::get_executable_path(LCompilers::binary_executable_path, dirname_length);
    LCompilers::LFortran::set_exec_path_and_mode(LCompilers::binary_executable_path, dirname_length);

    // TODO: This is now in compiler options and can be removed
    std::string runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();

    std::string rtlib_header_dir = LCompilers::LFortran::get_runtime_library_header_dir();
    Backend backend;
    std::string rtlib_c_header_dir = LCompilers::LFortran::get_runtime_library_c_header_dir();

    LCompilers::PassManager lfortran_pass_manager;

    lcli::LFortranCommandLineParser parser(argc, argv);
    try {
        parser.parse();
    } catch (const CLI::ParseError &e) {
        return parser.app.exit(e);
    } catch (const LCompilers::LCompilersException &e) {
        std::cerr << e.what() << std::endl;
        return 1;
    }

    CLI::App &fmt = *parser.fmt;
    CLI::App &kernel = *parser.kernel;
    CLI::App &mod = *parser.mod;
    CLI::App &pywrap = *parser.pywrap;
#ifdef WITH_LSP
    lsi::LanguageServerInterface &languageServerInterface = parser.languageServerInterface;
    CLI::App &server = *parser.server;
#endif // WITH_LSP

    lcli::LFortranCommandLineOpts &opts = parser.opts;
    CompilerOptions &compiler_options = opts.compiler_options;

    lcompilers_commandline_options = "";
    for (int i=0; i<argc; i++) {
        std::string option = std::string(argv[i]);
        if (option != "lfortran" && (option.size() < 4 || option.substr(option.size() - 4) != ".f90")) {
            lcompilers_commandline_options += option + " ";
        }
    }

    lcompilers_unique_ID = parser.opts.compiler_options.generate_object_code ? get_unique_ID() : "";

    if (opts.arg_version) {
        std::string version = LFORTRAN_VERSION;
        std::cout << "LFortran version: " << version << std::endl;
        std::cout << "Platform: " << pf2s(compiler_options.platform) << std::endl;
#ifdef HAVE_LFORTRAN_LLVM
        std::cout << "LLVM: " << LCompilers::LLVMEvaluator::llvm_version() << std::endl;
        std::cout << "Default target: " << LCompilers::LLVMEvaluator::get_default_target_triple() << std::endl;
#endif
        return 0;
    }
    compiler_options.po.time_report = compiler_options.time_report;

    if (opts.print_targets) {
#ifdef HAVE_LFORTRAN_LLVM
        LCompilers::LLVMEvaluator::print_targets();
        return 0;
#else
        std::cerr << "The --print-targets option requires the LLVM backend to be enabled. Recompile with `WITH_LLVM=yes`." << std::endl;
        return 1;
#endif
    }

    if(opts.static_link && opts.shared_link) {
        std::cerr << "Options '--static' and '--shared' cannot be used together" << std::endl;
        return 1;
    }

    if (fmt) {
        if (CLI::NonexistentPath(opts.arg_fmt_file).empty()) {
            std:: cerr << "error: no such file or directory: " << "'" << opts.arg_fmt_file << "'" << std::endl;
            return 1;
        }

        return format(opts.arg_fmt_file, opts.arg_fmt_inplace, !opts.arg_fmt_no_color,
            opts.arg_fmt_indent, opts.arg_fmt_indent_unit, compiler_options);
    }

    if (kernel) {
#ifdef HAVE_LFORTRAN_XEUS
        return LCompilers::LFortran::run_kernel(opts.arg_kernel_f);
#else
        std::cerr << "The kernel subcommand requires LFortran to be compiled with XEUS support. Recompile with `WITH_XEUS=yes`." << std::endl;
        return 1;
#endif
    }

    if (mod) {
        if (opts.arg_mod_show_asr) {
            Allocator al(1024*1024);
            LCompilers::ASR::TranslationUnit_t *asr;
            asr = LCompilers::LFortran::mod_to_asr(al, opts.arg_mod_file);
            std::cout << LCompilers::pickle(*asr, !opts.arg_mod_no_color) << std::endl;
            return 0;
        }
        return 0;
    }

    if (pywrap) {
        return python_wrapper(opts.arg_pywrap_file, opts.arg_pywrap_array_order,
            compiler_options);
    }

    if (opts.arg_backend == "llvm") {
        backend = Backend::llvm;
        lfortran_pass_manager.passes_to_skip_with_llvm.push_back("print_arr");
        lfortran_pass_manager.passes_to_skip_with_llvm.push_back("print_struct_type");
    } else if (opts.arg_backend == "c") {
        backend = Backend::c;
    } else if (opts.arg_backend == "cpp") {
        backend = Backend::cpp;
    } else if (opts.arg_backend == "x86") {
        backend = Backend::x86;
    } else if (opts.arg_backend == "wasm") {
        backend = Backend::wasm;
    } else if (opts.arg_backend == "fortran") {
        backend = Backend::fortran;
    } else if (opts.arg_backend == "mlir") {
        backend = Backend::mlir;
    } else {
        std::cerr << "The backend must be one of: llvm, cpp, x86, wasm, fortran, mlir." << std::endl;
        return 1;
    }

#ifdef WITH_LSP
    if (server) {
        try {
            languageServerInterface.serve();
        } catch (const LCompilers::LCompilersException &e) {
            std::cerr << e.what() << std::endl;
            return 1;
        } catch (const std::exception &e) {
            std::cerr << "Caught unhandled exception: " << e.what() << std::endl;
            return 1;
        }
        return 0;
    }
#endif

    if (opts.arg_files.size() == 0) {
#ifdef HAVE_LFORTRAN_LLVM
        return prompt(opts.arg_v, compiler_options);
#else
        std::cerr << "Interactive prompt requires the LLVM backend to be enabled. Recompile with `WITH_LLVM=yes`." << std::endl;
        return 1;
#endif
    }

    if(compiler_options.po.enable_gpu_offloading && !compiler_options.openmp) {
        std::cerr << "The option `--mlir-gpu-offloading` requires openmp pass "
            "to be applied. Rerun with `--openmp` option\n";
        return 1;
    }

    if (CLI::NonexistentPath(opts.arg_file).empty()) {
        throw LCompilers::LCompilersException(
            "error: no such file or directory: '" + opts.arg_file + "'"
        );
    }

    std::string outfile;
    std::filesystem::path basename = std::filesystem::path(opts.arg_file).filename();
    if (compiler_options.arg_o.size() > 0) {
        outfile = compiler_options.arg_o;
    } else if (opts.arg_S) {
        outfile = basename.replace_extension(".s").string();
    } else if (opts.arg_c) {
        outfile = basename.replace_extension(".o").string();
    } else if (opts.show_prescan) {
        outfile = basename.replace_extension(".prescan").string();
    } else if (opts.show_tokens) {
        outfile = basename.replace_extension(".tokens").string();
    } else if (opts.show_ast) {
        outfile = basename.replace_extension(".ast").string();
    } else if (opts.show_asr) {
        outfile = basename.replace_extension(".asr").string();
    } else if (opts.show_llvm) {
        outfile = basename.replace_extension(".ll").string();
    } else if (opts.show_wat) {
        outfile = basename.replace_extension(".wat").string();
    } else if (opts.show_julia) {
        outfile = basename.replace_extension(".jl").string();
    } else {
        outfile = basename.replace_extension(".out").string();
    }

    lfortran_pass_manager.parse_pass_arg(opts.arg_pass, opts.skip_pass);
    if (compiler_options.po.dump_fortran || compiler_options.po.dump_all_passes) {
        dump_all_passes(opts.arg_file, compiler_options, lfortran_pass_manager);
    }

    if (opts.arg_E) {
        return emit_c_preprocessor(opts.arg_file, compiler_options);
    }

    if (opts.show_prescan) {
        return emit_prescan(opts.arg_file, compiler_options);
    }
    if (opts.show_tokens) {
        return emit_tokens(opts.arg_file, false, compiler_options);
    }
    if (opts.show_ast) {
        return emit_ast(opts.arg_file, compiler_options);
    }
    if (opts.show_ast_f90) {
        return emit_ast_f90(opts.arg_file, compiler_options);
    }
    lfortran_pass_manager.parse_pass_arg(opts.arg_pass, opts.skip_pass);
    if (compiler_options.rename_symbol) {
        return LCompilers::get_all_occurences(opts.arg_file, compiler_options);
    }
    if (compiler_options.lookup_name) {
        return get_definitions(opts.arg_file, compiler_options);
    }
    if ( compiler_options.semantics_only ) {
        return run_parser_and_semantics(opts.arg_file, compiler_options);
    }
    if (opts.show_asr) {
        return emit_asr(opts.arg_file, lfortran_pass_manager,
                compiler_options);
    }
    if (opts.show_document_symbols) {
        return get_symbols(opts.arg_file, compiler_options);
    }

    if (opts.show_errors) {
        return get_errors(opts.arg_file, compiler_options);
    }
    lfortran_pass_manager.use_default_passes();
    if (opts.show_llvm) {
#ifdef HAVE_LFORTRAN_LLVM
        return emit_llvm(opts.arg_file, lfortran_pass_manager,
                            compiler_options);
#else
        std::cerr << "The --show-llvm option requires the LLVM backend to be enabled. Recompile with `WITH_LLVM=yes`." << std::endl;
        return 1;
#endif
    }
    if (opts.show_mlir || opts.show_llvm_from_mlir) {
#ifdef HAVE_LFORTRAN_MLIR
        return handle_mlir(opts.arg_file, outfile, compiler_options,
            opts.show_mlir, opts.show_llvm_from_mlir);
#else
        std::cerr << "The `--show-mlir` option requires the MLIR backend to be "
            "enabled. Recompile with `WITH_MLIR=yes`." << std::endl;
        return 1;
#endif
    }
    if (opts.show_asm) {
#ifdef HAVE_LFORTRAN_LLVM
        return emit_asm(opts.arg_file, compiler_options);
#else
        std::cerr << "The --show-asm option requires the LLVM backend to be enabled. Recompile with `WITH_LLVM=yes`." << std::endl;
        return 1;
#endif
    }
    if (opts.show_wat) {
        return emit_wat(opts.arg_file, compiler_options);
    }
    if (opts.show_cpp) {
        return emit_cpp(opts.arg_file, compiler_options);
    }
    if (opts.show_c) {
        return emit_c(opts.arg_file, lfortran_pass_manager, compiler_options);
    }
    if (opts.show_julia) {
        return emit_julia(opts.arg_file, compiler_options);
    }
    if (opts.show_fortran) {
        return emit_fortran(opts.arg_file, compiler_options);
    }
    if (opts.arg_S) {
        if (backend == Backend::llvm) {
#ifdef HAVE_LFORTRAN_LLVM
            return compile_to_assembly_file(opts.arg_file, outfile, compiler_options.time_report, compiler_options, lfortran_pass_manager);
#else
            std::cerr << "The -S option requires the LLVM backend to be enabled. Recompile with `WITH_LLVM=yes`." << std::endl;
            return 1;
#endif
        } else if (backend == Backend::cpp) {
            std::cerr << "The C++ backend does not work with the -S option yet." << std::endl;
            return 1;
        } else {
            LCOMPILERS_ASSERT(false);
        }
    }
    if (opts.arg_c) {
        if (backend == Backend::llvm) {
#ifdef HAVE_LFORTRAN_LLVM
            return compile_src_to_object_file(opts.arg_file, outfile, compiler_options.time_report, false,
                compiler_options, lfortran_pass_manager);
#else
            std::cerr << "The -c option requires the LLVM backend to be enabled. Recompile with `WITH_LLVM=yes`." << std::endl;
            return 1;
#endif
        } else if (backend == Backend::c) {
            return compile_to_object_file_c(opts.arg_file, outfile, opts.arg_v, false,
                    rtlib_c_header_dir, lfortran_pass_manager, compiler_options);
        } else if (backend == Backend::cpp) {
            return compile_to_object_file_cpp(opts.arg_file, outfile, opts.arg_v, false,
                    true, rtlib_c_header_dir, compiler_options);
        } else if (backend == Backend::x86) {
            return compile_to_binary_x86(opts.arg_file, outfile, compiler_options.time_report, compiler_options);
        } else if (backend == Backend::wasm) {
            return compile_to_binary_wasm(opts.arg_file, outfile, compiler_options.time_report, compiler_options);
        } else if (backend == Backend::fortran) {
            return compile_to_binary_fortran(opts.arg_file, outfile, compiler_options);
        } else if (backend == Backend::mlir) {
#ifdef HAVE_LFORTRAN_MLIR
            return handle_mlir(opts.arg_file, outfile, compiler_options, false, false);
#else
            std::cerr << "The -c option with `--backend=mlir` requires the "
                "MLIR backend to be enabled. Recompile with `WITH_MLIR=yes`."
                << std::endl;
            return 1;
#endif
        } else {
            throw LCompilers::LCompilersException("Unsupported backend.");
        }
    }

    int err_ = 0;
    std::vector<std::string> object_files;
    for (const auto &arg_file : opts.arg_files) {
        int err = 0;
        std::string tmp_o = std::filesystem::path(arg_file).replace_extension(".tmp.o").string();
        if (endswith(arg_file, ".f90") || endswith(arg_file, ".f") ||
            endswith(arg_file, ".F90") || endswith(arg_file, ".F")) {
            if (backend == Backend::x86) {
                return compile_to_binary_x86(arg_file, outfile,
                        compiler_options.time_report, compiler_options);
            }
            if (backend == Backend::llvm) {
#ifdef HAVE_LFORTRAN_LLVM
                err = compile_src_to_object_file(arg_file, tmp_o, compiler_options.time_report, false,
                    compiler_options, lfortran_pass_manager);
#else
                std::cerr << "Compiling Fortran files to object files requires the LLVM backend to be enabled. Recompile with `WITH_LLVM=yes`." << std::endl;
                return 1;
#endif
            } else if (backend == Backend::cpp) {
                err = compile_to_object_file_cpp(arg_file, tmp_o, opts.arg_v, false,
                        true, rtlib_header_dir, compiler_options);
            } else if (backend == Backend::c) {
                err = compile_to_object_file_c(arg_file, tmp_o, opts.arg_v,
                        false, rtlib_c_header_dir, lfortran_pass_manager, compiler_options);
            } else if (backend == Backend::fortran) {
                err = compile_to_binary_fortran(arg_file, tmp_o, compiler_options);
            } else if (backend == Backend::wasm) {
                err = compile_to_binary_wasm(arg_file, outfile,
                        compiler_options.time_report, compiler_options);
            } else if (backend == Backend::mlir) {
#ifdef HAVE_LFORTRAN_MLIR
                err = handle_mlir(arg_file, tmp_o, compiler_options, false, false);
#else
                std::cerr << "Compiling Fortran files to object files using "
                    "`--backend=mlir` requires the MLIR backend to be enabled. "
                    "Recompile with `WITH_MLIR=yes`." << std::endl;
                return 1;
#endif
            } else {
                throw LCompilers::LCompilersException("Backend not supported");
            }
        } else if (endswith(arg_file, ".ll")) {
            // this way we can execute LLVM IR files directly
#ifdef HAVE_LFORTRAN_LLVM
            err = compile_llvm_to_object_file(arg_file, tmp_o, compiler_options);
            if (err) return err;
#else
            std::cerr << "Compiling LLVM IR to object files requires the LLVM backend to be enabled. Recompile with `WITH_LLVM=yes`." << std::endl;
            return 1;
#endif
        } else {
            // assume it's an object file
            tmp_o = arg_file;
        }
        if (err && !compiler_options.continue_compilation) return err;
        err_ = err;
        if (!err) object_files.push_back(tmp_o);
    }
    if (object_files.size() == 0) {
        return err_;
    } else {
        int status_code = err_ + link_executable(object_files, outfile, compiler_options.time_report, runtime_library_dir,
                backend, opts.static_link, opts.shared_link, opts.linker, opts.linker_path, true,
                opts.arg_v, opts.arg_L, opts.arg_l, opts.linker_flags, compiler_options);
        auto end_time = std::chrono::high_resolution_clock::now();
        if (compiler_options.time_report) {
            int total_time = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time).count();
            std::string message = "Total time: " + std::to_string(total_time / 1000) + "." + std::to_string(total_time % 1000) + " ms";
            compiler_options.po.vector_of_time_report.push_back(message);

            print_time_report(compiler_options.po.vector_of_time_report);
        }

        return status_code;
    }
}

int main(int argc, char *argv[])
{
    LCompilers::initialize();
#if defined(HAVE_LFORTRAN_STACKTRACE)
    LCompilers::print_stack_on_segfault();
#endif
    try {
        return main_app(argc, argv);
    } catch(const LCompilers::LCompilersException &e) {
        std::cerr << "Internal Compiler Error: Unhandled exception" << std::endl;
        std::vector<LCompilers::StacktraceItem> d = e.stacktrace_addresses();
        get_local_addresses(d);
        get_local_info(d);
        std::cerr << stacktrace2str(d, LCompilers::stacktrace_depth);
        std::cerr << e.name() + ": " << e.msg() << std::endl;
        return 1;
    } catch(const std::runtime_error &e) {
        std::cerr << "runtime_error: " << e.what() << std::endl;
        return 1;
    } catch(const std::exception &e) {
        std::cerr << "std::exception: " << e.what() << std::endl;
        return 1;
    } catch(...) {
        std::cerr << "Unknown Exception" << std::endl;
        return 1;
    }
    return 0;
}
