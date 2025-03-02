#pragma once

#include <string>
#include <vector>

#include <libasr/utils.h>

#ifndef CLI11_HAS_FILESYSTEM
#define CLI11_HAS_FILESYSTEM 0
#endif // CLI11_HAS_FILESYSTEM
#include <bin/CLI11.hpp>

#ifdef WITH_LSP
#include <bin/language_server_interface.h>
#endif // WITH_LSP

namespace LCompilers::CommandLineInterface {
#ifdef WITH_LSP
    namespace lsi = LCompilers::LLanguageServer::Interface;
#endif

    struct LFortranCommandLineOpts {
        bool arg_S = false;
        bool arg_c = false;
        bool arg_v = false;
        bool arg_E = false;
        std::vector<std::string> arg_l;
        std::vector<std::string> arg_L;
        std::vector<std::string> arg_files;
        std::string arg_file;
        std::string arg_standard;
        bool arg_version = false;
        // see parser.prescan function for what 'prescanning' does
        bool show_prescan = false;
        bool show_tokens = false;
        bool show_ast = false;
        bool show_asr = false;
        bool show_ast_f90 = false;
        std::string arg_pass;
        bool arg_no_color = false;
        bool arg_no_indent = false;
        bool arg_no_prescan = false;
        bool show_llvm = false;
        bool show_mlir = false;
        bool show_llvm_from_mlir = false;
        bool show_cpp = false;
        bool show_c = false;
        bool show_asm = false;
        bool show_wat = false;
        bool show_julia = false;
        bool show_fortran = false;
        bool time_report = false;
        bool static_link = false;
        bool shared_link = false;
        std::string skip_pass;
        std::string arg_backend = "llvm";
        std::string arg_kernel_f;
        std::string linker{""};
        std::string linker_path{""};
        bool print_targets = false;
        bool fixed_form_infer = false;
        bool cpp = false;
        bool cpp_infer = false;
        bool no_cpp = false;

        // LSP specific options
        bool show_errors = false;
        bool show_document_symbols = false;

        std::string arg_fmt_file;
        int arg_fmt_indent = 4;
        bool arg_fmt_indent_unit = false;
        bool arg_fmt_inplace = false;
        bool arg_fmt_no_color = false;

        std::string arg_mod_file;
        bool arg_mod_show_asr = false;
        bool arg_mod_no_color = false;

        std::string arg_pywrap_file;
        std::string arg_pywrap_array_order="f";
        std::vector<std::string> linker_flags;
        std::vector<std::string> f_flags;
        std::vector<std::string> O_flags;

        CompilerOptions compiler_options;
    }; // struct LFortranCommandLineOpts

    class LFortranCommandLineParser {
    public:
        LFortranCommandLineParser(std::vector<std::string> &args);
        LFortranCommandLineParser(int argc, const char *const *argv);

        LFortranCommandLineOpts opts;

        CLI::App app{"LFortran: modern interactive LLVM-based Fortran compiler"};
        CLI::App *fmt = nullptr;
        CLI::App *kernel = nullptr;
        CLI::App *mod = nullptr;
        CLI::App *pywrap = nullptr;
#ifdef WITH_LSP
        lsi::LanguageServerInterface languageServerInterface;
        CLI::App *server = nullptr;
#endif // WITH_LSP

        auto parse() -> void;
    private:
        int argc;
        const char *const *argv = nullptr;
        std::vector<std::string> args;
    }; // class LFortranCommandLineParser

} // namespace LCompilers::CommandLineInterface
