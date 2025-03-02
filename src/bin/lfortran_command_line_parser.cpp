#include <libasr/exception.h>
#include <libasr/string_utils.h>

#include <lfortran/utils.h>

#ifndef CLI11_HAS_FILESYSTEM
#define CLI11_HAS_FILESYSTEM 0
#endif // CLI11_HAS_FILESYSTEM
#include <bin/CLI11.hpp>

#include <bin/lfortran_command_line_parser.h>

namespace LCompilers::CommandLineInterface {
    namespace lc = LCompilers;

    LFortranCommandLineParser::LFortranCommandLineParser(std::vector<std::string> &args)
        : args(args)
    {
        // empty
    }

    LFortranCommandLineParser::LFortranCommandLineParser(int argc, const char *const *argv)
        : argc(argc)
        , argv(argv)
    {
        // empty
    }

    auto LFortranCommandLineParser::parse() -> void {
        CompilerOptions &compiler_options = opts.compiler_options;
        compiler_options.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();

        // Standard options compatible with gfortran, gcc or clang
        // We follow the established conventions
        app.add_option("files", opts.arg_files, "Source files");
        app.add_flag("-S", opts.arg_S, "Emit assembly, do not assemble or link");
        app.add_flag("-c", opts.arg_c, "Compile and assemble, do not link");
        app.add_option("-o", compiler_options.arg_o, "Specify the file to place the compiler's output into");
        app.add_flag("-v", opts.arg_v, "Be more verbose");
        app.add_flag("-E", opts.arg_E, "Preprocess only; do not compile, assemble or link");
        app.add_option("-l", opts.arg_l, "Link library option")->allow_extra_args(false);
        app.add_option("-L", opts.arg_L, "Library path option")->allow_extra_args(false);
        app.add_option("-I", compiler_options.po.include_dirs, "Include path")->allow_extra_args(false);
        app.add_option("-J", compiler_options.po.mod_files_dir, "Where to save mod files");
        app.add_flag("-g", compiler_options.emit_debug_info, "Compile with debugging information");
        app.add_flag("--debug-with-line-column", compiler_options.emit_debug_line_column,
            "Convert the linear location info into line + column in the debugging information");
        app.add_option("-D", compiler_options.c_preprocessor_defines, "Define <macro>=<value> (or 1 if <value> omitted)")->allow_extra_args(false);
        app.add_flag("--version", opts.arg_version, "Display compiler version information");
        app.add_option("-W", opts.linker_flags, "Linker flags")->allow_extra_args(false);
        app.add_option("-f", opts.f_flags, "All `-f*` flags (only -fPIC & -fdefault-integer-8 supported for now)")->allow_extra_args(false);
        app.add_option("-O", opts.O_flags, "Optimization level (ignored for now)")->allow_extra_args(false);

        // LFortran specific options
        app.add_flag("--cpp", opts.cpp, "Enable C preprocessing");
        app.add_flag("--cpp-infer", opts.cpp_infer, "Use heuristics to infer if a file needs preprocessing");
        app.add_flag("--no-cpp", opts.no_cpp, "Disable C preprocessing");
        app.add_flag("--fixed-form", compiler_options.fixed_form, "Use fixed form Fortran source parsing");
        app.add_flag("--fixed-form-infer", opts.fixed_form_infer, "Use heuristics to infer if a file is in fixed form");
        app.add_option("--std", opts.arg_standard, "Select standard conformance (lf, f23, legacy)");
        app.add_flag("--no-prescan", opts.arg_no_prescan, "Turn off prescanning");
        app.add_flag("--show-prescan", opts.show_prescan, "Show the source code after prescanning and exit");
        app.add_flag("--show-tokens", opts.show_tokens, "Show tokens for the given file and exit");
        app.add_flag("--show-ast", opts.show_ast, "Show AST for the given file and exit");
        app.add_flag("--show-asr", opts.show_asr, "Show ASR for the given file and exit");
        app.add_flag("--with-intrinsic-mods", compiler_options.po.with_intrinsic_mods, "Show intrinsic modules in ASR");
        app.add_flag("--show-ast-f90", opts.show_ast_f90, "Show Fortran from AST for the given file and exit");
        app.add_flag("--no-color", opts.arg_no_color, "Turn off colored AST/ASR");
        app.add_flag("--no-indent", opts.arg_no_indent, "Turn off Indented print ASR/AST");
        app.add_flag("--tree", compiler_options.po.tree, "Tree structure print ASR/AST");
        app.add_flag("--json", compiler_options.po.json, "Print ASR/AST Json format");
        app.add_flag("--no-loc", compiler_options.po.no_loc, "Skip location information in ASR/AST Json format");
        app.add_flag("--visualize", compiler_options.po.visualize, "Print ASR/AST Visualization");
        app.add_option("--pass", opts.arg_pass, "Apply the ASR pass and show ASR (implies --show-asr)");
        app.add_option("--skip-pass", opts.skip_pass, "Skip an ASR pass in default pipeline");
        app.add_flag("--show-llvm", opts.show_llvm, "Show LLVM IR for the given file and exit");
        app.add_flag("--show-mlir", opts.show_mlir, "Show MLIR for the given file and exit");
        app.add_flag("--show-llvm-from-mlir", opts.show_llvm_from_mlir, "Show LLVM IR translated from MLIR for the given file and exit");
        app.add_flag("--show-cpp", opts.show_cpp, "Show C++ translation source for the given file and exit");
        app.add_flag("--show-c", opts.show_c, "Show C translation source for the given file and exit");
        app.add_flag("--show-asm", opts.show_asm, "Show assembly for the given file and exit");
        app.add_flag("--show-wat", opts.show_wat, "Show WAT (WebAssembly Text Format) and exit");
        app.add_flag("--show-julia", opts.show_julia, "Show Julia translation source for the given file and exit");
        app.add_flag("--show-fortran", opts.show_fortran, "Show Fortran translation source for the given file and exit");
        app.add_flag("--show-stacktrace", compiler_options.show_stacktrace, "Show internal stacktrace on compiler errors");
        app.add_flag("--symtab-only", compiler_options.symtab_only, "Only create symbol tables in ASR (skip executable stmt)");
        app.add_flag("--time-report", opts.time_report, "Show compilation time report");
        app.add_flag("--static", opts.static_link, "Create a static executable");
        app.add_flag("--shared", opts.shared_link, "Create a shared executable");
        app.add_flag("--logical-casting", compiler_options.logical_casting, "Allow logical casting");
        app.add_flag("--no-warnings", compiler_options.no_warnings, "Turn off all warnings");
        app.add_flag("--no-style-warnings", compiler_options.disable_style, "Turn off style suggestions");
        app.add_flag("--no-error-banner", compiler_options.no_error_banner, "Turn off error banner");
        app.add_option("--error-format", compiler_options.error_format, "Control how errors are produced (human, short)")->capture_default_str();
        app.add_option("--backend", opts.arg_backend, "Select a backend (llvm, c, cpp, x86, wasm, fortran, mlir)")->capture_default_str();
        app.add_flag("--openmp", compiler_options.openmp, "Enable openmp");
        app.add_flag("--openmp-lib-dir", compiler_options.openmp_lib_dir, "Pass path to openmp library")->capture_default_str();
        app.add_flag("--lookup-name", compiler_options.lookup_name, "Lookup a name specified by --line & --column in the ASR");
        app.add_flag("--rename-symbol", compiler_options.rename_symbol, "Returns list of locations where symbol specified by --line & --column appears in the ASR");
        app.add_option("--line", compiler_options.line, "Line number for --lookup-name")->capture_default_str();
        app.add_option("--column", compiler_options.column, "Column number for --lookup-name")->capture_default_str();
        app.add_flag("--continue-compilation", compiler_options.continue_compilation, "collect error message and continue compilation");
        app.add_flag("--semantics-only", compiler_options.semantics_only, "do parsing and semantics, and report all the errors");
        app.add_flag("--generate-object-code", compiler_options.generate_object_code, "Generate object code into .o files");
        app.add_flag("--rtlib", compiler_options.rtlib, "Include the full runtime library in the LLVM output");
        app.add_flag("--use-loop-variable-after-loop", compiler_options.po.use_loop_variable_after_loop, "Allow using loop variable after the loop");
        app.add_flag("--fast", compiler_options.po.fast, "Best performance (disable strict standard compliance)");
        app.add_flag("--linker", opts.linker, "Specify the linker to be used, available options: clang or gcc")->capture_default_str();
        app.add_flag("--linker-path", opts.linker_path, "Use the linker from this path")->capture_default_str();
        app.add_option("--target", compiler_options.target, "Generate code for the given target")->capture_default_str();
        app.add_flag("--print-targets", opts.print_targets, "Print the registered targets");
        app.add_flag("--implicit-typing", compiler_options.implicit_typing, "Allow implicit typing");
        app.add_flag("--implicit-interface", compiler_options.implicit_interface, "Allow implicit interface");
        app.add_flag("--implicit-argument-casting", compiler_options.implicit_argument_casting, "Allow implicit argument casting");
        app.add_flag("--print-leading-space", compiler_options.print_leading_space, "Print leading white space if format is unspecified");
        app.add_flag("--interactive-parse", compiler_options.interactive, "Use interactive parse");
        app.add_flag("--verbose", compiler_options.po.verbose, "Print debugging statements");
        app.add_flag("--dump-all-passes", compiler_options.po.dump_all_passes, "Apply all the passes and dump the ASR into a file");
        app.add_flag("--dump-all-passes-fortran", compiler_options.po.dump_fortran, "Apply all passes and dump the ASR after each pass into fortran file");
        app.add_flag("--cumulative", compiler_options.po.pass_cumulative, "Apply all the passes cumulatively till the given pass");
        app.add_flag("--realloc-lhs", compiler_options.po.realloc_lhs, "Reallocate left hand side automatically");
        app.add_flag("--module-mangling", compiler_options.po.module_name_mangling, "Mangles the module name");
        app.add_flag("--global-mangling", compiler_options.po.global_symbols_mangling, "Mangles all the global symbols");
        app.add_flag("--intrinsic-mangling", compiler_options.po.intrinsic_symbols_mangling, "Mangles all the intrinsic symbols");
        app.add_flag("--all-mangling", compiler_options.po.all_symbols_mangling, "Mangles all possible symbols");
        app.add_flag("--bindc-mangling", compiler_options.po.bindc_mangling, "Mangles functions with abi bind(c)");
        app.add_flag("--apply-fortran-mangling", compiler_options.po.fortran_mangling, "Mangle symbols with Fortran supported syntax");
        app.add_flag("--mangle-underscore", compiler_options.po.mangle_underscore, "Mangles with underscore");
        app.add_flag("--legacy-array-sections", compiler_options.legacy_array_sections, "Enables passing array items as sections if required");
        app.add_flag("--ignore-pragma", compiler_options.ignore_pragma, "Ignores all the pragmas");
        app.add_flag("--stack-arrays", compiler_options.stack_arrays, "Allocate memory for arrays on stack");
        app.add_flag("--wasm-html", compiler_options.wasm_html, "Generate HTML file using emscripten for LLVM->WASM");
        app.add_option("--emcc-embed", compiler_options.emcc_embed, "Embed a given file/directory using emscripten for LLVM->WASM");
        app.add_flag("--mlir-gpu-offloading", compiler_options.po.enable_gpu_offloading, "Enables gpu offloading using MLIR backend");

        // LSP specific options
        app.add_flag("--show-errors", opts.show_errors, "Show errors when LSP is running in the background");
        app.add_flag("--show-document-symbols", opts.show_document_symbols, "Show symbols in lfortran file");

        /*
        * Subcommands:
        */

        // fmt
        fmt = app.add_subcommand("fmt", "Format Fortran source files.");
        fmt->add_option("file", opts.arg_fmt_file, "Fortran source file to format")->required();
        fmt->add_flag("-i", opts.arg_fmt_inplace, "Modify <file> in-place (instead of writing to stdout)");
        fmt->add_option("--spaces", opts.arg_fmt_indent, "Number of spaces to use for indentation")->capture_default_str();
        fmt->add_flag("--indent-unit", opts.arg_fmt_indent_unit, "Indent contents of sub / fn / prog / mod");
        fmt->add_flag("--no-color", opts.arg_fmt_no_color, "Turn off color when writing to stdout");

        // kernel
        kernel = app.add_subcommand("kernel", "Run in Jupyter kernel mode.");
        kernel->add_option("-f", opts.arg_kernel_f, "The kernel connection file")->required();

        // mod
        mod = app.add_subcommand("mod", "Fortran mod file utilities.");
        mod->add_option("file", opts.arg_mod_file, "Mod file (*.mod)")->required();
        mod->add_flag("--show-asr", opts.arg_mod_show_asr, "Show ASR for the module");
        mod->add_flag("--no-color", opts.arg_mod_no_color, "Turn off colored ASR");

        // pywrap
        pywrap = app.add_subcommand("pywrap", "Python wrapper generator");
        pywrap->add_option("file", opts.arg_pywrap_file, "Fortran source file (*.f90)")->required();
        pywrap->add_option("--array-order", opts.arg_pywrap_array_order,
                "Select array order (c, f)")->capture_default_str();

        #ifdef WITH_LSP
            // server
            server = languageServerInterface.prepare(app);
        #endif

        app.get_formatter()->column_width(25);
        app.require_subcommand(0, 1);

        if (argv != nullptr) {
            app.parse(argc, argv);
        } else {
            app.parse(args);
        }

        if (opts.arg_standard == "" || opts.arg_standard == "lf") {
            // The default LFortran behavior, do nothing
        } else if (opts.arg_standard == "f23") {
            compiler_options.disable_style = true;
            compiler_options.implicit_typing = true;
            compiler_options.implicit_argument_casting = true;
            compiler_options.implicit_interface = true;
            compiler_options.print_leading_space = true;
            compiler_options.logical_casting = false;
            compiler_options.po.realloc_lhs = true;
        } else if (opts.arg_standard == "legacy") {
            // f23
            compiler_options.disable_style = true;
            compiler_options.implicit_typing = true;
            compiler_options.implicit_argument_casting = true;
            compiler_options.implicit_interface = true;
            compiler_options.print_leading_space = true;
            compiler_options.logical_casting = false;

            // legacy options
            compiler_options.fixed_form = true;
            compiler_options.legacy_array_sections = true;
            compiler_options.use_loop_variable_after_loop = true;
            compiler_options.generate_object_code = true;
        } else {
            throw lc::LCompilersException(
                "The option `--std=" + opts.arg_standard + "` is not supported"
            );
        }

        compiler_options.use_colors = !opts.arg_no_color;
        compiler_options.indent = !opts.arg_no_indent;
        compiler_options.prescan = !opts.arg_no_prescan;
        // set openmp in pass options
        compiler_options.po.openmp = compiler_options.openmp;

        for (auto &f_flag : opts.f_flags) {
            if (f_flag == "PIC") {
                // Position Independent Code
                // We do this by default, so we ignore for now
            } else if (f_flag == "default-integer-8") {
                compiler_options.po.default_integer_kind = 8;
            } else {
                throw lc::LCompilersException(
                    "The flag `-f" + f_flag + "` is not supported"
                );
            }
        }

        // if it's the only file, then we use that file
        // to set the compiler_options
        if (opts.arg_files.size() > 0) {
            opts.arg_file = opts.arg_files[0];
            for (const auto& file : opts.arg_files) {
                // if any Fortran file is present, use the first file to
                // set compiler_options
                if (endswith(file, ".f90") || endswith(file, ".f") ||
                    endswith(file, ".F90") || endswith(file, ".F")) {
                    opts.arg_file = file;
                    break;
                }
            }
        }

        // Decide if a file is fixed format based on the extension
        // Gfortran does the same thing
        if (opts.fixed_form_infer && endswith(opts.arg_file, ".f")) {
            compiler_options.fixed_form = true;
        }

        if (opts.cpp && opts.no_cpp) {
            throw LCompilers::LCompilersException("Cannot use --cpp and --no-cpp at the same time");
        } else if(opts.cpp) {
            compiler_options.c_preprocessor = true;
        } else if(opts.no_cpp) {
            compiler_options.c_preprocessor = false;
        // Decide if a file gets preprocessing based on the extension
        // Gfortran does the same thing
        } else if (opts.cpp_infer && (endswith(opts.arg_file, ".F90") || endswith(opts.arg_file, ".F"))) {
            compiler_options.c_preprocessor = true;
        } else {
            compiler_options.c_preprocessor = false;
        }
    }

} // namespace LCompilers::CommandLineInterface
