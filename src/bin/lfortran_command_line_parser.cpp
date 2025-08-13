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

        std::string group_warning_options = "Warning Options";
        std::string group_language_options = "Language Options";
        std::string group_preprocessing_options = "Preprocessing Options";
        std::string group_output_debugging_options = "Output and Debugging Options";
        std::string group_pass_transformation_options = "Pass and Transformation Options";
        std::string group_backend_codegen_options = "Backend and Codegen Options";
        std::string group_symbol_lookup_options = "Symbol and Lookup Options";
        std::string group_mangling_options = "Mangling Options";
        std::string group_miscellaneous_options = "Miscellaneous Options";
        std::string group_lsp_options = "LSP Options";
        bool disable_bounds_checking = false;

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
        // Warning-related flags
        app.add_flag("--no-warnings", compiler_options.no_warnings, "Turn off all warnings")->group(group_warning_options);
        app.add_flag("--no-style-warnings", compiler_options.disable_style, "Turn off style suggestions")->group(group_warning_options);
        app.add_flag("--no-error-banner", compiler_options.no_error_banner, "Turn off error banner")->group(group_warning_options);
        app.add_option("--error-format", compiler_options.error_format, "Control how errors are produced (human, short)")->capture_default_str()->group(group_warning_options);

        // Language-related flags (affecting Fortran language behavior, typing, and interfaces)
        app.add_flag("--fixed-form", compiler_options.fixed_form, "Use fixed form Fortran source parsing")->group(group_language_options);
        app.add_flag("--fixed-form-infer", opts.fixed_form_infer, "Use heuristics to infer if a file is in fixed form")->group(group_language_options);
        app.add_option("--std", opts.arg_standard, "Select standard conformance (lf, f23, legacy)")->group(group_language_options);
        app.add_flag("--implicit-typing", compiler_options.implicit_typing, "Allow implicit typing")->group(group_language_options);
        app.add_flag("--implicit-interface", compiler_options.implicit_interface, "Allow implicit interface")->group(group_language_options);
        app.add_flag("--implicit-argument-casting", compiler_options.implicit_argument_casting, "Allow implicit argument casting")->group(group_language_options);
        app.add_flag("--logical-casting", compiler_options.logical_casting, "Allow logical casting")->group(group_language_options);
        app.add_flag("--use-loop-variable-after-loop", compiler_options.po.use_loop_variable_after_loop, "Allow using loop variable after the loop")->group(group_language_options);
        app.add_flag("--legacy-array-sections", compiler_options.legacy_array_sections, "Enables passing array items as sections if required")->group(group_language_options);

        // Preprocessing-related flags
        app.add_flag("--cpp", opts.cpp, "Enable C preprocessing")->group(group_preprocessing_options);
        app.add_flag("--cpp-infer", opts.cpp_infer, "Use heuristics to infer if a file needs preprocessing")->group(group_preprocessing_options);
        app.add_flag("--no-cpp", opts.no_cpp, "Disable C preprocessing")->group(group_preprocessing_options);
        app.add_flag("--no-prescan", opts.arg_no_prescan, "Turn off prescanning")->group(group_preprocessing_options);
        app.add_flag("--show-prescan", opts.show_prescan, "Show the source code after prescanning and exit")->group(group_preprocessing_options);

        // Output and debugging-related flags
        app.add_flag("--show-tokens", opts.show_tokens, "Show tokens for the given file and exit")->group(group_output_debugging_options);
        app.add_flag("--show-ast", opts.show_ast, "Show AST for the given file and exit")->group(group_output_debugging_options);
        app.add_flag("--show-asr", opts.show_asr, "Show ASR for the given file and exit")->group(group_output_debugging_options);
        app.add_flag("--with-intrinsic-mods", compiler_options.po.with_intrinsic_mods, "Show intrinsic modules in ASR")->group(group_output_debugging_options);
        app.add_flag("--show-ast-f90", opts.show_ast_f90, "Show Fortran from AST for the given file and exit")->group(group_output_debugging_options);
        app.add_flag("--no-color", opts.arg_no_color, "Turn off colored AST/ASR")->group(group_output_debugging_options);
        app.add_flag("--no-indent", opts.arg_no_indent, "Turn off Indented print ASR/AST")->group(group_output_debugging_options);
        app.add_flag("--tree", compiler_options.po.tree, "Tree structure print ASR/AST")->group(group_output_debugging_options);
        app.add_flag("--json", compiler_options.po.json, "Print ASR/AST Json format")->group(group_output_debugging_options);
        app.add_flag("--no-loc", compiler_options.po.no_loc, "Skip location information in ASR/AST Json format")->group(group_output_debugging_options);
        app.add_flag("--visualize", compiler_options.po.visualize, "Print ASR/AST Visualization")->group(group_output_debugging_options);
        app.add_flag("--show-llvm", opts.show_llvm, "Show LLVM IR for the given file and exit")->group(group_output_debugging_options);
        app.add_flag("--show-mlir", opts.show_mlir, "Show MLIR for the given file and exit")->group(group_output_debugging_options);
        app.add_flag("--show-llvm-from-mlir", opts.show_llvm_from_mlir, "Show LLVM IR translated from MLIR for the given file and exit")->group(group_output_debugging_options);
        app.add_flag("--show-cpp", opts.show_cpp, "Show C++ translation source for the given file and exit")->group(group_output_debugging_options);
        app.add_flag("--show-c", opts.show_c, "Show C translation source for the given file and exit")->group(group_output_debugging_options);
        app.add_flag("--show-asm", opts.show_asm, "Show assembly for the given file and exit")->group(group_output_debugging_options);
        app.add_flag("--show-wat", opts.show_wat, "Show WAT (WebAssembly Text Format) and exit")->group(group_output_debugging_options);
        app.add_flag("--show-julia", opts.show_julia, "Show Julia translation source for the given file and exit")->group(group_output_debugging_options);
        app.add_flag("--show-fortran", opts.show_fortran, "Show Fortran translation source for the given file and exit")->group(group_output_debugging_options);
        app.add_flag("--show-stacktrace", compiler_options.show_stacktrace, "Show internal stacktrace on compiler errors")->group(group_output_debugging_options);
        app.add_flag("--time-report", compiler_options.time_report, "Show compilation time report")->group(group_output_debugging_options);

        // Pass and transformation-related flags
        app.add_option("--pass", opts.arg_pass, "Apply the ASR pass and show ASR (implies --show-asr)")->group(group_pass_transformation_options);
        app.add_option("--skip-pass", opts.skip_pass, "Skip an ASR pass in default pipeline")->group(group_pass_transformation_options);
        app.add_flag("--dump-all-passes", compiler_options.po.dump_all_passes, "Apply all the passes and dump the ASR into a file")->group(group_pass_transformation_options);
        app.add_flag("--dump-all-passes-fortran", compiler_options.po.dump_fortran, "Apply all passes and dump the ASR after each pass into fortran file")->group(group_pass_transformation_options);
        app.add_flag("--cumulative", compiler_options.po.pass_cumulative, "Apply all the passes cumulatively till the given pass")->group(group_pass_transformation_options);

        // Backend and code generation-related flags
        app.add_option("--backend", opts.arg_backend, "Select a backend (llvm, c, cpp, x86, wasm, fortran, mlir)")->capture_default_str()->group(group_backend_codegen_options);
        app.add_flag("--openmp", compiler_options.openmp, "Enable openmp")->group(group_backend_codegen_options);
        app.add_flag("--target-offload", compiler_options.target_offload_enabled, "Enable Target Offloading")->group(group_backend_codegen_options);
        app.add_flag("--openmp-lib-dir", compiler_options.openmp_lib_dir, "Pass path to openmp library")->capture_default_str()->group(group_backend_codegen_options);
        app.add_flag("--rtlib", compiler_options.rtlib, "Include the full runtime library in the LLVM output")->group(group_backend_codegen_options);
        app.add_flag("--separate-compilation", compiler_options.separate_compilation, "Generate object code into .o files")->group(group_backend_codegen_options);
        app.add_flag("--static", opts.static_link, "Create a static executable")->group(group_backend_codegen_options);
        app.add_flag("--shared", opts.shared_link, "Create a shared executable")->group(group_backend_codegen_options);
        app.add_flag("--linker", opts.linker, "Specify the linker to be used, available options: clang or gcc")->capture_default_str()->group(group_backend_codegen_options);
        app.add_flag("--linker-path", opts.linker_path, "Use the linker from this path")->capture_default_str()->group(group_backend_codegen_options);
        app.add_option("--target", compiler_options.target, "Generate code for the given target")->capture_default_str()->group(group_backend_codegen_options);
        app.add_flag("--print-targets", opts.print_targets, "Print the registered targets")->group(group_backend_codegen_options);
        app.add_flag("--wasm-html", compiler_options.wasm_html, "Generate HTML file using emscripten for LLVM->WASM")->group(group_backend_codegen_options);
        app.add_option("--emcc-embed", compiler_options.emcc_embed, "Embed a given file/directory using emscripten for LLVM->WASM")->group(group_backend_codegen_options);
        app.add_flag("--mlir-gpu-offloading", compiler_options.po.enable_gpu_offloading, "Enables gpu offloading using MLIR backend")->group(group_backend_codegen_options);

        // Symbol and lookup-related flags
        app.add_flag("--lookup-name", compiler_options.lookup_name, "Lookup a name specified by --line & --column in the ASR")->group(group_symbol_lookup_options);
        app.add_flag("--rename-symbol", compiler_options.rename_symbol, "Returns list of locations where symbol specified by --line & --column appears in the ASR")->group(group_symbol_lookup_options);
        app.add_option("--line", compiler_options.line, "Line number for --lookup-name")->capture_default_str()->group(group_symbol_lookup_options);
        app.add_option("--column", compiler_options.column, "Column number for --lookup-name")->capture_default_str()->group(group_symbol_lookup_options);
        app.add_flag("--symtab-only", compiler_options.symtab_only, "Only create symbol tables in ASR (skip executable stmt)")->group(group_symbol_lookup_options);

        // Mangling-related flags
        app.add_flag("--module-mangling", compiler_options.po.module_name_mangling, "Mangles the module name")->group(group_mangling_options);
        app.add_flag("--intrinsic-module-mangling", compiler_options.po.intrinsic_module_name_mangling, "Mangles only intrinsic module name")->group(group_mangling_options);
        app.add_flag("--global-mangling", compiler_options.po.global_symbols_mangling, "Mangles all the global symbols")->group(group_mangling_options);
        app.add_flag("--intrinsic-mangling", compiler_options.po.intrinsic_symbols_mangling, "Mangles all the intrinsic symbols")->group(group_mangling_options);
        app.add_flag("--all-mangling", compiler_options.po.all_symbols_mangling, "Mangles all possible symbols")->group(group_mangling_options);
        app.add_flag("--bindc-mangling", compiler_options.po.bindc_mangling, "Mangles functions with abi bind(c)")->group(group_mangling_options);
        app.add_flag("--apply-fortran-mangling", compiler_options.po.fortran_mangling, "Mangle symbols with Fortran supported syntax")->group(group_mangling_options);
        app.add_flag("--mangle-underscore", compiler_options.po.mangle_underscore, "Mangles with underscore")->group(group_mangling_options);

        // Miscellaneous flags
        app.add_flag("--continue-compilation", compiler_options.continue_compilation, "collect error message and continue compilation")->group(group_miscellaneous_options);
        app.add_flag("--semantics-only", compiler_options.semantics_only, "do parsing and semantics, and report all the errors")->group(group_miscellaneous_options);
        app.add_flag("--print-leading-space", compiler_options.print_leading_space, "Print leading white space if format is unspecified")->group(group_miscellaneous_options);
        app.add_flag("--interactive-parse", compiler_options.interactive, "Use interactive parse")->group(group_miscellaneous_options);
        app.add_flag("--verbose", compiler_options.po.verbose, "Print debugging statements")->group(group_miscellaneous_options);
        app.add_flag("--fast", compiler_options.po.fast, "Best performance (disable strict standard compliance)")->group(group_miscellaneous_options);
        app.add_flag("--realloc-lhs", compiler_options.po.realloc_lhs, "Reallocate left hand side automatically")->group(group_miscellaneous_options);
        app.add_flag("--ignore-pragma", compiler_options.ignore_pragma, "Ignores all the pragmas")->group(group_miscellaneous_options);
        app.add_flag("--stack-arrays", compiler_options.stack_arrays, "Allocate memory for arrays on stack")->group(group_miscellaneous_options);
        app.add_flag("--array-bounds-checking", compiler_options.bounds_checking, "Enables runtime array bounds checking")->group(group_miscellaneous_options);
        app.add_flag("--no-array-bounds-checking", disable_bounds_checking, "Disables runtime array bounds checking")->group(group_miscellaneous_options);

        // LSP specific options
        app.add_flag("--show-errors", opts.show_errors, "Show errors when LSP is running in the background")->group(group_lsp_options);
        app.add_flag("--show-document-symbols", opts.show_document_symbols, "Show symbols in lfortran file")->group(group_lsp_options);

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
            compiler_options.separate_compilation = true;
        } else {
            throw lc::LCompilersException(
                "The option `--std=" + opts.arg_standard + "` is not supported"
            );
        }

        if (disable_bounds_checking || compiler_options.po.fast) {
            compiler_options.bounds_checking = false;
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
