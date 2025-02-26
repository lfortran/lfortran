#include <lfortran/utils.h>

#include <bin/cli_utils.h>

namespace LCompilers::CommandLineInterface {

    auto init_compiler_options(
        CompilerOptions &compiler_options,
        CLI::App &app
    ) -> void {
        compiler_options.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();

        // Standard options compatible with gfortran, gcc or clang
        // We follow the established conventions
        app.add_option("-o", compiler_options.arg_o, "Specify the file to place the compiler's output into");
        app.add_option("-I", compiler_options.po.include_dirs, "Include path")->allow_extra_args(false);
        app.add_option("-J", compiler_options.po.mod_files_dir, "Where to save mod files");
        app.add_flag("-g", compiler_options.emit_debug_info, "Compile with debugging information");
        app.add_flag("--debug-with-line-column", compiler_options.emit_debug_line_column,
                "Convert the linear location info into line + column in the debugging information");
        app.add_option("-D", compiler_options.c_preprocessor_defines, "Define <macro>=<value> (or 1 if <value> omitted)")->allow_extra_args(false);

        // LFortran specific options
        app.add_flag("--fixed-form", compiler_options.fixed_form, "Use fixed form Fortran source parsing");
        app.add_flag("--with-intrinsic-mods", compiler_options.po.with_intrinsic_mods, "Show intrinsic modules in ASR");
        app.add_flag("--tree", compiler_options.po.tree, "Tree structure print ASR/AST");
        app.add_flag("--json", compiler_options.po.json, "Print ASR/AST Json format");
        app.add_flag("--no-loc", compiler_options.po.no_loc, "Skip location information in ASR/AST Json format");
        app.add_flag("--visualize", compiler_options.po.visualize, "Print ASR/AST Visualization");
        app.add_flag("--show-stacktrace", compiler_options.show_stacktrace, "Show internal stacktrace on compiler errors");
        app.add_flag("--symtab-only", compiler_options.symtab_only, "Only create symbol tables in ASR (skip executable stmt)");
        app.add_flag("--logical-casting", compiler_options.logical_casting, "Allow logical casting");
        app.add_flag("--no-warnings", compiler_options.no_warnings, "Turn off all warnings");
        app.add_flag("--no-style-warnings", compiler_options.disable_style, "Turn off style suggestions");
        app.add_flag("--no-error-banner", compiler_options.no_error_banner, "Turn off error banner");
        app.add_option("--error-format", compiler_options.error_format, "Control how errors are produced (human, short)")->capture_default_str();
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
        app.add_option("--target", compiler_options.target, "Generate code for the given target")->capture_default_str();
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
    }

    auto init_compiler_options(
        CompilerOptions &compiler_options,
        int argc,
        const char *const *argv
    ) -> int {
        CLI::App app{"LFortran: modern interactive LLVM-based Fortran compiler"};
        init_compiler_options(compiler_options, app);
        CLI11_PARSE(app, argc, argv);
        return 0;
    }

} // namespace LCompilers::CommandLineInterface
