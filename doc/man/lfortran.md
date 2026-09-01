# NAME

LFortran - modern interactive LLVM-based Fortran compiler

# SYNOPSIS

lfortran [OPTIONS] [files...] [SUBCOMMAND]

# DESCRIPTION

LFortran is a modern interactive Fortran compiler based on LLVM.

# OPTIONS

- `-h, --help`: Print this help message and exit
- `-S`: Emit assembly, do not assemble or link
- `-c`: Compile and assemble, do not link
- `-o TEXT`: Specify the file to place the compiler's output into
- `-v`: Be more verbose
- `-E`: Preprocess only; do not compile, assemble, or link
- `-l TEXT ...`: Link library option
- `-L TEXT ...`: Library path option
- `-I TEXT ...`: Include path
- `-J TEXT`: Where to save mod files
- `-g`: Compile with debugging information
- `-D TEXT ...`: Define `<macro>=<value>` (or 1 if `<value>` omitted)
- `--version`: Display compiler version information
- `-W TEXT ...`: Linker flags
- `-f TEXT ...`: All `-f*` flags (only -fPIC, -fPIE & -fdefault-integer-8 supported for now)
- `--cpp`: Enable C preprocessing
- `--fixed-form`: Use fixed form Fortran source parsing
- `--fixed-form-infer`: Use heuristics to infer if a file is in fixed form
- `--no-prescan`: Turn off prescan
- `--show-prescan`: Show tokens for the given file and exit
- `--show-tokens`: Show tokens for the given file and exit
- `--show-ast`: Show AST for the given file and exit
- `--show-asr`: Show ASR for the given file and exit
- `--with-intrinsic-mods`: Show intrinsic modules in ASR
- `--show-ast-f90`: Show Fortran from AST for the given file and exit
- `--no-color`: Turn off colored AST/ASR
- `--no-indent`: Turn off Indented print ASR/AST
- `--tree`: Tree structure print ASR/AST
- `--json`: Print ASR/AST Json format
- `--clojure`: Print lossless ASR in canonical Clojure/EDN format
- `--no-member-names`: Omit ASR member names in Clojure/EDN output
- `--from-asr`: Parse the input file as ASR text
- `--verify-asr`: Parse and verify standalone ASR text without running passes
- `--verify-all-passes`: Verify ASR after every pass
- `--no-loc`: Skip location information in ASR/AST Json format
- `--visualize`: Print ASR/AST Visualization
- `--pass TEXT`: Apply the ASR pass and show ASR (implies --show-asr)
- `--skip-pass TEXT`: Skip an ASR pass in the default pipeline
- `--show-llvm`: Show LLVM IR for the given file and exit
- `--show-cpp`: Show C++ translation source for the given file and exit
- `--show-c`: Show C translation source for the given file and exit
- `--show-asm`: Show assembly for the given file and exit
- `--show-wat`: Show WAT (WebAssembly Text Format) and exit
- `--show-julia`: Show Julia translation source for the given file and exit
- `--show-gpu-kernel-source`: Show the GPU kernel source for the backend selected by `--gpu` and exit
- `--show-fortran`: Show Fortran translation source for the given file and exit
- `--show-stacktrace`: Show internal stacktrace on compiler errors
- `--symtab-only`: Only create symbol tables in ASR (skip executable stmt)
- `--time-report`: Show compilation time report
- `--static`: Create a static executable
- `--no-warnings`: Turn off all warnings
- `--no-style-suggestions`: Turn off style suggestions
- `--no-error-banner`: Turn off error banner
- `--continue-compilation`: Collect error messages and continue compilation after encountering semantic errors
- `--error-format TEXT=human`: Control how errors are produced (human, short)
- `--backend TEXT=llvm`: Select a backend (llvm, cpp, x86, wasm, fortran)
- `--device-compiler TEXT=nvcc`: Toolchain driver used to compile and link GPU device code
- `--openmp`: Enable OpenMP
- `--separate-compilation`: Generate object code into .o files
- `--rtlib`: Include the full runtime library in the LLVM output
- `--use-loop-variable-after-loop`: Allow using loop variable after the loop
- `--fast`: Optimize for best performance on the host CPU (disable strict standard compliance)
- `--link-with-gcc`: Calls GCC for linking instead of clang
- `--target TEXT`: Generate code for the given target
- `--march TEXT`: Generate code for the selected instruction-set architecture (`native` for the host)
- `--mcpu TEXT`: Generate and tune code for the selected CPU (`native` for the host)
- `--mtune TEXT`: Tune code for the selected CPU without changing the instruction set (`native` for the host)
- `--print-targets`: Print the registered targets
- `--implicit-typing`: Allow implicit typing
- `--implicit-interface`: Allow implicit interface
- `--implicit-argument-casting`: Allow implicit argument casting
- `--print-leading-space`: Print leading white space if format is unspecified
- `--interactive-parse`: Use interactive parse
- `--verbose`: Print debugging statements
- `--dump-all-passes`: Apply all the passes and dump the ASR into a file
- `--dump-all-passes-fortran`: Apply all passes and dump the ASR after each pass into a Fortran file
- `--cumulative`: Apply all the passes cumulatively till the given pass
- `--realloc-lhs-arrays`: Reallocate left hand side automatically for arrays
- `--disable-realloc-lhs-arrays`: Disables reallocating left hand side automatically for arrays
- `--module-mangling`: Mangles the module name
- `--global-mangling`: Mangles all the global symbols
- `--intrinsic-mangling`: Mangles all the intrinsic symbols
- `--all-mangling`: Mangles all possible symbols
- `--bindc-mangling`: Mangles functions with ABI bind(c)
- `--apply-fortran-mangling`: Mangle symbols with Fortran supported syntax
- `--mangle-underscore`: Mangles with underscore
- `--legacy-array-sections`: Enables passing array items as sections if required
- `--ignore-pragma`: Ignores all the pragmas
- `--stack-arrays`: Allocate memory for arrays on stack

# SUBCOMMANDS

- `fmt`: Format Fortran source files.
- `kernel`: Run in Jupyter kernel mode.
- `mod`: Fortran mod file utilities.
- `pywrap`: Python wrapper generator

# SEE ALSO

For more information, visit the official LFortran documentation at https://docs.lfortran.org/.
