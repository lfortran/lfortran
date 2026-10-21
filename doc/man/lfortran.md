# NAME

LFortran - modern interactive LLVM-based Fortran compiler

# SYNOPSIS

lfortran [OPTIONS] [files...] [SUBCOMMAND]

lfortran [OPTIONS] [--connection-file PATH]

lfortran --serve --connection-file PATH

lfortran --connect PATH

# DESCRIPTION

LFortran is a modern interactive Fortran compiler based on LLVM.

With no source files, a native LLVM build launches an isolated managed kernel
and a terminal client. Multiple clients share its variables and serialized
execution queue. Ctrl-D at an empty prompt detaches; during a runtime READ it
sends EOF. A managed kernel stops after its last client disconnects. An explicit
server persists until protocol shutdown or termination.

Connection files contain local execution capabilities. Use a new path in a
user-private directory (0700 on POSIX, a user-only ACL on Windows); existing
files are never replaced. Endpoints are private Unix sockets or Windows named
pipes, not network listeners. No session artifacts are created in the current
working directory. Fatal runtime exit or lost transport makes an active
execution's outcome unknown; clients do not replay code automatically.

With optional `WITH_JUPYTER_ZMQ` support, the native server can also expose a
classic Jupyter TCP listener, and the terminal or a local bridge can connect to
an external kernel. These operations report an unavailable-adapter diagnostic
when that build option is disabled. See the
[native-session protocol documentation](https://github.com/lfortran/lfortran/blob/main/doc/src/native-session.md)
for connection formats, routing and limitations.

# OPTIONS

- `-h, --help`: Print this help message and exit
- `-S`: Emit assembly, do not assemble or link
- `-c`: Compile and assemble, do not link
- `-o TEXT`: Specify the file to place the compiler's output into
- `-v`: Be more verbose
- `--connection-file PATH`: Publish a new private native descriptor for a managed interactive session
- `--serve`: Run a persistent native local server; requires `--connection-file PATH`
- `--connect PATH`: Attach a terminal to a live native session, or a classic Jupyter kernel with `WITH_JUPYTER_ZMQ`, without creating another evaluator
- `--jupyter-connection-file PATH`: Add a standard Jupyter listener to `--serve`; requires `WITH_JUPYTER_ZMQ`
- `--bridge PATH`: Proxy an external Jupyter kernel through a new native `--connection-file`; requires `WITH_JUPYTER_ZMQ`
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
- `--gpu-allow-cpu-fallback`: Run a parallel loop that uses a construct the selected GPU does not support (`real(8)` on Metal, `real(10)` or `real(16)`, or input/output) on the CPU with a warning, instead of failing compilation. A loop the GPU offloading pipeline cannot lower is still an error
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
- `--legacy-array-sections`: Enables passing array items as sections if required, and lets the elements of a dummy array whose last dimension is `1`, as in `a(lda, 1)`, be referenced past that dimension, as for an assumed-size `a(lda, *)`
- `--ignore-pragma`: Ignores all the pragmas
- `--stack-arrays`: Allocate memory for arrays on stack

# SUBCOMMANDS

- `fmt`: Format Fortran source files.
- `kernel`: Run in Jupyter kernel mode.
- `mod`: Fortran mod file utilities.
- `pywrap`: Python wrapper generator

# SEE ALSO

For more information, visit the official LFortran documentation at https://docs.lfortran.org/.
