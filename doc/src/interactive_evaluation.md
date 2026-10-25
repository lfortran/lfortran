# Running a cell: the prompt and the Jupyter kernel

The Jupyter kernels, the built-in one (`src/lfortran/jupyter.cpp`), which the
terminal prompt starts and is a client of, and the optional Xeus one, run
cells with `FortranEvaluator::execute(code, verbose, io)` (`src/lfortran/fortran_evaluator.h`).
It evaluates one cell like `evaluate()`, with fresh diagnostics and the default
passes, and also runs the `%%showast`, `%%showasr`, `%%showllvm`, `%%showasm`,
`%%showcpp` and `%%showfmt` inspection cells (the magic is the first line of
the cell; trailing blanks and a CRLF line ending are ignored). An inspection
cell sees the session's declarations, but it does not run, so its own
declarations do not join the session. `CompilerOptions::interactive` must be
set before the first cell; the evaluator keeps the session's symbols across
cells (see {doc}`interactive_cells`). A failed cell is not rolled back.

The returned `CellResult` holds the typed `EvalResult` (or, when `inspection`
is set, the inspection `output`) and the rendered diagnostics; those of a
successful cell are its warnings, which both frontends show. For a failed cell
it holds an error name and message: `CompilerError` with the rendered
diagnostics, `DisplayError`, or, for an internal compiler error
(`internal_error`), the exception's name and message, with its stack trace
apart in `error_traceback`. `CellResult::error_text()` is what a frontend shows
for it. The kernel sends the message as the Jupyter `evalue` and the stack
trace as the `traceback`.

`EvalResult::text()` formats a value the way the prompt and the kernel show it:
for real and complex, the shortest text that reads back as the same value (at
most 9/17 significant digits), always with a decimal point or an exponent, so
that `1.0` is not shown as `1`; `True`/`False`; character text.
`EvalResult::has_value()` is false for statements and empty cells.

The `lfortran_display` module's `display_data` and `clear_output` call the
`lfortran_display_data` and `lfortran_clear_output` C functions of the runtime
library. While `execute()` runs a cell, it registers hooks and a context with
the runtime (`_lfortran_set_display_hooks`) that forward to that cell's
`CellIO`, including in WASM; at any other time, and in compiled programs, the
functions do nothing. The hooks are process-wide, so cells must not run
concurrently. An exception thrown by a hook does not unwind through generated
code; the cell fails with `DisplayError` and the exception's message. The
module is built with or without Xeus.

With `CellIO::output` set, `execute()` redirects the cell's stdout and stderr
(file descriptors 1 and 2) to temporary files and passes what it wrote to the
hook before each display, clear_output and input, when the cell ends, and when
it ends the process (STOP, a fatal error); unset, the cell writes to the
process's stdout and stderr. Both kernels set it. Jupyter messages must be
UTF-8, so the kernels show any other bytes of a cell's output as U+FFFD.

## Runtime input

`CellIO::input`, when set, supplies the records that READ from stdin consumes
while the cell runs: it returns 0 with one record (without a newline), -1 at
end of file, or a positive value when no input is available. An empty record
is a blank line, not end of file. Unset, the cell reads the process's stdin as
a compiled program does. An exception thrown by the hook becomes unavailable
input. `execute()` installs the hook through the runtime's C API
(`src/libasr/runtime/lfortran_stdin.h`) for that cell only and always removes
it, so a later cell can read again after end of file and unread bytes are
discarded.

The runtime keeps the record's bytes, its cursor and delimiters, so one record
can satisfy several list items and one READ can consume several records.
Default stdin and units connected to it (normally unit 5) share this cursor
for list-directed, formatted, nonadvancing and namelist reads of scalars,
arrays and complex values, and PAUSE. End of file and unavailable input are
latched for the rest of the cell: READs with IOSTAT see a negative or a
positive value; READs without one stop the program, as they do outside the
prompt. Named and internal files never consult the hook, and C code reading
stdin directly is not affected.

The `test_lfortran` cases `FortranEvaluator execute: *` (also run in the WASM
build of the JupyterLite kernel), `test_repl` and `test_kernel_evaluation`
(against whichever kernel is built) cover this, with `test_runtime_input` for the
runtime's record handling; the `display_data_01` integration test covers
compiled programs and `runtime_input_records` checks that ordinary named and
internal file reads are unchanged. The complex-result cases of `test_lfortran` also
exercise the JIT result ABI on Windows: there complex(4) comes back as two
float bit patterns in an integer register and complex(8) through an output
pointer, and `FortranEvaluator` calls those signatures explicitly rather than
relying on the C++ `std::complex` return convention.
