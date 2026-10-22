# Interactive evaluation

The terminal prompt and the optional Xeus kernel run cells with
`FortranEvaluator::execute(code, verbose, io)` (`src/lfortran/fortran_evaluator.h`).
It evaluates one cell like `evaluate()`, with fresh diagnostics and the default
passes, and also runs the `%%showast`, `%%showasr`, `%%showllvm`, `%%showasm`,
`%%showcpp` and `%%showfmt` inspection cells (the magic is the first line of
the cell; trailing blanks and a CRLF line ending are ignored). The evaluator
must be constructed
with `CompilerOptions::interactive` set; it keeps the session's symbols across
cells. A failed cell is not rolled back.

The returned `CellResult` holds the typed `EvalResult` (or, when `inspection`
is set, the inspection `output`), the rendered diagnostics and, for a failed
cell, an error name and text. The diagnostics of a successful cell are its
warnings; both frontends show them. Compiler exceptions become an error with
the stack trace in its text. `EvalResult::text()` formats a value the way the
prompt and the kernel show it (8/17 significant digits for real/complex, always
with a decimal point or an exponent, so that `1.0` is not shown as `1`;
`True`/`False`; character text) and `EvalResult::has_value()` is false for
statements and empty cells.

The `lfortran_display` module's `display_data` and `clear_output` call the
`lfortran_display_data` and `lfortran_clear_output` C functions of the runtime
library. While `execute()` runs a cell, it registers hooks with the runtime
(`_lfortran_set_display_hooks`) that forward to that cell's `CellIO`, including
in WASM; at any other time, and in compiled programs, the functions do
nothing. The hooks are process-wide, so cells must not run concurrently. An
exception thrown by a hook does not unwind through generated code; the cell
fails with `DisplayError`. The module is built with or without Xeus.
`execute()` does not capture stdout/stderr; that is up to the frontend.

The `test_lfortran` cases `FortranEvaluator execute: *` (also run in the WASM
build of the JupyterLite kernel), `test_repl` and, in Xeus builds,
`test_kernel_evaluation` cover this; the `display_data_01` integration test
covers compiled programs. The complex-result cases of `test_lfortran` also exercise the JIT result
ABI on Windows: there complex(4) comes back as two float bit patterns in an
integer register and complex(8) through an output pointer, and
`FortranEvaluator` calls those signatures explicitly rather than relying on
the C++ `std::complex` return convention.
