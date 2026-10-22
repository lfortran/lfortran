# Interactive evaluation

The terminal prompt and the optional Xeus kernel run cells with
`FortranEvaluator::execute(code, verbose, io)` (`src/lfortran/fortran_evaluator.h`).
It evaluates one cell like `evaluate()`, with fresh diagnostics and the default
passes, and also runs the `%%showast`, `%%showasr`, `%%showllvm`, `%%showasm`,
`%%showcpp` and `%%showfmt` inspection cells. The evaluator must be constructed
with `CompilerOptions::interactive` set; it keeps the session's symbols across
cells. A failed cell is not rolled back.

The returned `CellResult` holds the typed `EvalResult` (or the inspection
`output`), the rendered diagnostics and, for a failed cell, an error name and
text. Compiler exceptions become an error with the stack trace in its text.
`EvalResult::text()` formats a value the way the prompt prints it (8/17
significant digits for real/complex, `True`/`False`, character text) and
`EvalResult::has_value()` is false for statements and empty cells.

The `lfortran_display` module's `display_data` and `clear_output` call the
`lfortran_display_data` and `lfortran_clear_output` C functions, which forward
to the `CellIO` hooks of the cell being executed, including in WASM. They are
process-wide, so cells must not run concurrently. An exception thrown by a hook
does not unwind through generated code; the cell fails with `DisplayError`.
The module is built with or without Xeus. `execute()` does not capture
stdout/stderr; that is up to the frontend.

The `test_lfortran` cases `FortranEvaluator execute: *` and `test_repl` cover
this. The complex-result cases of `test_lfortran` also exercise the JIT result
ABI on Windows: there complex(4) comes back as two float bit patterns in an
integer register and complex(8) through an output pointer, and
`FortranEvaluator` calls those signatures explicitly rather than relying on
the C++ `std::complex` return convention.
