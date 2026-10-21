# Interactive evaluation

The terminal prompt and optional Xeus interpreter use
`LCompilers::LFortran::EvaluationService` (`src/lfortran/evaluation.h`).
The service owns its interactive compiler options and persistent
`FortranEvaluator`. Call `execute(code, events, settings)` serially. It creates
the location manager, default passes and diagnostics for each cell; source and
symbol lifetimes remain those of `FortranEvaluator`. Failed compilation is not
a promise of transactional rollback.

`EvaluationOutcome` contains the typed evaluator value, shared text formatting,
diagnostics and error information. `has_value` distinguishes expression results
from statements and empty cells. Real/complex output preserves the prompt's
8/17 significant-digit formatting; logical and character results work in both
frontends. The existing `%%showast`, `%%showasr`, `%%showllvm`, `%%showasm`,
`%%showcpp` and `%%showfmt` inspection cells use the same service.

The JIT invocation boundary preserves the ASR result kind even when its LLVM
signature is not distinctive. On Windows, complex(4) returns two float bit
patterns in an integer register; complex(8) writes two doubles through an
output pointer. `FortranEvaluator` calls those primitive signatures explicitly
rather than relying on the C++ `std::complex` return convention. This does not
change compiled Fortran's ABI or the service's typed-result contract.

`EvaluationEvents` receives inspection streams, display data and clear-output
notifications synchronously on the execution thread. The existing
`lfortran_display_data(const char *, const char *)` and
`lfortran_clear_output()` C symbols forward only to the current execution's
sink, including in WASM. Display callbacks cannot unwind through generated code;
delivery failures become an unsuccessful outcome. The display Fortran module
is available without Xeus. This API does not capture stdout/stderr: native
capture, ordering fences, transport messages and scheduling belong outside the
compiler/evaluation service. Runtime state and display bindings are
process-local: do not execute evaluators concurrently or nest executions.

## Compatibility

The native prompt remains an in-process frontend. Xeus still owns its protocol
envelope, execution counter and optional `lfortran kernel -f` entry point; WASM
retains xeus-lite/browser stdio. Legacy Xeus stream capture is adapter code,
not part of the evaluation API. The service alone implements neither a native
session server nor Jupyter wire transport, asynchronous capture, a terminal
client, or interrupt recovery.

Focused CTest targets are `test_evaluation` and the existing native `test_repl`.
The registered `test_lfortran` complex-result cases exercise the JIT-only
invocation boundary, including Windows lowering on native JIT hosts and the
native Windows caller in Windows CI; a compiled Fortran program cannot test
this C++ invocation boundary.
