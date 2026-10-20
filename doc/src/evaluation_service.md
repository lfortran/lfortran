# Interactive evaluation and runtime input

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

`EvaluationEvents` receives inspection streams, display data and clear-output
notifications synchronously on the execution thread. The existing
`lfortran_display_data(const char *, const char *)` and
`lfortran_clear_output()` C symbols forward only to the current execution's
sink, including in WASM. Display callbacks cannot unwind through generated code;
delivery failures become an unsuccessful outcome. The display Fortran module
is available without Xeus. This API does not capture stdout/stderr: native
capture, ordering fences, transport messages and scheduling belong outside the
compiler/evaluation service.

## Opt-in stdin records

`src/libasr/runtime/lfortran_stdin.h` exposes a transport-independent C API:

```c
typedef int (*lfortran_stdin_provider)(
    void *context, const char **record, size_t *length);
void _lfortran_stdin_install(lfortran_stdin_provider, void *context);
void _lfortran_stdin_reset(void);
```

A provider supplies **one record without LF** when actual input needs bytes.
Return zero and a borrowed byte buffer (copied immediately), `-1` for EOF, or
positive for unavailable/disconnected input. An empty record is a blank input
line, not EOF. Embedded LF is rejected. No UTF-8 interpretation occurs here.
Providers must not throw; the evaluation service also catches callback
exceptions before control returns through the C runtime.

The runtime retains record bytes, the cursor, delimiters and pushback. A single
record can satisfy multiple list items; a transfer can request multiple records.
Formatted/nonadvancing reads and record draining share this cursor with default
stdin and explicit/dynamic units connected to stdin (normally unit 5). Scalar,
array, complex, namelist and PAUSE readers use the same byte source. PAUSE keeps
its existing `go`/terminate behavior, including termination on EOF/unavailable
input; it is not a recoverable input error.

EOF and unavailable input are latched until install/reset. EOF-aware READs
report negative IOSTAT; unavailable input reports positive IOSTAT. As with
ordinary runtime errors, READ without IOSTAT and fatal runtime operations may
terminate the process. The service installs `EvaluationSettings::input` for the
execution and always resets it on returning, including diagnostic failures.
A later execution can read again after EOF. Installing/resetting discards any
unread provider bytes; a sequence of nonadvancing transfers shares its record
within that execution. Providers must keep returned storage valid until the
next callback (the runtime copies it immediately).

Named and internal files never request provider input. Without a provider all
runtime reads retain their existing libc stdio behavior, including Emscripten
stdin handling. This is not libc interposition and does not intercept foreign
C libraries reading stdin. Provider state, runtime units and display bindings
are process-local: do not execute evaluators concurrently or nest executions.

## Compatibility and frontends

The native prompt is a protocol-only terminal connected to a separate native
kernel process. The kernel's serialized worker owns this evaluation service;
session transport and output capture live outside it. See
[Native sessions](native-session.md) for the terminal, local binding and optional
`WITH_JUPYTER_ZMQ` wire adapter/bridge. Xeus still owns
its protocol envelope, execution counter and optional `lfortran kernel -f`
entry point; WASM retains xeus-lite/browser stdio when no provider is supplied.
Legacy Xeus stream capture is adapter code, not part of the evaluation API.
The service alone implements neither a native session server nor Jupyter wire
transport, asynchronous capture, a terminal client, or interrupt recovery.

Focused CTest targets are `test_evaluation`, `test_runtime_input` and the
existing native `test_repl` (including its compiled READ PTY case).
`integration_tests/runtime_input_records.f90` checks ordinary named/internal
file behavior with both LFortran and GFortran.
