# Running a cell: the prompt and the Jupyter kernel

The terminal prompt and the optional Xeus kernel run cells with
`FortranEvaluator::execute(code, verbose, io)` (`src/lfortran/fortran_evaluator.h`).
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

## Native cell interruption

The native evaluator inserts cooperative checks into interactive LLVM code
only when preparing it for JIT execution. File compilation and LLVM/assembly
inspection (including `--interactive-parse` and `%%showllvm`) do not add these
checks. Executed cells receive them before LLVM optimization, including in
`--fast` mode.

The checks poll at initialized statement and loop boundaries and before a
caller consumes a function result. A pending interrupt leaves enclosing
BLOCK/ASSOCIATE constructs and finalizes procedure-owned locals and temporary
storage using the same scope cleanup as normal returns. Cleanup, including
user final subroutines, temporarily masks further checks without clearing the
pending interrupt; session-global and saved allocations remain live.
Declaration initialization is also an atomic region: cancellation waits until
the scope's descriptors have been initialized. Result propagation checks only
an interruption already observed by the callee, so a newly arriving interrupt
cannot strand a successfully returned allocation before the caller owns it.
The interrupt returns normally through generated callers, then raises
`EvaluationInterrupted` at the C++ JIT invocation boundary. `execute()` reports
`KeyboardInterrupt` and unwinds its ordinary output-capture and runtime-hook
guards. No signal handler unwinds generated or C++ frames.

`FortranEvaluator::request_interrupt()` is lock-free and may be called by the
kernel's SIGINT handler. Both native connection-file kernels service control
through `CellIO::poll_interrupt` on the calling thread, even for silent loops.
The evaluation worker's runtime checks only observe the atomic interrupt flag;
no frontend callback or socket moves to it. Noninterrupt control requests are
retained for dispatch after execution rather than dropped or executed
reentrantly. Shutdown requests cancel the active cell before shutdown.
Pending hooks and output are drained and the worker is joined before an
interruption reaches the cell's error boundary or runtime hooks are removed.
Runtime callback installation lasts only for the cell.
`execute_command_line` polls the runtime callback while waiting;
on interruption it terminates the owned process group (a job on Windows) and
reaps the command before returning. Ordinary compiled programs retain their
existing `system()` execution path.

Cancellation preserves the session, not a transaction: assignments and other
side effects already performed remain. Blocking runtime input or foreign
calls without cooperative polling must return before interruption can be
observed. The WASM executor does not use this native cancellation path.

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

## Runtime output

When `CellIO::output` is set, `execute()` captures stdout and stderr. Native
LLVM evaluation runs on a worker while the calling thread drains pipes and
delivers output during the cell, not just at completion. All `CellIO` hooks
remain on the calling thread, so frontend sockets never move to the worker.
Output is drained before display, clear-output and input hooks, and before
returning; descriptors and hooks are restored even after a failed cell.
The output hook receives arbitrary byte chunks, not necessarily whole lines.
Without an output hook, stdout and stderr are not redirected.

The runtime acknowledges each completed write to a preconnected output stream
before the cell continues, draining it on the calling thread. This preserves
sequential Fortran writes across stdout and stderr without merging their stream
labels, including nonadvancing writes. Ordinary files and standalone programs
are unaffected. Foreign code and child processes still write directly to the
captured descriptors; their bytes are streamed, but cross-stream order cannot
be reconstructed from two independent pipes without producer-side boundaries.

For native Xeus connection-file kernels, IOPub uses a caller-owned XPUB socket
with `XPUB_NODROP`. Its bounded queues apply backpressure to evaluation rather
than silently dropping stream or idle messages at the high-water mark. This
bypasses Xeus-ZMQ's internal PUB/SUB publisher hop, which is also lossy;
coalescing adjacent stdout writes alone cannot protect alternating stdout and
stderr writes without changing their order. The other Xeus channels and
request sockets stay on that same caller, which polls control while waiting
for publication space. A connected subscriber that stops reading holds up
evaluation until it resumes, disconnects, or execution is interrupted. In the
last case, an already backpressured XPUB is closed with zero linger and
rebound: subscribers reconnect, but their undelivered backlog is explicitly
abandoned. Complete output/error/idle delivery is not promised to a stalled
subscriber on that cancellation path. The execute reply still completes and
a healthy reader can run the next cell in the same session.
The built-in kernel retains its bounded nonblocking subscriber queues;
a stalled subscriber never holds up control or evaluation. Xeus registration
configurations retain its default server; WASM uses its own transport.
The same native streaming transport supports Xeus 5 with xeus-zmq 3 and
Xeus 6 with xeus-zmq 4; older supported versions do not fall back to lossy
publication.

WASM, where native threads are unavailable, retains synchronous capture:
output is delivered at runtime writes, hooks and cell completion. Jupyter
messages must be UTF-8; the kernel preserves characters split between chunks
and shows invalid bytes as U+FFFD. Running a cell still serializes other
execution requests.

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
build of the JupyterLite kernel), `test_repl` and, in Xeus builds,
`test_kernel_evaluation` cover this, with `test_runtime_input` for the
runtime's record handling; the `display_data_01` integration test covers
compiled programs and `runtime_input_records` checks that ordinary named and
internal file reads are unchanged. The complex-result cases of `test_lfortran` also
exercise the JIT result ABI on Windows: there complex(4) comes back as two
float bit patterns in an integer register and complex(8) through an output
pointer, and `FortranEvaluator` calls those signatures explicitly rather than
relying on the C++ `std::complex` return convention.

The registered `test_session` also checks output-before-idle on a live kernel
and separated output arrivals in the actual prompt, repeated cells, large
UTF-8 streams, stream labels and cross-stream order, and session cleanup. Its
`--streaming-only` selector runs these checks directly; `--ordering-only`
selects just the live ordering cases. `--compiler-arg=--fast` selects the
kernel's compilation mode.

The registered `test_kernel_stream_interrupt` combines live output and
cancellation on the built-in and real Xeus adapters in normal, `--fast` and
`--std=f23` modes. It checks silent loops, alternating output before the error
and idle, retained control requests, stalled subscribers, allocation/finalizer
cleanup, state recovery, next cells and active-cell shutdown.
