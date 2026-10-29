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
module is built with or without Xeus. `execute()` does not capture
stdout/stderr; that is up to the frontend. Jupyter messages must be UTF-8, so
the kernel shows any other bytes of a cell's output as U+FFFD.

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
The hook also receives `interruptible`, a worker-snapshotted boolean passed
through the caller mailbox. When false (protected initialization or cleanup),
it must keep servicing control without abandoning the input wait.

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

## Cooperative cancellation in the native evaluator

Native callers can set `CellIO::cancel_requested` to a bounded, nonblocking
callback returning whether to cancel the current cell. Like the output and
display hooks, it runs on the `execute()` caller, not on the execution worker.
It is pumped even without output capture or output from the cell. The worker
reads only an atomic request; transport and socket ownership stay with the
caller. Only one cell may execute at a time.

Native JIT lowering receives an explicit `JITCancellation` callback contract
from `evaluate()`, never from file compilation or inspection. Checkpoints
follow structured ASR statements and loop tests after local descriptors have
been initialized. Calls propagate an already observed cancellation before
consuming a function result. Initialization and user finalizers defer
cancellation; a cancelled frame leaves enclosing BLOCK/ASSOCIATE constructs,
reuses procedure cleanup, and copies CFI descriptors back before returning.
It abandons its result instead of loading an incomplete return variable.
The callbacks refer to the current cell's thread-local execution context, so
procedures defined by earlier cells can cancel too.

After the worker returns normally and is joined, a completed cancellation
becomes a failed `CellResult` named `KeyboardInterrupt`, with no result value.
The session, procedure definitions and completed side effects survive; there
is no transactional rollback. A request is not carried into the next cell.
Interruption remains an execution error and honors Jupyter `stop_on_error`:
the interrupted request's default `true` may abort already queued cells;
`false` lets them continue. A newly submitted cell can run with either setting.
An idle request, or one arriving after the last checkpoint, need not turn a
completed cell into a failure.

The built-in kernel and the native Xeus TCP/IPv4 server (including hostnames)
connect control-channel `interrupt_request` to this pump, including silent
loops. Other requests are deferred until execution returns. Interrupt replies
have the interrupt as parent; the execution error/reply and final idle retain
the cell's parent. Idle interrupts do not latch into subsequent cells.

Both adapters use `InputChannel` for cancellable stdin waits, pumping control
and output on the caller. A native JIT READ without user IOSTAT uses an internal
status and checks cancellation before reporting an ordinary input error.
This lets READ return through its runtime frame and normal JIT cleanup, even
as the last statement of a cell. Ordinary EOF/error without IOSTAT remains
fatal. Cancellation discards the pending input request; correlated replies
to an older request are ignored. Unparented input replies, as sent by some
Jupyter clients, cannot distinguish an old answer from a new one.

While executing, prompt Ctrl-C sets only a signal marker. Its client loop
sends the interrupt on the control channel; it continues servicing messages
while waiting for terminal input. Owning and attached prompts retain the
session. At an idle prompt, Ctrl-C retains its existing exit behavior.
Protected READs advertise `lfortran_interruptible: false` in input_request;
the prompt keeps accepting their input after Ctrl-C so cleanup can finish.

During native execution with a cancellation callback, EXECUTE_COMMAND_LINE
waits poll that same cell state. POSIX commands run in their own process group;
Windows commands run in an owned job. Cancellation terminates the owned shell
and descendants and waits for the shell before returning through normal JIT
cleanup. Captured stdout/stderr and the process stdin are inherited as before.
Protected initialization/finalization commands must finish. Ordinary compiled
files and executions without a cancellation callback still use `system()`.
WAIT=.false. retains its existing synchronous behavior; this is not an
implementation of asynchronous commands. Ownership lasts for the synchronous
command wait, not for background jobs after its shell has returned. POSIX
descendants that deliberately leave the process group are not tracked.

Compilation, arbitrary foreign calls without safepoints, and WASM execution
are not interrupted. Foreign calls may defer cancellation until they return.
Initialization and cleanup, including their input waits, must finish before
cancellation can proceed. Xeus registration configurations, IPv6 and non-TCP
transports retain their legacy server and blocking input behavior.
No exception, signal unwind, or nonlocal jump crosses a generated/C frame.
