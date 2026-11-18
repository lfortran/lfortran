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
If the cell can be interrupted (`FortranEvaluator::interruptible()`), the
hook may return early, with any status, once `CellIO::interrupt_requested`
returns true: the cell then stops without failing the READ. Otherwise it must
keep waiting for input, as nothing would stop the cell.

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

## Interrupting a cell

A frontend that sets `CellIO::interrupt_requested` can interrupt a running
cell, as Jupyter's Interrupt (`interrupt_request` on the control channel) and
Ctrl-C at the `lfortran` prompt do. The callback is bounded and nonblocking.
`execute()` calls it on its caller every few milliseconds, and after each of
the other `CellIO` callbacks, which all run on the caller, while the cell
runs on a thread of its own. Once it returns true, the cell fails with a
`CellResult` named `KeyboardInterrupt` ("cell execution interrupted") and no
value. Only one cell runs at a time.

The cell's thread stops at a poll. Code compiled for an interactive session
loads a runtime flag at the back edge of a counted `do` (and of `do
concurrent`), at the head of a `do while` or a `do` with no control, at a
backward `goto`, and on entry to a procedure. When the flag is set, the poll
abandons the cell and returns to `execute()`. Those places are between
statements, or before a procedure has done any of its work, so an assignment
the cell finished keeps its new value and one it had not started is unchanged.
A batch compilation has none of these checks. No signal is sent, and none is
reserved. `execute()` calls the callback about every 10 milliseconds.

The runtime's waits stop at once: SLEEP, which wakes every 10 milliseconds to
look at the flag, EXECUTE_COMMAND_LINE, whose command runs in a process group
of its own that is killed (with its descendants that stay in the group) and
reaped first, and a READ from stdin that waits for input. (In a process that
is the foreground job of a terminal, the command runs as with `system()`: the
terminal's Ctrl-C reaches it, and the cell waits for it.) Output statements
and formatted READ also stop where their runtime call begins, so that a loop
that spends its time in the runtime stops at its next statement.

The abandoned frames are not unwound. What the interrupted cell allocated and
did not free leaks; nothing is rolled back. What it did before the interrupt
stays done, as in IPython: variables it had assigned keep their new values,
and output it had written is shown. The session keeps everything that earlier
cells defined, so later cells can use their variables and procedures. An
allocatable or a deferred-length string that the cell was assigning stays
consistent, and a later cell may use it, including to deallocate it or in
`move_alloc`. A request is not carried into the next cell: an interrupt that
arrives while no cell runs does nothing. An interrupted cell is a failed
cell, so with Jupyter's `stop_on_error` (the default) the cells already
queued behind it are aborted; with `stop_on_error=false` they run. Cells
sent afterwards run as usual.

A single long call into foreign code, or into one runtime or library routine,
stops only when that call returns. A loop that calls it stops at the next
iteration. A loop inside a final subroutine, a defined assignment, or a
defined input or output procedure is deferred: the poll returns, and the cell
stops at the next poll after that statement. If that loop does not end, the
cell does not stop. At the prompt, a second Ctrl-C then ends the session.
Compilation is not interrupted either; a request that is already set stops
the cell before it runs. Interruption is supported on Linux and macOS
(x86-64 and AArch64). It is not supported on Windows, in WASM (JupyterLite)
or with `--openmp`, where a cell runs to its end, and a READ keeps waiting
for its input (`lfortran_interruptible` is false in the kernel's
`input_request`).

The built-in kernel reads its control channel while a cell runs, through the
callback, and answers `interrupt_request` at once; it defers other control
requests until the cell ends. The Xeus kernel uses Xeus's split server
(`make_xserver_shell_main`), which runs shell requests on the main thread and
control requests on their own thread, where `interrupt_request` sets the flag
that the callback reads. Both kernels publish the `KeyboardInterrupt` as an
`error` message before they reply. A READ waits for its `input_reply` on
LFortran's `InputChannel`, which an interrupt cancels; replies to an older
`input_request` are ignored. For that, the Xeus kernel replaces only Xeus's
stdin transport. With Xeus registration configurations, IPv6 or non-TCP
transports, a READ uses Xeus's blocking stdin transport, and an interrupt
takes effect once the READ has its input.

During a cell, Ctrl-C at the prompt only marks the interrupt; the prompt's
client loop sends the `interrupt_request` and keeps reading the terminal
without blocking, so that it also notices Ctrl-C while a READ waits for input.

The `test_lfortran` cases `FortranEvaluator interrupt*`, `test_kernel_evaluation`
(both kernels), `test_session_regressions` (`test_interrupt_prompt`) and
`test_repl` cover this.
