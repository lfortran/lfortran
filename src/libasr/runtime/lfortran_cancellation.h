#ifndef LFORTRAN_CANCELLATION_H
#define LFORTRAN_CANCELLATION_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Interactive execution only. The thread that runs an interruptible cell
 * installs these hooks for itself. A runtime wait (SLEEP,
 * EXECUTE_COMMAND_LINE) asks `requested` while it waits. Once it returns
 * nonzero, the wait ends early and releases what it holds, then calls
 * `stop`, which abandons the cell and does not return. Where nothing is to
 * be stopped, `stop` returns and the wait returns as if it had ended.
 * Returns the previous `requested` hook, so that scoped users can restore
 * the hooks; NULL hooks restore normal waits.
 */
typedef int (*lfortran_cancel_requested)(void);
typedef void (*lfortran_cancel_stop)(void);
#ifdef _WIN32
__declspec(dllexport)
#endif
void _lfortran_set_cancel_hooks(lfortran_cancel_requested requested,
    lfortran_cancel_stop stop);

/*
 * The cell's thread polls `_lfortran_cell_interrupt` between statements.
 * `set_cell_interrupt` publishes a request. `set_interrupt_env` installs the
 * `jmp_buf` (a pointer to it) that `stop_if_interrupted` jumps to; NULL
 * disarms it. While the defer counter is nonzero, a poll returns instead of
 * jumping, so a call into user code from the middle of a statement can
 * finish. `reset_after_interrupt` tells the allocation checker to ignore
 * what the abandoned cell allocated.
 */
#ifdef _WIN32
__declspec(dllexport)
#endif
void _lfortran_set_cell_interrupt(int requested);
#ifdef _WIN32
__declspec(dllexport)
#endif
int _lfortran_cell_interrupt_requested(void);
#ifdef _WIN32
__declspec(dllexport)
#endif
void _lfortran_set_interrupt_env(void *env);
#ifdef _WIN32
__declspec(dllexport)
#endif
void _lfortran_interrupt_defer_enter(void);
#ifdef _WIN32
__declspec(dllexport)
#endif
void _lfortran_interrupt_defer_leave(void);
#ifdef _WIN32
__declspec(dllexport)
#endif
void _lfortran_stop_if_interrupted(void);
#ifdef _WIN32
__declspec(dllexport)
#endif
void _lfortran_reset_after_interrupt(void);

#ifdef __cplusplus
}
#endif
#endif
