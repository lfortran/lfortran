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
 * After an interrupted cell, on the thread that ran it: clears the state of
 * the runtime calls that the cell abandoned, such as an I/O statement that
 * spans several of them, so that later cells start clean.
 */
#ifdef _WIN32
__declspec(dllexport)
#endif
void _lfortran_reset_after_interrupt(void);

#ifdef __cplusplus
}
#endif
#endif
