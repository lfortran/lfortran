#ifndef LFORTRAN_INTERRUPT_H
#define LFORTRAN_INTERRUPT_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Interactive execution only (doc/src/interactive_evaluation.md).
 *
 * Code compiled with CompilerOptions::cell_interrupts polls the process-wide
 * flag `_lfortran_cell_interrupt` at loop back edges, backward gotos and
 * procedure entry, and calls `_lfortran_stop_if_interrupted` once it is set.
 * `set_cell_interrupt` publishes or clears a request, from any thread.
 * `set_interrupt_env` installs, for the calling thread, the `jmp_buf` (a
 * pointer to it) that `stop_if_interrupted` jumps to; NULL disarms it. While
 * the defer counter is nonzero, a poll returns instead of jumping, so that a
 * call into user code from the middle of a statement (a final subroutine, a
 * defined assignment, a defined input/output procedure) can finish.
 * `reset_after_interrupt` tells the allocation checker to ignore what the
 * abandoned cell allocated. Where interruption is not supported, these do
 * nothing, and `stop_if_interrupted` always returns.
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
