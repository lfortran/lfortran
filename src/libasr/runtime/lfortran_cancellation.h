#ifndef LFORTRAN_CANCELLATION_H
#define LFORTRAN_CANCELLATION_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Native interactive execution only. The worker installs a nonthrowing poll
 * of its cell's existing cancellation state. It returns nonzero once the cell
 * is cancelled, unless initialization or cleanup has to complete; a runtime
 * wait (a command, sleep) then stops early and returns, and generated code
 * observes the cancellation after the call.
 * The runtime calls it on that same thread; no frontend callback runs here.
 * Return the previous hook so scoped users can restore it on every exit.
 */
typedef int (*lfortran_cancel_poll)(void);
#ifdef _WIN32
__declspec(dllexport)
#endif
lfortran_cancel_poll _lfortran_set_cancel_poll(lfortran_cancel_poll poll);

#ifdef __cplusplus
}
#endif
#endif
