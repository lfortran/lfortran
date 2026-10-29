#ifndef LFORTRAN_CANCELLATION_H
#define LFORTRAN_CANCELLATION_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Native interactive execution only. The worker installs a nonthrowing poll
 * of its cell's existing cancellation state, including protected cleanup.
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
