#ifndef LFORTRAN_STDIN_H
#define LFORTRAN_STDIN_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * An opt-in, process-local source of external stdin records. Calls are serial.
 * Return 0 and a borrowed UTF-8/byte record without a line terminator (length
 * zero is a blank record), -1 for EOF, or a positive value for unavailable
 * input. The borrowed storage must remain valid until the next provider call;
 * the runtime copies it before returning to generated code. The callback must
 * not throw. EOF/error is sticky until the next install/reset.
 *
 * Install once per execution, reset on every exit. Install/reset discard any
 * unread provider bytes. Neither function reads or clears libc stdin.
 * A NULL provider restores normal compiled-program stdio.
 * This is not a hook for foreign C input or for named/internal Fortran files.
 */
typedef int (*lfortran_stdin_provider)(void *context,
    const char **record, size_t *length);

void _lfortran_stdin_install(lfortran_stdin_provider provider, void *context);
void _lfortran_stdin_reset(void);

#ifdef __cplusplus
}
#endif
#endif
