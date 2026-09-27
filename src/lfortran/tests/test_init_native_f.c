/*
 * The library test_init_native_e.c needs, so the loader starts it first:
 * its constructor enters the engine before that of test_init_native_e.c
 * has run. See test_init_native_reload.c.
 */
#include <libasr/runtime/lfortran_intrinsics.h>

void test_init_reload_note(const char *what, const char *id);

void test_init_native_f_used(void) {
}

__attribute__((constructor)) static void startup(void) {
    test_init_reload_note("constructor", "f");
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
}
