/*
 * The library test_init_native_reload.c loads, unloads and loads again. It
 * needs test_init_native_f.c, whose constructor dispatches before this
 * one's: every load of it, a load at the address of an unloaded one
 * included, is initialized by that dispatch already.
 */
#include <stddef.h>
#include <stdint.h>

#include <libasr/runtime/lfortran_intrinsics.h>

void test_init_reload_note(const char *what, const char *id);
void test_init_native_f_used(void);

/* Fresh in every mapping of this image. */
static uint32_t instance;
static int32_t state;

static void ensure_ne(void) {
    if (_lcompilers_init_begin(&state)) {
        test_init_reload_note("ready", "m:ne");
        _lcompilers_init_end(&state);
    }
}

static const lcompilers_init_record records[] = {
    {"m:ne", ensure_ne, NULL, &state, 0, 0},
};

#if defined(__APPLE__)
__attribute__((used, section("__DATA,__lcomp_init")))
static lcompilers_init_table table = {lcompilers_init_abi_version, 1, records,
    &instance};
#elif !defined(_WIN32)
static const lcompilers_init_table table __asm__("test_init_native_table")
    __attribute__((used)) = {
    lcompilers_init_abi_version, 1, records, &instance};
#include "test_init_native_note.h"
#endif

const void *test_init_native_e_table(void) {
    return &table;
}

__attribute__((constructor)) static void startup(void) {
    test_init_native_f_used();
    test_init_reload_note("constructor", "m:ne");
    _lcompilers_init_ctor(&table);
}

__attribute__((destructor)) static void shutdown(void) {
    _lcompilers_init_unload(&table);
}
