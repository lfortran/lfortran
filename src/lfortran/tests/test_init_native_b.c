/*
 * One object file of test_init_native.c, with what compiled code puts there
 * for the engine to find spelled out in C: its table of initialization
 * records, the one constructor that enters the engine and the one destructor
 * that tells it the image is going away. See
 * lcompilers_init_abi.h.
 */
#include <stddef.h>
#include <stdint.h>

#include <libasr/runtime/lfortran_intrinsics.h>

void test_init_native_note(const char *what, const char *id);
void ensure_n2(void);

static int32_t state;

static void ensure_n1(void) {
    if (_lcompilers_init_begin(&state)) {
        test_init_native_note("enter", "m:n1");
        ensure_n2();
        test_init_native_note("ready", "m:n1");
        _lcompilers_init_end(&state);
    }
}

static void teardown(void) {
    test_init_native_note("teardown", "m:n1");
}

/* Fresh in every mapping of this image. */
static uint32_t instance;

static const lcompilers_init_record records[] = {
    {"m:n1", ensure_n1, teardown, &state, 0, 0},
};

#if defined(__APPLE__)
__attribute__((used, section("__DATA,__lcomp_init")))
static lcompilers_init_table table = {lcompilers_init_abi_version, 1, records,
    &instance};

__attribute__((constructor)) static void startup(void) {
    test_init_native_note("constructor", "m:n1");
    _lcompilers_init_ctor(&table);
}

__attribute__((destructor)) static void shutdown(void) {
    _lcompilers_init_unload(&table);
}
#elif !defined(_WIN32)
static const lcompilers_init_table table __asm__("test_init_native_table")
    __attribute__((used)) = {
    lcompilers_init_abi_version, 1, records, &instance};
#include "test_init_native_note.h"

__attribute__((constructor)) static void startup(void) {
    test_init_native_note("constructor", "m:n1");
    _lcompilers_init_ctor(&table);
}

__attribute__((destructor)) static void shutdown(void) {
    _lcompilers_init_unload(&table);
}
#endif
