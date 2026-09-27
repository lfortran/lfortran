/*
 * A shared library whose every load fails: it needs a symbol nothing
 * defines. Its records are mapped, and listed by the loader, for as long as
 * a load of it takes, but no dispatch may ever run them.
 */
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <libasr/runtime/lfortran_intrinsics.h>

void test_init_native_missing(void);

static int32_t state;

static void ensure_bad(void) {
    abort();
}

void test_init_native_bad_use(void) {
    test_init_native_missing();
}

/* Fresh in every mapping of this image. */
static uint32_t instance;

static const lcompilers_init_record records[] = {
    {"m:bad", ensure_bad, NULL, &state, 0, 0},
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

__attribute__((constructor)) static void startup(void) {
    _lcompilers_init_ctor(&table);
}

__attribute__((destructor)) static void shutdown(void) {
    _lcompilers_init_unload(&table);
}
