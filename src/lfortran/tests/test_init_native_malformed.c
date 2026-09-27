/*
 * A shared library whose table a loader lists without the instance word
 * every such table has: the engine fails where it finds it -- in the scan
 * of the loaded images, holding the loader's lock and its own -- and the
 * process ends there rather than hanging. It has no constructor of its own,
 * so that the scan is where the table is found.
 */
#include <stddef.h>
#include <stdint.h>

#include <libasr/runtime/lfortran_intrinsics.h>

static int32_t state;

static void ensure_malformed(void) {
    if (_lcompilers_init_begin(&state)) _lcompilers_init_end(&state);
}

static const lcompilers_init_record records[] = {
    {"m:malformed", ensure_malformed, NULL, &state, 0, 0},
};

#if defined(__APPLE__)
__attribute__((used, section("__DATA,__lcomp_init")))
static lcompilers_init_table table = {lcompilers_init_abi_version, 1, records,
    NULL};
#elif !defined(_WIN32)
static const lcompilers_init_table table __asm__("test_init_native_table")
    __attribute__((used)) = {
    lcompilers_init_abi_version, 1, records, NULL};
#include "test_init_native_note.h"
#endif
