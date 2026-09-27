/*
 * The first of the two shared libraries test_init_native_order.c is linked
 * with; test_init_native_lb.c needs it, so the loader starts it first. It
 * keeps the log of both, and each constructor records how deeply it is
 * nested in another, which a loader starts one after the other.
 */
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include <libasr/runtime/lfortran_intrinsics.h>

char test_init_order_events[16][32];
int test_init_order_nevents = 0;
static int depth = 0;

void test_init_order_note(const char *what, const char *id) {
    if (test_init_order_nevents < 16) {
        snprintf(test_init_order_events[test_init_order_nevents],
            sizeof(test_init_order_events[0]), "%s %s", what, id);
        test_init_order_nevents++;
    }
}

void test_init_order_enter(const char *id) {
    depth++;
    test_init_order_note(depth == 1 ? "constructor" : "nested constructor", id);
}

void test_init_order_leave(void) {
    depth--;
}

static int32_t state;

static void ensure_la(void) {
    if (_lcompilers_init_begin(&state)) {
        test_init_order_note("ready", "m:la");
        _lcompilers_init_end(&state);
    }
}

/* Fresh in every mapping of this image. */
static uint32_t instance;

static const lcompilers_init_record records[] = {
    {"m:la", ensure_la, NULL, &state, 0, 0},
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
    test_init_order_enter("m:la");
    _lcompilers_init_ctor(&table);
    test_init_order_leave();
}

__attribute__((destructor)) static void shutdown(void) {
    _lcompilers_init_unload(&table);
}
