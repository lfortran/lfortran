/*
 * The second of the two shared libraries test_init_native_order.c is linked
 * with. It needs test_init_native_la.c, where the log is, so the loader
 * starts it after that one; its records are initialized already by the
 * dispatch of that one's constructor.
 */
#include <stddef.h>
#include <stdint.h>

#include <libasr/runtime/lfortran_intrinsics.h>

void test_init_order_note(const char *what, const char *id);
void test_init_order_enter(const char *id);
void test_init_order_leave(void);

static int32_t state;

static void ensure_lb(void) {
    if (_lcompilers_init_begin(&state)) {
        test_init_order_note("ready", "m:lb");
        _lcompilers_init_end(&state);
    }
}

int test_init_order_lb_ready(void) {
    return state == lcompilers_init_ready;
}

/* Fresh in every mapping of this image. */
static uint32_t instance;

static const lcompilers_init_record records[] = {
    {"m:lb", ensure_lb, NULL, &state, 0, 0},
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
    test_init_order_enter("m:lb");
    _lcompilers_init_ctor(&table);
    test_init_order_leave();
}

__attribute__((destructor)) static void shutdown(void) {
    _lcompilers_init_unload(&table);
}
