/*
 * The shared library test_init_native_dlopen.c loads, unloads and loads
 * again, with what compiled code puts in an object file spelled out in C as
 * in test_init_native_a.c. Its record depends on m:n2, which the program
 * that loads it defines.
 *
 * Its destructor also enters the engine after the unload returned, while
 * the loader still lists the library, with the state word cleared as memory
 * reused after the unmap would be: a dispatch may then no longer use the
 * library's records, which belong to an image on its way out. An empty batch
 * published meanwhile makes sure the dispatch walks the records again.
 */
#include <stddef.h>
#include <stdint.h>

#include <libasr/runtime/lfortran_intrinsics.h>

void test_init_native_note(const char *what, const char *id);
void ensure_n2(void);

static int32_t state;

static void ensure_nc(void) {
    if (_lcompilers_init_begin(&state)) {
        test_init_native_note("enter", "m:nc");
        ensure_n2();
        test_init_native_note("ready", "m:nc");
        _lcompilers_init_end(&state);
    }
}

static void teardown(void) {
    test_init_native_note("teardown", "m:nc");
}

/* Fresh in every mapping of this image. */
static uint32_t instance;

static const lcompilers_init_record records[] = {
    {"m:nc", ensure_nc, teardown, &state, 0, 0},
};

#if !defined(_WIN32)
static const lcompilers_init_table nudge = {lcompilers_init_abi_version, 0, NULL};
#endif

#if defined(__APPLE__)
__attribute__((used, section("__DATA,__lcomp_init")))
static lcompilers_init_table table = {lcompilers_init_abi_version, 1, records,
    &instance};

__attribute__((constructor)) static void startup(void) {
    test_init_native_note("constructor", "m:nc");
    _lcompilers_init_ctor(&table);
}

__attribute__((destructor)) static void shutdown(void) {
    _lcompilers_init_unload(&table);
    state = 0;
    _lcompilers_init_add_records(&nudge);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    _lcompilers_init_remove_records(&nudge);
}
#elif !defined(_WIN32)
static const lcompilers_init_table table = {
    lcompilers_init_abi_version, 1, records, &instance};

typedef struct {
    uint32_t namesz, descsz, type;
    char name[4];
    const lcompilers_init_table *desc;
} elf_note;

__attribute__((used, section(".note.lcompilers.init"), aligned(4)))
static elf_note note = {4, sizeof(void *), lcompilers_init_elf_note_type, "LCP",
    &table};

__attribute__((constructor)) static void startup(void) {
    test_init_native_note("constructor", "m:nc");
    _lcompilers_init_ctor(&note);
}

__attribute__((destructor)) static void shutdown(void) {
    _lcompilers_init_unload(&table);
    state = 0;
    _lcompilers_init_add_records(&nudge);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    _lcompilers_init_remove_records(&nudge);
}
#endif
