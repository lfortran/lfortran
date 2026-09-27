/*
 * The shared library test_init_native_race.c loads and unloads while other
 * threads walk the records and tear them down. Its record's initializer and
 * teardown count themselves in the program.
 */
#include <stddef.h>
#include <stdint.h>

#include <libasr/runtime/lfortran_intrinsics.h>

void test_init_race_count(int teardown);

static int32_t state;

static void ensure_nd(void) {
    if (_lcompilers_init_begin(&state)) {
        test_init_race_count(0);
        _lcompilers_init_end(&state);
    }
}

static void teardown(void) {
    test_init_race_count(1);
}

/* Fresh in every mapping of this image. */
static uint32_t instance;

static const lcompilers_init_record records[] = {
    {"m:nd", ensure_nd, teardown, &state, 0, 0},
};

#if defined(__APPLE__)
__attribute__((used, section("__DATA,__lcomp_init")))
static lcompilers_init_table table = {lcompilers_init_abi_version, 1, records,
    &instance};
static const void *anchor(void) { return &table; }
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
static const void *anchor(void) { return &note; }
#endif

__attribute__((constructor)) static void startup(void) {
    _lcompilers_init_ctor(anchor());
}

__attribute__((destructor)) static void shutdown(void) {
    _lcompilers_init_unload(&table);
}
