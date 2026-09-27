#ifndef LCOMPILERS_INIT_ABI_H
#define LCOMPILERS_INIT_ABI_H

/*
 * The object-format ABI of startup initialization, shared by the code
 * generators, which emit it, and the runtime engine, which reads it. It is
 * plain C so that it is usable from both.
 *
 * Every object file that defines at least one startup initializer contributes
 * one `lcompilers_init_table`, pointing to its `lcompilers_init_record`s:
 *
 * - ELF: an allocated, read-only SHT_NOTE in section `.note.lcompilers.init`
 *   whose owner is "LCP", whose type is `lcompilers_init_elf_note_type` and
 *   whose descriptor is one signed, pointer-sized offset: the table's address
 *   minus the note's own (that of its `namesz`). The linker resolves it, so the note needs no
 *   dynamic relocation and is right in the file as it is in memory: it stays
 *   in the read-only segment with the other notes, and a PT_NOTE that also
 *   spans the file's copy of something else still reads correctly. The note
 *   is 4-byte aligned, so the offset is read with `memcpy`.
 * - Mach-O: the table itself in section `__DATA,__lcomp_init`.
 * - COFF: the table itself in section `.lcinit$m`.
 * - WebAssembly: this is not loader discovery. A module has no loader to
 *   enumerate, and wasm-ld defines no bounds of a custom section. Instead the
 *   linker's constructor order is the contract: wasm-ld puts every object
 *   file's constructors into the one `__wasm_call_ctors`, sorted by
 *   priority, and priorities up to 100 are reserved to the implementation.
 *   Each object file adds its table with `_lcompilers_init_add_records` from
 *   a constructor of priority `lcompilers_init_wasm_publish_priority`, and
 *   the runtime marks publication complete from one of priority
 *   `lcompilers_init_wasm_published_priority`, after every publisher and
 *   before any constructor user code can have. The engine refuses to
 *   dispatch before that mark: a startup that runs earlier -- or a host that
 *   calls into the module before running its constructors, which WASI and
 *   Emscripten both require first -- is a fatal error, never a dispatch over
 *   a partial set.
 *
 * Each object file also has one constructor, which passes the address of its
 * table to `_lcompilers_init_ctor`, and one destructor, which passes it to
 * `_lcompilers_init_unload`. These two alone are a complete, if later,
 * registration: a table is discovered from its constructor on, on every
 * object format, which is all an object file has that was built from records
 * without the form above (LLVM IR that `--show-llvm` prints, compiled by
 * another tool). The form above is what makes the tables of every loaded
 * image known before any of their constructors runs: the engine finds them
 * itself -- through the program headers with `dl_iterate_phdr`, through the
 * notifications dyld gives for every image it maps and unmaps, or through the
 * module list -- so the first constructor already sees all of them -- except
 * on Windows, where a DLL's table is taken in only once its own constructor
 * has run; see lcompilers_init.h.
 */

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

enum {
    lcompilers_init_abi_version = 3
};

/* The value of an initializer's state word. */
enum {
    lcompilers_init_uninitialized = 0,
    lcompilers_init_initializing = 1,
    lcompilers_init_ready = 2
};

/* `lcompilers_init_record::flags`: at most one of them. */
enum {
    /* Only a collective startup boundary may initialize this record. */
    lcompilers_init_collective = 1,
    /* A collective bootstrap: `ensure` is not guarded and `state` and
     * `teardown` are NULL. A collective boundary runs a bootstrap after the
     * local records and before any collective one, outside every
     * initializer and without the engine's lock, so that it can start a
     * runtime -- PRIF -- that collective initializers use and that may
     * itself load images or enter startup. It starts a runtime of the
     * process: one of a stable id is passed over while a table whose
     * bootstrap of that id ran is still live, and runs again once none
     * is. */
    lcompilers_init_bootstrap = 2
};

/* The argument of `_lcompilers_init_dispatch`. */
enum {
    lcompilers_init_dispatch_local = 0,
    lcompilers_init_dispatch_collective = 1
};

enum {
    lcompilers_init_elf_note_type = 1
};

/* The value of a table's instance word. */
enum {
    lcompilers_init_instance_fresh = 0,
    lcompilers_init_instance_adopted = 1,
    lcompilers_init_instance_retired = 2
};

/* WebAssembly: the constructor priorities that publish an object's table,
 * and that mark every table published. */
enum {
    lcompilers_init_wasm_publish_priority = 1,
    lcompilers_init_wasm_published_priority = 2
};

static const char lcompilers_init_elf_note_section[] = ".note.lcompilers.init";
static const char lcompilers_init_elf_note_owner[] = "LCP";
static const char lcompilers_init_macho_segment[] = "__DATA";
static const char lcompilers_init_macho_section[] = "__lcomp_init";
static const char lcompilers_init_coff_section[] = ".lcinit$m";
static const char lcompilers_init_coff_image_section[] = ".lcinit";

typedef struct lcompilers_init_record {
    /* The ASR owner's stable id: the same on every image and in every
     * compilation, and the key of the order roots are initialized in. */
    const char *stable_id;
    /* The owner's guarded initializer. It returns only once `*state` is
     * `lcompilers_init_ready`; otherwise the process has terminated. */
    void (*ensure)(void);
    /* Frees what the owner's storage owns, or NULL. It is run only for a
     * record whose state is ready. */
    void (*teardown)(void);
    /* The owner's state word, the one `ensure` guards; NULL for a
     * bootstrap. */
    int32_t *state;
    uint32_t flags;
    uint32_t reserved;
} lcompilers_init_record;

typedef struct lcompilers_init_table {
    uint32_t abi_version;   /* lcompilers_init_abi_version, never 0 */
    uint32_t count;
    const lcompilers_init_record *records;
    /* A private, zero-initialized word of the object's own writable data:
     * `lcompilers_init_instance_fresh` in every new mapping of its image,
     * `lcompilers_init_instance_adopted` once the engine has taken that
     * mapping's table in, `lcompilers_init_instance_retired` once that
     * mapping was unloaded, which it stays however long the loader still
     * lists it. The engine changes it, only in that order. Required for a
     * table a loader lists; ignored for a batch the host adds. */
    uint32_t *instance;
} lcompilers_init_table;

/* The ELF note of one object file: owner "LCP", type
 * `lcompilers_init_elf_note_type`, and a descriptor of the offset of the
 * object's table from the note. Emitted in
 * `lcompilers_init_elf_note_section`, 4-byte aligned, allocated and
 * read-only; `table_offset` is a link-time constant, which C cannot spell as
 * an initializer, so C emits the note with assembler directives. */
typedef struct lcompilers_init_elf_note {
    uint32_t namesz;        /* sizeof(lcompilers_init_elf_note_owner) */
    uint32_t descsz;        /* sizeof(intptr_t) */
    uint32_t type;          /* lcompilers_init_elf_note_type */
    char name[4];           /* "LCP" */
    intptr_t table_offset;  /* (char *)table - (char *)&namesz */
} lcompilers_init_elf_note;

#ifdef __cplusplus
}
#endif

#endif /* LCOMPILERS_INIT_ABI_H */
