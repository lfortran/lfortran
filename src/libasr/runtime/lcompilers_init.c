/*
 * The startup initialization engine; see lcompilers_init.h for the protocol
 * and lcompilers_init_abi.h for the records it discovers.
 *
 * The engine keeps no permanent "done" state. Whether a definition is
 * initialized is the state word inside the image that defines it, so a
 * definition in an image loaded, unloaded and loaded again starts out
 * uninitialized again, and one in an image that stayed loaded stays ready.
 * What the engine caches is only the load generation it last dispatched,
 * which every image load, unload and host batch changes.
 *
 * Lifetimes: a table, its records, their ids and the code and state words
 * they point to belong to an image or a host batch, and can be unmapped as
 * soon as the image's `_lcompilers_init_unload` or the batch's
 * `_lcompilers_init_remove_records` has returned. The engine therefore
 * reads none of it except under a lease on the table: an entry of the
 * engine's own registry, one per table address, counts the leases taken on
 * it, and withdrawing the table retires the entry and then waits for the
 * leases other threads hold on that table -- on that table alone, so that
 * unloading one image never waits for an initializer of another. What a
 * dispatch sorts and compares is copied out under the lease. A retired
 * entry stays as a tombstone: an image can still be listed by the loader
 * while its destructors run, after its unload returned, so a table the
 * loader lists is used only while its entry is live, and the entry comes
 * back to life only through a genuine new load or publication -- the
 * image's constructor, a loader notification of the load, or
 * `_lcompilers_init_add_records`.
 *
 * Locking: the initialization lock, taken recursively, is held from
 * `_lcompilers_init_begin` to the matching `_lcompilers_init_end`, and by
 * `_lcompilers_init_teardown_all`. The registry lock and the completion log
 * lock are leaves: nothing is called while either is held, least of all the
 * loader, so a loader callback may take them. Loader state is never
 * enumerated with the initialization lock held: a dispatch enumerates first
 * and takes the lock only through the initializers it calls, and a thread
 * that already holds it does not dispatch at all. Withdrawing a table does
 * not take the initialization lock.
 */

#if defined(__linux__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <libasr/runtime/lfortran_intrinsics.h>

#if defined(COMPILE_TO_WASM)
#elif defined(_WIN32)
#include <windows.h>
#include <psapi.h>
#elif defined(__APPLE__)
#include <pthread.h>
#include <sched.h>
#include <dlfcn.h>
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#include <mach-o/loader.h>
#else
#include <pthread.h>
#include <sched.h>
#include <dlfcn.h>
#include <link.h>
#include <elf.h>
#endif

/* ------------------------------------------------------------------------
 * Atomics and the locks
 * ------------------------------------------------------------------------ */

static int32_t lcompilers_init_load_acquire(const int32_t *p) {
#if defined(_MSC_VER)
    return InterlockedCompareExchange((volatile LONG *)p, 0, 0);
#else
    return __atomic_load_n(p, __ATOMIC_ACQUIRE);
#endif
}

static void lcompilers_init_store_release(int32_t *p, int32_t v) {
#if defined(_MSC_VER)
    InterlockedExchange((volatile LONG *)p, v);
#else
    __atomic_store_n(p, v, __ATOMIC_RELEASE);
#endif
}

/* Sets `*p` from 0 to 1; whether this call did. */
static int lcompilers_init_claim(int32_t *p) {
#if defined(_MSC_VER)
    return InterlockedCompareExchange((volatile LONG *)p, 1, 0) == 0;
#else
    int32_t expected = 0;
    return __atomic_compare_exchange_n(p, &expected, 1, 0, __ATOMIC_ACQ_REL,
        __ATOMIC_ACQUIRE);
#endif
}

static uint64_t lcompilers_init_load_u64(const uint64_t *p) {
#if defined(_MSC_VER)
    return (uint64_t)InterlockedCompareExchange64((volatile LONG64 *)p, 0, 0);
#else
    return __atomic_load_n(p, __ATOMIC_ACQUIRE);
#endif
}

static void lcompilers_init_store_u64(uint64_t *p, uint64_t v) {
#if defined(_MSC_VER)
    InterlockedExchange64((volatile LONG64 *)p, (LONG64)v);
#else
    __atomic_store_n(p, v, __ATOMIC_RELEASE);
#endif
}

static void lcompilers_init_increment_u64(uint64_t *p) {
#if defined(_MSC_VER)
    InterlockedIncrement64((volatile LONG64 *)p);
#else
    __atomic_add_fetch(p, 1, __ATOMIC_ACQ_REL);
#endif
}

static void lcompilers_init_yield(void) {
#if defined(COMPILE_TO_WASM)
#elif defined(_WIN32)
    SwitchToThread();
#else
    sched_yield();
#endif
}

/* A lease this thread holds on a registry entry; see
 * `lcompilers_init_lease_acquire`. They form a stack, in the frames of the
 * functions that hold them. */
typedef struct lcompilers_init_lease {
    size_t entry;
    struct lcompilers_init_lease *next;
} lcompilers_init_lease;

/* The thread's own identity, the address of a thread local. */
#if defined(COMPILE_TO_WASM)
static int lcompilers_init_thread_token;
static lcompilers_init_lease *lcompilers_init_own_leases;
/* How deep in dispatches this thread is, and whether it runs the
 * collective phase. */
static int lcompilers_init_dispatch_depth;
static int lcompilers_init_collective_active;
/* Whether the bootstraps of the collective boundary this thread runs are
 * done, so that collective records may run. */
static int lcompilers_init_bootstraps_ran;
#elif defined(_MSC_VER)
static __declspec(thread) int lcompilers_init_thread_token;
static __declspec(thread) lcompilers_init_lease *lcompilers_init_own_leases;
static __declspec(thread) int lcompilers_init_dispatch_depth;
static __declspec(thread) int lcompilers_init_collective_active;
static __declspec(thread) int lcompilers_init_bootstraps_ran;
#else
static _Thread_local int lcompilers_init_thread_token;
static _Thread_local lcompilers_init_lease *lcompilers_init_own_leases;
static _Thread_local int lcompilers_init_dispatch_depth;
static _Thread_local int lcompilers_init_collective_active;
static _Thread_local int lcompilers_init_bootstraps_ran;
#endif

#if defined(COMPILE_TO_WASM)
#elif defined(_WIN32)
static SRWLOCK lcompilers_init_mutex = SRWLOCK_INIT;
static SRWLOCK lcompilers_init_registry_mutex = SRWLOCK_INIT;
static SRWLOCK lcompilers_init_log_mutex = SRWLOCK_INIT;
#else
static pthread_mutex_t lcompilers_init_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t lcompilers_init_registry_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t lcompilers_init_log_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

/* The thread holding the initialization lock, and how many times it has
 * taken it. */
static uint64_t lcompilers_init_owner;
static int lcompilers_init_depth;

static uint64_t lcompilers_init_self(void) {
    return (uint64_t)(uintptr_t)&lcompilers_init_thread_token;
}

static int lcompilers_init_lock_held(void) {
    return lcompilers_init_load_u64(&lcompilers_init_owner) == lcompilers_init_self();
}

static void lcompilers_init_lock(void) {
    if (lcompilers_init_lock_held()) {
        lcompilers_init_depth++;
        return;
    }
#if defined(COMPILE_TO_WASM)
#elif defined(_WIN32)
    AcquireSRWLockExclusive(&lcompilers_init_mutex);
#else
    pthread_mutex_lock(&lcompilers_init_mutex);
#endif
    lcompilers_init_store_u64(&lcompilers_init_owner, lcompilers_init_self());
    lcompilers_init_depth = 1;
}

static void lcompilers_init_unlock(void) {
    if (--lcompilers_init_depth > 0) return;
    lcompilers_init_store_u64(&lcompilers_init_owner, 0);
#if defined(COMPILE_TO_WASM)
#elif defined(_WIN32)
    ReleaseSRWLockExclusive(&lcompilers_init_mutex);
#else
    pthread_mutex_unlock(&lcompilers_init_mutex);
#endif
}

static void lcompilers_init_registry_lock(void) {
#if defined(COMPILE_TO_WASM)
#elif defined(_WIN32)
    AcquireSRWLockExclusive(&lcompilers_init_registry_mutex);
#else
    pthread_mutex_lock(&lcompilers_init_registry_mutex);
#endif
}

static void lcompilers_init_registry_unlock(void) {
#if defined(COMPILE_TO_WASM)
#elif defined(_WIN32)
    ReleaseSRWLockExclusive(&lcompilers_init_registry_mutex);
#else
    pthread_mutex_unlock(&lcompilers_init_registry_mutex);
#endif
}

#if !defined(COMPILE_TO_WASM) && !defined(_WIN32) && !defined(__APPLE__)
static int lcompilers_init_registry_trylock(void) {
    return pthread_mutex_trylock(&lcompilers_init_registry_mutex) == 0;
}
#endif

static void lcompilers_init_log_lock(void) {
#if defined(COMPILE_TO_WASM)
#elif defined(_WIN32)
    AcquireSRWLockExclusive(&lcompilers_init_log_mutex);
#else
    pthread_mutex_lock(&lcompilers_init_log_mutex);
#endif
}

static void lcompilers_init_log_unlock(void) {
#if defined(COMPILE_TO_WASM)
#elif defined(_WIN32)
    ReleaseSRWLockExclusive(&lcompilers_init_log_mutex);
#else
    pthread_mutex_unlock(&lcompilers_init_log_mutex);
#endif
}

static void lcompilers_init_fail(const char *message) {
    fflush(stdout);
    fprintf(stderr, "error: startup initialization: %s\n", message);
    exit(1);
}

/* A failure where exit handlers may already be running -- in a destructor,
 * or with a leaf lock held that a destructor would take again: the process
 * ends at once. */
static void lcompilers_init_fail_now(const char *message) {
    fflush(stdout);
    fprintf(stderr, "error: startup initialization: %s\n", message);
    fflush(stderr);
    _Exit(1);
}

static void *lcompilers_init_grow(void *p, size_t *capacity, size_t count,
        size_t element) {
    if (count < *capacity) return p;
    size_t grown = *capacity == 0 ? 16 : *capacity * 2;
    if (grown < *capacity || grown > SIZE_MAX / element) {
        lcompilers_init_fail_now("too many initializers");
    }
    void *q = realloc(p, grown * element);
    if (q == NULL) lcompilers_init_fail_now("out of memory");
    *capacity = grown;
    return q;
}

/* ------------------------------------------------------------------------
 * The registry and its leases
 * ------------------------------------------------------------------------ */

typedef struct {
    const lcompilers_init_table *table;
    /* Changes whenever the entry is retired or comes back to life, so that
     * what was discovered before cannot be leased afterwards. */
    uint64_t incarnation;
    /* The leases held on it, by every thread. */
    int64_t leases;
    int retired;
    /* Mach-O: whether dyld lists the image, as its notifications told. */
    int listed;
    /* Whether the image's own constructor has run since it was last
     * retired, so that its load completed. Such a table is live until its
     * destructor retires it, whether or not the loader lists it: an object
     * file built from the object-format-independent records (see
     * lcompilers_init_abi.h) is found this way alone. */
    int constructed;
} lcompilers_init_entry;

/* All of it is protected by the registry lock. An entry is never removed,
 * so its index names it for good. */
static lcompilers_init_entry *lcompilers_init_entries;
static size_t lcompilers_init_entry_count, lcompilers_init_entry_capacity;
/* Bumped whenever an entry is retired or comes back to life, is taken in
 * by its image's constructor, or a host batch is added or removed. Written
 * and read with the registry lock held. */
static uint64_t lcompilers_init_registry_generation;

/* With the registry lock held. */
static void lcompilers_init_registry_changed(void) {
    lcompilers_init_store_u64(&lcompilers_init_registry_generation,
        lcompilers_init_registry_generation + 1);
}
/* The tables of the batches the host added. */
static const lcompilers_init_table **lcompilers_init_host_tables;
static size_t lcompilers_init_host_count, lcompilers_init_host_capacity;

/* The index of `table`'s entry, or SIZE_MAX. With the registry lock held. */
static size_t lcompilers_init_find_entry(const lcompilers_init_table *table) {
    for (size_t i = 0; i < lcompilers_init_entry_count; i++) {
        if (lcompilers_init_entries[i].table == table) return i;
    }
    return SIZE_MAX;
}

/* The index of `table`'s entry, a live one made if there is none yet. With
 * the registry lock held. */
static size_t lcompilers_init_entry_of(const lcompilers_init_table *table) {
    size_t found = lcompilers_init_find_entry(table);
    if (found != SIZE_MAX) return found;
    lcompilers_init_entries = (lcompilers_init_entry *)lcompilers_init_grow(
        (void *)lcompilers_init_entries, &lcompilers_init_entry_capacity,
        lcompilers_init_entry_count, sizeof(*lcompilers_init_entries));
    lcompilers_init_entry *e = &lcompilers_init_entries[lcompilers_init_entry_count];
    e->table = table;
    e->incarnation = 1;
    e->leases = 0;
    e->retired = 0;
    e->listed = 0;
    e->constructed = 0;
    return lcompilers_init_entry_count++;
}

/* With the registry lock held. */
static void lcompilers_init_revive_locked(size_t i) {
    lcompilers_init_entry *e = &lcompilers_init_entries[i];
    if (!e->retired) return;
    e->retired = 0;
    e->incarnation++;
    lcompilers_init_registry_changed();
}

/* With the registry lock held. */
static void lcompilers_init_retire_locked(size_t i) {
    lcompilers_init_entry *e = &lcompilers_init_entries[i];
    e->constructed = 0;
    if (!e->retired) {
        e->retired = 1;
        e->incarnation++;
    }
    lcompilers_init_registry_changed();
}

/* Takes a lease on entry `entry` as it was in `incarnation`; 0 when it has
 * been retired since, and nothing of its table may be read. */
static int lcompilers_init_lease_acquire(lcompilers_init_lease *lease,
        size_t entry, uint64_t incarnation) {
    lcompilers_init_registry_lock();
    lcompilers_init_entry *e = &lcompilers_init_entries[entry];
    int live = !e->retired && e->incarnation == incarnation;
    if (live) e->leases++;
    lcompilers_init_registry_unlock();
    if (!live) return 0;
    lease->entry = entry;
    lease->next = lcompilers_init_own_leases;
    lcompilers_init_own_leases = lease;
    return 1;
}

static void lcompilers_init_lease_release(lcompilers_init_lease *lease) {
    if (lcompilers_init_own_leases != lease) {
        lcompilers_init_fail_now("a lease was released out of order");
    }
    lcompilers_init_own_leases = lease->next;
    lcompilers_init_registry_lock();
    lcompilers_init_entries[lease->entry].leases--;
    lcompilers_init_registry_unlock();
}

/* Waits until no other thread holds a lease on retired entry `entry`. A
 * lease of this thread's own is one it holds further up its stack, around
 * the initializer or teardown that withdraws the table. */
static void lcompilers_init_wait_for_leases(size_t entry) {
    int64_t own = 0;
    for (lcompilers_init_lease *l = lcompilers_init_own_leases; l != NULL; l = l->next) {
        if (l->entry == entry) own++;
    }
    for (;;) {
        lcompilers_init_registry_lock();
        int64_t others = lcompilers_init_entries[entry].leases - own;
        lcompilers_init_registry_unlock();
        if (others == 0) return;
        if (lcompilers_init_lock_held()) {
            /* A thread holding a lease on it can be waiting for this
             * thread's initialization lock. */
            lcompilers_init_fail_now("records were withdrawn from inside an "
                "initializer while another thread may still be running "
                "their initializers");
        }
        lcompilers_init_yield();
    }
}

/* ------------------------------------------------------------------------
 * Generations
 * ------------------------------------------------------------------------ */

/* What changes whenever an image or a host batch comes or goes. */
typedef struct {
    uint64_t registry;
    uint64_t images;
} lcompilers_init_generation_t;

/* The generation of the last dispatch that completed, under the registry
 * lock. */
static lcompilers_init_generation_t lcompilers_init_dispatched;
static int lcompilers_init_dispatched_valid;

/* The engine's registry and the loader notifications it installs live as
 * long as the process: the image that holds the engine -- the shared runtime,
 * or whatever a static runtime is linked into -- is pinned the first time the
 * engine registers a notification, so no later unload can leave the loader
 * calling into unmapped code. */
#if defined(__APPLE__) && !defined(COMPILE_TO_WASM)
static int32_t lcompilers_init_engine_pinned;

static void lcompilers_init_pin_engine(void) {
    if (lcompilers_init_load_acquire(&lcompilers_init_engine_pinned) != 0) return;
    Dl_info info;
    if (dladdr((const void *)&lcompilers_init_pin_engine, &info) == 0
            || info.dli_fname == NULL || info.dli_fbase == NULL) {
        lcompilers_init_fail("the image of the startup engine cannot be kept "
            "loaded");
    }
    /* The program itself is never unloaded. */
    if (((const struct mach_header *)info.dli_fbase)->filetype != MH_EXECUTE
            && dlopen(info.dli_fname, RTLD_NOLOAD | RTLD_NODELETE) == NULL) {
        lcompilers_init_fail("the image of the startup engine cannot be kept "
            "loaded");
    }
    lcompilers_init_store_release(&lcompilers_init_engine_pinned, 1);
}
#elif defined(_WIN32) && !defined(COMPILE_TO_WASM)
static int32_t lcompilers_init_engine_pinned;

static void lcompilers_init_pin_engine(void) {
    if (lcompilers_init_load_acquire(&lcompilers_init_engine_pinned) != 0) return;
    HMODULE module = NULL;
    if (!GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS
            | GET_MODULE_HANDLE_EX_FLAG_PIN,
            (LPCSTR)(void *)&lcompilers_init_pin_engine, &module)) {
        lcompilers_init_fail("the image of the startup engine cannot be kept "
            "loaded");
    }
    lcompilers_init_store_release(&lcompilers_init_engine_pinned, 1);
}
#endif

/* ------------------------------------------------------------------------
 * Discovery
 * ------------------------------------------------------------------------ */

/* A table discovered live: which entry, in which incarnation. */
typedef struct {
    const lcompilers_init_table *table;
    size_t entry;
    uint64_t incarnation;
} lcompilers_init_found;

/* A record as copied out under its table's lease. */
typedef struct {
    char *stable_id;
    void (*ensure)(void);
    uint32_t flags;
    size_t entry;
    uint64_t incarnation;
    size_t order;
} lcompilers_init_root;

typedef struct {
    lcompilers_init_found *items;
    size_t count, capacity;
} lcompilers_init_snapshot;

/* Adds live entry `i`, with the registry lock held; allocates nothing when
 * `out` has room, as the scan of the loaded images makes sure it has. */
static void lcompilers_init_snapshot_add(lcompilers_init_snapshot *out, size_t i) {
    const lcompilers_init_entry *e = &lcompilers_init_entries[i];
    if (e->retired) return;
    for (size_t k = 0; k < out->count; k++) {
        if (out->items[k].entry == i) return;
    }
    out->items = (lcompilers_init_found *)lcompilers_init_grow(
        (void *)out->items, &out->capacity, out->count, sizeof(*out->items));
    out->items[out->count].table = e->table;
    out->items[out->count].entry = i;
    out->items[out->count].incarnation = e->incarnation;
    out->count++;
}

#if !defined(COMPILE_TO_WASM)
/* A table the loader lists, of an image known to stay mapped meanwhile,
 * with the registry lock held: its entry, added to `out` unless the image
 * was unloaded. The table's instance word tells what this mapping of the
 * image is. An address is reused only after an unmap, and a mapping the
 * engine took in is unmapped only after its destructor retired it, so a
 * fresh word is a new mapping -- whose entry is created, or revived from
 * the tombstone of an earlier mapping at that address, whether or not its
 * constructor has run -- and a retired word is a dying one, which the
 * loader may still list after its unload. Called where the loader holds its
 * lock too: it calls nothing of the loader and fails without exit
 * handlers, which would take the registry lock again. */
static void lcompilers_init_adopt_locked(const lcompilers_init_table *t,
        lcompilers_init_snapshot *out) {
    if (t->abi_version != lcompilers_init_abi_version) {
        lcompilers_init_fail_now("an image was compiled for another version "
            "of the startup initialization ABI");
    }
    if (t->instance == NULL) {
        lcompilers_init_fail_now("an initialization table of an image is "
            "malformed");
    }
    int existed = lcompilers_init_find_entry(t) != SIZE_MAX;
    size_t i = lcompilers_init_entry_of(t);
    uint32_t instance = (uint32_t)lcompilers_init_load_acquire((const int32_t *)t->instance);
    if (instance == lcompilers_init_instance_retired) return;
    if (instance == lcompilers_init_instance_fresh) {
        lcompilers_init_entry *e = &lcompilers_init_entries[i];
        if (e->retired) {
            lcompilers_init_revive_locked(i);
        } else if (existed) {
            /* A live entry for a new mapping: an earlier one went away
             * without its destructor, which no loader the engine supports
             * does. Whatever was copied from it or leased is stale. */
            e->incarnation++;
            lcompilers_init_registry_changed();
        }
        lcompilers_init_store_release((int32_t *)t->instance,
            lcompilers_init_instance_adopted);
    } else if (instance != lcompilers_init_instance_adopted
            || lcompilers_init_entries[i].retired) {
        lcompilers_init_fail_now("the instance word of an initialization "
            "table is inconsistent with the engine's registry");
    }
    if (out != NULL) lcompilers_init_snapshot_add(out, i);
}
#endif


#if !defined(COMPILE_TO_WASM) && (defined(_WIN32) || defined(__APPLE__))
/* The tables in `size` bytes of a section at `data` that object files
 * contribute to, each one `lcompilers_init_table`, passed to `visit`. The
 * linker aligns every contribution and may pad between them, or after the
 * last one up to the section's size, with zeros; a table's `abi_version` is
 * never 0, so a zero word is padding. Only for a section of an image that
 * stays mapped meanwhile. */
static void lcompilers_init_scan_section(const unsigned char *data, size_t size,
        void (*visit)(const lcompilers_init_table *, void *), void *context) {
    size_t off = 0;
    while (off + sizeof(lcompilers_init_table) <= size) {
        const lcompilers_init_table *t = (const lcompilers_init_table *)(data + off);
        if (t->abi_version == 0) {
            off += sizeof(void *);
            continue;
        }
        visit(t, context);
        off += sizeof(lcompilers_init_table);
    }
}

#endif


#if defined(COMPILE_TO_WASM)
/* Every object file publishes its table from a constructor that runs before
 * this one, and this one before any that can dispatch; see
 * lcompilers_init_abi.h. */
static int lcompilers_init_published;

__attribute__((constructor(lcompilers_init_wasm_published_priority)))
static void lcompilers_init_mark_published(void) {
    lcompilers_init_published = 1;
}

static void lcompilers_init_image_generation(lcompilers_init_generation_t *g) {
    g->images = 0;
}

static void lcompilers_init_discover_images(lcompilers_init_snapshot *out) {
    (void)out;
    if (!lcompilers_init_published) {
        lcompilers_init_fail("startup was entered before the constructors "
            "that publish every object file's initialization records ran: "
            "call the module's constructors (_start, _initialize or "
            "__wasm_call_ctors) before calling into it");
    }
}
#elif defined(_WIN32)
/* 0 before the notifications were asked for, 2 once the loader delivers
 * them and 3 when it cannot. */
static int32_t lcompilers_init_watching;
static uint64_t lcompilers_init_images_changed;

typedef LONG (NTAPI *lcompilers_ldr_register_t)(ULONG, PVOID, PVOID, PVOID *);

static VOID CALLBACK lcompilers_init_dll_changed(ULONG reason,
        const void *data, void *context) {
    (void)reason;
    (void)data;
    (void)context;
    lcompilers_init_increment_u64(&lcompilers_init_images_changed);
}

/* Nothing waits for another thread's registration: a thread that finds none
 * made registers its own, and a second one only counts every change
 * twice. */
static void lcompilers_init_watch_images(void) {
    if (lcompilers_init_load_acquire(&lcompilers_init_watching) != 0) return;
    lcompilers_init_pin_engine();
    HMODULE ntdll = GetModuleHandleA("ntdll.dll");
    lcompilers_ldr_register_t reg = ntdll == NULL ? NULL
        : (lcompilers_ldr_register_t)(void *)GetProcAddress(ntdll,
            "LdrRegisterDllNotification");
    void *cookie = NULL;
    int notified = reg != NULL
        && reg(0, (PVOID)lcompilers_init_dll_changed, NULL, &cookie) == 0;
    lcompilers_init_store_release(&lcompilers_init_watching, notified ? 2 : 3);
}

/* The loaded modules, however many there are at the time of the call. */
static HMODULE *lcompilers_init_modules(DWORD *count) {
    HMODULE *modules = NULL;
    DWORD capacity = 0, needed = 0;
    /* A module can be loaded between two calls, so ask until the list the
     * call wrote is the whole list. */
    for (;;) {
        if (!K32EnumProcessModules(GetCurrentProcess(), modules,
                capacity * sizeof(HMODULE), &needed)) {
            free(modules);
            lcompilers_init_fail("the loaded modules cannot be enumerated");
        }
        if (needed <= capacity * sizeof(HMODULE)) break;
        capacity = needed / sizeof(HMODULE) + 16;
        HMODULE *grown = (HMODULE *)realloc(modules, capacity * sizeof(HMODULE));
        if (grown == NULL) {
            free(modules);
            lcompilers_init_fail("out of memory");
        }
        modules = grown;
    }
    *count = needed / sizeof(HMODULE);
    return modules;
}

static void lcompilers_init_image_generation(lcompilers_init_generation_t *g) {
    lcompilers_init_watch_images();
    if (lcompilers_init_load_acquire(&lcompilers_init_watching) == 2) {
        g->images = lcompilers_init_load_u64(&lcompilers_init_images_changed);
        return;
    }
    /* Without notifications, the set of modules loaded now: a module
     * unloaded and loaded again at its address is told apart by its
     * constructor, which revives its entry. */
    DWORD n = 0;
    HMODULE *modules = lcompilers_init_modules(&n);
    uint64_t h = 1469598103934665603ULL;
    for (DWORD i = 0; i < n; i++) {
        h = (h ^ (uint64_t)(uintptr_t)modules[i]) * 1099511628211ULL;
    }
    free(modules);
    g->images = (h ^ n) * 1099511628211ULL;
}

typedef struct {
    lcompilers_init_snapshot *out;
    int executable;
} lcompilers_init_windows_scan;

/* The module list also holds a DLL another thread's LoadLibrary is still
 * loading -- its imports maybe not yet bound -- and a load that then fails
 * unmaps it without detaching it, so without its destructors. So a DLL's
 * table is taken in only once its own constructor has run, which the CRT
 * does in the DLL's attach, after binding; the executable's is always
 * complete. That loses nothing: a DLL's constructor declines to dispatch,
 * and every dispatch comes later, from the executable's constructor, a
 * Fortran main program or lfortran_initialize(). */
static void lcompilers_init_visit_windows(const lcompilers_init_table *t,
        void *context) {
    lcompilers_init_windows_scan *scan = (lcompilers_init_windows_scan *)context;
    if (!scan->executable
            && !lcompilers_init_entries[lcompilers_init_entry_of(t)].constructed) {
        return;
    }
    lcompilers_init_adopt_locked(t, scan->out);
}

static void lcompilers_init_discover_images(lcompilers_init_snapshot *out) {
    DWORD n = 0;
    HMODULE *modules = lcompilers_init_modules(&n);
    for (DWORD i = 0; i < n; i++) {
        /* A reference of its own keeps the module mapped while its headers
         * are read; one unloaded since it was listed is not found. A module
         * whose load completed leaves only through its destructors, which
         * retire its tables and wait for their leases. */
        HMODULE held = NULL;
        if (!GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS,
                (LPCSTR)(void *)modules[i], &held)) {
            continue;
        }
        const unsigned char *base = (const unsigned char *)held;
        const IMAGE_DOS_HEADER *dos = (const IMAGE_DOS_HEADER *)base;
        const IMAGE_NT_HEADERS *nt = (const IMAGE_NT_HEADERS *)(base + dos->e_lfanew);
        lcompilers_init_windows_scan scan;
        scan.out = out;
        scan.executable = held == GetModuleHandleA(NULL);
        lcompilers_init_registry_lock();
        if (dos->e_magic == IMAGE_DOS_SIGNATURE && nt->Signature == IMAGE_NT_SIGNATURE) {
            const IMAGE_SECTION_HEADER *section = IMAGE_FIRST_SECTION(nt);
            for (WORD s = 0; s < nt->FileHeader.NumberOfSections; s++, section++) {
                char name[IMAGE_SIZEOF_SHORT_NAME + 1];
                memcpy(name, section->Name, IMAGE_SIZEOF_SHORT_NAME);
                name[IMAGE_SIZEOF_SHORT_NAME] = 0;
                if (strcmp(name, lcompilers_init_coff_image_section) != 0) continue;
                /* The logical size: the raw data is padded to the file
                 * alignment. */
                lcompilers_init_scan_section(base + section->VirtualAddress,
                    section->Misc.VirtualSize, lcompilers_init_visit_windows, &scan);
            }
        }
        lcompilers_init_registry_unlock();
        FreeLibrary(held);
    }
    free(modules);
}

typedef BOOLEAN (NTAPI *lcompilers_rtl_shutdown_t)(void);

/* Whether the process is exiting: its other threads have been ended,
 * possibly holding a lease or a lock, and nothing of it will run again. */
static int lcompilers_init_process_ending(void) {
    HMODULE ntdll = GetModuleHandleA("ntdll.dll");
    lcompilers_rtl_shutdown_t ending = ntdll == NULL ? NULL
        : (lcompilers_rtl_shutdown_t)(void *)GetProcAddress(ntdll,
            "RtlDllShutdownInProgress");
    return ending != NULL && ending();
}
#elif defined(__APPLE__)
/* The tables of the image whose header is `header`, passed to `visit`. */
static void lcompilers_init_macho_tables(const struct mach_header *header,
        void (*visit)(const lcompilers_init_table *, void *), void *context) {
    static const char *const segments[] = {"__DATA", "__DATA_CONST"};
    if (header == NULL || header->magic != MH_MAGIC_64) return;
    for (size_t s = 0; s < sizeof(segments) / sizeof(segments[0]); s++) {
        unsigned long size = 0;
        /* Already slid: the address in the image as loaded. */
        const uint8_t *data = getsectiondata(
            (const struct mach_header_64 *)header, segments[s],
            lcompilers_init_macho_section, &size);
        if (data != NULL) lcompilers_init_scan_section(data, size, visit, context);
    }
}

/* dyld tells of every image it maps before running any of the image's
 * initializers, and of every image it unmaps before doing so, holding its
 * own lock meanwhile, so a table is read here while it is mapped. The
 * registry is all the discovery there is: dyld's list of images can change
 * under a reader. That dyld never unmaps an image it told of without
 * running its terminators -- a load failing after the notification -- is
 * assumed, not verified. The notifications take the registry lock and may
 * allocate, holding dyld's lock: an allocator that enters dyld while a
 * thread holds the registry lock -- one that unwinds the stack on every
 * allocation, say -- is not supported. */
static void lcompilers_init_listed(const lcompilers_init_table *t, void *context) {
    (void)context;
    lcompilers_init_adopt_locked(t, NULL);
    lcompilers_init_entries[lcompilers_init_entry_of(t)].listed = 1;
    lcompilers_init_registry_changed();
}

static void lcompilers_init_unlisted(const lcompilers_init_table *t, void *context) {
    (void)context;
    size_t i = lcompilers_init_entry_of(t);
    lcompilers_init_entries[i].listed = 0;
    lcompilers_init_retire_locked(i);
}

static void lcompilers_init_image_added(const struct mach_header *header,
        intptr_t slide) {
    (void)slide;
    lcompilers_init_registry_lock();
    lcompilers_init_macho_tables(header, lcompilers_init_listed, NULL);
    lcompilers_init_registry_unlock();
}

static void lcompilers_init_image_removed(const struct mach_header *header,
        intptr_t slide) {
    (void)slide;
    lcompilers_init_registry_lock();
    lcompilers_init_macho_tables(header, lcompilers_init_unlisted, NULL);
    lcompilers_init_registry_unlock();
}

/* Set once some thread's notifications are registered, by which time dyld
 * has reported to them every image loaded then. */
static int32_t lcompilers_init_watching;

/* Nothing waits for another thread's registration, which can itself be
 * waiting for dyld's lock, held by this thread in an initializer: a thread
 * that finds none made registers its own, and dyld reports every image
 * loaded already to it before the call returns. A second registration only
 * reports every image twice, which the registry takes as once. */
static void lcompilers_init_watch_images(void) {
    if (lcompilers_init_load_acquire(&lcompilers_init_watching) != 0) return;
    lcompilers_init_pin_engine();
    /* Removals first: no image reported added can then go unreported when
     * it is removed. */
    _dyld_register_func_for_remove_image(lcompilers_init_image_removed);
    _dyld_register_func_for_add_image(lcompilers_init_image_added);
    lcompilers_init_store_release(&lcompilers_init_watching, 1);
}

/* As early as the engine's own image starts: then no first dispatch has to
 * register, whichever image it comes from. */
__attribute__((constructor)) static void lcompilers_init_watch_early(void) {
    lcompilers_init_watch_images();
}

static void lcompilers_init_image_generation(lcompilers_init_generation_t *g) {
    /* Every change of the listed tables changes the registry's
     * generation. */
    lcompilers_init_watch_images();
    g->images = 0;
}

static void lcompilers_init_discover_images(lcompilers_init_snapshot *out) {
    (void)out;
    lcompilers_init_watch_images();
}
#else
static int lcompilers_init_count_loads(struct dl_phdr_info *info, size_t size,
        void *data) {
    (void)size;
    uint64_t *out = (uint64_t *)data;
    *out = (uint64_t)info->dlpi_adds + (uint64_t)info->dlpi_subs;
    return 1;
}

static void lcompilers_init_image_generation(lcompilers_init_generation_t *g) {
    uint64_t loads = 0;
    dl_iterate_phdr(lcompilers_init_count_loads, &loads);
    g->images = loads;
}


static size_t lcompilers_init_align_up(size_t n, size_t a) {
    return (n + a - 1) & ~(a - 1);
}

/* Passes the tables the notes of the image `info` describe to `visit`. A
 * note segment's entries are aligned to its own alignment, 4 or 8 (as glibc
 * reads them); one of any other alignment is malformed and passed over. */
static void lcompilers_init_image_notes(const struct dl_phdr_info *info,
        void (*visit)(const lcompilers_init_table *, void *), void *context) {
    for (ElfW(Half) i = 0; i < info->dlpi_phnum; i++) {
        const ElfW(Phdr) *ph = &info->dlpi_phdr[i];
        if (ph->p_type != PT_NOTE || ph->p_align > 8) continue;
        size_t align = ph->p_align == 8 ? 8 : 4;
        const unsigned char *p = (const unsigned char *)(info->dlpi_addr + ph->p_vaddr);
        size_t len = ph->p_memsz;
        size_t off = 0;
        while (off + 12 <= len) {
            uint32_t namesz, descsz, type;
            memcpy(&namesz, p + off, 4);
            memcpy(&descsz, p + off + 4, 4);
            memcpy(&type, p + off + 8, 4);
            size_t name_off = off + 12;
            size_t desc_off = lcompilers_init_align_up(name_off + namesz, align);
            size_t next = lcompilers_init_align_up(desc_off + descsz, align);
            if (desc_off > len || next > len || next <= off) break;
            if (type == lcompilers_init_elf_note_type
                    && namesz == sizeof(lcompilers_init_elf_note_owner)
                    && memcmp(p + name_off, lcompilers_init_elf_note_owner,
                        namesz) == 0
                    && descsz == sizeof(intptr_t)) {
                intptr_t offset;
                memcpy(&offset, p + desc_off, sizeof(offset));
                /* The table's offset from the note. */
                visit((const lcompilers_init_table *)(void *)
                    ((uintptr_t)(p + off) + (uintptr_t)offset), context);
            }
            off = next;
        }
    }
}

typedef struct {
    uint64_t adds;
    int first;
    /* The notes listed, an upper bound on what the scan finds if no image
     * is added meanwhile. */
    size_t notes;
    /* Whether the scan has to start over: an image was added, the registry
     * lock was busy, or there was no room for what the scan found. */
    int again;
    lcompilers_init_snapshot *out;
} lcompilers_init_elf_scan;

static void lcompilers_init_count_note(const lcompilers_init_table *t, void *context) {
    (void)t;
    ((lcompilers_init_elf_scan *)context)->notes++;
}

static int lcompilers_init_count_notes(struct dl_phdr_info *info, size_t size,
        void *data) {
    (void)size;
    lcompilers_init_elf_scan *scan = (lcompilers_init_elf_scan *)data;
    if (scan->first) {
        scan->adds = (uint64_t)info->dlpi_adds;
        scan->first = 0;
    }
    lcompilers_init_image_notes(info, lcompilers_init_count_note, scan);
    return 0;
}

/* Takes a table in, with the registry lock held, unless that needs memory:
 * the scan allocates nothing, since an allocator may enter the loader, whose
 * lock the scan holds. */
static void lcompilers_init_scan_adopt(const lcompilers_init_table *t, void *context) {
    lcompilers_init_elf_scan *scan = (lcompilers_init_elf_scan *)context;
    if (scan->again) return;
    if ((lcompilers_init_find_entry(t) == SIZE_MAX
                && lcompilers_init_entry_count >= lcompilers_init_entry_capacity)
            || scan->out->count >= scan->out->capacity) {
        scan->again = 1;
        return;
    }
    lcompilers_init_adopt_locked(t, scan->out);
}

/* Every callback of one dl_iterate_phdr sees the same count of loads: the
 * loader adds an image to its list, and bumps the count, only under the
 * lock the iteration holds throughout. So the first callback tells whether
 * an image was added since the count was taken, before any note is read.
 * The registry lock is only tried: a thread holding it may itself be
 * waiting for the loader, through its allocator. */
static int lcompilers_init_read_notes(struct dl_phdr_info *info, size_t size,
        void *data) {
    (void)size;
    lcompilers_init_elf_scan *scan = (lcompilers_init_elf_scan *)data;
    if ((uint64_t)info->dlpi_adds != scan->adds || !lcompilers_init_registry_trylock()) {
        scan->again = 1;
        return 1;
    }
    lcompilers_init_image_notes(info, lcompilers_init_scan_adopt, scan);
    lcompilers_init_registry_unlock();
    return scan->again;
}

/* Room for `n` more entries and for `n` snapshot items, made before a scan. */
static void lcompilers_init_reserve(lcompilers_init_snapshot *out, size_t n) {
    lcompilers_init_registry_lock();
    size_t want = lcompilers_init_entry_count + n;
    if (want > lcompilers_init_entry_capacity) {
        lcompilers_init_entry *grown = (lcompilers_init_entry *)realloc(
            lcompilers_init_entries, want * sizeof(*grown));
        if (grown == NULL) lcompilers_init_fail_now("out of memory");
        lcompilers_init_entries = grown;
        lcompilers_init_entry_capacity = want;
    }
    lcompilers_init_registry_unlock();
    if (n > out->capacity) {
        lcompilers_init_found *grown = (lcompilers_init_found *)realloc(
            out->items, n * sizeof(*grown));
        if (grown == NULL) lcompilers_init_fail("out of memory");
        out->items = grown;
        out->capacity = n;
    }
}

/* For the engine's own tests: called between the barrier and the scan. */
static void (*lcompilers_init_discovery_pause)(void);
static uint64_t lcompilers_init_discovery_retries;

LFORTRAN_API void _lcompilers_init_test_discovery_pause(void (*pause)(void)) {
    lcompilers_init_discovery_pause = pause;
}

LFORTRAN_API uint64_t _lcompilers_init_test_discovery_retries(void) {
    return lcompilers_init_load_u64(&lcompilers_init_discovery_retries);
}

static void lcompilers_init_count_valid_note(const lcompilers_init_table *t,
        void *context) {
    if (t->abi_version == lcompilers_init_abi_version) (*(uint64_t *)context)++;
}

static int lcompilers_init_count_valid_notes(struct dl_phdr_info *info,
        size_t size, void *data) {
    (void)size;
    lcompilers_init_image_notes(info, lcompilers_init_count_valid_note, data);
    return 0;
}

/* For the engine's own tests: how many tables of this ABI the notes of the
 * loaded images name, as discovery reads them. */
LFORTRAN_API uint64_t _lcompilers_init_test_note_tables(void) {
    uint64_t n = 0;
    dl_iterate_phdr(lcompilers_init_count_valid_notes, &n);
    return n;
}

/* The tables of the loaded images, as addresses only: none of them is
 * followed before a lease is taken on its entry.
 *
 * glibc lists an image from the moment another thread's dlopen maps it,
 * before it is relocated -- its note's pointer maybe not yet relocated --
 * and a load that fails unmaps its images without running any destructor.
 * A dlopen holds dl_load_lock until it has returned, and dladdr takes that
 * lock. So after the barrier every image listed when the count of loads was
 * taken is either fully loaded or gone, and a scan that sees the same count
 * lists no image added since. On the thread of a load, in one of its
 * constructors, the lock is its own: the load's images are relocated
 * before any constructor runs, and a load no longer fails once its
 * constructors run. An image a scan accepts thus finished loading, and
 * leaves only through its destructors, which retire its tables and wait for
 * their leases. This is glibc's loader. musl's is believed to allow the
 * same trivially -- it lists only relocated images and never unloads --
 * which is not verified; the BSDs' are not verified.
 *
 * Preconditions: the engine is not entered from an IFUNC resolver or an
 * audit module, which run during relocation; dladdr's result is ignored,
 * it only waits; and discovery never runs with the initialization lock or
 * a lease held. */
static void lcompilers_init_discover_images(lcompilers_init_snapshot *out) {
    for (;;) {
        lcompilers_init_elf_scan scan;
        memset(&scan, 0, sizeof(scan));
        scan.first = 1;
        scan.out = out;
        dl_iterate_phdr(lcompilers_init_count_notes, &scan);
        lcompilers_init_reserve(out, scan.notes);
        Dl_info info;
        dladdr((const void *)&lcompilers_init_discover_images, &info);
        if (lcompilers_init_discovery_pause != NULL) lcompilers_init_discovery_pause();
        out->count = 0;
        dl_iterate_phdr(lcompilers_init_read_notes, &scan);
        if (!scan.again) return;
        lcompilers_init_increment_u64(&lcompilers_init_discovery_retries);
        lcompilers_init_yield();
    }
}
#endif

/* For the engine's own tests: how many times a dispatch asked the loader
 * and the registry what changed. */
static uint64_t lcompilers_init_generation_reads;

LFORTRAN_API uint64_t _lcompilers_init_test_generation_reads(void) {
    return lcompilers_init_load_u64(&lcompilers_init_generation_reads);
}

static void lcompilers_init_generation(lcompilers_init_generation_t *g) {
    lcompilers_init_increment_u64(&lcompilers_init_generation_reads);
    lcompilers_init_image_generation(g);
    lcompilers_init_registry_lock();
    g->registry = lcompilers_init_load_u64(&lcompilers_init_registry_generation);
    lcompilers_init_registry_unlock();
}

static int lcompilers_init_same_generation(const lcompilers_init_generation_t *a,
        const lcompilers_init_generation_t *b) {
    return a->registry == b->registry && a->images == b->images;
}

/* Every live table of every loaded image and of every host batch. Called
 * without the initialization lock; reads none of the tables. */
static void lcompilers_init_discover(lcompilers_init_snapshot *out) {
    memset(out, 0, sizeof(*out));
    lcompilers_init_discover_images(out);
    lcompilers_init_registry_lock();
    for (size_t i = 0; i < lcompilers_init_entry_count; i++) {
        if (lcompilers_init_entries[i].listed || lcompilers_init_entries[i].constructed) {
            lcompilers_init_snapshot_add(out, i);
        }
    }
    for (size_t i = 0; i < lcompilers_init_host_count; i++) {
        lcompilers_init_snapshot_add(out,
            lcompilers_init_entry_of(lcompilers_init_host_tables[i]));
    }
    lcompilers_init_registry_unlock();
}

static void lcompilers_init_release(lcompilers_init_snapshot *snapshot) {
    free(snapshot->items);
}


static int lcompilers_init_compare_roots(const void *a, const void *b) {
    const lcompilers_init_root *x = (const lcompilers_init_root *)a;
    const lcompilers_init_root *y = (const lcompilers_init_root *)b;
    int c = strcmp(x->stable_id, y->stable_id);
    if (c != 0) return c;
    return x->order < y->order ? -1 : (x->order > y->order ? 1 : 0);
}

static char *lcompilers_init_copy_string(const char *s) {
    size_t n = strlen(s) + 1;
    char *copy = (char *)malloc(n);
    if (copy == NULL) lcompilers_init_fail("out of memory");
    memcpy(copy, s, n);
    return copy;
}

/* The records of the live tables of `snapshot`, copied under each table's
 * lease -- which its image's unload, or its batch's removal, waits for --
 * by stable id. The procedures and state words a record points to are
 * taken as values, not followed. A table retired since it was discovered is
 * passed over: its retirement changed the generation, so the dispatch walks
 * again. */
static lcompilers_init_root *lcompilers_init_roots(
        const lcompilers_init_snapshot *snapshot, size_t *n) {
    lcompilers_init_root *roots = NULL;
    size_t count = 0, capacity = 0;
    for (size_t i = 0; i < snapshot->count; i++) {
        const lcompilers_init_found *f = &snapshot->items[i];
        lcompilers_init_lease lease;
        if (!lcompilers_init_lease_acquire(&lease, f->entry, f->incarnation)) continue;
        const lcompilers_init_table *t = f->table;
        if (t->abi_version != lcompilers_init_abi_version) {
            lcompilers_init_fail("an image was compiled for another version "
                "of the startup initialization ABI");
        }
        if (t->count > 0 && t->records == NULL) {
            lcompilers_init_fail("an initialization table is malformed");
        }
        for (uint32_t j = 0; j < t->count; j++) {
            const lcompilers_init_record *r = &t->records[j];
            int bootstrap = r->flags == lcompilers_init_bootstrap;
            if (r->stable_id == NULL || r->ensure == NULL
                    || (r->flags != 0 && r->flags != lcompilers_init_collective
                        && !bootstrap)
                    || (r->state == NULL) != bootstrap
                    || (bootstrap && r->teardown != NULL)
                    || r->reserved != 0) {
                lcompilers_init_fail("a record of an initialization table is "
                    "malformed");
            }
            roots = (lcompilers_init_root *)lcompilers_init_grow((void *)roots,
                &capacity, count, sizeof(*roots));
            roots[count].stable_id = lcompilers_init_copy_string(r->stable_id);
            roots[count].ensure = r->ensure;
            roots[count].flags = r->flags;
            roots[count].entry = f->entry;
            roots[count].incarnation = f->incarnation;
            roots[count].order = count;
            count++;
        }
        lcompilers_init_lease_release(&lease);
    }
    if (count > 0) qsort(roots, count, sizeof(*roots), lcompilers_init_compare_roots);
    /* Collective initializers allocate in stable id order on every image, so
     * two distinct ones with the same id would be ordered by how images and
     * batches happened to be discovered. */
    for (size_t i = 0; i + 1 < count; i++) {
        const lcompilers_init_root *a = &roots[i];
        const lcompilers_init_root *b = &roots[i + 1];
        if ((a->flags & b->flags & lcompilers_init_collective) != 0
                && a->ensure != b->ensure
                && strcmp(a->stable_id, b->stable_id) == 0) {
            fflush(stdout);
            fprintf(stderr, "error: startup initialization: two collective "
                "initializers have the stable id '%s', so their order would "
                "depend on the link\n", a->stable_id);
            exit(1);
        }
    }
    *n = count;
    return roots;
}

static void lcompilers_init_free_roots(lcompilers_init_root *roots, size_t n) {
    for (size_t i = 0; i < n; i++) free(roots[i].stable_id);
    free(roots);
}

/* ------------------------------------------------------------------------
 * The guard and the completion log
 * ------------------------------------------------------------------------ */

/* The state words that became ready, in the order they did; protected by the
 * log lock. */
static int32_t **lcompilers_init_completed;
static size_t lcompilers_init_completed_count, lcompilers_init_completed_capacity;
/* The completions `_lcompilers_init_teardown_all` took out of the log and
 * has not torn down yet, older than every one in the log, or NULL; an entry
 * becomes NULL once it is torn down, or taken by a withdrawal of its table
 * from inside a teardown. Protected by the log lock. */
static int32_t **lcompilers_init_tearing;
static size_t lcompilers_init_tearing_count;

LFORTRAN_API int32_t _lcompilers_init_begin(int32_t *state) {
    if (lcompilers_init_load_acquire(state) == lcompilers_init_ready) return 0;
    lcompilers_init_lock();
    int32_t s = *state;
    if (s == lcompilers_init_ready) {
        lcompilers_init_unlock();
        return 0;
    }
    if (s == lcompilers_init_initializing) {
        /* Only the holder of the lock initializes, and that is this thread. */
        lcompilers_init_fail("an initializer was entered again while it was "
            "running: the initialization of a definition depends on itself");
    }
    lcompilers_init_store_release(state, lcompilers_init_initializing);
    return 1;
}

LFORTRAN_API void _lcompilers_init_end(int32_t *state) {
    if (!lcompilers_init_lock_held()
            || lcompilers_init_load_acquire(state) != lcompilers_init_initializing) {
        lcompilers_init_fail("an initializer ended that had not begun");
    }
    lcompilers_init_log_lock();
    lcompilers_init_completed = (int32_t **)lcompilers_init_grow(
        (void *)lcompilers_init_completed, &lcompilers_init_completed_capacity,
        lcompilers_init_completed_count, sizeof(*lcompilers_init_completed));
    lcompilers_init_completed[lcompilers_init_completed_count++] = state;
    lcompilers_init_log_unlock();
    lcompilers_init_store_release(state, lcompilers_init_ready);
    lcompilers_init_unlock();
}

LFORTRAN_API void _lcompilers_init_require_collective(void) {
    /* Not from a bootstrap, which runs before the runtime that collective
     * work uses is started. */
    if (!lcompilers_init_collective_active || !lcompilers_init_bootstraps_ran) {
        lcompilers_init_fail("a saved coarray has to be allocated at the "
            "collective startup boundary, which every image enters: the "
            "Fortran main program, or lfortran_initialize() called by the "
            "host on every image");
    }
}

static int lcompilers_init_table_holds(const lcompilers_init_table *table,
        const int32_t *state) {
    for (uint32_t j = 0; j < table->count; j++) {
        if (table->records[j].state == state) return 1;
    }
    return 0;
}

/* Takes the state words of `table`'s records out of the completion log, and
 * out of what a teardown of everything running on this thread has yet to
 * tear down, in the order they became ready; `table` is the caller's own,
 * still mapped. */
static int32_t **lcompilers_init_take_completed(const lcompilers_init_table *table,
        size_t *n) {
    int32_t **taken = NULL;
    size_t count = 0, capacity = 0;
    lcompilers_init_log_lock();
    for (size_t i = 0; i < lcompilers_init_tearing_count; i++) {
        int32_t *state = lcompilers_init_tearing[i];
        if (state == NULL || !lcompilers_init_table_holds(table, state)) continue;
        taken = (int32_t **)lcompilers_init_grow((void *)taken, &capacity,
            count, sizeof(*taken));
        taken[count++] = state;
        lcompilers_init_tearing[i] = NULL;
    }
    size_t kept = 0;
    for (size_t i = 0; i < lcompilers_init_completed_count; i++) {
        int32_t *state = lcompilers_init_completed[i];
        if (lcompilers_init_table_holds(table, state)) {
            taken = (int32_t **)lcompilers_init_grow((void *)taken, &capacity,
                count, sizeof(*taken));
            taken[count++] = state;
        } else {
            lcompilers_init_completed[kept++] = state;
        }
    }
    lcompilers_init_completed_count = kept;
    lcompilers_init_log_unlock();
    *n = count;
    return taken;
}

/* Whether `states[i]` is the last time its state word became ready: an image
 * loaded again reuses the addresses of the one before. */
static int lcompilers_init_last_completion(int32_t *const *states, size_t n,
        size_t i) {
    for (size_t j = i + 1; j < n; j++) {
        if (states[j] == states[i]) return 0;
    }
    return 1;
}

/* ------------------------------------------------------------------------
 * Dispatch
 * ------------------------------------------------------------------------ */

/* Calls `r`'s initializer under a lease on its table, unless the table was
 * retired since it was discovered. */
static void lcompilers_init_call(const lcompilers_init_root *r) {
    lcompilers_init_lease lease;
    if (!lcompilers_init_lease_acquire(&lease, r->entry, r->incarnation)) return;
    r->ensure();
    lcompilers_init_lease_release(&lease);
}

/* The bootstraps that have run, each for one incarnation of its table's
 * entry; under the registry lock. A bootstrap starts a runtime of the
 * process, which one of its tables starting is enough for: one of a stable
 * id is passed over, and its table counted as started too, while a table
 * that ran or was counted for one of that id is live. Once every such table
 * is gone -- the runtime may have gone with its image -- the next one of
 * that id runs again. Whether one ran is checked before it
 * is called and recorded after, without holding anything meanwhile: only
 * one thread at a time runs a collective boundary; see
 * `lcompilers_init_run`. */
typedef struct {
    size_t entry;
    uint64_t incarnation;
    char *stable_id;
} lcompilers_init_bootstrapped;

static lcompilers_init_bootstrapped *lcompilers_init_bootstraps;
static size_t lcompilers_init_bootstrap_count, lcompilers_init_bootstrap_capacity;

static int lcompilers_init_bootstrap_ran(const lcompilers_init_root *r) {
    int ran = 0;
    lcompilers_init_registry_lock();
    for (size_t i = 0; i < lcompilers_init_bootstrap_count && !ran; i++) {
        const lcompilers_init_bootstrapped *b = &lcompilers_init_bootstraps[i];
        const lcompilers_init_entry *e = &lcompilers_init_entries[b->entry];
        ran = !e->retired && e->incarnation == b->incarnation
            && strcmp(b->stable_id, r->stable_id) == 0;
    }
    lcompilers_init_registry_unlock();
    return ran;
}

static void lcompilers_init_bootstrap_done(const lcompilers_init_root *r) {
    char *id = lcompilers_init_copy_string(r->stable_id);
    lcompilers_init_registry_lock();
    for (size_t i = 0; i < lcompilers_init_bootstrap_count; i++) {
        const lcompilers_init_bootstrapped *b = &lcompilers_init_bootstraps[i];
        if (b->entry == r->entry && b->incarnation == r->incarnation
                && strcmp(b->stable_id, id) == 0) {
            lcompilers_init_registry_unlock();
            free(id);
            return;
        }
    }
    lcompilers_init_bootstraps = (lcompilers_init_bootstrapped *)lcompilers_init_grow(
        (void *)lcompilers_init_bootstraps, &lcompilers_init_bootstrap_capacity,
        lcompilers_init_bootstrap_count, sizeof(*lcompilers_init_bootstraps));
    lcompilers_init_bootstrapped *b = &lcompilers_init_bootstraps[lcompilers_init_bootstrap_count++];
    b->entry = r->entry;
    b->incarnation = r->incarnation;
    b->stable_id = id;
    lcompilers_init_registry_unlock();
}

/* The records one walk runs: the local ones, the bootstraps, the collective
 * ones. */
enum {
    lcompilers_init_walk_local,
    lcompilers_init_walk_bootstrap,
    lcompilers_init_walk_collective
};

/* One walk over the records of kind `kind` discovered now. */
static void lcompilers_init_walk(int kind) {
    lcompilers_init_snapshot snapshot;
    lcompilers_init_discover(&snapshot);
    size_t n = 0;
    lcompilers_init_root *roots = lcompilers_init_roots(&snapshot, &n);
    uint32_t flags = kind == lcompilers_init_walk_bootstrap ? lcompilers_init_bootstrap
        : (kind == lcompilers_init_walk_collective ? lcompilers_init_collective : 0);
    /* A bootstrap runs at the collective boundary too: it is collective
     * itself, as starting PRIF is. */
    lcompilers_init_collective_active = kind != lcompilers_init_walk_local;
    lcompilers_init_bootstraps_ran = kind == lcompilers_init_walk_collective;
    for (size_t i = 0; i < n; i++) {
        if (roots[i].flags != flags) continue;
        if (kind != lcompilers_init_walk_bootstrap) {
            lcompilers_init_call(&roots[i]);
        } else {
            /* A table whose bootstrap another live table's already ran is
             * covered by it: the runtime is the process's. */
            if (!lcompilers_init_bootstrap_ran(&roots[i])) lcompilers_init_call(&roots[i]);
            lcompilers_init_bootstrap_done(&roots[i]);
        }
    }
    lcompilers_init_collective_active = 0;
    lcompilers_init_bootstraps_ran = 0;
    lcompilers_init_free_roots(roots, n);
    lcompilers_init_release(&snapshot);
}

/* Set while a collective boundary runs. */
static int32_t lcompilers_init_collective_running;

/* A dispatch of phase `phase`. */
static void lcompilers_init_run(int32_t phase) {
    if (phase != lcompilers_init_dispatch_local
            && phase != lcompilers_init_dispatch_collective) {
        lcompilers_init_fail("unknown dispatch phase");
    }
    /* A thread that is dispatching, or that runs an initializer, does not
     * dispatch again: it could not complete what is already running, and it
     * must not enumerate the loader while it holds the lock. */
    if (lcompilers_init_dispatch_depth > 0 || lcompilers_init_lock_held()) return;
    lcompilers_init_generation_t generation;
    lcompilers_init_generation(&generation);
    if (phase == lcompilers_init_dispatch_local) {
        lcompilers_init_registry_lock();
        int done = lcompilers_init_dispatched_valid
            && lcompilers_init_same_generation(&lcompilers_init_dispatched, &generation);
        lcompilers_init_registry_unlock();
        if (done) return;
    }
    /* One thread of an image enters its collective boundary, once for every
     * set of collective records: two at once would start the runtime and
     * allocate coarrays in an order no other image shares. */
    if (phase == lcompilers_init_dispatch_collective
            && !lcompilers_init_claim(&lcompilers_init_collective_running)) {
        lcompilers_init_fail("two threads entered the collective startup "
            "boundary at once");
    }
    lcompilers_init_dispatch_depth++;
    /* An initializer or a bootstrap can load or unload an image or publish
     * or withdraw a batch, which changes the startup set; walk it again,
     * discovering outside the lock, until a walk sees no change. Ready
     * records are passed over at once. At a collective boundary the local
     * records and the bootstraps settle first, so that every image enters
     * the collective records with the same set. */
    for (;;) {
        lcompilers_init_generation_t after;
        lcompilers_init_walk(lcompilers_init_walk_local);
        if (phase == lcompilers_init_dispatch_collective) {
            lcompilers_init_walk(lcompilers_init_walk_bootstrap);
            lcompilers_init_generation(&after);
            if (!lcompilers_init_same_generation(&after, &generation)) {
                generation = after;
                continue;
            }
            lcompilers_init_walk(lcompilers_init_walk_collective);
        }
        lcompilers_init_generation(&after);
        if (lcompilers_init_same_generation(&after, &generation)) break;
        generation = after;
    }
    lcompilers_init_dispatch_depth--;
    if (phase == lcompilers_init_dispatch_collective) {
        lcompilers_init_store_release(&lcompilers_init_collective_running, 0);
    }
    lcompilers_init_registry_lock();
    lcompilers_init_dispatched = generation;
    lcompilers_init_dispatched_valid = 1;
    lcompilers_init_registry_unlock();
}

LFORTRAN_API void _lcompilers_init_ctor(const lcompilers_init_table *table) {
#if defined(COMPILE_TO_WASM)
    /* Published already, from a constructor that ran before this one,
     * unless the object file has only the object-format-independent
     * records. */
    _lcompilers_init_add_records(table);
#else
    /* The image is loaded: take its table in, if no discovery has yet. */
    lcompilers_init_registry_lock();
    lcompilers_init_adopt_locked(table, NULL);
    lcompilers_init_entry *e = &lcompilers_init_entries[lcompilers_init_entry_of(table)];
    if (!e->constructed) {
        /* Its table is discovered from now on, however it is listed. */
        e->constructed = 1;
        lcompilers_init_registry_changed();
    }
    lcompilers_init_registry_unlock();
#endif
#if defined(_WIN32) && !defined(COMPILE_TO_WASM)
    /* A DLL's constructors run under the loader lock, where running
     * initializers is unsafe. The executable's constructor, a Fortran main
     * program or the host's lfortran_initialize() then does the work, with
     * this DLL's records discovered like any other image's. */
    HMODULE module = NULL;
    if (!GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS
            | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
            (LPCSTR)(const void *)table, &module) || module != GetModuleHandleA(NULL)) {
        return;
    }
#endif
    lcompilers_init_run(lcompilers_init_dispatch_local);
}

LFORTRAN_API void _lcompilers_init_dispatch(int32_t phase) {
    lcompilers_init_run(phase);
}

/* The host startup of LFortran's ISO_Fortran_binding.h. The runtime's own
 * state is set up once, as a Fortran main program sets it up, the command
 * line by the first call that passes one; every call is a collective
 * boundary, which also initializes what images loaded since the last one
 * define. */
static int32_t lcompilers_init_host_started, lcompilers_init_host_command_line;

LFORTRAN_API void lfortran_initialize(int argc, char *argv[]) {
    if (argc > 0 && argv != NULL
            && lcompilers_init_claim(&lcompilers_init_host_command_line)) {
        _lpython_set_argv(argc, argv);
    }
    if (lcompilers_init_claim(&lcompilers_init_host_started)) {
        _lfortran_init_random_clock();
    }
    lcompilers_init_run(lcompilers_init_dispatch_collective);
}

LFORTRAN_API void lfortran_finalize(void) {
    _lfortran_internal_alloc_finalize();
}

LFORTRAN_API void _lcompilers_init_add_records(const lcompilers_init_table *table) {
    if (table == NULL) return;
    if (table->abi_version != lcompilers_init_abi_version) {
        lcompilers_init_fail("a batch was compiled for another version of the "
            "startup initialization ABI");
    }
    lcompilers_init_registry_lock();
    /* Publishing a batch that is already published changes nothing. */
    for (size_t i = 0; i < lcompilers_init_host_count; i++) {
        if (lcompilers_init_host_tables[i] == table) {
            lcompilers_init_registry_unlock();
            return;
        }
    }
    lcompilers_init_host_tables = (const lcompilers_init_table **)lcompilers_init_grow(
        (void *)lcompilers_init_host_tables, &lcompilers_init_host_capacity,
        lcompilers_init_host_count, sizeof(*lcompilers_init_host_tables));
    lcompilers_init_host_tables[lcompilers_init_host_count++] = table;
    lcompilers_init_revive_locked(lcompilers_init_entry_of(table));
    lcompilers_init_registry_changed();
    lcompilers_init_registry_unlock();
}

LFORTRAN_API void _lcompilers_init_remove_records(const lcompilers_init_table *table) {
    lcompilers_init_registry_lock();
    for (size_t i = 0; i < lcompilers_init_host_count; i++) {
        if (lcompilers_init_host_tables[i] != table) continue;
        memmove(&lcompilers_init_host_tables[i], &lcompilers_init_host_tables[i + 1],
            (lcompilers_init_host_count - i - 1) * sizeof(*lcompilers_init_host_tables));
        lcompilers_init_host_count--;
        break;
    }
    size_t entry = lcompilers_init_entry_of(table);
    lcompilers_init_retire_locked(entry);
    lcompilers_init_registry_unlock();
    lcompilers_init_wait_for_leases(entry);
    /* The batch's state words are about to be unmapped with it. */
    size_t n = 0;
    free(lcompilers_init_take_completed(table, &n));
}

LFORTRAN_API void _lcompilers_init_unload(const lcompilers_init_table *table) {
    if (table == NULL) return;
#if defined(_WIN32) && !defined(COMPILE_TO_WASM)
    if (lcompilers_init_process_ending()) return;
#endif
    lcompilers_init_registry_lock();
    size_t entry = lcompilers_init_entry_of(table);
    /* This mapping of the image is on its way out, however long the loader
     * still lists it. */
    if (table->instance != NULL) {
        lcompilers_init_store_release((int32_t *)table->instance,
            lcompilers_init_instance_retired);
    }
    lcompilers_init_retire_locked(entry);
    lcompilers_init_registry_unlock();
    lcompilers_init_wait_for_leases(entry);
    /* Latest first, as `_lcompilers_init_teardown_all` would. A state still
     * in the log has not been torn down; the teardown at the end of a
     * program empties the log before the destructors run. No other
     * initializer needs what these own: whatever depends on this image keeps
     * it loaded. */
    size_t n = 0;
    int32_t **taken = lcompilers_init_take_completed(table, &n);
    for (size_t i = n; i-- > 0;) {
        if (!lcompilers_init_last_completion(taken, n, i)) continue;
        for (uint32_t j = 0; j < table->count; j++) {
            const lcompilers_init_record *r = &table->records[j];
            if (r->state != taken[i]) continue;
            if (r->teardown != NULL
                    && lcompilers_init_load_acquire(taken[i]) == lcompilers_init_ready) {
                r->teardown();
            }
            break;
        }
    }
    free(taken);
}

LFORTRAN_API void _lcompilers_init_teardown_all(void) {
    /* A teardown frees what the owners' storage holds and leaves every state
     * ready, so a dispatch after it has nothing to run again. */
    lcompilers_init_snapshot snapshot;
    lcompilers_init_discover(&snapshot);
    lcompilers_init_lock();
    /* Called from a teardown: the teardown running tears everything down. */
    lcompilers_init_log_lock();
    int nested = lcompilers_init_tearing != NULL;
    lcompilers_init_log_unlock();
    if (nested) {
        lcompilers_init_unlock();
        lcompilers_init_release(&snapshot);
        return;
    }
    /* A lease on every table still live, held until the teardowns of its
     * records have run: an image unloaded meanwhile waits for them before it
     * takes its records' completions out of the log. */
    lcompilers_init_lease *leases = NULL;
    int *held = NULL;
    if (snapshot.count > 0) {
        leases = (lcompilers_init_lease *)malloc(snapshot.count * sizeof(*leases));
        held = (int *)malloc(snapshot.count * sizeof(*held));
        if (leases == NULL || held == NULL) lcompilers_init_fail("out of memory");
    }
    for (size_t t = 0; t < snapshot.count; t++) {
        held[t] = lcompilers_init_lease_acquire(&leases[t], snapshot.items[t].entry,
            snapshot.items[t].incarnation);
    }
    /* The log, split at once: the completions of the leased tables' records
     * are taken out and torn down here, and every other one stays -- one of
     * a table withdrawn before its lease, which its withdrawal takes out
     * and tears down itself, or of a table discovered after the snapshot. */
    lcompilers_init_log_lock();
    int32_t **states = lcompilers_init_completed;
    size_t n = lcompilers_init_completed_count;
    int32_t **taken = NULL;
    const lcompilers_init_record **records = NULL;
    if (n > 0) {
        taken = (int32_t **)malloc(n * sizeof(*taken));
        records = (const lcompilers_init_record **)malloc(n * sizeof(*records));
        if (taken == NULL || records == NULL) lcompilers_init_fail_now("out of memory");
    }
    size_t kept = 0, m = 0;
    for (size_t i = 0; i < n; i++) {
        const lcompilers_init_record *record = NULL;
        for (size_t t = 0; t < snapshot.count && record == NULL; t++) {
            if (!held[t]) continue;
            const lcompilers_init_table *table = snapshot.items[t].table;
            for (uint32_t k = 0; k < table->count; k++) {
                if (table->records[k].state == states[i]) {
                    record = &table->records[k];
                    break;
                }
            }
        }
        if (record == NULL) {
            states[kept++] = states[i];
        } else {
            taken[m] = states[i];
            records[m++] = record;
        }
    }
    lcompilers_init_completed_count = kept;
    /* Only the last completion of a state word is torn down: an image
     * loaded again reuses the addresses of the one before. */
    for (size_t i = 0; i < m; i++) {
        if (!lcompilers_init_last_completion(taken, m, i)) taken[i] = NULL;
    }
    /* What is left to tear down, which a withdrawal from inside one of the
     * teardowns -- of a host batch, or an image unloaded by a finalizer --
     * takes out as it would take its completions out of the log: the
     * withdrawn table is gone once the withdrawal returns, and those of its
     * records' teardowns that are to run, the withdrawal runs. Every other
     * table stays mapped meanwhile: a withdrawal on another thread waits for
     * the leases held here. */
    lcompilers_init_tearing = taken;
    lcompilers_init_tearing_count = m;
    lcompilers_init_log_unlock();
    for (size_t i = m; i-- > 0;) {
        lcompilers_init_log_lock();
        int32_t *state = taken[i];
        taken[i] = NULL;
        lcompilers_init_log_unlock();
        if (state == NULL) continue;
        if (records[i]->teardown != NULL
                && lcompilers_init_load_acquire(state) == lcompilers_init_ready) {
            records[i]->teardown();
        }
    }
    lcompilers_init_log_lock();
    lcompilers_init_tearing = NULL;
    lcompilers_init_tearing_count = 0;
    lcompilers_init_log_unlock();
    free(taken);
    free(records);
    for (size_t t = snapshot.count; t-- > 0;) {
        if (held[t]) lcompilers_init_lease_release(&leases[t]);
    }
    free(leases);
    free(held);
    lcompilers_init_unlock();
    lcompilers_init_release(&snapshot);
}
