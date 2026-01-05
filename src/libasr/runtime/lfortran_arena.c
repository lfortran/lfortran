/*
 * Arena Allocator Implementation for LFortran Runtime
 *
 * Chunk-based bump-pointer allocator with automatic growth.
 * Two thread-local scratch arenas for conflict-free nested allocation.
 */

#include "lfortran_arena.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Compile-time validation that ARENA_ALIGNMENT is a power of two */
#if (ARENA_ALIGNMENT & (ARENA_ALIGNMENT - 1)) != 0
#error "ARENA_ALIGNMENT must be a power of two"
#endif

/* --- Internal Chunk Structure --- */

struct ArenaChunk {
    struct ArenaChunk* next;
    size_t size;  /* Total size of this chunk (including header) */
    /* Data area follows immediately after this struct */
};

/* --- Arena Structure --- */

struct Arena {
    ArenaChunk* first_chunk;
    ArenaChunk* current_chunk;
    char* current_ptr;
    size_t remaining_in_chunk;
    size_t default_chunk_size;
};

/* --- Thread-Local Scratch Arenas --- */

static LFORTRAN_ARENA_TLS Arena* scratch_arenas[2] = {NULL, NULL};
static LFORTRAN_ARENA_TLS int scratch_initialized = 0;

/*
 * Thread-local current arena pointer.
 * Tracks which arena the current function is using.
 * NULL means no function has claimed an arena yet (top-level).
 * Callees use this to get a DIFFERENT arena, avoiding conflicts.
 */
static LFORTRAN_ARENA_TLS Arena* scratch_current_arena = NULL;

/* Thread-local pointer for legacy API inline access (static - use accessors) */
static LFORTRAN_ARENA_TLS char* _lfortran_arena_ptr = NULL;

/* Accessor functions for TLS variable (Windows doesn't support dllexport on TLS) */
LFORTRAN_ARENA_API char* _lfortran_get_arena_ptr(void) {
    return _lfortran_arena_ptr;
}

LFORTRAN_ARENA_API void _lfortran_set_arena_ptr(char* ptr) {
    _lfortran_arena_ptr = ptr;
}

/*
 * Handle structure for _lfortran_scratch_begin/_lfortran_scratch_end.
 * Encodes all state needed to restore on scope exit.
 */
typedef struct {
    Arena* arena;           /* Arena being used in this scope */
    Arena* parent_arena;    /* Arena to restore as current on exit */
    ArenaChunk* saved_chunk;/* Position: chunk */
    char* saved_ptr;        /* Position: pointer within chunk */
} ScratchHandle;

/*
 * Thread-local handle pool to avoid malloc/free on every scratch_begin/end.
 * Max depth of 256 should be sufficient for any reasonable call stack.
 */
#define SCRATCH_HANDLE_POOL_SIZE 256
static LFORTRAN_ARENA_TLS ScratchHandle handle_pool[SCRATCH_HANDLE_POOL_SIZE];
static LFORTRAN_ARENA_TLS int handle_depth = 0;

/* --- Helper Functions --- */

static inline size_t align_up(size_t val, size_t alignment) {
    /* Handle overflow: if adding (alignment - 1) would overflow, saturate */
    if (val > SIZE_MAX - alignment + 1) {
        return SIZE_MAX;
    }
    return (val + alignment - 1) & ~(alignment - 1);
}

static void fatal_error(const char* msg) {
    fprintf(stderr, "LFortran Arena Error: %s\n", msg);
    exit(1);
}

/* Formatted fatal error with pointer context for debugging */
static void fatal_error_ptr(const char* msg, void* ptr) {
    fprintf(stderr, "LFortran Arena Error: %s (ptr=%p)\n", msg, ptr);
    exit(1);
}

/* --- Arena Implementation --- */

LFORTRAN_ARENA_API Arena* arena_new(size_t initial_size) {
    Arena* arena = (Arena*)malloc(sizeof(Arena));
    if (!arena) {
        return NULL;
    }

    if (initial_size < ARENA_MIN_CHUNK_SIZE) {
        initial_size = ARENA_MIN_CHUNK_SIZE;
    }
    arena->default_chunk_size = initial_size;
    arena->first_chunk = NULL;
    arena->current_chunk = NULL;
    arena->current_ptr = NULL;
    arena->remaining_in_chunk = 0;

    /* Allocate the first chunk (with overflow check) */
    size_t header_plus_align = sizeof(ArenaChunk) + ARENA_ALIGNMENT;
    if (initial_size > SIZE_MAX - header_plus_align) {
        free(arena);
        return NULL;  /* Overflow: requested size too large */
    }
    size_t chunk_size = header_plus_align + initial_size;
    ArenaChunk* first = (ArenaChunk*)malloc(chunk_size);
    if (!first) {
        free(arena);
        return NULL;
    }

    first->next = NULL;
    first->size = chunk_size;

    arena->first_chunk = first;
    arena->current_chunk = first;

    /* Set up allocation pointer after the chunk header, aligned */
    uintptr_t data_start = align_up((uintptr_t)(first + 1), ARENA_ALIGNMENT);
    uintptr_t chunk_end = (uintptr_t)first + chunk_size;

    arena->current_ptr = (char*)data_start;
    arena->remaining_in_chunk = (data_start < chunk_end) ? (chunk_end - data_start) : 0;

    return arena;
}

LFORTRAN_ARENA_API void* arena_alloc(Arena* arena, size_t size) {
    if (!arena) {
        fatal_error("arena_alloc: NULL arena - ensure scratch_init() was called");
    }

    /* Handle zero-size allocations by returning current pointer */
    if (size == 0) {
        return arena->current_ptr;
    }

    size_t aligned_size = align_up(size, ARENA_ALIGNMENT);

    /* Check for overflow in alignment */
    if (aligned_size == SIZE_MAX && size != SIZE_MAX) {
        fatal_error("arena_alloc: size overflow during alignment");
    }

try_alloc:
    /* Fast path: current chunk has enough space */
    if (aligned_size <= arena->remaining_in_chunk) {
        void* ptr = arena->current_ptr;
        arena->current_ptr += aligned_size;
        arena->remaining_in_chunk -= aligned_size;
        return ptr;
    }

    /* Try to move to the next existing chunk (reuse after reset) */
    if (arena->current_chunk && arena->current_chunk->next) {
        arena->current_chunk = arena->current_chunk->next;

        ArenaChunk* chunk = arena->current_chunk;
        uintptr_t data_start = align_up((uintptr_t)(chunk + 1), ARENA_ALIGNMENT);
        uintptr_t chunk_end = (uintptr_t)chunk + chunk->size;

        arena->current_ptr = (char*)data_start;
        arena->remaining_in_chunk = (data_start < chunk_end) ? (chunk_end - data_start) : 0;

        goto try_alloc;
    }

    /* Allocate a new chunk */
    size_t new_chunk_data_size = arena->default_chunk_size;
    if (aligned_size > new_chunk_data_size) {
        new_chunk_data_size = aligned_size;
    }

    /* Overflow check for chunk size calculation */
    size_t header_plus_align = sizeof(ArenaChunk) + ARENA_ALIGNMENT;
    if (new_chunk_data_size > SIZE_MAX - header_plus_align) {
        fatal_error("arena_alloc: chunk size overflow");
    }
    size_t new_chunk_size = header_plus_align + new_chunk_data_size;
    ArenaChunk* new_chunk = (ArenaChunk*)malloc(new_chunk_size);
    if (!new_chunk) {
        fatal_error("arena_alloc: malloc failed for new chunk - system out of memory");
    }

    new_chunk->next = NULL;
    new_chunk->size = new_chunk_size;

    /* Link the new chunk */
    if (arena->current_chunk) {
        arena->current_chunk->next = new_chunk;
    } else {
        arena->first_chunk = new_chunk;
    }
    arena->current_chunk = new_chunk;

    /* Set up allocation pointer */
    uintptr_t data_start = align_up((uintptr_t)(new_chunk + 1), ARENA_ALIGNMENT);
    uintptr_t chunk_end = (uintptr_t)new_chunk + new_chunk_size;

    arena->current_ptr = (char*)data_start;
    arena->remaining_in_chunk = (data_start < chunk_end) ? (chunk_end - data_start) : 0;

    goto try_alloc;
}

LFORTRAN_ARENA_API ArenaPos arena_get_pos(Arena* arena) {
    ArenaPos pos;
    pos.chunk = arena->current_chunk;
    pos.ptr = arena->current_ptr;
    return pos;
}

LFORTRAN_ARENA_API void arena_reset(Arena* arena, ArenaPos pos) {
    if (!arena || !pos.chunk || !pos.ptr) {
        fatal_error("arena_reset: invalid arguments - arena, chunk, or ptr is NULL");
    }

    /* Validate that pos.chunk belongs to this arena's linked list */
    ArenaChunk* chunk = arena->first_chunk;
    int chunk_found = 0;
    while (chunk) {
        if (chunk == pos.chunk) {
            chunk_found = 1;
            break;
        }
        chunk = chunk->next;
    }
    if (!chunk_found) {
        fatal_error_ptr("arena_reset: chunk does not belong to this arena", pos.chunk);
    }

    /* Validate that pos.ptr is within chunk bounds */
    uintptr_t data_start = (uintptr_t)(pos.chunk + 1);
    uintptr_t chunk_end = (uintptr_t)pos.chunk + pos.chunk->size;
    uintptr_t ptr_val = (uintptr_t)pos.ptr;
    if (ptr_val < data_start || ptr_val > chunk_end) {
        fatal_error_ptr("arena_reset: pointer out of chunk bounds", pos.ptr);
    }

    arena->current_chunk = pos.chunk;
    arena->current_ptr = pos.ptr;

    /* Recalculate remaining space in the restored chunk */
    uintptr_t current_pos = (uintptr_t)pos.ptr;

    arena->remaining_in_chunk = (current_pos < chunk_end) ? (chunk_end - current_pos) : 0;
}

LFORTRAN_ARENA_API void arena_free(Arena* arena) {
    if (!arena) return;

    ArenaChunk* chunk = arena->first_chunk;
    while (chunk) {
        ArenaChunk* next = chunk->next;
        free(chunk);
        chunk = next;
    }
    free(arena);
}

/* --- Scratch Implementation --- */

LFORTRAN_ARENA_API void scratch_init(void) {
    if (scratch_initialized) {
        return;
    }
    scratch_arenas[0] = arena_new(ARENA_INITIAL_SIZE);
    scratch_arenas[1] = arena_new(ARENA_INITIAL_SIZE);
    if (!scratch_arenas[0] || !scratch_arenas[1]) {
        /* Clean up if one succeeded but the other failed */
        if (scratch_arenas[0]) arena_free(scratch_arenas[0]);
        if (scratch_arenas[1]) arena_free(scratch_arenas[1]);
        scratch_arenas[0] = scratch_arenas[1] = NULL;
        fatal_error("scratch_init: failed to create scratch arenas");
    }
    scratch_initialized = 1;

    /* Initialize legacy arena pointer to first arena's current position */
    _lfortran_arena_ptr = scratch_arenas[0]->current_ptr;
}

static Scratch scratch_begin_from_arena(Arena* arena) {
    Scratch s;
    s.arena = arena;
    s.saved_pos = arena_get_pos(arena);
    return s;
}

LFORTRAN_ARENA_API Scratch scratch_begin(void) {
    return scratch_begin_avoid_conflict(NULL);
}

LFORTRAN_ARENA_API Scratch scratch_begin_avoid_conflict(Arena* conflict) {
    if (!scratch_initialized) {
        scratch_init();
    }

    for (int i = 0; i < 2; i++) {
        if (scratch_arenas[i] != conflict) {
            return scratch_begin_from_arena(scratch_arenas[i]);
        }
    }

    fatal_error("scratch_begin_avoid_conflict: no conflict-free arena available");

    /* Unreachable, but satisfies compiler */
    Scratch s = {NULL, {NULL, NULL}};
    return s;
}

LFORTRAN_ARENA_API void scratch_end(Scratch scratch) {
    arena_reset(scratch.arena, scratch.saved_pos);
}

LFORTRAN_ARENA_API Arena* scratch_get_arena(void) {
    if (!scratch_initialized) {
        scratch_init();
    }
    return scratch_arenas[0];
}

LFORTRAN_ARENA_API Arena* scratch_get_arena_avoid_conflict(Arena* conflict) {
    if (!scratch_initialized) {
        scratch_init();
    }

    for (int i = 0; i < 2; i++) {
        if (scratch_arenas[i] != conflict) {
            return scratch_arenas[i];
        }
    }

    fatal_error("scratch_get_arena_avoid_conflict: no conflict-free arena available");
    return NULL;
}

/* --- Conflict-Aware API Implementation --- */

LFORTRAN_ARENA_API Arena* scratch_get_current(void) {
    return scratch_current_arena;
}

LFORTRAN_ARENA_API void scratch_set_current(Arena* arena) {
    scratch_current_arena = arena;
}

/* --- New Codegen API Implementation --- */

LFORTRAN_ARENA_API void* _lfortran_scratch_begin(void) {
    if (!scratch_initialized) {
        scratch_init();
    }

    /* Use thread-local handle pool instead of malloc */
    if (handle_depth >= SCRATCH_HANDLE_POOL_SIZE) {
        fatal_error("_lfortran_scratch_begin: handle pool exhausted - call depth exceeds 256");
    }
    ScratchHandle* handle = &handle_pool[handle_depth];
    handle_depth++;

    /* Get parent arena (what caller is using) */
    Arena* parent = scratch_current_arena;

    /* Get an arena different from parent */
    Arena* my_arena = scratch_get_arena_avoid_conflict(parent);

    /* Save position on my arena */
    ArenaPos saved = arena_get_pos(my_arena);

    /* Set my arena as current */
    scratch_current_arena = my_arena;

    /* Fill in handle */
    handle->arena = my_arena;
    handle->parent_arena = parent;
    handle->saved_chunk = saved.chunk;
    handle->saved_ptr = saved.ptr;

    return handle;
}

LFORTRAN_ARENA_API void* _lfortran_scratch_alloc(int64_t size) {
    /* Validate signed-to-unsigned conversion */
    if (size < 0) {
        fatal_error("_lfortran_scratch_alloc: negative size");
    }

    if (!scratch_initialized) {
        scratch_init();
    }

    /* Allocate from the current arena, or default to scratch_arenas[0] */
    Arena* arena = scratch_current_arena;
    if (!arena) {
        arena = scratch_arenas[0];
    }

    return arena_alloc(arena, (size_t)size);
}

LFORTRAN_ARENA_API void _lfortran_scratch_end(void* handle_ptr) {
    if (!handle_ptr) {
        fatal_error("_lfortran_scratch_end: NULL handle - missing scratch_begin call?");
    }

    ScratchHandle* handle = (ScratchHandle*)handle_ptr;

    /* Validate handle is from our pool and matches current depth */
    if (handle_depth <= 0) {
        fatal_error("_lfortran_scratch_end: handle pool underflow - more ends than begins");
    }
    ScratchHandle* expected = &handle_pool[handle_depth - 1];
    if (handle != expected) {
        fatal_error_ptr("_lfortran_scratch_end: handle mismatch - scratch_end called out of order (LIFO violation)", handle_ptr);
    }

    /* Restore arena position */
    ArenaPos saved;
    saved.chunk = handle->saved_chunk;
    saved.ptr = handle->saved_ptr;
    arena_reset(handle->arena, saved);

    /* Restore parent as current */
    scratch_current_arena = handle->parent_arena;

    /* Return handle to pool (just decrement depth, no free needed) */
    handle_depth--;
}

/* --- Legacy API Implementation --- */

LFORTRAN_ARENA_API void _lfortran_arena_init(void) {
    scratch_init();
}

LFORTRAN_ARENA_API void* _lfortran_arena_alloc(int64_t size) {
    if (!scratch_initialized) {
        scratch_init();
    }
    void* ptr = arena_alloc(scratch_arenas[0], (size_t)size);

    /* Update legacy pointer for potential inline access */
    _lfortran_arena_ptr = scratch_arenas[0]->current_ptr;

    return ptr;
}

LFORTRAN_ARENA_API void* _lfortran_arena_save(void) {
    if (!scratch_initialized) {
        scratch_init();
    }
    return scratch_arenas[0]->current_ptr;
}

LFORTRAN_ARENA_API void _lfortran_arena_restore(void* saved_ptr) {
    if (!scratch_initialized) {
        return;  /* Nothing to restore if never initialized */
    }

    Arena* arena = scratch_arenas[0];

    /* Find which chunk contains the saved pointer and restore to it */
    ArenaChunk* chunk = arena->first_chunk;
    while (chunk) {
        uintptr_t chunk_start = (uintptr_t)(chunk + 1);
        uintptr_t chunk_end = (uintptr_t)chunk + chunk->size;
        uintptr_t ptr = (uintptr_t)saved_ptr;

        if (ptr >= chunk_start && ptr <= chunk_end) {
            arena->current_chunk = chunk;
            arena->current_ptr = (char*)saved_ptr;
            arena->remaining_in_chunk = chunk_end - ptr;

            /* Update legacy pointer */
            _lfortran_arena_ptr = (char*)saved_ptr;
            return;
        }
        chunk = chunk->next;
    }

    fatal_error("_lfortran_arena_restore: saved pointer not found in arena");
}
