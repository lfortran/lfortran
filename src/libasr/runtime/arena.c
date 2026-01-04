/*
 * Arena Allocator Implementation for LFortran Runtime
 *
 * Chunk-based bump-pointer allocator with automatic growth.
 * Two thread-local scratch arenas for conflict-free nested allocation.
 */

#include "arena.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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

/* Thread-local pointer for legacy API inline access */
LFORTRAN_ARENA_API LFORTRAN_ARENA_TLS char* _lfortran_arena_ptr = NULL;

/* --- Helper Functions --- */

static inline size_t align_up(size_t val, size_t alignment) {
    return (val + alignment - 1) & ~(alignment - 1);
}

static void fatal_error(const char* msg) {
    fprintf(stderr, "LFortran Arena Error: %s\n", msg);
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

    /* Allocate the first chunk */
    size_t chunk_size = sizeof(ArenaChunk) + initial_size + ARENA_ALIGNMENT;
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
    if (!arena || size == 0) {
        fatal_error("arena_alloc: invalid arguments");
    }

    size_t aligned_size = align_up(size, ARENA_ALIGNMENT);

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

    size_t new_chunk_size = sizeof(ArenaChunk) + new_chunk_data_size + ARENA_ALIGNMENT;
    ArenaChunk* new_chunk = (ArenaChunk*)malloc(new_chunk_size);
    if (!new_chunk) {
        fatal_error("arena_alloc: failed to allocate new chunk");
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
        fatal_error("arena_reset: invalid arguments");
    }

    arena->current_chunk = pos.chunk;
    arena->current_ptr = pos.ptr;

    /* Recalculate remaining space in the restored chunk */
    uintptr_t chunk_end = (uintptr_t)pos.chunk + pos.chunk->size;
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
