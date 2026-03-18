#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#define MEM_DEBUG_IMPL /*Stop malloc, free, realloc, etc redirection*/
#include "mem_debug.h"
#include "hashtable.h"

#define MEM_DEBUG_INITIAL_BUCKETS 1024

typedef struct Alloc_info {
    size_t       size;
} Alloc_info;


unsigned long mem_debugger_HASH(void* const ptr) { // Thomas Wang hash
    unsigned long v = (unsigned long)ptr;
    v ^= v >> 16;
    v *= 0x45d9f3b;
    v ^= v >> 16;
    return v;
}

int mem_debugger_EQUAL(void *a, void *b){
    return (a == b);
}

DEFINE_HASHTABLE_FOR_TYPES(void*, Alloc_info, mem_debugger)

static mem_debugger mem_dbg_hashTable = {NULL, 0, 0};


// Create `Alloc_info` and map to the ptr
static void register_ptr(void* const ptr, size_t size){
    if(!ptr) return;
    if(!mem_dbg_hashTable.buckets) mem_debugger_init(&mem_dbg_hashTable, MEM_DEBUG_INITIAL_BUCKETS);
    
    Alloc_info a;
    a.size = size;
    mem_debugger_insert(&mem_dbg_hashTable, ptr, a);
}

void* dbg_malloc(void *context, int64_t size) {
    (void)context;
    size_t alloc_size = (size_t)size;
    void *ptr = malloc(alloc_size);
    if(alloc_size && !ptr) fprintf(stderr, "ERROR : Unexpected error at dbg_malloc\n");
    register_ptr(ptr, alloc_size);
    return ptr;

}

void* dbg_calloc(void *context, int64_t nmemb, int64_t size) {
    (void)context;
    size_t count = (size_t)nmemb;
    size_t alloc_size = (size_t)size;
    void *ptr = calloc(count, alloc_size);
    if((alloc_size * count) && !ptr) fprintf(stderr, "ERROR : Unexpected error at dbg_calloc\n");
    register_ptr(ptr, alloc_size * count);
    return ptr;
}

void  *dbg_realloc(void *context, void *ptr, int64_t size){
    if (!ptr) {
        return dbg_malloc(context, size);
    }
    if (size == 0) {
        dbg_free(context, ptr);
        return NULL;
    }

    size_t alloc_size = (size_t)size;
    void *new_ptr = realloc(ptr, alloc_size);
    if (alloc_size && !new_ptr) {
        fprintf(stderr, "ERROR : Unexpected error at dbg_realloc\n");
        return NULL;
    }
    
    if(!mem_dbg_hashTable.buckets){
        fprintf(stderr, "ERROR : Unexpected error at dbg_realloc:hashtable not initialized\n");
        return NULL;
    }


    Alloc_info *found = mem_debugger_get(&mem_dbg_hashTable, ptr);
    if(!found){
        fprintf(stderr, "ERROR : Not registered ptr\n");
        exit(1);
    }

    if (new_ptr == ptr) {
        found->size = alloc_size;
    } else {
        mem_debugger_remove(&mem_dbg_hashTable, ptr);
        register_ptr(new_ptr, alloc_size);
    }

    return new_ptr;
}



void dbg_free(void *context, void *ptr) {
    (void)context;
    if (!ptr) return;
    if(!mem_dbg_hashTable.buckets){
        fprintf(stderr, "Error: at dbg_free -- hastable not initialized\n");
        exit(1);
    }
    Alloc_info *found = mem_debugger_get(&mem_dbg_hashTable, ptr);
    if (!found) {
        fprintf(stderr, "Error: at dbg_free -- ptr not found\n");
        exit(1);
    }
    free(ptr);
    mem_debugger_remove(&mem_dbg_hashTable, ptr);
}

// Called to report any leaks
void dbg_report() {
    size_t leaks = 0;
    size_t total_bytes = 0;
    fprintf(stdout, "\n---------------- Memory Leak Report ----------------\n");
    for (size_t i = 0; i < mem_dbg_hashTable.num_buckets; i++) {
        if (mem_dbg_hashTable.buckets[i].state != OCCUPIED_BKT) continue;
        Alloc_info *a = &mem_dbg_hashTable.buckets[i].value;
        fprintf(stderr, "   ==> LEAK: %zu bytes\n", a->size);
        leaks++;
        total_bytes += a->size;
    }
    fprintf(stdout, "----------------------------------------------------\n");
    if (leaks){
        fprintf(stderr, "\n----------------------------------------------------\n");
        fprintf(stderr, "TOTAL LEAKS FOUND : %zu, %zu bytes total\n", leaks, total_bytes);
        fprintf(stderr, "----------------------------------------------------\n");
        exit(1);
    }
    else {
        fprintf(stdout, "NO LEAKS FOUND\n");
    }
}
