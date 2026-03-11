#include <stdio.h>
#include <stdlib.h>
#define MEM_DEBUG_IMPL /*Stop malloc, free, realloc, etc redirection*/
#include "mem_debug.h"

typedef struct Alloc {
    const void   *ptr;
    size_t       size;
    // const char  *file;
    // int          row;
    // int          col;


    struct Alloc *next; // Table Collisions
} Alloc;


#define TABLE_SIZE 1024
static Alloc *table[TABLE_SIZE] = {NULL};

unsigned int hash_ptr(void* const ptr) { // Thomas Wang hash
    unsigned long v = (unsigned long)ptr;
    v ^= v >> 16;
    v *= 0x45d9f3b;
    v ^= v >> 16;
    return (unsigned int)(v % TABLE_SIZE);
}

// Register ptr into table
void register_ptr(void* const ptr, size_t size){
    Alloc *a  = malloc(sizeof(Alloc));
    a->ptr    = ptr;
    a->size   = size;
    
    unsigned int h = hash_ptr(ptr);
    // push node into table
    a->next   = table[h];
    table[h]  = a;
    
}

void* dbg_malloc(size_t size) {
    void *ptr = malloc(size);
    if(size && !ptr) fprintf(stderr, "Unexpected error at dbg_malloc");
    register_ptr(ptr, size);
    return ptr;

}

void* dbg_calloc(size_t nmemb, size_t size) {
    void *ptr = calloc(nmemb, size);
    if((size*nmemb) && !ptr) fprintf(stderr, "Unexpected error at dbg_calloc");
    register_ptr(ptr, size * nmemb);
    return ptr;
}

void  *dbg_realloc(void *ptr, size_t size){
    if (!ptr) {
        return dbg_malloc(size);
    }
    if (size == 0) {
        dbg_free(ptr);
        return NULL;
    }

    if(!ptr && !size) fprintf(stderr, "Unhandled case at dbg_realloc");

    void *new_ptr = realloc(ptr, size);
    if(size && !ptr) fprintf(stderr, "Unexpected error at dbg_realloc");

    
    // Fetch registered alloc node.
    unsigned int h = hash_ptr(ptr);
    Alloc **node = &table[h];
    while (*node && (*node)->ptr != ptr) {
        node = &((*node)->next);
    }

    // Update size + re-register node
    if (*node) {
        Alloc *found = *node;
        found->size = size;
        if (new_ptr != ptr) {
            *node = found->next;
            found->ptr = new_ptr;
            unsigned int new_h = hash_ptr(new_ptr);
            found->next = table[new_h];
            table[new_h] = found;
        }
    } else {    
        register_ptr(new_ptr, size);
    }
    return new_ptr;
}



void dbg_free(void *ptr) {
    if (!ptr) return;

    unsigned int h = hash_ptr(ptr);
    Alloc** node = &table[h];
    while (*node) {
        if ((*node)->ptr == ptr) {
            Alloc *found = *node;
            *node = found->next;
            free(found); // Free `found` node
            free(ptr);  // Free allocated memory
            return;
        }
        node = &((*node)->next);
    }
}

void dbg_report() {
    size_t leaks = 0, bytes = 0;
    for (int i = 0; i < TABLE_SIZE; i++) {
        for (Alloc *a = table[i]; a != NULL; a = a->next) {
            fprintf(stderr, "LEAK: %zu bytes\n", a->size);
            leaks++;
            bytes += a->size;
        }
    }
    if (leaks){
        fprintf(stderr, "Total Leaks Found : %zu, %zu bytes total\n", leaks, bytes);
        exit(1);
    }
    else {
        fprintf(stderr, "no leaks\n");
    }
}
