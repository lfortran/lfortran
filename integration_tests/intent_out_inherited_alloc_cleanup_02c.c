#include <stddef.h>
#include "../src/libasr/runtime/lfortran_intrinsics.h"
#include <stdio.h>

static lfortran_allocator_t original;
static lfortran_allocator_t *active;
static int allocations;
static int outstanding;

static void *count_alloc(void *context, int64_t size)
{
    (void)context;
    void *result = original.alloc(original.context, size);
    if (result) {
        allocations++;
        outstanding++;
    }
    return result;
}

static void *count_realloc(void *context, void *ptr, int64_t size)
{
    (void)context;
    void *result = original.realloc_func(original.context, ptr, size);
    if (!ptr && result) {
        allocations++;
        outstanding++;
    }
    return result;
}

static void count_dealloc(void *context, void *ptr)
{
    (void)context;
    if (ptr) outstanding--;
    original.dealloc(original.context, ptr);
}

void begin_count(void)
{
    active = _lfortran_get_default_allocator();
    original = *active;
    allocations = 0;
    outstanding = 0;
    active->alloc = count_alloc;
    active->realloc_func = count_realloc;
    active->dealloc = count_dealloc;
}

int end_count(void)
{
    *active = original;
    fprintf(stderr, "inherited cleanup: %d allocations, %d outstanding\n",
        allocations, outstanding);
    return allocations ? outstanding : -1;
}
