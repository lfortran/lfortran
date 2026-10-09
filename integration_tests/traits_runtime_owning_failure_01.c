#include <stdlib.h>
#include "../src/libasr/runtime/lfortran_intrinsics.h"

static int calls;
static int fail_at;
static lfortran_allocator_t debug_base;

static void* test_alloc(void* context, int64_t size)
{
    (void)context;
    if (++calls == fail_at) return NULL;
    return malloc((size_t)size);
}

static void* test_realloc(void* context, void* ptr, int64_t size)
{
    (void)context;
    if (++calls == fail_at) return NULL;
    return realloc(ptr, (size_t)size);
}

static void test_dealloc(void* context, void* ptr)
{
    (void)context;
    free(ptr);
}

static void* test_debug_alloc(void* context, int64_t size)
{
    (void)context;
    if (++calls == fail_at) return NULL;
    return debug_base.alloc(debug_base.context, size);
}

static void* test_debug_realloc(void* context, void* ptr, int64_t size)
{
    (void)context;
    if (++calls == fail_at) return NULL;
    return debug_base.realloc_func(debug_base.context, ptr, size);
}

lfortran_allocator_t* _lfortran_get_default_allocator(void)
{
    static lfortran_allocator_t allocator = {
        test_alloc, test_realloc, test_dealloc, NULL
    };
    return &allocator;
}

void start_failures(void)
{
    const char* setting = getenv("LFORTRAN_TEST_FAIL_AT");
    calls = 0;
    fail_at = setting ? atoi(setting) : 0;
    // Instrument the selected debug allocator without bypassing its tracking.
    lfortran_allocator_t* debug = _lfortran_get_compiler_mem_dbg_allocator();
    debug_base = *debug;
    debug->alloc = test_debug_alloc;
    debug->realloc_func = test_debug_realloc;
}

int stop_failures(void)
{
    fail_at = 0;
    *_lfortran_get_compiler_mem_dbg_allocator() = debug_base;
    return calls;
}

int failure_mode(void)
{
    const char* setting = getenv("LFORTRAN_TEST_FAILURE_MODE");
    return setting ? atoi(setting) : 0;
}
