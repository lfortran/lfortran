#include <stdlib.h>
#include "../src/libasr/runtime/lfortran_intrinsics.h"

static int calls;
static int fail_at;

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
}

int stop_failures(void)
{
    fail_at = 0;
    return calls;
}

int failure_mode(void)
{
    const char* setting = getenv("LFORTRAN_TEST_FAILURE_MODE");
    return setting ? atoi(setting) : 0;
}
