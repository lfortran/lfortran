#include <stdio.h>

int global_init_24_probe(void);

/*
 * Calls global_init_24_probe before the startup hook of the object file
 * that defines global_init_24_m has run. The constructor is registered the
 * way global_init_10c.c describes, which is what makes it run ahead of that
 * hook: CMakeLists.txt lists this file ahead of the module's.
 */
static int early = -1;

#if defined(_MSC_VER)
static void call_before_module_startup(void);
#pragma section(".CRT$XCT", read)
__declspec(allocate(".CRT$XCT")) void (*global_init_24_crt_initializer)(void) =
    call_before_module_startup;
#pragma comment(linker, "/include:global_init_24_crt_initializer")
#elif defined(__APPLE__)
__attribute__((constructor)) static void call_before_module_startup(void);
#else
__attribute__((constructor(101))) static void call_before_module_startup(void);
#endif

static void call_before_module_startup(void) {
    early = global_init_24_probe();
}

int main(void) {
    if (early != 0) {
        printf("probe from the constructor: %d\n", early);
        return 1;
    }
    int late = global_init_24_probe();
    if (late != 0) {
        printf("probe from main: %d\n", late);
        return 2;
    }
    printf("ok\n");
    return 0;
}
