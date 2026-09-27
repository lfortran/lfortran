#include <stdio.h>

/* lfortran_initialize(), with LFortran; see ISO_Fortran_binding.h. */
#include <ISO_Fortran_binding.h>

int global_init_24_probe(void);

/*
 * Calls global_init_24_probe before the startup hook of the object file
 * that defines global_init_24_m has run, after starting the runtime, as
 * ISO_Fortran_binding.h asks of a caller that may come first; the startup
 * initializes the module there already. The constructor is registered the
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
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_initialize(0, NULL);
#endif
    early = global_init_24_probe();
}

int main(int argc, char **argv) {
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_initialize(argc, argv);
#else
    (void)argc;
    (void)argv;
#endif
    if (early != 0) {
        printf("probe from the constructor: %d\n", early);
        return 1;
    }
    int late = global_init_24_probe();
    if (late != 0) {
        printf("probe from main: %d\n", late);
        return 2;
    }
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_finalize();
#endif
    printf("ok\n");
    return 0;
}
