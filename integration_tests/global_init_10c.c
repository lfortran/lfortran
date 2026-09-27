#include <stdlib.h>

/* lfortran_initialize(), with LFortran; see ISO_Fortran_binding.h. */
#include <ISO_Fortran_binding.h>

int global_init_10_initial(void);
void global_init_10_set(void);
void global_init_10_check(void);

/*
 * Changes the module's variables before the startup hook of the object file
 * that defines the module has run, which must not undo the change. That
 * is before the runtime is started too, which ISO_Fortran_binding.h does
 * not promise to work; it does for variables whose initial state is static
 * data, and startup never stores a value static data holds. LLVM
 * registers that hook with the default priority, and this constructor runs
 * ahead of it:
 * - on ELF, because of its priority;
 * - on Mach-O, whose object files carry no priorities (GCC rejects one there)
 *   and whose constructors run in link order, because this object file comes
 *   before the module's on the link line: CMakeLists.txt lists
 *   global_init_10c.c ahead of global_init_10_m.f90;
 * - with the MSVC runtime, which runs the pointers in the .CRT$XC* sections
 *   in the order of the section names, because the hook is in .CRT$XCU and
 *   this constructor in .CRT$XCT. Nothing refers to the pointer, so /include
 *   keeps the linker from discarding it.
 */
#if defined(_MSC_VER)
static void change_before_module_startup(void);
#pragma section(".CRT$XCT", read)
__declspec(allocate(".CRT$XCT")) void (*global_init_10_crt_initializer)(void) =
    change_before_module_startup;
#pragma comment(linker, "/include:global_init_10_crt_initializer")
#elif defined(__APPLE__)
__attribute__((constructor)) static void change_before_module_startup(void);
#else
__attribute__((constructor(101))) static void change_before_module_startup(void);
#endif

static void change_before_module_startup(void) {
    /* The static data already holds the initial state. */
    if (!global_init_10_initial()) exit(2);
    global_init_10_set();
}

int main(int argc, char **argv) {
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_initialize(argc, argv);
#else
    (void)argc;
    (void)argv;
#endif
    global_init_10_check();
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_finalize();
#endif
    return 0;
}
