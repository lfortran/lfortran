/*
 * Linked into one shared library with global_init_20_b and global_init_20_a,
 * ahead of them, so that this constructor calls into the library's Fortran
 * code before the library's own startup hooks have run: on ELF because of
 * its priority, on Mach-O because constructors of one image run in link
 * order. It starts the runtime first, as ISO_Fortran_binding.h asks of a
 * caller that may come before it is started, which initializes the modules
 * there already; nothing that runs later may undo what this constructor
 * changes. global_init_22.c checks both.
 */
/* lfortran_initialize(), with LFortran; see ISO_Fortran_binding.h. */
#include <ISO_Fortran_binding.h>

int global_init_20_check_initial(void);
void global_init_20_mutate(void);

static int status = -1;

#if defined(__APPLE__)
__attribute__((constructor)) static void call_before_module_startup(void);
#else
__attribute__((constructor(101))) static void call_before_module_startup(void);
#endif

static void call_before_module_startup(void) {
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_initialize(0, NULL);
#endif
    status = global_init_20_check_initial();
    if (status == 0) global_init_20_mutate();
}

int global_init_22_status(void) {
    return status;
}
