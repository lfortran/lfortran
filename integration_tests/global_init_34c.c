/* lfortran_initialize(), with LFortran; see ISO_Fortran_binding.h. */
#include <ISO_Fortran_binding.h>

/* A library's own start of the runtime, with a command line of its own;
 * see global_init_34.f90. */
void global_init_34_lib_start(void) {
#ifdef LFORTRAN_HAS_INITIALIZE
    char prog[] = "library", extra[] = "extra";
    char *argv[] = {prog, extra, NULL};
    lfortran_initialize(2, argv);
#endif
}
