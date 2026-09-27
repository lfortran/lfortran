/* lfortran_initialize(), with LFortran; see ISO_Fortran_binding.h. */
#include <ISO_Fortran_binding.h>

void global_init_17_s(void);

int main(int argc, char **argv) {
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_initialize(argc, argv);
#else
    (void)argc;
    (void)argv;
#endif
    global_init_17_s();
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_finalize();
#endif
    return 0;
}
