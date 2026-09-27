#include <stdio.h>

/* lfortran_initialize(), with LFortran; see ISO_Fortran_binding.h. */
#include <ISO_Fortran_binding.h>

int global_init_22_status(void);
int global_init_20_check_mutated(void);

/* See global_init_22c.c. */
int main(int argc, char **argv) {
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_initialize(argc, argv);
#else
    (void)argc;
    (void)argv;
#endif
    int rc = global_init_22_status();
    if (rc) {
        printf("initial check in the constructor %d\n", rc);
        return 1;
    }
    rc = global_init_20_check_mutated();
    if (rc) {
        printf("mutated check %d\n", rc);
        return 2;
    }
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_finalize();
#endif
    printf("ok\n");
    return 0;
}
