#include <stdint.h>
#include <stdio.h>

/* lfortran_initialize(), the host startup of the LFortran runtime. */
#include <ISO_Fortran_binding.h>

/* PRIF's prif_init, as the PRIF implementation compiled by LFortran
 * (Caffeine) defines it. */
void __module_prif_prif_init(int32_t *stat);
int coarrays_60_run(void);
int coarrays_60_again(void);

int main(int argc, char **argv) {
    int32_t stat = -1;
    __module_prif_prif_init(&stat);
    if (stat != 0) {
        /* ci/test_caffeine.sh looks for this: the launcher does not always
         * pass an image's exit status on. */
        printf("ERROR STOP 90 (the host's start of the runtime: %d)\n", (int)stat);
        return 1;
    }
    lfortran_initialize(argc, argv);
    int rc = coarrays_60_run();
    if (rc != 0) {
        printf("ERROR STOP %d\n", rc);
        return 1;
    }
    lfortran_initialize(argc, argv);
    rc = coarrays_60_again();
    if (rc != 0) {
        printf("ERROR STOP %d (after the second startup)\n", 10 + rc);
        return 1;
    }
    lfortran_finalize();
    printf("ok\n");
    return 0;
}
