#include <stdint.h>
#include <stdio.h>

/* lfortran_initialize(), the host startup of the LFortran runtime. */
#include <ISO_Fortran_binding.h>

/* The start of the coarray runtime (src/runtime/prif/lcompilers_prif.f90). */
void lcompilers_prif_start(int32_t *stat);
int coarrays_60_run(void);
int coarrays_60_again(void);

int main(int argc, char **argv) {
    int32_t stat = -1;
    lcompilers_prif_start(&stat);
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
