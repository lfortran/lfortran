#include <stdio.h>

/* lfortran_initialize(), the host startup of the LFortran runtime, called
 * on every image. */
#include <ISO_Fortran_binding.h>

int coarrays_59_run(void);

int main(int argc, char **argv) {
    lfortran_initialize(argc, argv);
    int rc = coarrays_59_run();
    if (rc != 0) {
        /* ci/test_caffeine.sh looks for this: the launcher does not always
         * pass an image's exit status on. */
        printf("ERROR STOP %d\n", rc);
        return 1;
    }
    lfortran_finalize();
    printf("ok\n");
    return 0;
}
