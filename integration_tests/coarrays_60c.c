#include <stdint.h>
#include <stdio.h>

/* The start of the coarray runtime (src/runtime/prif/lcompilers_prif.f90)
 * and the host startup entry of the LFortran runtime. */
void lcompilers_prif_start(int32_t *stat);
void lcompilers_initialize(void);
int coarrays_60_run(void);
int coarrays_60_again(void);

int main(void) {
    int32_t stat = -1;
    lcompilers_prif_start(&stat);
    if (stat != 0) {
        /* ci/test_caffeine.sh looks for this: the launcher does not always
         * pass an image's exit status on. */
        printf("ERROR STOP 90 (the host's start of the runtime: %d)\n", (int)stat);
        return 1;
    }
    lcompilers_initialize();
    int rc = coarrays_60_run();
    if (rc != 0) {
        printf("ERROR STOP %d\n", rc);
        return 1;
    }
    lcompilers_initialize();
    rc = coarrays_60_again();
    if (rc != 0) {
        printf("ERROR STOP %d (after the second startup)\n", 10 + rc);
        return 1;
    }
    printf("ok\n");
    return 0;
}
