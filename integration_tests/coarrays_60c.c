#include <stdint.h>
#include <stdio.h>

/* PRIF's prif_init, as the PRIF implementation compiled by LFortran
 * (Caffeine) defines it, and the host startup entry of the LFortran
 * runtime. */
void __module_prif_prif_init(int32_t *stat);
void lcompilers_initialize(void);
int coarrays_60_run(void);
int coarrays_60_again(void);

int main(void) {
    int32_t stat = -1;
    __module_prif_prif_init(&stat);
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
