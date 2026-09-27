#include <stdio.h>

/* The host startup entry of the LFortran runtime, called on every image. */
void lcompilers_initialize(void);
int coarrays_59_run(void);

int main(void) {
    lcompilers_initialize();
    int rc = coarrays_59_run();
    if (rc != 0) {
        /* ci/test_caffeine.sh looks for this: the launcher does not always
         * pass an image's exit status on. */
        printf("ERROR STOP %d\n", rc);
        return 1;
    }
    printf("ok\n");
    return 0;
}
