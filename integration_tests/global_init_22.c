#include <stdio.h>

int global_init_22_status(void);
int global_init_20_check_mutated(void);

/* See global_init_22c.c. */
int main(void) {
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
    printf("ok\n");
    return 0;
}
