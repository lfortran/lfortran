#include <stdio.h>

int global_init_20_check_initial(void);
void global_init_20_mutate(void);
int global_init_20_check_mutated(void);
int global_init_20_a_first(void);

/*
 * global_init_20_a and global_init_20_b as two shared libraries, the second
 * depending on the first, linked with dead stripping into this program in
 * both orders; see CMakeLists.txt.
 */
int main(void) {
    int rc = global_init_20_check_initial();
    if (rc) {
        printf("initial check %d\n", rc);
        return 1;
    }
    global_init_20_mutate();
    rc = global_init_20_check_mutated();
    if (rc) {
        printf("mutated check %d\n", rc);
        return 2;
    }
    if (global_init_20_a_first() != 1) {
        printf("global_init_20_a_first\n");
        return 3;
    }
    printf("ok\n");
    return 0;
}
