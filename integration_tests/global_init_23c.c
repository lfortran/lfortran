#include <dlfcn.h>
#include <stdio.h>

/* lfortran_initialize(), with LFortran; see ISO_Fortran_binding.h. */
#include <ISO_Fortran_binding.h>

int global_init_20_check_initial(void);
void global_init_20_mutate(void);
int global_init_20_check_mutated(void);
int global_init_20_a_first(void);

typedef int (*check_fn)(int);
typedef void (*mutate_fn)(void);

/*
 * global_init_20_a and global_init_20_b are shared libraries this program is
 * linked with; the one whose path is the first argument, holding
 * global_init_20_c, is loaded twice with dlopen, and closed after each time.
 * The program starts the runtime once, first, so the library's own
 * constructors initialize it when it is loaded. Loading it must initialize
 * its module, which depends on global_init_20_a, without initializing
 * global_init_20_a again: the change made to that before the first load has
 * to survive both. Whether dlclose unloads the library is
 * up to the platform, so the second load expects global_init_20_c's storage
 * at its initial state only if the library was really unloaded.
 */
int main(int argc, char **argv) {
    int rc, unloaded = 0;
    if (argc < 2) return 90;
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_initialize(argc, argv);
#endif
    rc = global_init_20_check_initial();
    if (rc) {
        printf("initial check %d\n", rc);
        return 1;
    }
    global_init_20_mutate();
    for (int round = 0; round < 2; round++) {
        void *lib = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
        if (!lib) {
            printf("dlopen: %s\n", dlerror());
            return 91;
        }
        check_fn check = (check_fn)dlsym(lib, "global_init_20_c_check");
        mutate_fn mutate = (mutate_fn)dlsym(lib, "global_init_20_c_mutate");
        if (!check || !mutate) {
            printf("dlsym: %s\n", dlerror());
            return 92;
        }
        rc = check(round == 0 || unloaded);
        if (rc) {
            printf("round %d, load check %d\n", round, rc);
            return 10 + round;
        }
        mutate();
        rc = check(0);
        if (rc) {
            printf("round %d, mutated check %d\n", round, rc);
            return 20 + round;
        }
        rc = global_init_20_check_mutated();
        if (rc) {
            printf("round %d, the linked modules changed: %d\n", round, rc);
            return 30 + round;
        }
        if (dlclose(lib)) {
            printf("dlclose: %s\n", dlerror());
            return 93;
        }
        void *still = dlopen(argv[1], RTLD_NOW | RTLD_NOLOAD);
        unloaded = still == NULL;
        if (still) dlclose(still);
    }
    if (global_init_20_a_first() != 1) {
        printf("global_init_20_a_first\n");
        return 94;
    }
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_finalize();
#endif
    printf("ok (%s)\n", unloaded ? "unloaded" : "not unloaded");
    return 0;
}
