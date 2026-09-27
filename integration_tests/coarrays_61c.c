#include <dlfcn.h>
#include <stdio.h>

/*
 * A host that keeps the coarray runtime loaded (Caffeine is linked into
 * it) while the plugin whose path is its argument, coarrays_61_p.f90, is
 * loaded, closed and loaded again on every image. Each load is followed by
 * the collective startup boundary, lfortran_initialize(): the first starts
 * the runtime, and the second runs the bootstrap of the plugin loaded again,
 * which finds the runtime started and has to accept that.
 */
#include <ISO_Fortran_binding.h>

typedef int (*run_fn)(void);

int main(int argc, char **argv) {
    if (argc < 2) return 2;
    for (int round = 0; round < 2; round++) {
        void *lib = dlopen(argv[1], RTLD_NOW | RTLD_GLOBAL);
        if (!lib) {
            printf("ERROR STOP 91 (dlopen: %s)\n", dlerror());
            return 1;
        }
        lfortran_initialize(argc, argv);
        run_fn run = (run_fn)dlsym(lib, "coarrays_61_run");
        if (!run) {
            printf("ERROR STOP 92 (dlsym: %s)\n", dlerror());
            return 1;
        }
        int rc = run();
        if (rc != 0) {
            /* ci/test_caffeine.sh looks for this: the launcher does not
             * always pass an image's exit status on. */
            printf("ERROR STOP %d (round %d)\n", 10 * (round + 1) + rc, round);
            return 1;
        }
        if (dlclose(lib)) {
            printf("ERROR STOP 93 (dlclose: %s)\n", dlerror());
            return 1;
        }
    }
    lfortran_finalize();
    printf("ok\n");
    return 0;
}
