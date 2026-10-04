/*
 * Loads test_init_native_malformed.c, the argument, and dispatches; see
 * there. Reaching the end is a failure.
 */
#include <dlfcn.h>
#include <stdio.h>

#include <libasr/runtime/lfortran_intrinsics.h>

/* Publishing it changes the generation, so that the dispatch scans. */
static const lcompilers_init_table nudge = {lcompilers_init_abi_version, 0, NULL, NULL};

int main(int argc, char **argv) {
    if (argc < 2) return 2;
    void *lib = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
    if (lib == NULL) {
        printf("dlopen: %s\n", dlerror());
        return 1;
    }
    _lcompilers_init_add_records(&nudge);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    printf("malformed table not detected\n");
    return 1;
}
