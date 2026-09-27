/*
 * The program of run_host_load_test.cmake: it starts the Fortran runtime,
 * loads the library whose path is its argument and calls the library's
 * bind(c) procedure, which has to find the module state it reaches
 * initialized, as the call the library's C constructor made, if it has
 * one, had to.
 */
#include <dlfcn.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>

int main(int argc, char **argv) {
    if (argc != 2) return 2;
    lfortran_initialize(argc, argv);
    void *lib = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
    if (lib == NULL) {
        printf("dlopen: %s\n", dlerror());
        return 1;
    }
    int *in_ctor = (int *)dlsym(lib, "test_init_host_load_in_ctor");
    void (*entry)(int *) = (void (*)(int *))dlsym(lib, "test_init_host_load_e");
    if (in_ctor == NULL || entry == NULL) return 1;
    int after = -1;
    entry(&after);
    lfortran_finalize();
    printf("in its constructor %d, after its load %d\n", *in_ctor, after);
    return *in_ctor == 7 && after == 7 ? 0 : 1;
}
