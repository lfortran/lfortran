/*
 * Loads the library of run_entry_load_test.cmake, whose C constructor calls
 * the library's bind(c) entry point during the load, and calls the entry
 * point again after the load: both calls have to find the module state the
 * entry point reaches initialized.
 */
#include <dlfcn.h>
#include <stdio.h>

int main(int argc, char **argv) {
    if (argc != 2) return 2;
    void *lib = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
    if (lib == NULL) {
        printf("dlopen: %s\n", dlerror());
        return 1;
    }
    int *in_ctor = (int *)dlsym(lib, "test_init_entry_load_in_ctor");
    void (*entry)(int *) = (void (*)(int *))dlsym(lib, "test_init_entry_load_e");
    if (in_ctor == NULL || entry == NULL) return 1;
    int after = -1;
    entry(&after);
    printf("in its constructor %d, after its load %d\n", *in_ctor, after);
    return *in_ctor == 7 && after == 7 ? 0 : 1;
}
