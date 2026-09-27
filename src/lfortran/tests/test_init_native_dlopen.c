/*
 * Records of an image loaded, unloaded and loaded again at run time: the
 * program defines m:n2 (test_init_native_a.c) and loads the shared library
 * whose path is its argument, holding m:nc, which depends on m:n2
 * (test_init_native_c.c). Loading the library initializes m:nc and nothing
 * the program already initialized; unloading it tears m:nc down, before the
 * library's code is gone; loading it again, if it really was unloaded,
 * initializes a fresh m:nc. The final teardown then finds only m:n2.
 */
#include <dlfcn.h>
#include <stdio.h>
#include <string.h>

#include <libasr/runtime/lfortran_intrinsics.h>

static char events[32][32];
static int nevents = 0;

void test_init_native_note(const char *what, const char *id) {
    if (nevents < 32) {
        snprintf(events[nevents], sizeof(events[nevents]), "%s %s", what, id);
        nevents++;
    }
}

static int failures = 0;

static void expect_next(int *at, const char *const *expected, int n, const char *what) {
    int ok = *at + n <= nevents;
    for (int i = 0; ok && i < n; i++) {
        ok = strcmp(events[*at + i], expected[i]) == 0;
    }
    if (!ok) {
        printf("FAIL: %s; events so far:\n", what);
        for (int i = 0; i < nevents; i++) printf("    %s\n", events[i]);
        failures++;
    }
    *at += n;
}

int main(int argc, char **argv) {
    static const char *const startup[] = {
        "constructor m:n2", "enter m:n2", "ready m:n2"};
    static const char *const load[] = {
        "constructor m:nc", "enter m:nc", "ready m:nc"};
    static const char *const unload[] = {"teardown m:nc"};
    static const char *const end[] = {"teardown m:n2"};
    int at = 0;
    if (argc < 2) return 2;
    expect_next(&at, startup, 3, "program startup");
    int unloaded = 1;
    for (int round = 0; round < 2; round++) {
        void *lib = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
        if (!lib) {
            printf("FAIL: dlopen: %s\n", dlerror());
            return 1;
        }
        if (unloaded) {
            expect_next(&at, load, 3, "library load");
        } else if (at != nevents) {
            printf("FAIL: a library that stayed loaded was initialized again\n");
            failures++;
            at = nevents;
        }
        if (dlclose(lib)) {
            printf("FAIL: dlclose: %s\n", dlerror());
            return 1;
        }
        void *still = dlopen(argv[1], RTLD_NOW | RTLD_NOLOAD);
        unloaded = still == NULL;
        if (still) dlclose(still);
        if (unloaded) expect_next(&at, unload, 1, "library unload");
    }
    _lcompilers_init_teardown_all();
    expect_next(&at, end, 1, "final teardown");
    if (at != nevents) {
        printf("FAIL: unexpected events after the final teardown\n");
        failures++;
    }
    if (failures == 0) printf("ok (%s)\n", unloaded ? "unloaded" : "not unloaded");
    return failures != 0;
}
