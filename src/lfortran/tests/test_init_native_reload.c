/*
 * A library loaded, unloaded and loaded again, most likely at the same
 * address, where the engine keeps a retired entry for the first load's
 * table. The library it needs dispatches from its constructor before the
 * library's own constructor runs: that dispatch already initializes the new
 * load, which the engine tells from the unloaded one by the table's mapping
 * word, not by the constructor. The argument is the library's path. A run
 * where the second load is at another address shows nothing of that, and is
 * skipped (exit status 77).
 */
#include <dlfcn.h>
#include <stdio.h>
#include <string.h>

static char events[32][32];
static int nevents = 0;

void test_init_reload_note(const char *what, const char *id) {
    if (nevents < 32) {
        snprintf(events[nevents], sizeof(events[nevents]), "%s %s", what, id);
        nevents++;
    }
}

int main(int argc, char **argv) {
    static const char *const load[] = {
        "constructor f", "ready m:ne", "constructor m:ne"};
    int failures = 0, unloaded = 1;
    const void *first_table = NULL;
    if (argc < 2) return 2;
    for (int round = 0; round < 2 && unloaded; round++) {
        int at = nevents;
        void *lib = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
        if (lib == NULL) {
            printf("FAIL: dlopen: %s\n", dlerror());
            return 1;
        }
        const void *(*table)(void) = (const void *(*)(void))dlsym(lib,
            "test_init_native_e_table");
        if (table == NULL) {
            printf("FAIL: dlsym: %s\n", dlerror());
            return 1;
        }
        if (round == 0) {
            first_table = table();
        } else if (table() != first_table) {
            printf("SKIP: the library was loaded again at another address\n");
            dlclose(lib);
            return 77;
        }
        int ok = nevents == at + 3;
        for (int i = 0; ok && i < 3; i++) ok = strcmp(events[at + i], load[i]) == 0;
        if (!ok) {
            printf("FAIL: load %d; events:\n", round + 1);
            for (int i = 0; i < nevents; i++) printf("    %s\n", events[i]);
            failures++;
        }
        dlclose(lib);
        void *still = dlopen(argv[1], RTLD_NOW | RTLD_NOLOAD);
        unloaded = still == NULL;
        if (still) dlclose(still);
    }
    if (failures == 0) printf("ok (%s)\n", unloaded ? "unloaded" : "not unloaded");
    return failures != 0;
}
