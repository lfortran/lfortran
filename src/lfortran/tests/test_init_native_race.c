/*
 * Loader events racing the engine, one scenario per process, the first
 * argument; the others are the paths of test_init_native_d.c and
 * test_init_native_bad.c built as shared libraries.
 *
 * failed_load: one thread keeps trying to load the library whose load
 *     fails, while another keeps walking the records: the walk never runs,
 *     nor reads, the records of an image whose load does not complete.
 * teardown_race: the library is loaded and unloaded over and over while
 *     another thread keeps tearing everything down: every initialization of
 *     its record is torn down exactly once, by the one or by the unload.
 * reload_during_walk: the library is unloaded and loaded again while
 *     another thread keeps walking: a load after an unload starts out
 *     uninitialized and is initialized again.
 * first_registration: in a process that never entered the engine, one
 *     thread loads the library while another enters first, through a batch
 *     of its own: neither waits for the other (a hang fails the test on its
 *     time limit), and both are initialized.
 */
#include <dlfcn.h>
#include <pthread.h>
#include <stdio.h>
#include <string.h>

#include <libasr/runtime/lfortran_intrinsics.h>

static pthread_mutex_t count_lock = PTHREAD_MUTEX_INITIALIZER;
static int inits = 0, teardowns = 0;

void test_init_race_count(int teardown) {
    pthread_mutex_lock(&count_lock);
    if (teardown) teardowns++; else inits++;
    pthread_mutex_unlock(&count_lock);
}

static int counted(int teardown) {
    pthread_mutex_lock(&count_lock);
    int n = teardown ? teardowns : inits;
    pthread_mutex_unlock(&count_lock);
    return n;
}

static pthread_mutex_t stop_lock = PTHREAD_MUTEX_INITIALIZER;
static int stop = 0;

static int stopping(void) {
    pthread_mutex_lock(&stop_lock);
    int s = stop;
    pthread_mutex_unlock(&stop_lock);
    return s;
}

static void stop_all(void) {
    pthread_mutex_lock(&stop_lock);
    stop = 1;
    pthread_mutex_unlock(&stop_lock);
}

/* An empty batch: publishing and withdrawing it changes the generation, so
 * that every dispatch walks the records again. */
static const lcompilers_init_table nudge = {lcompilers_init_abi_version, 0, NULL};

static void *keep_walking(void *unused) {
    (void)unused;
    while (!stopping()) {
        _lcompilers_init_add_records(&nudge);
        _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
        _lcompilers_init_remove_records(&nudge);
    }
    return NULL;
}

static void *keep_tearing_down(void *unused) {
    (void)unused;
    while (!stopping()) _lcompilers_init_teardown_all();
    return NULL;
}

static const char *bad_path;

static void *keep_failing(void *loaded) {
    for (int i = 0; i < 200; i++) {
        void *lib = dlopen(bad_path, RTLD_NOW | RTLD_LOCAL);
        if (lib != NULL) {
            *(int *)loaded = 1;
            dlclose(lib);
        }
    }
    return NULL;
}

static int scenario_failed_load(void) {
    pthread_t walker, loader;
    int loaded = 0;
    if (pthread_create(&walker, NULL, keep_walking, NULL) != 0) return 2;
    if (pthread_create(&loader, NULL, keep_failing, &loaded) != 0) return 2;
    pthread_join(loader, NULL);
    stop_all();
    pthread_join(walker, NULL);
    if (loaded) {
        printf("FAIL: the library that needs a missing symbol loaded\n");
        return 1;
    }
    printf("ok\n");
    return 0;
}

static int still_loaded(const char *path) {
    void *lib = dlopen(path, RTLD_NOW | RTLD_NOLOAD);
    if (lib != NULL) dlclose(lib);
    return lib != NULL;
}

static int scenario_load_loop(const char *path, void *(*other)(void *),
        int check_reload) {
    pthread_t t;
    int fresh_loads = 0, failures = 0, unloaded = 1;
    if (pthread_create(&t, NULL, other, NULL) != 0) return 2;
    for (int i = 0; i < 100; i++) {
        int before = counted(0);
        void *lib = dlopen(path, RTLD_NOW | RTLD_LOCAL);
        if (lib == NULL) {
            printf("FAIL: dlopen: %s\n", dlerror());
            failures++;
            break;
        }
        if (unloaded) {
            fresh_loads++;
            if (check_reload && counted(0) != before + 1) {
                printf("FAIL: load %d was not initialized once\n", i);
                failures++;
            }
        }
        dlclose(lib);
        unloaded = !still_loaded(path);
    }
    stop_all();
    pthread_join(t, NULL);
    _lcompilers_init_teardown_all();
    if (check_reload && counted(0) != fresh_loads) {
        printf("FAIL: %d initializations for %d fresh loads\n", counted(0),
            fresh_loads);
        failures++;
    }
    if (counted(1) != counted(0)) {
        printf("FAIL: %d teardowns for %d initializations\n", counted(1),
            counted(0));
        failures++;
    }
    if (failures == 0) printf("ok (%s)\n", unloaded ? "unloaded" : "not unloaded");
    return failures != 0;
}

static int32_t st_first;
static int first_bodies = 0;

static void ensure_first(void) {
    if (_lcompilers_init_begin(&st_first)) {
        first_bodies++;
        _lcompilers_init_end(&st_first);
    }
}

static const lcompilers_init_record first_records[] = {
    {"m:first", ensure_first, NULL, &st_first, 0, 0},
};
static const lcompilers_init_table first_table = {
    lcompilers_init_abi_version, 1, first_records};

static const char *d_path;

static void *load_d(void *lib) {
    *(void **)lib = dlopen(d_path, RTLD_NOW | RTLD_LOCAL);
    return NULL;
}

static void *enter_first(void *unused) {
    (void)unused;
    _lcompilers_init_add_records(&first_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    return NULL;
}

static int scenario_first_registration(void) {
    pthread_t loader, enterer;
    void *lib = NULL;
    if (pthread_create(&loader, NULL, load_d, &lib) != 0) return 2;
    if (pthread_create(&enterer, NULL, enter_first, NULL) != 0) return 2;
    pthread_join(loader, NULL);
    pthread_join(enterer, NULL);
    int ok = lib != NULL && counted(0) == 1 && first_bodies == 1
        && st_first == lcompilers_init_ready;
    if (lib != NULL) dlclose(lib);
    _lcompilers_init_remove_records(&first_table);
    if (!ok) {
        printf("FAIL: loaded %d, library initialized %d, batch initialized %d\n",
            lib != NULL, counted(0), first_bodies);
        return 1;
    }
    printf("ok\n");
    return 0;
}

int main(int argc, char **argv) {
    if (argc < 4) return 2;
    d_path = argv[2];
    bad_path = argv[3];
    const char *s = argv[1];
    if (strcmp(s, "failed_load") == 0) return scenario_failed_load();
    if (strcmp(s, "teardown_race") == 0) {
        return scenario_load_loop(d_path, keep_tearing_down, 0);
    }
    if (strcmp(s, "reload_during_walk") == 0) {
        return scenario_load_loop(d_path, keep_walking, 1);
    }
    if (strcmp(s, "first_registration") == 0) return scenario_first_registration();
    printf("unknown scenario '%s'\n", s);
    return 2;
}
