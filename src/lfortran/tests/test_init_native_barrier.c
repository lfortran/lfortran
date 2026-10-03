/*
 * glibc lists an image while another thread's dlopen maps it, before it is
 * relocated, and unmaps it again when that load fails. The engine reads the
 * notes of the loaded images only in a scan that saw no image added since
 * its barrier: here a load that fails relocation (of test_init_native_bad.c,
 * the argument) maps an image between the barrier and the scan, which has
 * to start over, and never runs that image's records.
 */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <link.h>
#include <pthread.h>
#include <stdio.h>
#include <time.h>

#include <libasr/runtime/lfortran_intrinsics.h>

void _lcompilers_init_test_discovery_pause(void (*pause)(void));
uint64_t _lcompilers_init_test_discovery_retries(void);

static const char *bad_path;
static int paused = 0, loaded = 0;

static int count_adds(struct dl_phdr_info *info, size_t size, void *data) {
    (void)size;
    *(unsigned long long *)data = info->dlpi_adds;
    return 1;
}

static unsigned long long adds(void) {
    unsigned long long n = 0;
    dl_iterate_phdr(count_adds, &n);
    return n;
}

static void *load_bad(void *unused) {
    (void)unused;
    void *lib = dlopen(bad_path, RTLD_NOW | RTLD_LOCAL);
    if (lib != NULL) {
        loaded = 1;
        dlclose(lib);
    }
    return NULL;
}

/* Between the barrier and the scan, once: a load maps an image and fails. */
static void pause_once(void) {
    if (paused) return;
    paused = 1;
    unsigned long long before = adds();
    pthread_t t;
    if (pthread_create(&t, NULL, load_bad, NULL) != 0) return;
    pthread_join(t, NULL);
    if (adds() == before) {
        printf("FAIL: the failing load mapped no image\n");
    }
}

static const lcompilers_init_table nudge = {lcompilers_init_abi_version, 0, NULL};

int main(int argc, char **argv) {
    if (argc < 2) return 2;
    bad_path = argv[1];
    _lcompilers_init_test_discovery_pause(pause_once);
    _lcompilers_init_add_records(&nudge);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    _lcompilers_init_test_discovery_pause(NULL);
    _lcompilers_init_remove_records(&nudge);
    if (!paused || loaded || _lcompilers_init_test_discovery_retries() == 0) {
        printf("FAIL: paused %d, loaded %d, retries %llu\n", paused, loaded,
            (unsigned long long)_lcompilers_init_test_discovery_retries());
        return 1;
    }
    printf("ok\n");
    return 0;
}
