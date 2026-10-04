#include <dlfcn.h>
#include <stdio.h>
#include <string.h>

/*
 * A C program linked with nothing but the dynamic loader, so the Fortran
 * runtime comes into the process only with the first plugin it loads, and
 * may leave it again when the plugins are closed. Its arguments are the
 * paths of two plugins, global_init_27_p and global_init_27_q. It loads and
 * closes the first, then the second, then the first again, and each load
 * has to find its plugin's modules initialized: fresh if the plugin was
 * really unloaded, as they were left otherwise. Nothing the runtime set up
 * for an earlier load may be called once that is gone.
 *
 * On macOS the LFortran runtime installs a loader notification that cannot
 * be removed again, so it keeps its own image loaded for the rest of the
 * process: that is checked once every plugin is closed. Elsewhere it may
 * unload with the last plugin, and on ELF it installs no notification.
 */
typedef int (*check_fn)(int);
typedef void (*mutate_fn)(void);

#if defined(__APPLE__)
/* The path of the image that defines the LFortran runtime's dispatcher, as
 * the plugin at `lib` resolves it; empty for a runtime without one. */
static char runtime_path[1024];

static void find_runtime(void *lib) {
    Dl_info info;
    void *dispatch = dlsym(lib, "_lcompilers_init_dispatch");
    if (dispatch && dladdr(dispatch, &info) && info.dli_fname) {
        snprintf(runtime_path, sizeof(runtime_path), "%s", info.dli_fname);
    }
}
#endif

static int unloaded(const char *path) {
    void *still = dlopen(path, RTLD_NOW | RTLD_NOLOAD);
    if (still) dlclose(still);
    return still == NULL;
}

static int use_plugin(const char *path, const char *name, int fresh) {
    char sym[64];
    void *lib = dlopen(path, RTLD_NOW | RTLD_LOCAL);
    if (!lib) {
        printf("dlopen %s: %s\n", name, dlerror());
        return 90;
    }
#if defined(__APPLE__)
    if (runtime_path[0] == '\0') find_runtime(lib);
#endif
    snprintf(sym, sizeof(sym), "global_init_27_%s_check", name);
    check_fn check = (check_fn)dlsym(lib, sym);
    snprintf(sym, sizeof(sym), "global_init_27_%s_mutate", name);
    mutate_fn mutate = (mutate_fn)dlsym(lib, sym);
    if (!check || !mutate) {
        printf("dlsym %s: %s\n", name, dlerror());
        return 91;
    }
    int rc = check(fresh);
    if (rc) {
        printf("%s: load check %d\n", name, rc);
        return 10 + rc;
    }
    mutate();
    rc = check(0);
    if (rc) {
        printf("%s: mutated check %d\n", name, rc);
        return 20 + rc;
    }
    if (dlclose(lib)) {
        printf("dlclose %s: %s\n", name, dlerror());
        return 92;
    }
    return 0;
}

int main(int argc, char **argv) {
    int rc;
    if (argc < 3) return 2;
    if ((rc = use_plugin(argv[1], "p", 1)) != 0) return rc;
    int p_unloaded = unloaded(argv[1]);
    if ((rc = use_plugin(argv[2], "q", 1)) != 0) return rc;
    if ((rc = use_plugin(argv[1], "p", p_unloaded)) != 0) return rc;
#if defined(__APPLE__)
    if (runtime_path[0] != '\0' && unloaded(runtime_path)) {
        printf("the runtime %s was unloaded with the plugins\n", runtime_path);
        return 3;
    }
#endif
    printf("ok (%s)\n", p_unloaded ? "unloaded" : "not unloaded");
    return 0;
}
