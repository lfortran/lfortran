/*
 * The engine's discovery of the records of every loaded image, with two
 * object files (test_init_native_a.c, test_init_native_b.c) linked in both
 * orders into test_init_native_ab and test_init_native_ba, with dead
 * stripping. Whichever constructor runs first has to find the records of
 * both and initialize them in stable-id order: m:n1, which depends on m:n2,
 * first. The other constructor then finds nothing left to do.
 */
#include <stdio.h>
#include <string.h>

#include <libasr/runtime/lfortran_intrinsics.h>

static char events[16][32];
static int nevents = 0;

void test_init_native_note(const char *what, const char *id) {
    if (nevents < 16) {
        snprintf(events[nevents], sizeof(events[nevents]), "%s %s", what, id);
        nevents++;
    }
}

int main(void) {
    static const char *const init_order[] = {
        "enter m:n1", "enter m:n2", "ready m:n2", "ready m:n1"};
    int ok = nevents == 6
        && strncmp(events[0], "constructor ", 12) == 0
        && strncmp(events[5], "constructor ", 12) == 0
        && strcmp(events[0], events[5]) != 0;
    for (int i = 0; ok && i < 4; i++) {
        ok = strcmp(events[i + 1], init_order[i]) == 0;
    }
    _lcompilers_init_teardown_all();
    ok = ok && nevents == 8 && strcmp(events[6], "teardown m:n1") == 0
        && strcmp(events[7], "teardown m:n2") == 0;
    if (!ok) {
        printf("FAIL; got:\n");
        for (int i = 0; i < nevents; i++) printf("    %s\n", events[i]);
        return 1;
    }
    printf("ok\n");
    return 0;
}
