#include <stdio.h>
#include <stdlib.h>

void _lfortran_register_module_teardown(void (*teardown)(void));

enum { n_teardowns = 3000 };

static int n_run = 0;

static void count_teardown(void) {
    n_run++;
}

static void check_every_teardown_ran(void) {
    if (n_run != n_teardowns) {
        fprintf(stderr, "%d of %d module teardowns ran\n", n_run, n_teardowns);
        _Exit(1);
    }
}

void register_teardowns(void) {
    if (atexit(check_every_teardown_ran) != 0) exit(1);
    for (int i = 0; i < n_teardowns; i++) {
        _lfortran_register_module_teardown(count_teardown);
    }
}

int teardowns_run(void) {
    return n_run;
}
