#include <stdio.h>
#include <stdlib.h>

#include <lfortran_intrinsics.h>

/*
 * Far more initialization records than any fixed-size list would hold, each
 * with a teardown, published the way JIT code publishes its records. Every
 * record's initializer is `initialize_all`, which makes all of them ready,
 * so each call after the first returns at once.
 */
enum { n_teardowns = 3000 };

static char ids[n_teardowns][32];
static int32_t states[n_teardowns];
static lcompilers_init_record records[n_teardowns];
static lcompilers_init_table table;

static int n_run = 0;

static void count_teardown(void) {
    n_run++;
}

static void initialize_all(void) {
    for (int i = 0; i < n_teardowns; i++) {
        if (_lcompilers_init_begin(&states[i])) _lcompilers_init_end(&states[i]);
    }
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
        snprintf(ids[i], sizeof(ids[i]), "t:module_teardown_01_%04d", i);
        lcompilers_init_record r = {ids[i], initialize_all, count_teardown,
            &states[i], 0, 0};
        records[i] = r;
    }
    table.abi_version = lcompilers_init_abi_version;
    table.count = n_teardowns;
    table.records = records;
    _lcompilers_init_add_records(&table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    for (int i = 0; i < n_teardowns; i++) {
        if (states[i] != lcompilers_init_ready) exit(2);
    }
}

int teardowns_run(void) {
    return n_run;
}
