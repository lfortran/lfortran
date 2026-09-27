/*
 * White-box tests of the startup initialization engine (lcompilers_init.h),
 * driven with records a test adds itself, the way JIT code adds its own.
 * Each scenario is a separate process, the first argument; CMakeLists.txt
 * registers one test for each. The fake initializers follow the guard
 * compiled code uses:
 *
 *     if (_lcompilers_init_begin(&state)) {
 *         <initializers of what it depends on>
 *         <body>
 *         _lcompilers_init_end(&state);
 *     }
 */

#if !defined(_WIN32)
/* mmap's MAP_ANON, which strict ISO and POSIX modes do not declare. */
#if defined(__APPLE__) && !defined(_DARWIN_C_SOURCE)
#define _DARWIN_C_SOURCE
#elif !defined(__APPLE__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <libasr/runtime/lfortran_intrinsics.h>

#if !defined(_WIN32)
#include <pthread.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>

/* A deadline `ms` milliseconds from now, for pthread_cond_timedwait. ISO C
 * timespec_get, since clock_gettime and nanosleep are not declared in a
 * strict ISO C mode. */
static struct timespec deadline_in(long ms) {
    struct timespec t;
    timespec_get(&t, TIME_UTC);
    t.tv_nsec += ms * 1000 * 1000;
    t.tv_sec += t.tv_nsec / (1000 * 1000 * 1000);
    t.tv_nsec %= 1000 * 1000 * 1000;
    return t;
}

/* Sleeps for `ms` milliseconds. */
static void pause_ms(long ms) {
    pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;
    pthread_cond_t c = PTHREAD_COND_INITIALIZER;
    struct timespec until = deadline_in(ms);
    pthread_mutex_lock(&m);
    while (pthread_cond_timedwait(&c, &m, &until) == 0) {
    }
    pthread_mutex_unlock(&m);
}
#endif

static int failures = 0;

static void expect(int cond, const char *what) {
    if (!cond) {
        printf("FAIL: %s\n", what);
        failures++;
    }
}

/* What the fake initializers and teardowns did, in order. */
static char events[64][32];
static int nevents = 0;

static void note(const char *what, const char *id) {
    if (nevents < 64) {
        snprintf(events[nevents], sizeof(events[nevents]), "%s %s", what, id);
        nevents++;
    }
}

static void expect_events(const char *const *expected, int n, const char *what) {
    int ok = nevents == n;
    for (int i = 0; ok && i < n; i++) {
        ok = strcmp(events[i], expected[i]) == 0;
    }
    if (!ok) {
        printf("FAIL: %s; got:\n", what);
        for (int i = 0; i < nevents; i++) printf("    %s\n", events[i]);
        failures++;
    }
}

static int finish(void) {
    if (failures == 0) printf("ok\n");
    return failures != 0;
}

/* ---------------------------------------------------------------------- */
/* Dependency completion order, root order and teardown order.            */
/*                                                                        */
/* m:alpha sorts first but depends on m:omega, so m:omega becomes ready   */
/* first; m:mid depends on nothing. The records are given in two orders,  */
/* neither of them the stable-id order, and the result must not depend on */
/* which. Teardown runs in the reverse of the order states became ready.  */
/* ---------------------------------------------------------------------- */

static int32_t st_alpha, st_mid, st_omega;

static void ensure_omega(void) {
    if (_lcompilers_init_begin(&st_omega)) {
        expect(st_omega == lcompilers_init_initializing, "m:omega initializing");
        note("enter", "m:omega");
        note("ready", "m:omega");
        _lcompilers_init_end(&st_omega);
    }
}

static void ensure_alpha(void) {
    if (_lcompilers_init_begin(&st_alpha)) {
        note("enter", "m:alpha");
        ensure_omega();
        expect(st_omega == lcompilers_init_ready, "m:omega ready inside m:alpha");
        note("ready", "m:alpha");
        _lcompilers_init_end(&st_alpha);
    }
}

static void ensure_mid(void) {
    if (_lcompilers_init_begin(&st_mid)) {
        note("enter", "m:mid");
        note("ready", "m:mid");
        _lcompilers_init_end(&st_mid);
    }
}

static void teardown_alpha(void) { note("teardown", "m:alpha"); }
static void teardown_mid(void) { note("teardown", "m:mid"); }
static void teardown_omega(void) { note("teardown", "m:omega"); }

static const lcompilers_init_record order_forward_records[] = {
    {"m:omega", ensure_omega, teardown_omega, &st_omega, 0, 0},
    {"m:mid", ensure_mid, teardown_mid, &st_mid, 0, 0},
    {"m:alpha", ensure_alpha, teardown_alpha, &st_alpha, 0, 0},
};
static const lcompilers_init_record order_reverse_records[] = {
    {"m:mid", ensure_mid, teardown_mid, &st_mid, 0, 0},
    {"m:alpha", ensure_alpha, teardown_alpha, &st_alpha, 0, 0},
    {"m:omega", ensure_omega, teardown_omega, &st_omega, 0, 0},
};
static const lcompilers_init_table order_forward_table = {
    lcompilers_init_abi_version, 3, order_forward_records};
static const lcompilers_init_table order_reverse_table = {
    lcompilers_init_abi_version, 3, order_reverse_records};

static int scenario_order(const lcompilers_init_table *table) {
    static const char *const init_order[] = {
        "enter m:alpha", "enter m:omega", "ready m:omega", "ready m:alpha",
        "enter m:mid", "ready m:mid"};
    static const char *const all_order[] = {
        "enter m:alpha", "enter m:omega", "ready m:omega", "ready m:alpha",
        "enter m:mid", "ready m:mid",
        "teardown m:mid", "teardown m:alpha", "teardown m:omega"};
    _lcompilers_init_add_records(table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect_events(init_order, 6, "initialization order");
    expect(st_alpha == lcompilers_init_ready && st_mid == lcompilers_init_ready
        && st_omega == lcompilers_init_ready, "every state ready");
    /* A second dispatch finds everything ready and calls no body. */
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect_events(init_order, 6, "a second dispatch does nothing");
    _lcompilers_init_teardown_all();
    expect_events(all_order, 9, "teardown order");
    _lcompilers_init_remove_records(table);
    return finish();
}

/* ---------------------------------------------------------------------- */
/* A dependency cycle on one thread is reported and ends the process.     */
/* ---------------------------------------------------------------------- */

static int32_t st_cycle_a, st_cycle_b;
static void ensure_cycle_b(void);

static void ensure_cycle_a(void) {
    if (_lcompilers_init_begin(&st_cycle_a)) {
        ensure_cycle_b();
        _lcompilers_init_end(&st_cycle_a);
    }
}

static void ensure_cycle_b(void) {
    if (_lcompilers_init_begin(&st_cycle_b)) {
        ensure_cycle_a();
        _lcompilers_init_end(&st_cycle_b);
    }
}

static const lcompilers_init_record cycle_records[] = {
    {"m:cycle_a", ensure_cycle_a, NULL, &st_cycle_a, 0, 0},
    {"m:cycle_b", ensure_cycle_b, NULL, &st_cycle_b, 0, 0},
};
static const lcompilers_init_table cycle_table = {
    lcompilers_init_abi_version, 2, cycle_records};

static int scenario_cycle(void) {
    _lcompilers_init_add_records(&cycle_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    /* Without the engine's diagnostic, run_fatal_test.cmake fails the test. */
    printf("cycle not detected\n");
    return 1;
}

/* ---------------------------------------------------------------------- */
/* Concurrent first calls: one thread initializes, the others wait for it */
/* instead of taking the state they find initializing for a cycle.        */
/* ---------------------------------------------------------------------- */

#if !defined(_WIN32)

enum { nthreads = 8 };

static int32_t st_slow;
static int slow_bodies = 0;
static int slow_value = 0;
static pthread_mutex_t start_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t start_cond = PTHREAD_COND_INITIALIZER;
static int started = 0;
static int go = 0;

static void ensure_slow(void) {
    if (_lcompilers_init_begin(&st_slow)) {
        slow_bodies++;
        pause_ms(50);
        slow_value = 42;
        _lcompilers_init_end(&st_slow);
    }
}

static void *first_call(void *seen) {
    pthread_mutex_lock(&start_lock);
    started++;
    pthread_cond_broadcast(&start_cond);
    while (!go) pthread_cond_wait(&start_cond, &start_lock);
    pthread_mutex_unlock(&start_lock);
    ensure_slow();
    *(int *)seen = slow_value;
    return NULL;
}

static int scenario_concurrent(void) {
    pthread_t threads[nthreads];
    int seen[nthreads];
    for (int i = 0; i < nthreads; i++) {
        seen[i] = 0;
        if (pthread_create(&threads[i], NULL, first_call, &seen[i]) != 0) {
            printf("FAIL: pthread_create\n");
            return 1;
        }
    }
    pthread_mutex_lock(&start_lock);
    while (started < nthreads) pthread_cond_wait(&start_cond, &start_lock);
    go = 1;
    pthread_cond_broadcast(&start_cond);
    pthread_mutex_unlock(&start_lock);
    for (int i = 0; i < nthreads; i++) pthread_join(threads[i], NULL);
    expect(slow_bodies == 1, "one body for all the first calls");
    for (int i = 0; i < nthreads; i++) {
        expect(seen[i] == 42, "every caller returns after the body finished");
    }
    expect(st_slow == lcompilers_init_ready, "state ready");
    return finish();
}

#endif

/* ---------------------------------------------------------------------- */
/* Removing a batch and adding one at the same address again, as a        */
/* library that is unloaded and loaded again at the same address is.      */
/* ---------------------------------------------------------------------- */

static int32_t st_again, st_stays;
static int again_bodies = 0, stays_bodies = 0;
static int again_teardowns = 0, stays_teardowns = 0;

static void ensure_again(void) {
    if (_lcompilers_init_begin(&st_again)) {
        again_bodies++;
        _lcompilers_init_end(&st_again);
    }
}

static void ensure_stays(void) {
    if (_lcompilers_init_begin(&st_stays)) {
        stays_bodies++;
        _lcompilers_init_end(&st_stays);
    }
}

static void teardown_again(void) { again_teardowns++; }
static void teardown_stays(void) { stays_teardowns++; }

static const lcompilers_init_record again_records[] = {
    {"m:again", ensure_again, teardown_again, &st_again, 0, 0},
};
static const lcompilers_init_record stays_records[] = {
    {"m:stays", ensure_stays, teardown_stays, &st_stays, 0, 0},
};
static const lcompilers_init_table again_table = {
    lcompilers_init_abi_version, 1, again_records};
static const lcompilers_init_table stays_table = {
    lcompilers_init_abi_version, 1, stays_records};

static int scenario_republish(void) {
    _lcompilers_init_add_records(&stays_table);
    _lcompilers_init_add_records(&again_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(again_bodies == 1 && stays_bodies == 1, "first batch initialized once");

    _lcompilers_init_remove_records(&again_table);
    /* What a fresh mapping of the same image holds. */
    st_again = lcompilers_init_uninitialized;
    _lcompilers_init_add_records(&again_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(again_bodies == 2, "the batch added again is initialized again");
    expect(stays_bodies == 1, "the batch that stayed is not initialized again");
    expect(st_stays == lcompilers_init_ready, "the batch that stayed keeps its state");

    /* A removed batch's teardown is never called once its code is gone. */
    _lcompilers_init_remove_records(&again_table);
    _lcompilers_init_teardown_all();
    expect(again_teardowns == 0, "no teardown of a removed batch");
    expect(stays_teardowns == 1, "teardown of the batch that stayed");
    _lcompilers_init_remove_records(&stays_table);
    return finish();
}

/* ---------------------------------------------------------------------- */
/* Once a dispatch has completed and nothing that registers records has   */
/* changed, a dispatch -- which every call of a foreign entry point makes */
/* -- neither asks the loader nor takes a lock; a batch added and a       */
/* teardown make the next one look again.                                */
/* ---------------------------------------------------------------------- */

uint64_t _lcompilers_init_test_generation_reads(void);

static int32_t st_settled, st_later;
static int settled_bodies = 0, later_bodies = 0;

static void ensure_settled(void) {
    if (_lcompilers_init_begin(&st_settled)) {
        settled_bodies++;
        _lcompilers_init_end(&st_settled);
    }
}

static void ensure_later(void) {
    if (_lcompilers_init_begin(&st_later)) {
        later_bodies++;
        _lcompilers_init_end(&st_later);
    }
}

static const lcompilers_init_record settled_records[] = {
    {"m:settled", ensure_settled, NULL, &st_settled, 0, 0},
};
static const lcompilers_init_record later_records[] = {
    {"m:later", ensure_later, NULL, &st_later, 0, 0},
};
static const lcompilers_init_table settled_table = {
    lcompilers_init_abi_version, 1, settled_records};
static const lcompilers_init_table later_table = {
    lcompilers_init_abi_version, 1, later_records};

#if !defined(_WIN32)
static void *dispatch_often(void *unused) {
    (void)unused;
    for (int i = 0; i < 100000; i++) {
        _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    }
    return NULL;
}
#endif

static int scenario_fast_path(void) {
    _lcompilers_init_add_records(&settled_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(settled_bodies == 1, "the batch is initialized");
    uint64_t reads = _lcompilers_init_test_generation_reads();
    for (int i = 0; i < 1000; i++) {
        _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    }
#if !defined(_WIN32)
    pthread_t threads[4];
    for (int i = 0; i < 4; i++) pthread_create(&threads[i], NULL, dispatch_often, NULL);
    for (int i = 0; i < 4; i++) pthread_join(threads[i], NULL);
#endif
    expect(_lcompilers_init_test_generation_reads() == reads,
        "a dispatch with nothing changed looks at nothing");

    _lcompilers_init_add_records(&later_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(later_bodies == 1, "a batch added later is initialized by the next dispatch");
    expect(_lcompilers_init_test_generation_reads() > reads,
        "a dispatch after a batch was added looks again");

    _lcompilers_init_teardown_all();
    reads = _lcompilers_init_test_generation_reads();
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(_lcompilers_init_test_generation_reads() > reads,
        "a dispatch after a teardown looks again");
    expect(settled_bodies == 1 && later_bodies == 1, "nothing is initialized twice");
    _lcompilers_init_remove_records(&later_table);
    _lcompilers_init_remove_records(&settled_table);
    return finish();
}

/* ---------------------------------------------------------------------- */
/* Collective records wait for an explicit collective boundary; a second  */
/* batch of them needs one of its own, which leaves the first batch, and  */
/* every local record, as it is.                                          */
/* ---------------------------------------------------------------------- */

static int32_t st_local1, st_coll1, st_local2, st_coll2;
static int local1_bodies = 0, coll1_bodies = 0, local2_bodies = 0, coll2_bodies = 0;

static void ensure_local1(void) {
    if (_lcompilers_init_begin(&st_local1)) {
        local1_bodies++;
        _lcompilers_init_end(&st_local1);
    }
}

static void ensure_coll1(void) {
    if (_lcompilers_init_begin(&st_coll1)) {
        _lcompilers_init_require_collective();
        coll1_bodies++;
        _lcompilers_init_end(&st_coll1);
    }
}

static void ensure_local2(void) {
    if (_lcompilers_init_begin(&st_local2)) {
        local2_bodies++;
        _lcompilers_init_end(&st_local2);
    }
}

static void ensure_coll2(void) {
    if (_lcompilers_init_begin(&st_coll2)) {
        _lcompilers_init_require_collective();
        coll2_bodies++;
        _lcompilers_init_end(&st_coll2);
    }
}

static const lcompilers_init_record batch1_records[] = {
    {"m:coll1", ensure_coll1, NULL, &st_coll1, lcompilers_init_collective, 0},
    {"m:local1", ensure_local1, NULL, &st_local1, 0, 0},
};
static const lcompilers_init_record batch2_records[] = {
    {"m:coll2", ensure_coll2, NULL, &st_coll2, lcompilers_init_collective, 0},
    {"m:local2", ensure_local2, NULL, &st_local2, 0, 0},
};
static const lcompilers_init_table batch1_table = {
    lcompilers_init_abi_version, 2, batch1_records};
static const lcompilers_init_table batch2_table = {
    lcompilers_init_abi_version, 2, batch2_records};

static int scenario_collective(void) {
    _lcompilers_init_add_records(&batch1_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(local1_bodies == 1, "local record of the first batch at the local phase");
    expect(coll1_bodies == 0 && st_coll1 == lcompilers_init_uninitialized,
        "collective record waits for the boundary");
    lcompilers_initialize();
    expect(coll1_bodies == 1, "collective record at the boundary");

    _lcompilers_init_add_records(&batch2_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(local2_bodies == 1, "local record of the second batch at the local phase");
    expect(coll2_bodies == 0, "second collective record waits for a boundary");
    lcompilers_initialize();
    expect(coll2_bodies == 1, "second collective record at the second boundary");
    expect(coll1_bodies == 1 && local1_bodies == 1,
        "the first batch is not initialized again");
    expect(st_coll1 == lcompilers_init_ready && st_local1 == lcompilers_init_ready,
        "the first batch keeps its states");
    _lcompilers_init_remove_records(&batch2_table);
    _lcompilers_init_remove_records(&batch1_table);
    return finish();
}

#if !defined(_WIN32)
/* ---------------------------------------------------------------------- */
/* A collective boundary runs every bootstrap once, after the local       */
/* records and before any collective one, outside every initializer and   */
/* without the engine's lock: another thread initializes a record         */
/* meanwhile. What the bootstrap brings -- a batch with a local and a     */
/* collective record, as an image it loads would -- settles before any    */
/* collective record runs.                                                */
/* ---------------------------------------------------------------------- */

static int32_t st_b_local, st_b_coll, st_b_local2, st_b_coll2, st_b_other;
static int bootstrap_calls = 0, other_done = 0;
static pthread_mutex_t boot_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t boot_cond = PTHREAD_COND_INITIALIZER;
static const lcompilers_init_table boot_loaded_table;

static void ensure_b_local(void) {
    if (_lcompilers_init_begin(&st_b_local)) {
        note("ready", "m:b_local");
        _lcompilers_init_end(&st_b_local);
    }
}

static void ensure_b_local2(void) {
    if (_lcompilers_init_begin(&st_b_local2)) {
        note("ready", "m:b_local2");
        _lcompilers_init_end(&st_b_local2);
    }
}

static void ensure_b_coll(void) {
    if (_lcompilers_init_begin(&st_b_coll)) {
        _lcompilers_init_require_collective();
        note("ready", "m:b_coll");
        _lcompilers_init_end(&st_b_coll);
    }
}

static void ensure_b_coll2(void) {
    if (_lcompilers_init_begin(&st_b_coll2)) {
        _lcompilers_init_require_collective();
        note("ready", "m:b_coll2");
        _lcompilers_init_end(&st_b_coll2);
    }
}

static void *initialize_other(void *unused) {
    (void)unused;
    if (_lcompilers_init_begin(&st_b_other)) _lcompilers_init_end(&st_b_other);
    pthread_mutex_lock(&boot_lock);
    other_done = 1;
    pthread_cond_broadcast(&boot_cond);
    pthread_mutex_unlock(&boot_lock);
    return NULL;
}

static void bootstrap(void) {
    bootstrap_calls++;
    note("bootstrap", "b:boot");
    expect(st_b_local == lcompilers_init_ready, "local records before the bootstrap");
    expect(st_b_coll == lcompilers_init_uninitialized,
        "no collective record before the bootstrap");
    pthread_t t;
    if (pthread_create(&t, NULL, initialize_other, NULL) == 0) {
        struct timespec until = deadline_in(5000);
        pthread_mutex_lock(&boot_lock);
        while (!other_done) {
            if (pthread_cond_timedwait(&boot_cond, &boot_lock, &until) != 0) break;
        }
        int done = other_done;
        pthread_mutex_unlock(&boot_lock);
        expect(done, "another thread initializes while the bootstrap runs");
        pthread_join(t, NULL);
    }
    _lcompilers_init_add_records(&boot_loaded_table);
}

static const lcompilers_init_record boot_records[] = {
    {"b:boot", bootstrap, NULL, NULL, lcompilers_init_bootstrap, 0},
    {"m:b_coll", ensure_b_coll, NULL, &st_b_coll, lcompilers_init_collective, 0},
    {"m:b_local", ensure_b_local, NULL, &st_b_local, 0, 0},
};
static const lcompilers_init_table boot_table = {
    lcompilers_init_abi_version, 3, boot_records};
static const lcompilers_init_record boot_loaded_records[] = {
    {"m:b_coll2", ensure_b_coll2, NULL, &st_b_coll2, lcompilers_init_collective, 0},
    {"m:b_local2", ensure_b_local2, NULL, &st_b_local2, 0, 0},
};
static const lcompilers_init_table boot_loaded_table = {
    lcompilers_init_abi_version, 2, boot_loaded_records};

/* No constructor dispatched before the boundary: the local record the
 * bootstrap reads is initialized by the boundary itself, first. */
static int scenario_bootstrap(void) {
    _lcompilers_init_add_records(&boot_table);
    lcompilers_initialize();
    static const char *const expected[] = {
        "ready m:b_local", "bootstrap b:boot", "ready m:b_local2",
        "ready m:b_coll", "ready m:b_coll2"};
    expect_events(expected, 5, "bootstrap between the local and collective records");
    lcompilers_initialize();
    expect(bootstrap_calls == 1, "a bootstrap runs once");
    _lcompilers_init_remove_records(&boot_loaded_table);
    _lcompilers_init_remove_records(&boot_table);
    return finish();
}

static int scenario_bootstrap_local(void) {
    _lcompilers_init_add_records(&boot_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(bootstrap_calls == 0, "no bootstrap outside a collective boundary");
    expect(st_b_local == lcompilers_init_ready, "the local record initialized");
    _lcompilers_init_remove_records(&boot_table);
    return finish();
}

/* A bootstrap runs before the runtime collective work needs is started, so
 * collective work from inside one ends the process. */
static void bootstrap_collective_work(void) {
    _lcompilers_init_require_collective();
}

static const lcompilers_init_record boot_bad_records[] = {
    {"b:bad", bootstrap_collective_work, NULL, NULL, lcompilers_init_bootstrap, 0},
};
static const lcompilers_init_table boot_bad_table = {
    lcompilers_init_abi_version, 1, boot_bad_records};

/* Another thread entering the collective boundary while this one runs it
 * ends the process: images would disagree on the order. */
static void *initialize_again(void *unused) {
    (void)unused;
    lcompilers_initialize();
    return NULL;
}

static void bootstrap_second_boundary(void) {
    pthread_t t;
    if (pthread_create(&t, NULL, initialize_again, NULL) == 0) pthread_join(t, NULL);
}

static const lcompilers_init_record boot_twice_records[] = {
    {"b:twice", bootstrap_second_boundary, NULL, NULL, lcompilers_init_bootstrap, 0},
};
static const lcompilers_init_table boot_twice_table = {
    lcompilers_init_abi_version, 1, boot_twice_records, NULL};

static int scenario_collective_twice(void) {
    _lcompilers_init_add_records(&boot_twice_table);
    lcompilers_initialize();
    printf("two threads in the collective boundary not detected\n");
    return 1;
}

/* Every object file that uses a runtime has its bootstrap, all of one
 * stable id: one boundary starts the runtime once, and a later table of
 * that id, published while one that ran it is live, does not start it
 * again. */
static int shared_bootstrap_runs = 0;

static void bootstrap_shared(void) {
    shared_bootstrap_runs++;
}

static const lcompilers_init_record shared1_records[] = {
    {"b:shared", bootstrap_shared, NULL, NULL, lcompilers_init_bootstrap, 0},
};
static const lcompilers_init_record shared2_records[] = {
    {"b:shared", bootstrap_shared, NULL, NULL, lcompilers_init_bootstrap, 0},
};
static const lcompilers_init_record shared3_records[] = {
    {"b:shared", bootstrap_shared, NULL, NULL, lcompilers_init_bootstrap, 0},
};
static const lcompilers_init_table shared1_table = {
    lcompilers_init_abi_version, 1, shared1_records, NULL};
static const lcompilers_init_table shared2_table = {
    lcompilers_init_abi_version, 1, shared2_records, NULL};
static const lcompilers_init_table shared3_table = {
    lcompilers_init_abi_version, 1, shared3_records, NULL};

static int scenario_bootstrap_shared(void) {
    _lcompilers_init_add_records(&shared1_table);
    _lcompilers_init_add_records(&shared2_table);
    lcompilers_initialize();
    expect(shared_bootstrap_runs == 1, "two tables of one bootstrap start it once");
    _lcompilers_init_remove_records(&shared1_table);
    _lcompilers_init_add_records(&shared3_table);
    lcompilers_initialize();
    expect(shared_bootstrap_runs == 1,
        "a table published while one that ran it is live does not start it again");
    _lcompilers_init_remove_records(&shared2_table);
    _lcompilers_init_remove_records(&shared3_table);
    _lcompilers_init_add_records(&shared1_table);
    lcompilers_initialize();
    expect(shared_bootstrap_runs == 2, "once every table that ran it is gone, it runs again");
    _lcompilers_init_remove_records(&shared1_table);
    return finish();
}

static int scenario_bootstrap_collective_work(void) {
    _lcompilers_init_add_records(&boot_bad_table);
    lcompilers_initialize();
    printf("collective work from a bootstrap not detected\n");
    return 1;
}
#endif

/* A collective initializer reached outside a collective boundary ends the
 * process rather than doing collective work on this image alone. */
static int scenario_collective_outside(void) {
    _lcompilers_init_add_records(&batch1_table);
    ensure_coll1();
    /* Without the engine's diagnostic, run_fatal_test.cmake fails the test. */
    printf("collective work outside the boundary not detected\n");
    return 1;
}

/* ---------------------------------------------------------------------- */
/* A batch published while a dispatch is running, by an initializer, as  */
/* code that loads a library or JIT code during its initialization does:  */
/* the one dispatch that is running has to initialize it too.             */
/* ---------------------------------------------------------------------- */

static int32_t st_publisher, st_published;
static int publisher_bodies = 0, published_bodies = 0;
static const lcompilers_init_table published_table;

static void ensure_published(void) {
    if (_lcompilers_init_begin(&st_published)) {
        published_bodies++;
        _lcompilers_init_end(&st_published);
    }
}

static void ensure_publisher(void) {
    if (_lcompilers_init_begin(&st_publisher)) {
        publisher_bodies++;
        _lcompilers_init_add_records(&published_table);
        _lcompilers_init_end(&st_publisher);
    }
}

static const lcompilers_init_record publisher_records[] = {
    {"m:publisher", ensure_publisher, NULL, &st_publisher, 0, 0},
};
static const lcompilers_init_record published_records[] = {
    {"m:published", ensure_published, NULL, &st_published, 0, 0},
};
static const lcompilers_init_table publisher_table = {
    lcompilers_init_abi_version, 1, publisher_records};
static const lcompilers_init_table published_table = {
    lcompilers_init_abi_version, 1, published_records};

static int scenario_publish_during_init(void) {
    _lcompilers_init_add_records(&publisher_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(publisher_bodies == 1 && st_publisher == lcompilers_init_ready,
        "the publisher initialized once");
    expect(published_bodies == 1 && st_published == lcompilers_init_ready,
        "the batch published during the dispatch initialized by it");
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(publisher_bodies == 1 && published_bodies == 1,
        "a later dispatch initializes neither again");
    _lcompilers_init_remove_records(&published_table);
    _lcompilers_init_remove_records(&publisher_table);
    return finish();
}

/* ---------------------------------------------------------------------- */
/* The same batch published twice is one batch: removing it once is what */
/* entitles the caller to unmap it. The batch is then overwritten, as     */
/* unmapped memory reused would be, and nothing may reach it.            */
/* ---------------------------------------------------------------------- */

static int32_t st_twice, st_after, st_poison;
static int twice_bodies = 0, after_bodies = 0, poisoned_calls = 0;

static void ensure_twice(void) {
    if (_lcompilers_init_begin(&st_twice)) {
        twice_bodies++;
        _lcompilers_init_end(&st_twice);
    }
}

static void ensure_after(void) {
    if (_lcompilers_init_begin(&st_after)) {
        after_bodies++;
        _lcompilers_init_end(&st_after);
    }
}

static void poisoned(void) { poisoned_calls++; }

static const lcompilers_init_record after_records[] = {
    {"m:after", ensure_after, NULL, &st_after, 0, 0},
};
static const lcompilers_init_table after_table = {
    lcompilers_init_abi_version, 1, after_records};

static int scenario_publish_twice(void) {
    lcompilers_init_record *records = malloc(sizeof(*records));
    lcompilers_init_table *table = malloc(sizeof(*table));
    if (!records || !table) return 2;
    lcompilers_init_record r = {"m:twice", ensure_twice, poisoned, &st_twice, 0, 0};
    *records = r;
    table->abi_version = lcompilers_init_abi_version;
    table->count = 1;
    table->records = records;

    _lcompilers_init_add_records(table);
    _lcompilers_init_add_records(table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(twice_bodies == 1, "a batch published twice initialized once");
    _lcompilers_init_remove_records(table);

    records->stable_id = "m:poisoned";
    records->ensure = poisoned;
    records->teardown = poisoned;
    records->state = &st_poison;
    _lcompilers_init_add_records(&after_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(after_bodies == 1, "a later batch initialized");
    _lcompilers_init_teardown_all();
    expect(poisoned_calls == 0, "nothing reaches a batch removed once");
    _lcompilers_init_remove_records(&after_table);
    free(table);
    free(records);
    return finish();
}

/* ---------------------------------------------------------------------- */
/* A batch withdrawn while a dispatch that found it is running, by an     */
/* initializer (remove_during_init) or by another thread                  */
/* (remove_concurrent). Once _lcompilers_init_remove_records returns, the */
/* caller may unmap the batch: it is then overwritten here, as reused     */
/* memory would be, and, where pages can be protected, made unreadable,   */
/* as unmapped memory is. Nothing may read it or call into it any more:   */
/* not its records' ids or flags, which the dispatch has yet to reach.    */
/* ---------------------------------------------------------------------- */

static int32_t st_remover, st_removed, st_removed_poison;
static int removed_bodies = 0, removed_poisoned_calls = 0;
static lcompilers_init_record *removed_records = NULL;
static lcompilers_init_table *removed_table = NULL;
/* The page the batch, its record and its id are in. */
static void *removed_page = NULL;
static size_t removed_page_size = 0;

static void ensure_removed(void) {
    if (_lcompilers_init_begin(&st_removed)) {
        removed_bodies++;
        _lcompilers_init_end(&st_removed);
    }
}

static void removed_poisoned(void) { removed_poisoned_calls++; }

static void withdraw_removed(void) {
    _lcompilers_init_remove_records(removed_table);
    removed_records->stable_id = "m:zz_poisoned";
    removed_records->ensure = removed_poisoned;
    removed_records->teardown = removed_poisoned;
    removed_records->state = &st_removed_poison;
#if !defined(_WIN32)
    mprotect(removed_page, removed_page_size, PROT_NONE);
#endif
}

static void ensure_remover(void) {
    if (_lcompilers_init_begin(&st_remover)) {
        withdraw_removed();
        _lcompilers_init_end(&st_remover);
    }
}

static const lcompilers_init_record remover_records[] = {
    {"m:a_remover", ensure_remover, NULL, &st_remover, 0, 0},
};
static const lcompilers_init_table remover_table = {
    lcompilers_init_abi_version, 1, remover_records};

static int make_removed_batch(void) {
#if !defined(_WIN32)
    removed_page_size = (size_t)sysconf(_SC_PAGESIZE);
    removed_page = mmap(NULL, removed_page_size, PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANON, -1, 0);
    if (removed_page == MAP_FAILED) return 0;
#else
    removed_page_size = 256;
    removed_page = malloc(removed_page_size);
    if (!removed_page) return 0;
#endif
    removed_table = (lcompilers_init_table *)removed_page;
    removed_records = (lcompilers_init_record *)((char *)removed_page + 64);
    char *id = (char *)removed_page + 192;
    strcpy(id, "m:b_removed");
    lcompilers_init_record r = {id, ensure_removed, removed_poisoned,
        &st_removed, 0, 0};
    *removed_records = r;
    removed_table->abi_version = lcompilers_init_abi_version;
    removed_table->count = 1;
    removed_table->records = removed_records;
    return 1;
}

static void free_removed_batch(void) {
#if !defined(_WIN32)
    munmap(removed_page, removed_page_size);
#else
    free(removed_page);
#endif
}

static int scenario_remove_during_init(void) {
    if (!make_removed_batch()) return 2;
    /* m:a_remover sorts first, so the dispatch reaches it before
     * m:b_removed, which it withdraws. */
    _lcompilers_init_add_records(&remover_table);
    _lcompilers_init_add_records(removed_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(st_remover == lcompilers_init_ready, "the remover initialized");
    expect(removed_bodies == 0 && removed_poisoned_calls == 0,
        "nothing calls into a batch withdrawn during the dispatch");
    int before = removed_poisoned_calls;
    _lcompilers_init_teardown_all();
    expect(removed_poisoned_calls == before, "no teardown of the withdrawn batch");
    _lcompilers_init_remove_records(&remover_table);
    free_removed_batch();
    return finish();
}

#if !defined(_WIN32)

/* The first root of the dispatch pauses, holding no engine lock, until the
 * batch it precedes was removed and overwritten, or for at most half a
 * second: an engine whose removal waits for the dispatch to finish makes the
 * pause time out instead, and then initializes the batch before the removal
 * returns, which is as correct. */
static int32_t st_pause;
static pthread_mutex_t remove_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t remove_cond = PTHREAD_COND_INITIALIZER;
static int pause_entered = 0, batch_withdrawn = 0;

static void ensure_pause(void) {
    struct timespec until = deadline_in(500);
    pthread_mutex_lock(&remove_lock);
    pause_entered = 1;
    pthread_cond_broadcast(&remove_cond);
    while (!batch_withdrawn) {
        if (pthread_cond_timedwait(&remove_cond, &remove_lock, &until) != 0) break;
    }
    pthread_mutex_unlock(&remove_lock);
    if (_lcompilers_init_begin(&st_pause)) _lcompilers_init_end(&st_pause);
}

static const lcompilers_init_record pause_records[] = {
    {"m:a_pause", ensure_pause, NULL, &st_pause, 0, 0},
};
static const lcompilers_init_table pause_table = {
    lcompilers_init_abi_version, 1, pause_records};

static void *dispatch_thread(void *unused) {
    (void)unused;
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    return NULL;
}

static int scenario_remove_concurrent(void) {
    pthread_t t;
    if (!make_removed_batch()) return 2;
    _lcompilers_init_add_records(&pause_table);
    _lcompilers_init_add_records(removed_table);
    if (pthread_create(&t, NULL, dispatch_thread, NULL) != 0) return 2;
    pthread_mutex_lock(&remove_lock);
    while (!pause_entered) pthread_cond_wait(&remove_cond, &remove_lock);
    pthread_mutex_unlock(&remove_lock);
    /* The dispatch has found both batches and is paused in m:a_pause. */
    withdraw_removed();
    pthread_mutex_lock(&remove_lock);
    batch_withdrawn = 1;
    pthread_cond_broadcast(&remove_cond);
    pthread_mutex_unlock(&remove_lock);
    pthread_join(t, NULL);
    expect(st_pause == lcompilers_init_ready, "the pausing record initialized");
    expect(removed_poisoned_calls == 0,
        "nothing calls into a batch once its removal returned");
    int before = removed_poisoned_calls;
    _lcompilers_init_teardown_all();
    expect(removed_poisoned_calls == before, "no teardown of the withdrawn batch");
    _lcompilers_init_remove_records(&pause_table);
    free_removed_batch();
    return finish();
}

/* Withdrawing a batch from inside an initializer, which holds the engine
 * lock, while another thread's dispatch is running an initializer of that
 * batch: waiting for that one could deadlock, so the engine ends the process
 * instead. */
static int32_t st_inside;

static int scenario_remove_inside_while_pinned(void) {
    pthread_t t;
    _lcompilers_init_add_records(&pause_table);
    if (pthread_create(&t, NULL, dispatch_thread, NULL) != 0) return 2;
    pthread_mutex_lock(&remove_lock);
    while (!pause_entered) pthread_cond_wait(&remove_cond, &remove_lock);
    pthread_mutex_unlock(&remove_lock);
    if (_lcompilers_init_begin(&st_inside)) {
        _lcompilers_init_remove_records(&pause_table);
        _lcompilers_init_end(&st_inside);
    }
    /* Without the engine's diagnostic, run_fatal_test.cmake fails the test. */
    printf("withdrawal from inside an initializer not detected\n");
    pthread_join(t, NULL);
    return 1;
}

/* Unloading an image, or withdrawing a batch, waits only for the
 * initializers of that image or batch. An initializer of another one, which
 * holds the engine lock, can meanwhile wait for the thread unloading -- for
 * the loader lock, which a dlclose holds while it runs the destructor that
 * unloads: here, for the unload to return. It waits five seconds at most, so
 * an engine that waits for it hangs no longer than that and fails. */
static int32_t st_holder, st_gone;
static int gone_teardowns = 0, holder_timed_out = 0;
static pthread_mutex_t unrelated_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t unrelated_cond = PTHREAD_COND_INITIALIZER;
static int holder_entered = 0, gone_withdrawn = 0;

static void ensure_holder(void) {
    if (_lcompilers_init_begin(&st_holder)) {
        struct timespec until = deadline_in(5000);
        pthread_mutex_lock(&unrelated_lock);
        holder_entered = 1;
        pthread_cond_broadcast(&unrelated_cond);
        while (!gone_withdrawn) {
            if (pthread_cond_timedwait(&unrelated_cond, &unrelated_lock, &until) != 0) {
                holder_timed_out = 1;
                break;
            }
        }
        pthread_mutex_unlock(&unrelated_lock);
        _lcompilers_init_end(&st_holder);
    }
}

static void ensure_gone(void) {
    if (_lcompilers_init_begin(&st_gone)) _lcompilers_init_end(&st_gone);
}

static void teardown_gone(void) { gone_teardowns++; }

static const lcompilers_init_record holder_records[] = {
    {"m:holder", ensure_holder, NULL, &st_holder, 0, 0},
};
static const lcompilers_init_table holder_table = {
    lcompilers_init_abi_version, 1, holder_records};
static const lcompilers_init_record gone_records[] = {
    {"m:gone", ensure_gone, teardown_gone, &st_gone, 0, 0},
};
static const lcompilers_init_table gone_table = {
    lcompilers_init_abi_version, 1, gone_records};

static int scenario_withdraw_unrelated(void) {
    pthread_t t;
    _lcompilers_init_add_records(&gone_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(st_gone == lcompilers_init_ready, "m:gone initialized");
    _lcompilers_init_add_records(&holder_table);
    if (pthread_create(&t, NULL, dispatch_thread, NULL) != 0) return 2;
    pthread_mutex_lock(&unrelated_lock);
    while (!holder_entered) pthread_cond_wait(&unrelated_cond, &unrelated_lock);
    pthread_mutex_unlock(&unrelated_lock);
    /* What a JIT session's shutdown does with a cell. */
    _lcompilers_init_unload(&gone_table);
    _lcompilers_init_remove_records(&gone_table);
    pthread_mutex_lock(&unrelated_lock);
    gone_withdrawn = 1;
    pthread_cond_broadcast(&unrelated_cond);
    pthread_mutex_unlock(&unrelated_lock);
    pthread_join(t, NULL);
    expect(!holder_timed_out, "withdrawing m:gone did not wait for m:holder");
    expect(gone_teardowns == 1, "m:gone torn down once, by its unload");
    expect(st_holder == lcompilers_init_ready, "m:holder initialized");
    _lcompilers_init_teardown_all();
    expect(gone_teardowns == 1, "no teardown of the unloaded m:gone");
    _lcompilers_init_remove_records(&holder_table);
    return finish();
}

#endif

/* ---------------------------------------------------------------------- */
/* A bootstrap runs once for each incarnation of its table: removing the  */
/* table and adding it again, as a library unloaded and loaded again,     */
/* runs it again at the next collective boundary. scenario_bootstrap      */
/* covers its place in the order and what it may do.                      */
/* ---------------------------------------------------------------------- */

static int32_t st_bs_local, st_bs_coll, st_bs_late;
static int incarnation_bootstrap_runs = 0;

static void ensure_bs_local(void) {
    if (_lcompilers_init_begin(&st_bs_local)) {
        note("ready", "m:bs_local");
        _lcompilers_init_end(&st_bs_local);
    }
}

static void ensure_bs_coll(void) {
    if (_lcompilers_init_begin(&st_bs_coll)) {
        _lcompilers_init_require_collective();
        note("ready", "m:bs_coll");
        _lcompilers_init_end(&st_bs_coll);
    }
}

static void ensure_bs_late(void) {
    if (_lcompilers_init_begin(&st_bs_late)) {
        note("ready", "m:bs_late");
        _lcompilers_init_end(&st_bs_late);
    }
}

static const lcompilers_init_record late_records[] = {
    {"m:bs_late", ensure_bs_late, NULL, &st_bs_late, 0, 0},
};
static const lcompilers_init_table late_table = {
    lcompilers_init_abi_version, 1, late_records};

static void bootstrap_counted(void) {
    incarnation_bootstrap_runs++;
    note("bootstrap", "b:runtime");
    /* A runtime starting up may load images of its own. */
    _lcompilers_init_add_records(&late_table);
}

/* Stable ids that sort a collective record and the bootstrap ahead of the
 * local one, which still has to come first. */
static const lcompilers_init_record bootstrap_records[] = {
    {"b:runtime", bootstrap_counted, NULL, NULL, lcompilers_init_bootstrap, 0},
    {"a:bs_coll", ensure_bs_coll, NULL, &st_bs_coll, lcompilers_init_collective, 0},
    {"z:bs_local", ensure_bs_local, NULL, &st_bs_local, 0, 0},
};
static const lcompilers_init_table bootstrap_table = {
    lcompilers_init_abi_version, 3, bootstrap_records};

static int scenario_bootstrap_incarnation(void) {
    static const char *const first[] = {
        "ready m:bs_local", "bootstrap b:runtime", "ready m:bs_late", "ready m:bs_coll"};
    _lcompilers_init_add_records(&bootstrap_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    expect(incarnation_bootstrap_runs == 0, "a local dispatch runs no bootstrap");
    lcompilers_initialize();
    expect_events(first, 4, "local, then bootstrap, then collective");
    expect(incarnation_bootstrap_runs == 1, "the bootstrap ran once");
    expect(st_bs_late == lcompilers_init_ready,
        "a batch the bootstrap published initialized at the same boundary");
    lcompilers_initialize();
    expect(incarnation_bootstrap_runs == 1, "a second boundary does not run it again");
    /* A new incarnation of the table, as a library loaded again. */
    _lcompilers_init_remove_records(&bootstrap_table);
    st_bs_local = st_bs_coll = lcompilers_init_uninitialized;
    _lcompilers_init_add_records(&bootstrap_table);
    lcompilers_initialize();
    expect(incarnation_bootstrap_runs == 2, "a table added again bootstraps again");
    _lcompilers_init_remove_records(&bootstrap_table);
    _lcompilers_init_remove_records(&late_table);
    return finish();
}

/* A bootstrap has no state word to guard it. */
static int32_t st_bad_bootstrap;
static const lcompilers_init_record bad_bootstrap_records[] = {
    {"b:bad", bootstrap_counted, NULL, &st_bad_bootstrap, lcompilers_init_bootstrap, 0},
};
static const lcompilers_init_table bad_bootstrap_table = {
    lcompilers_init_abi_version, 1, bad_bootstrap_records};

static int scenario_malformed_bootstrap(void) {
    _lcompilers_init_add_records(&bad_bootstrap_table);
    lcompilers_initialize();
    printf("bootstrap with a state word not detected\n");
    return 1;
}

/* Both flags at once. */
static const lcompilers_init_record both_flags_records[] = {
    {"b:both", bootstrap_counted, NULL, NULL,
        lcompilers_init_bootstrap | lcompilers_init_collective, 0},
};
static const lcompilers_init_table both_flags_table = {
    lcompilers_init_abi_version, 1, both_flags_records};

static int scenario_both_flags(void) {
    _lcompilers_init_add_records(&both_flags_table);
    lcompilers_initialize();
    printf("record with both flags not detected\n");
    return 1;
}

/* A batch compiled for the previous ABI. */
static const lcompilers_init_table old_version_table = {
    lcompilers_init_abi_version - 1, 1, order_forward_records};

static int scenario_old_abi(void) {
    _lcompilers_init_add_records(&old_version_table);
    printf("batch of another ABI version not detected\n");
    return 1;
}

/* ---------------------------------------------------------------------- */
/* Tables the engine refuses, ending the process rather than guessing.    */
/* ---------------------------------------------------------------------- */

static int32_t st_bad, st_dup1, st_dup2;

static void ensure_bad(void) {
    if (_lcompilers_init_begin(&st_bad)) _lcompilers_init_end(&st_bad);
}

static void ensure_dup1(void) {
    if (_lcompilers_init_begin(&st_dup1)) {
        _lcompilers_init_require_collective();
        _lcompilers_init_end(&st_dup1);
    }
}

static void ensure_dup2(void) {
    if (_lcompilers_init_begin(&st_dup2)) {
        _lcompilers_init_require_collective();
        _lcompilers_init_end(&st_dup2);
    }
}

/* A reserved word that is not 0. */
static const lcompilers_init_record malformed_records[] = {
    {"m:bad", ensure_bad, NULL, &st_bad, 0, 1},
};
static const lcompilers_init_table malformed_table = {
    lcompilers_init_abi_version, 1, malformed_records};

/* Two collective initializers of one stable id would be ordered by how they
 * happened to be discovered, which can differ between images. */
static const lcompilers_init_record duplicate1_records[] = {
    {"m:dup", ensure_dup1, NULL, &st_dup1, lcompilers_init_collective, 0},
};
static const lcompilers_init_record duplicate2_records[] = {
    {"m:dup", ensure_dup2, NULL, &st_dup2, lcompilers_init_collective, 0},
};
static const lcompilers_init_table duplicate1_table = {
    lcompilers_init_abi_version, 1, duplicate1_records};
static const lcompilers_init_table duplicate2_table = {
    lcompilers_init_abi_version, 1, duplicate2_records};

static int scenario_malformed_record(void) {
    _lcompilers_init_add_records(&malformed_table);
    _lcompilers_init_dispatch(lcompilers_init_dispatch_local);
    printf("malformed record not detected\n");
    return 1;
}

static int scenario_unknown_phase(void) {
    _lcompilers_init_dispatch(7);
    printf("unknown dispatch phase not detected\n");
    return 1;
}

static int scenario_duplicate_collective(void) {
    _lcompilers_init_add_records(&duplicate1_table);
    _lcompilers_init_add_records(&duplicate2_table);
    lcompilers_initialize();
    printf("duplicate collective stable id not detected\n");
    return 1;
}

int main(int argc, char **argv) {
    const char *s = argc > 1 ? argv[1] : "";
    if (strcmp(s, "order_forward") == 0) return scenario_order(&order_forward_table);
    if (strcmp(s, "order_reverse") == 0) return scenario_order(&order_reverse_table);
    if (strcmp(s, "cycle") == 0) return scenario_cycle();
#if !defined(_WIN32)
    if (strcmp(s, "concurrent") == 0) return scenario_concurrent();
    if (strcmp(s, "remove_concurrent") == 0) return scenario_remove_concurrent();
    if (strcmp(s, "remove_inside_while_pinned") == 0) {
        return scenario_remove_inside_while_pinned();
    }
    if (strcmp(s, "withdraw_unrelated") == 0) return scenario_withdraw_unrelated();
    if (strcmp(s, "bootstrap") == 0) return scenario_bootstrap();
    if (strcmp(s, "bootstrap_local") == 0) return scenario_bootstrap_local();
    if (strcmp(s, "collective_twice") == 0) return scenario_collective_twice();
    if (strcmp(s, "bootstrap_shared") == 0) return scenario_bootstrap_shared();
    if (strcmp(s, "bootstrap_collective_work") == 0) {
        return scenario_bootstrap_collective_work();
    }
#endif
    if (strcmp(s, "republish") == 0) return scenario_republish();
    if (strcmp(s, "fast_path") == 0) return scenario_fast_path();
    if (strcmp(s, "collective") == 0) return scenario_collective();
    if (strcmp(s, "collective_outside") == 0) return scenario_collective_outside();
    if (strcmp(s, "publish_during_init") == 0) return scenario_publish_during_init();
    if (strcmp(s, "publish_twice") == 0) return scenario_publish_twice();
    if (strcmp(s, "remove_during_init") == 0) return scenario_remove_during_init();
    if (strcmp(s, "malformed_record") == 0) return scenario_malformed_record();
    if (strcmp(s, "bootstrap_incarnation") == 0) return scenario_bootstrap_incarnation();
    if (strcmp(s, "malformed_bootstrap") == 0) return scenario_malformed_bootstrap();
    if (strcmp(s, "both_flags") == 0) return scenario_both_flags();
    if (strcmp(s, "old_abi") == 0) return scenario_old_abi();
    if (strcmp(s, "unknown_phase") == 0) return scenario_unknown_phase();
    if (strcmp(s, "duplicate_collective") == 0) return scenario_duplicate_collective();
    printf("unknown scenario '%s'\n", s);
    return 2;
}
