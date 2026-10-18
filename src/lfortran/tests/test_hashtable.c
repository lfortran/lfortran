#include <stdio.h>
#include <stdlib.h>

#include "../../libasr/runtime/hashtable.h"

static size_t table_HASH(size_t key) { return key; }
static int table_EQUAL(size_t a, size_t b) { return a == b; }
DEFINE_HASHTABLE_FOR_TYPES(size_t, int, table)

static void fail(const char *msg, size_t a, size_t b) {
    fprintf(stderr, "hashtable: %s (%zu, %zu)\n", msg, a, b);
    exit(1);
}

static size_t count_state(const table *t, int state) {
    size_t n = 0;
    for (size_t i = 0; i < t->num_buckets; i++) {
        if (t->buckets[i].state == state) n++;
    }
    return n;
}

// Deleted buckets must not accumulate: at most 75% of the buckets may be
// non-empty, so every probe sequence ends at an empty bucket.
static void check_load(const table *t) {
    size_t non_empty = t->num_buckets - count_state(t, EMPTY_BKT);
    if (4 * non_empty > 3 * t->num_buckets) {
        fail("too many non-empty buckets", non_empty, t->num_buckets);
    }
    if (count_state(t, OCCUPIED_BKT) != t->size) {
        fail("size does not match occupied buckets",
            count_state(t, OCCUPIED_BKT), t->size);
    }
}

// Reproducer from issue #15352: an empty table whose buckets are all deleted.
static void test_deleted_buckets_reclaimed(void) {
    table t;
    table_init(&t, 8192);
    for (size_t i = 0; i < 8192; i++) {
        table_insert(&t, i, 7);
        table_remove(&t, i);
    }
    for (size_t i = 0; i < 10000; i++) {
        table_insert(&t, 0, 7);
        int *v = table_get(&t, 0);
        if (!v || *v != 7) fail("lookup after insert", i, 0);
        if (i % 100 == 0) check_load(&t);
        table_remove(&t, 0);
        if (table_get(&t, 0)) fail("lookup after remove", i, 0);
    }
    if (t.size != 0) fail("table not empty", t.size, 0);
    free(t.buckets);
}

// Grow without deletions; all keys must stay reachable.
static void test_growth(void) {
    table t;
    table_init(&t, 16);
    for (size_t i = 0; i < 10000; i++) {
        table_insert(&t, 3 * i, (int)i);
    }
    check_load(&t);
    if (t.size != 10000) fail("size after growth", t.size, 10000);
    for (size_t i = 0; i < 10000; i++) {
        int *v = table_get(&t, 3 * i);
        if (!v || *v != (int)i) fail("lookup after growth", i, 0);
        if (table_get(&t, 3 * i + 1)) fail("unexpected key", 3 * i + 1, 0);
    }
    free(t.buckets);
}

// Random inserts, updates and removals checked against a direct-mapped model.
static void test_random_operations(void) {
    enum { NKEYS = 3000, NOPS = 200000 };
    static int present[NKEYS];
    static int values[NKEYS];
    size_t live = 0;
    unsigned long long seed = 12345;
    table t;
    table_init(&t, 64);
    for (size_t op = 0; op < NOPS; op++) {
        seed = seed * 6364136223846793005ULL + 1442695040888963407ULL;
        size_t r = (size_t)(seed >> 33);
        size_t key = r % NKEYS;
        // Keys with a common factor, so that their home buckets cluster.
        size_t hkey = key * 8;
        if ((r / NKEYS) % 3 != 0) {
            int value = (int)(op % 1000);
            table_insert(&t, hkey, value);
            if (!present[key]) live++;
            present[key] = 1;
            values[key] = value;
        } else {
            table_remove(&t, hkey);
            if (present[key]) live--;
            present[key] = 0;
        }
        if (t.size != live) fail("size mismatch", t.size, live);
        int *v = table_get(&t, hkey);
        if (present[key] ? (!v || *v != values[key]) : (v != NULL)) {
            fail("lookup mismatch", key, op);
        }
        if (op % 5000 == 0) {
            check_load(&t);
            for (size_t k = 0; k < NKEYS; k++) {
                v = table_get(&t, k * 8);
                if (present[k] ? (!v || *v != values[k]) : (v != NULL)) {
                    fail("full lookup mismatch", k, op);
                }
            }
        }
    }
    free(t.buckets);
}

int main(void) {
    test_deleted_buckets_reclaimed();
    test_growth();
    test_random_operations();
    printf("hashtable: all tests passed\n");
    return 0;
}
