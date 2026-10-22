// The defined output (DT) path of the runtime formatter, called the way
// compiled code calls it: a derived-type item is serialized as
// `D<k>-<bytes>` and passed by address like any other item, and
// `dtio_procs[k]` is the callback of its defined output procedure. A
// callback writes like a compiled child WRITE statement: it formats its
// record with `_lcompilers_string_format_fortran` and writes it with
// `_lfortran_file_write` to the unit it was given.
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <libasr/runtime/lfortran_intrinsics.h>

typedef struct point {
    int32_t x;
    struct point *inner; // Written as `<inner>` after `x` if not NULL.
} point;

typedef struct {
    const char *data;
    int64_t length;
} string_descriptor;

static lfortran_allocator_t *al;
static char dt_item[32];   // The serialization of a `point` item.
static int failures;

// What the callbacks were called with, in call order.
static int n_calls;
static int32_t seen_unit[4];
static char seen_iotype[4][16];
static int32_t seen_n_v[4];
static int32_t seen_v_list[4][4];

static void write_point(void *dtv, int32_t *unit, const char *iotype,
    int32_t *n_v, int32_t *v_list, int32_t *iostat, char *iomsg,
    int64_t iotype_len, int64_t iomsg_len);
static lfortran_dtio_write_fn procs[] = {write_point};

// Formats up to two items as a formatted output statement does.
static char *format(const char *fmt, const char *serialization,
        int32_t dtio_unit, void *item1, void *item2, int64_t *len) {
    return _lcompilers_string_format_fortran(al, fmt, (int64_t)strlen(fmt),
        serialization, len, 0, 0, 0, 0, 0, 0, dtio_unit, 1, procs,
        item1, item2);
}

static void check(int64_t len, char *result, const char *expected,
        const char *what) {
    if (len != (int64_t)strlen(expected) || memcmp(result, expected, len) != 0) {
        fprintf(stderr, "%s: got '%.*s', expected '%s'\n", what, (int)len,
            result, expected);
        failures++;
    }
    ALLOCATOR_DEALLOC(al, result);
}

static void check_true(bool condition, const char *what) {
    if (!condition) {
        fprintf(stderr, "%s\n", what);
        failures++;
    }
}

// `write(unit, '(i0)') dtv%x`, then, if `dtv%inner` is associated,
// `write(unit, "('<',dt,'>')") dtv%inner`.
static void write_point(void *dtv, int32_t *unit, const char *iotype,
        int32_t *n_v, int32_t *v_list, int32_t *iostat, char *iomsg,
        int64_t iotype_len, int64_t iomsg_len) {
    point *p = (point *)dtv;
    (void)iomsg; (void)iomsg_len;
    if (n_calls < 4) {
        seen_unit[n_calls] = *unit;
        snprintf(seen_iotype[n_calls], 16, "%.*s", (int)iotype_len, iotype);
        seen_n_v[n_calls] = *n_v;
        for (int32_t i = 0; i < *n_v && i < 4; i++) {
            seen_v_list[n_calls][i] = v_list[i];
        }
    }
    n_calls++;
    int64_t len;
    char *s = _lcompilers_string_format_fortran(al, "(i0)", 4, "I4", &len,
        0, 0, 0, 0, 0, 0, 0, 0, NULL, &p->x);
    _lfortran_file_write(*unit, iostat, "%s%s", 4, s, len, "\n", (int64_t)1);
    ALLOCATOR_DEALLOC(al, s);
    if (p->inner != NULL) {
        s = format("('<',dt,'>')", dt_item, *unit, p->inner, NULL, &len);
        _lfortran_file_write(*unit, iostat, "%s%s", 4, s, len, "\n", (int64_t)1);
        ALLOCATOR_DEALLOC(al, s);
    }
}

// Formats `item1` and `item2` and checks the result.
static void expect(const char *fmt, const char *serialization,
        int32_t dtio_unit, void *item1, void *item2, const char *expected) {
    int64_t len;
    char *result = format(fmt, serialization, dtio_unit, item1, item2, &len);
    check(len, result, expected, fmt);
}

// Formats `item` and checks that the result is the runtime error `message`.
static void expect_error(const char *fmt, const char *serialization,
        int32_t n_dtio, void *item, const char *message) {
    int64_t len;
    char *result = _lcompilers_string_format_fortran(al, fmt,
        (int64_t)strlen(fmt), serialization, &len, 0, 0, 0, 0, 0, 0, 6,
        n_dtio, procs, item);
    check_true(len > 0 && result[0] == '\b' && strstr(result, message) != NULL,
        message);
    ALLOCATOR_DEALLOC(al, result);
}

int main(void) {
    al = _lfortran_get_default_allocator();
    point p = {7, NULL}, q = {2, NULL}, outer = {1, &q}, wide = {12345, NULL};
    int32_t i = 42;
    string_descriptor z = {"Z", 1};
    char dt_i4[48], dt_a[48], dt_dt[48];
    snprintf(dt_item, sizeof(dt_item), "D0-%zu", sizeof(point));
    snprintf(dt_i4, sizeof(dt_i4), "%s,I4", dt_item);
    snprintf(dt_a, sizeof(dt_a), "%s,S-DESC-K1-1", dt_item);
    snprintf(dt_dt, sizeof(dt_dt), "%s,%s", dt_item, dt_item);

    // The child's output is at the parent's position, and the parent's
    // edit descriptors follow it. The child gets the parent's unit, iotype
    // "DT" and an empty v_list.
    n_calls = 0;
    expect("(dt,1x,i0)", dt_i4, 6, &p, &i, "7 42");
    check_true(n_calls == 1 && seen_unit[0] == 6 &&
        strcmp(seen_iotype[0], "DT") == 0 && seen_n_v[0] == 0, "plain dt");

    // The character literal and the v_list reach the procedure.
    n_calls = 0;
    expect("(dt'x'(3,4))", dt_item, 6, &p, NULL, "7");
    check_true(n_calls == 1 && strcmp(seen_iotype[0], "DTx") == 0 &&
        seen_n_v[0] == 2 && seen_v_list[0][0] == 3 && seen_v_list[0][1] == 4,
        "dt'x'(3,4): iotype and v_list");

    // The parent positions relative to its record, child output included:
    // after TL3 its next field overwrites the child's output.
    expect("(dt,tl3,a)", dt_a, 6, &wide, &z, "12Z45");

    // DT is a data edit descriptor: format reversion and `:` apply to it.
    expect("(*(dt,:,','))", dt_dt, 6, &p, &q, "7,2");

    // Nested child: the procedure itself writes a derived-type item with DT.
    // The inner child is captured into the outer child's statement, whose
    // output goes into the parent record.
    n_calls = 0;
    expect("(dt,1x,i0)", dt_i4, 6, &outer, &i, "1<2> 42");
    check_true(n_calls == 2 && seen_unit[1] == 6, "nested child unit");

    // An internal file (INT32_MIN) has no unit: the child gets a
    // pseudo-unit, which its own nested child statement then writes to.
    n_calls = 0;
    expect("(dt)", dt_item, INT32_MIN, &outer, NULL, "1<2>");
    check_true(n_calls == 2 && seen_unit[0] == INT32_MIN + 1 &&
        seen_unit[1] == INT32_MIN + 1, "internal file pseudo-unit");

    expect_error("(i0)", dt_item, 1, &p,
        "derived-type item with defined output requires the DT edit descriptor, found i0");
    expect_error("(dt)", "I4", 1, &i,
        "DT edit descriptor requires a derived-type item with defined output, got an item of type INTEGER");
    expect_error("(dt)", dt_item, 0, &p,
        "no defined output procedure for a derived-type item");

    // With WITH_INTERNAL_ALLOC_CHECK this exits with status 1 if anything
    // is still allocated, such as the capture stack of the children.
    _lfortran_internal_alloc_finalize();
    return failures != 0;
}
