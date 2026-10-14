/*
 * Deterministic tests of the SYSTEM_CLOCK runtime functions.
 *
 * This executable links the static runtime library, so its definition of
 * clock_gettime() below replaces the C library's one for the runtime, which
 * lets us check the counts at any uptime.
 */

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>

int32_t _lfortran_i32sys_clock_count(void);
int32_t _lfortran_i32sys_clock_count_rate(void);
int32_t _lfortran_i32sys_clock_count_max(void);
float _lfortran_i32r32sys_clock_count_rate(void);
uint64_t _lfortran_i64sys_clock_count(void);
int64_t _lfortran_i64sys_clock_count_rate(void);
int64_t _lfortran_i64sys_clock_count_max(void);
double _lfortran_i64r64sys_clock_count_rate(void);

static int clock_works = 1;
static time_t clock_sec = 0;
static long clock_nsec = 0;
static int failures = 0;

int clock_gettime(clockid_t clock_id, struct timespec *ts)
{
    if (!clock_works || clock_id != CLOCK_MONOTONIC) {
        return -1;
    }
    ts->tv_sec = clock_sec;
    ts->tv_nsec = clock_nsec;
    return 0;
}

static void check_i64(const char *what, int64_t value, int64_t expected)
{
    if (value != expected) {
        printf("FAIL: %s at %lld s %ld ns: got %lld, expected %lld\n", what,
            (long long)clock_sec, clock_nsec, (long long)value,
            (long long)expected);
        failures++;
    }
}

static void check_double(const char *what, double value, double expected)
{
    if (value != expected) {
        printf("FAIL: %s: got %g, expected %g\n", what, value, expected);
        failures++;
    }
}

static void test_default_kind_count(void)
{
    /* Milliseconds since an arbitrary origin wrap around to 0 after
       COUNT_MAX = 2^31 - 1 */
    struct {
        time_t sec;
        long nsec;
        int32_t count;
    } cases[] = {
        {0, 0, 0},
        {0, 999999, 0},
        {1, 1000000, 1001},
        {2147483, 646999999, 2147483646},
        {2147483, 647000000, 2147483647},
        {2147483, 647999999, 2147483647},
        {2147483, 648000000, 0},
        {2147483, 648999999, 0},
        {2147483, 649000000, 1},
        {2170579, 330000000, 23095682},
        {6442463, 289000000, 12345},
        {8589934, 591999999, 2147483647},
        {8589934, 592000000, 0},
        {2147483648, 7000000, 7},
    };
    clock_works = 1;
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        clock_sec = cases[i].sec;
        clock_nsec = cases[i].nsec;
        check_i64("integer(4) count", _lfortran_i32sys_clock_count(),
            cases[i].count);
        check_i64("integer(4) count_rate",
            _lfortran_i32sys_clock_count_rate(), 1000);
        check_i64("integer(4) count_max",
            _lfortran_i32sys_clock_count_max(), INT32_MAX);
        check_double("real(4) count_rate",
            _lfortran_i32r32sys_clock_count_rate(), 1000);
    }
}

static void test_kind8_count(void)
{
    struct {
        time_t sec;
        long nsec;
        int64_t count;
    } cases[] = {
        {0, 0, 0},
        {2147483, 648000001, 2147483648000001LL},
        {8589934, 592000000, 8589934592000000LL},
    };
    clock_works = 1;
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        clock_sec = cases[i].sec;
        clock_nsec = cases[i].nsec;
        check_i64("integer(8) count",
            (int64_t)_lfortran_i64sys_clock_count(), cases[i].count);
        check_i64("integer(8) count_rate",
            _lfortran_i64sys_clock_count_rate(), 1000000000);
        check_i64("integer(8) count_max",
            _lfortran_i64sys_clock_count_max(), INT64_MAX);
        check_double("real(8) count_rate",
            _lfortran_i64r64sys_clock_count_rate(), 1e9);
    }
}

static void test_no_clock(void)
{
    clock_works = 0;
    clock_sec = 0;
    clock_nsec = 0;
    check_i64("integer(4) count", _lfortran_i32sys_clock_count(), -INT32_MAX);
    check_i64("integer(4) count_rate", _lfortran_i32sys_clock_count_rate(), 0);
    check_i64("integer(4) count_max", _lfortran_i32sys_clock_count_max(), 0);
    check_double("real(4) count_rate",
        _lfortran_i32r32sys_clock_count_rate(), 0);
    check_i64("integer(8) count", (int64_t)_lfortran_i64sys_clock_count(),
        -INT64_MAX);
    check_i64("integer(8) count_rate", _lfortran_i64sys_clock_count_rate(), 0);
    check_i64("integer(8) count_max", _lfortran_i64sys_clock_count_max(), 0);
    check_double("real(8) count_rate",
        _lfortran_i64r64sys_clock_count_rate(), 0);
}

int main(void)
{
    test_default_kind_count();
    test_kind8_count();
    test_no_clock();
    if (failures > 0) {
        printf("%d check(s) failed\n", failures);
        return 1;
    }
    printf("All system_clock checks passed\n");
    return 0;
}
