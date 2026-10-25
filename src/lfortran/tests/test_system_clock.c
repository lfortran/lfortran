#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

extern int32_t _lfortran_i32sys_clock_count(void);
extern int32_t _lfortran_i32sys_clock_count_rate(void);
extern int32_t _lfortran_i32sys_clock_count_max(void);
extern uint64_t _lfortran_i64sys_clock_count(void);
extern int64_t _lfortran_i64sys_clock_count_rate(void);
extern int64_t _lfortran_i64sys_clock_count_max(void);

static struct timespec clock_value;
static int clock_status;
static int monotonic_calls;

// Interpose the platform clock while testing the actual static runtime.
// Other clocks may be queried by libc internals (e.g. on FreeBSD), so only
// CLOCK_MONOTONIC calls are counted; check_count() verifies the runtime used it.
int clock_gettime(clockid_t clock_id, struct timespec *value) {
    if (clock_id == CLOCK_MONOTONIC) {
        monotonic_calls++;
    }
    *value = clock_value;
    return clock_status;
}

static void check_count(time_t seconds, long nanoseconds,
        int32_t expected32, uint64_t expected64) {
    clock_value.tv_sec = seconds;
    clock_value.tv_nsec = nanoseconds;
    int calls = monotonic_calls;
    int32_t count32 = _lfortran_i32sys_clock_count();
    uint64_t count64 = _lfortran_i64sys_clock_count();
    if (monotonic_calls != calls + 2) {
        fprintf(stderr, "unexpected system_clock source\n");
        exit(1);
    }
    if (count32 != expected32 || count64 != expected64) {
        fprintf(stderr, "system_clock: got %" PRId32 ", %" PRIu64
            "; expected %" PRId32 ", %" PRIu64 "\n",
            count32, count64, expected32, expected64);
        exit(1);
    }
}

int main(void) {
    check_count(0, 0, 0, 0);
    check_count(0, 999999, 0, 999999);
    check_count(2147483, 647999999, INT32_MAX, UINT64_C(2147483647999999));
    check_count(2147483, 648000000, 0, UINT64_C(2147483648000000));
    check_count(2147483, 649000000, 1, UINT64_C(2147483649000000));
    check_count(4294967, 295000000, INT32_MAX, UINT64_C(4294967295000000));
    check_count(4294967, 296000000, 0, UINT64_C(4294967296000000));
    if (sizeof(time_t) >= sizeof(int64_t)) {
        // Seconds no longer fit int32_t; nanoseconds reach both signed and
        // unsigned 64-bit boundaries and finally the largest signed time_t.
        check_count((time_t)UINT64_C(2147483648), 0, 0, UINT64_C(2147483648000000000));
        check_count((time_t)UINT64_C(9223372036), 854775807, 2077252342, INT64_MAX);
        check_count((time_t)UINT64_C(9223372036), 854775808, 2077252342, 0);
        check_count((time_t)UINT64_C(18446744073), 709551615, 2007021037, INT64_MAX);
        check_count((time_t)UINT64_C(18446744073), 709551616, 2007021037, 0);
        check_count((time_t)UINT64_C(18446744073), 709551617, 2007021037, 1);
        check_count((time_t)INT64_MAX, 999999999, INT32_MAX, INT64_MAX);
    }
    if (_lfortran_i32sys_clock_count_rate() != 1000 ||
            _lfortran_i32sys_clock_count_max() != INT32_MAX ||
            _lfortran_i64sys_clock_count_rate() != INT64_C(1000000000) ||
            _lfortran_i64sys_clock_count_max() != INT64_MAX) return 1;
    clock_status = -1;
    check_count(0, 0, -INT32_MAX, (uint64_t)-INT64_MAX);
    if (_lfortran_i32sys_clock_count_rate() != 0 ||
            _lfortran_i32sys_clock_count_max() != 0 ||
            _lfortran_i64sys_clock_count_rate() != 0 ||
            _lfortran_i64sys_clock_count_max() != 0) return 1;
    return 0;
}
