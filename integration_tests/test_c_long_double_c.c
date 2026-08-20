#include <math.h>
#include <float.h>

long double add_long_double(long double a, long double b) {
    return a + b;
}

int check_long_double(long double val) {
    long double expected = 3.234567890123456789L;
    long double diff = fabsl(val - expected);
    /* Use 4 ULP relative to the result as tolerance.
     * This works on all platforms:
     *   macOS x86-64  : long double = double  (LDBL_EPSILON ~ 2.2e-16)
     *   Linux x86-64  : long double = x86_fp80 (LDBL_EPSILON ~ 1.1e-19)
     *   Apple Silicon : long double = fp128    (LDBL_EPSILON ~ 1.9e-34) */
    long double tol = 4.0L * LDBL_EPSILON * fabsl(expected);
    return (diff <= tol) ? 1 : 0;
}
