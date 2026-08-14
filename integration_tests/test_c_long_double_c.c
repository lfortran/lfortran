#include <math.h>

long double add_long_double(long double a, long double b) {
    return a + b;
}

int check_long_double(long double val) {
    long double expected = 3.234567890123456789L;
    long double diff = fabsl(val - expected);
    return (diff <= 1.0e-17L) ? 1 : 0;
}
