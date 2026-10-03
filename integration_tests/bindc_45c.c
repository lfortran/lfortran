/*
 * C function for bindc_45: mutual recursion with Fortran
 *
 * c45_mutual_fib calls back into Fortran's f45_fib, which in turn
 * calls c45_mutual_fib for sub-problems, creating a Fortran<->C
 * recursive call chain.
 */
#include <stdint.h>

extern int32_t f45_fib(int32_t n);

int32_t c45_mutual_fib(int32_t n) {
    return f45_fib(n);
}
