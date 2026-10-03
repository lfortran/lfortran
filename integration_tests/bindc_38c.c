/*
 * C helpers for bindc_38: VOLATILE, ASYNCHRONOUS, PURE BIND(C)
 *
 * Tests volatile global variable interop, asynchronous array passing,
 * and invoking Fortran function pointers from C.
 */
#include <stdint.h>

/* ---- VOLATILE module variables ---- */
extern volatile int g38_volatile_int;
extern volatile double g38_volatile_dbl;

void c38_set_volatile_int(int val) {
    g38_volatile_int = val;
}

void c38_set_volatile_dbl(double val) {
    g38_volatile_dbl = val;
}

/* ---- ASYNCHRONOUS array fill ---- */
void c38_fill_async(int *arr, int n) {
    for (int i = 0; i < n; i++) {
        arr[i] = arr[i] * 2;
    }
}

/* ---- Invoke a Fortran BIND(C) function pointer ---- */
typedef int (*int_binop_t)(int, int);

int c38_invoke_binop(int_binop_t fp, int a, int b) {
    return fp(a, b);
}
