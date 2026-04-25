#include <stdint.h>
#include <stdbool.h>

/* ---- TYPE(*) scalar ---- */
int32_t c39_type_star_scalar(const void *x) {
    /* We know it's int32_t from the Fortran call site */
    return *(const int32_t *)x;
}

/* ---- TYPE(*) assumed-size ---- */
int32_t c39_type_star_assumed_size(const void *x, int32_t n) {
    const int32_t *arr = (const int32_t *)x;
    int32_t sum = 0;
    for (int i = 0; i < n; i++) sum += arr[i];
    return sum;
}

/* ---- Assumed-size integer array ---- */
int32_t c39_sum_assumed_size_int(const int32_t *arr, int32_t n) {
    int32_t sum = 0;
    for (int i = 0; i < n; i++) sum += arr[i];
    return sum;
}

/* ---- Assumed-size real(c_double) array ---- */
double c39_sum_assumed_size_dbl(const double *arr, int32_t n) {
    double sum = 0.0;
    for (int i = 0; i < n; i++) sum += arr[i];
    return sum;
}

/* ---- BLOCK DATA common block ----
 * The common block /c39_bd_common/ has BIND(C, NAME="c39_bd_common")
 * so it appears as a C struct with two int32_t members. */
extern struct { int32_t a, b; } c39_bd_common;

int32_t c39_get_common_a(void) { return c39_bd_common.a; }
int32_t c39_get_common_b(void) { return c39_bd_common.b; }
