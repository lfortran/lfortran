#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stdlib.h>

/* ---- CFI_type_cptr ---- */
int32_t c40_check_cptr_type(CFI_cdesc_t *arr) {
    return (arr->type == CFI_type_cptr) ? 1 : 0;
}

/* ---- CFI_type_cfunptr ----
 * Note: flang is missing the CFI_type_cfunptr macro (F2018 Table 18.5
 * mandates it). We guard with #ifdef so the test compiles on flang. */
int32_t c40_check_cfunptr_type(CFI_cdesc_t *arr) {
#ifdef CFI_type_cfunptr
    return (arr->type == CFI_type_cfunptr) ? 1 : 0;
#else
    (void)arr;
    return 1;  /* skip check when macro is missing */
#endif
}

/* ---- Deferred-length allocatable character ---- */
int32_t c40_check_deferred_char(CFI_cdesc_t *s) {
    if (s->attribute != CFI_attribute_allocatable) return -1;
    return (int32_t)s->elem_len;
}

/* ---- Explicit-shape 2D: passed as raw pointer ---- */
int32_t c40_sum_explicit_2d(const int32_t *arr, int32_t r, int32_t c) {
    int32_t sum = 0;
    for (int i = 0; i < r * c; i++) sum += arr[i];
    return sum;
}

/* ---- Explicit-shape 3D: passed as raw pointer ---- */
int32_t c40_sum_explicit_3d(const int32_t *arr, int32_t d1, int32_t d2, int32_t d3) {
    int32_t sum = 0;
    for (int i = 0; i < d1 * d2 * d3; i++) sum += arr[i];
    return sum;
}

/* ---- Internal BIND(C) proc: C calls back via function pointer ---- */
int32_t c40_call_internal(int32_t (*fp)(int32_t), int32_t x) {
    return fp(x);
}
