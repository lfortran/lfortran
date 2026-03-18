#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stdlib.h>

/* ---- CFI_type_cptr ---- */
int32_t c40_check_cptr_type(CFI_cdesc_t *arr) {
    return (arr->type == CFI_type_cptr) ? 1 : 0;
}

/* ---- CFI_type_cfunptr ---- */
int32_t c40_check_cfunptr_type(CFI_cdesc_t *arr) {
    return (arr->type == CFI_type_cfunptr) ? 1 : 0;
}

/* ---- character(len=4) elem_len check ---- */
int32_t c40_check_char4_elem_len(CFI_cdesc_t *arr) {
    return (arr->elem_len == 4) ? 1 : 0;
}

/* ---- character(len=4): sum ichar of first byte of each element ---- */
int32_t c40_sum_char4_first(CFI_cdesc_t *arr) {
    int32_t sum = 0;
    CFI_index_t n = arr->dim[0].extent;
    for (CFI_index_t i = 0; i < n; i++) {
        CFI_index_t sub[1] = { arr->dim[0].lower_bound + i };
        char *ptr = (char *)CFI_address(arr, sub);
        sum += (unsigned char)ptr[0];
    }
    return sum;
}

/* ---- Deferred-length allocatable character ---- */
int32_t c40_check_deferred_char(CFI_cdesc_t *s) {
    /* For a scalar allocatable character, rank=0, elem_len = string length */
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
typedef int32_t (*int_unary_fn)(int32_t);
int32_t c40_call_internal(int32_t (*fp)(int32_t), int32_t x) {
    return fp(x);
}
