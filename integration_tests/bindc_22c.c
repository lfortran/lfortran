/*
 * C helpers for bindc_22: array sections, derived types, non-default bounds,
 * C calling Fortran with descriptor args
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>

/* generic recursive sum for int32 */
static int32_t sum_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int32_t *)base;
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

/* ---- 1D sum (used for negative stride, non-default bounds) ---- */
int32_t c22_sum_1d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- 2D sum (used for 2D sections) ---- */
int32_t c22_sum_2d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- derived type array: point_t has { int32 x, int32 y } ---- */
int32_t c22_sum_points(CFI_cdesc_t *a) {
    int32_t total = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        int32_t *pt = (int32_t *)(base + i * a->dim[0].sm);
        total += pt[0] + pt[1]; /* x + y */
    }
    return total;
}

int c22_point_elem_size(CFI_cdesc_t *a) {
    return (int)a->elem_len;
}

/* ---- non-default bounds ---- */
int32_t c22_sum_nondefault(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

int c22_get_extent(CFI_cdesc_t *a) {
    return (int)a->dim[0].extent;
}
