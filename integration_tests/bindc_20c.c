/*
 * C helpers for bindc_20: multi-rank alloc/ptr, scalar alloc/ptr
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

/* generic recursive double-in-place for int32 */
static void dbl_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) { *(int32_t *)base *= 2; return; }
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        dbl_i32(base + i * d[k].sm, d, rank, k + 1);
}

/* ---- allocatable 2D ---- */
int32_t c20_sum_alloc_2d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
int c20_attr_alloc_2d(CFI_cdesc_t *a) {
    return a->attribute == CFI_attribute_allocatable;
}
void c20_double_alloc_2d(CFI_cdesc_t *a) {
    dbl_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable 3D ---- */
int32_t c20_sum_alloc_3d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- pointer 2D ---- */
int32_t c20_sum_ptr_2d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
int c20_attr_ptr_2d(CFI_cdesc_t *a) {
    return a->attribute == CFI_attribute_pointer;
}

/* ---- pointer 3D ---- */
int32_t c20_sum_ptr_3d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- pointer inout ---- */
void c20_double_ptr_1d(CFI_cdesc_t *a) {
    dbl_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable scalar (rank 0) ---- */
int32_t c20_read_alloc_scalar(CFI_cdesc_t *a) {
    return *(int32_t *)a->base_addr;
}
int c20_attr_alloc_scalar(CFI_cdesc_t *a) {
    return a->attribute == CFI_attribute_allocatable;
}

/* ---- pointer scalar (rank 0) ---- */
int32_t c20_read_ptr_scalar(CFI_cdesc_t *a) {
    return *(int32_t *)a->base_addr;
}
int c20_attr_ptr_scalar(CFI_cdesc_t *a) {
    return a->attribute == CFI_attribute_pointer;
}
