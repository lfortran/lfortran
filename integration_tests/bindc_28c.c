/*
 * C helpers for bindc_28: attribute combinations and array features
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stdbool.h>
#include <complex.h>

/* generic recursive sum helpers */
static float sum_f32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(float *)base;
    float total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_f32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

static double sum_f64(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(double *)base;
    double total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_f64(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

static int64_t sum_i64(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int64_t *)base;
    int64_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i64(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

static int32_t sum_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int32_t *)base;
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

/* ---- allocatable float ---- */
float c28_sum_alloc_float(CFI_cdesc_t *a) {
    return sum_f32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable double ---- */
double c28_sum_alloc_double(CFI_cdesc_t *a) {
    return sum_f64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable int64 ---- */
int64_t c28_sum_alloc_int64(CFI_cdesc_t *a) {
    return sum_i64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable complex ---- */
void c28_sum_alloc_complex(CFI_cdesc_t *a, float *re, float *im) {
    *re = 0;
    *im = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        float *c = (float *)(base + i * a->dim[0].sm);
        *re += c[0];
        *im += c[1];
    }
}

/* ---- allocatable logical ---- */
int c28_count_alloc_bool(CFI_cdesc_t *a) {
    int count = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        if (*(unsigned char *)(base + i * a->dim[0].sm)) count++;
    }
    return count;
}

/* ---- pointer float ---- */
float c28_sum_ptr_float(CFI_cdesc_t *a) {
    return sum_f32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- pointer double ---- */
double c28_sum_ptr_double(CFI_cdesc_t *a) {
    return sum_f64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- pointer int64 ---- */
int64_t c28_sum_ptr_int64(CFI_cdesc_t *a) {
    return sum_i64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- zero-size array ---- */
int c28_zero_size_extent(CFI_cdesc_t *a) {
    return (int)a->dim[0].extent;
}

/* ---- unallocated allocatable ---- */
int c28_is_allocated(CFI_cdesc_t *a) {
    return (a->base_addr != NULL) ? 1 : 0;
}

/* ---- disassociated pointer ---- */
int c28_is_associated(CFI_cdesc_t *a) {
    return (a->base_addr != NULL) ? 1 : 0;
}

/* ---- rank 4 ---- */
int32_t c28_sum_4d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

int c28_get_rank_4d(CFI_cdesc_t *a) {
    return (int)a->rank;
}

/* ---- explicit shape: receives raw pointer + length ---- */
int32_t c28_sum_explicit(int32_t *a, int n) {
    int32_t total = 0;
    for (int i = 0; i < n; i++) total += a[i];
    return total;
}

/* ---- optional + contiguous ---- */
int32_t c28_opt_contig_sum(CFI_cdesc_t *a) {
    if (a == NULL) return 0;
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable + assumed-rank ---- */
int c28_alloc_ar_rank(CFI_cdesc_t *a) {
    return (int)a->rank;
}

/* ---- pointer + assumed-rank ---- */
int c28_ptr_ar_rank(CFI_cdesc_t *a) {
    return (int)a->rank;
}
