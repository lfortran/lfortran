/*
 * C helpers for bindc_21: assumed-rank extensions, optional combos,
 * multi-argument
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>

/* generic recursive helpers */
static int32_t sum_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int32_t *)base;
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
static void dbl_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) { *(int32_t *)base *= 2; return; }
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        dbl_i32(base + i * d[k].sm, d, rank, k + 1);
}

/* ---- assumed-rank inout ---- */
void c21_double_ar(CFI_cdesc_t *a) {
    dbl_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- assumed-rank typed sums ---- */
static int64_t sum_i64(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int64_t *)base;
    int64_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i64(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
int64_t c21_sum_ar_i64(CFI_cdesc_t *a) {
    return sum_i64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

static double sum_f64(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(double *)base;
    double total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_f64(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
int c21_sum_ar_dbl(CFI_cdesc_t *a) {
    return (int)sum_f64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- assumed-rank rank query ---- */
/* (rank-0 test moved to bindc_23) */

/* ---- optional assumed-rank ---- */
int c21_opt_ar_present(CFI_cdesc_t *a) {
    return (a != NULL) ? 1 : 0;
}

/* ---- optional scalar ----
 * For optional scalars without allocatable/pointer, some compilers pass
 * a raw pointer (not a descriptor). We check for NULL.
 */
int c21_opt_scalar_present(int32_t *x) {
    return (x != NULL) ? 1 : 0;
}
int32_t c21_opt_scalar_value(int32_t *x) {
    return (x != NULL) ? *x : 0;
}

/* ---- multiple descriptor arguments ---- */
int32_t c21_dot_product(CFI_cdesc_t *a, CFI_cdesc_t *b) {
    int32_t total = 0;
    char *ba = (char *)a->base_addr;
    char *bb = (char *)b->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        int32_t va = *(int32_t *)(ba + i * a->dim[0].sm);
        int32_t vb = *(int32_t *)(bb + i * b->dim[0].sm);
        total += va * vb;
    }
    return total;
}

void c21_add_arrays(CFI_cdesc_t *a, CFI_cdesc_t *b, CFI_cdesc_t *c) {
    char *ba = (char *)a->base_addr;
    char *bb = (char *)b->base_addr;
    char *bc = (char *)c->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        int32_t va = *(int32_t *)(ba + i * a->dim[0].sm);
        int32_t vb = *(int32_t *)(bb + i * b->dim[0].sm);
        *(int32_t *)(bc + i * c->dim[0].sm) = va + vb;
    }
}
