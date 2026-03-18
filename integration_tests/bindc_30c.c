/*
 * C helpers for bindc_30: features not yet supported by LFortran
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>

/* generic recursive sum of char codes */
static int sum_chars(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return (int)*(unsigned char *)base;
    int total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_chars(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

/* ---- character allocatable ---- */
int c30_char_alloc_sum(CFI_cdesc_t *a) {
    return sum_chars((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

int c30_char_alloc_attr(CFI_cdesc_t *a) {
    return a->attribute == CFI_attribute_allocatable;
}

/* ---- character pointer ---- */
int c30_char_ptr_sum(CFI_cdesc_t *a) {
    return sum_chars((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

int c30_char_ptr_attr(CFI_cdesc_t *a) {
    return a->attribute == CFI_attribute_pointer;
}

/* ---- allocatable + assumed-rank ---- */
int c30_alloc_ar_rank(CFI_cdesc_t *a) {
    return (int)a->rank;
}

/* ---- pointer + assumed-rank ---- */
int c30_ptr_ar_rank(CFI_cdesc_t *a) {
    return (int)a->rank;
}
