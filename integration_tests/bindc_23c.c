/*
 * C helpers for bindc_23: features not yet supported by LFortran LLVM
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>

/* ---- character assumed-shape ---- */
int c23_char_sum_1d(CFI_cdesc_t *a) {
    int total = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        unsigned char *ch = (unsigned char *)(base + i * a->dim[0].sm);
        total += *ch;
    }
    return total;
}

/* ---- assumed-rank rank query (scalar = rank 0) ---- */
int c23_get_rank(CFI_cdesc_t *a) {
    return (int)a->rank;
}

/* ---- optional allocatable ---- */
static int32_t sum_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int32_t *)base;
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

int c23_opt_alloc_present(CFI_cdesc_t *a) {
    return (a != NULL) ? 1 : 0;
}

int32_t c23_opt_alloc_sum(CFI_cdesc_t *a) {
    if (a == NULL) return 0;
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- optional pointer ---- */
int c23_opt_ptr_present(CFI_cdesc_t *a) {
    return (a != NULL) ? 1 : 0;
}

/* ---- C calling Fortran with descriptor ---- */
extern int32_t fortran_sum_1d(CFI_cdesc_t *a);

int32_t c23_call_fortran_sum(CFI_cdesc_t *a) {
    return fortran_sum_1d(a);
}
