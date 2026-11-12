#include "ISO_Fortran_binding.h"

/* r => a(i, :), with i a Fortran (1-based) row index */
int get_row(CFI_cdesc_t *a, int i, CFI_cdesc_t *r) {
    CFI_index_t row = a->dim[0].lower_bound + i - 1;
    CFI_index_t lower[2] = {row, a->dim[1].lower_bound};
    CFI_index_t upper[2] = {row, a->dim[1].lower_bound + a->dim[1].extent - 1};
    CFI_index_t strides[2] = {0, 1};
    return CFI_section(r, a, lower, upper, strides);
}

/* allocate x(1:n) passing a wrong elem_len; return the elem_len it ends with */
int alloc_with_bad_elem_len(CFI_cdesc_t *x, int n) {
    CFI_index_t lower[1] = {1};
    CFI_index_t upper[1] = {n};
    if (CFI_allocate(x, lower, upper, 9999) != CFI_SUCCESS) return -1;
    return (int)x->elem_len;
}

int is_contig(CFI_cdesc_t *a) {
    return CFI_is_contiguous(a);
}
