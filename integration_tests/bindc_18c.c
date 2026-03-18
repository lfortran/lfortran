/*
 * C functions that receive assumed-shape arrays from Fortran
 * via CFI descriptors (ISO_Fortran_binding.h).
 */
#include <ISO_Fortran_binding.h>

/* Sum elements of an assumed-shape integer array a(:) */
int c_sum_array(CFI_cdesc_t *a) {
    int n = (int)a->dim[0].extent;
    int *data = (int *)a->base_addr;
    CFI_index_t stride = a->dim[0].sm / a->elem_len;
    int total = 0;
    for (int i = 0; i < n; i++) {
        total += data[i * stride];
    }
    return total;
}

/* Double each element of an assumed-shape integer array a(:) in-place */
void c_double_array(CFI_cdesc_t *a) {
    int n = (int)a->dim[0].extent;
    int *data = (int *)a->base_addr;
    CFI_index_t stride = a->dim[0].sm / a->elem_len;
    for (int i = 0; i < n; i++) {
        data[i * stride] = data[i * stride] * 2;
    }
}
