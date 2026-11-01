#include <ISO_Fortran_binding.h>

void c_alloc(CFI_cdesc_t *d) {
    const CFI_index_t lb[1] = {1};
    const CFI_index_t ub[1] = {3};
    if (CFI_allocate(d, lb, ub, 0) == CFI_SUCCESS) {
        double *p = (double *)d->base_addr;
        p[0] = 10;
        p[1] = 20;
        p[2] = 30;
    }
}
