
#include <ISO_Fortran_binding.h>

void c_alloc_real(CFI_cdesc_t *res, int *ierr) {
    const CFI_index_t lb[1] = {1};
    const CFI_index_t ub[1] = {3};

    *ierr = CFI_allocate(res, lb, ub, 0);

    if (*ierr == CFI_SUCCESS) {
        double *p = (double *)res->base_addr;
        p[0] = 1.5;
        p[1] = 2.5;
        p[2] = 3.5;
    }
}
