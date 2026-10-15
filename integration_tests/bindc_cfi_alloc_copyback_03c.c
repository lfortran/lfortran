
#include <ISO_Fortran_binding.h>

void c_alloc(CFI_cdesc_t *res, int *ierr) {
    const CFI_index_t lb[1] = {1};
    const CFI_index_t ub[1] = {3};



    *ierr = CFI_allocate(res, lb, ub, 0);



    if (*ierr == CFI_SUCCESS) {
        int *p = (int *)res->base_addr;
        p[0] = 10;
        p[1] = 20;
        p[2] = 30;
    }
}
