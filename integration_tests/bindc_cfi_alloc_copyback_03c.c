
#include <stdio.h>
#include <ISO_Fortran_binding.h>

void c_alloc(CFI_cdesc_t *res, int *ierr) {
    const CFI_index_t lb[1] = {1};
    const CFI_index_t ub[1] = {3};

    fprintf(stderr, "Before: rank=%d attr=%d base=%p\n",
            (int)res->rank, (int)res->attribute, res->base_addr);

    *ierr = CFI_allocate(res, lb, ub, 0);

    fprintf(stderr, "After: ierr=%d base=%p extent=%td\n",
            *ierr, res->base_addr, res->dim[0].extent);

    if (*ierr == CFI_SUCCESS) {
        int *p = (int *)res->base_addr;
        p[0] = 10;
        p[1] = 20;
        p[2] = 30;
    }
}
