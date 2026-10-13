/* C side of bindc_67: allocates, deallocates and associates Fortran
   allocatable and pointer arrays through their CFI descriptors. */
#include <ISO_Fortran_binding.h>
void bindc_67_alloc2d(CFI_cdesc_t *res) {
    const CFI_index_t lb[2] = {0, 1}, ub[2] = {1, 3};
    CFI_allocate(res, lb, ub, 0);
    double *p = (double *)res->base_addr;
    for (int i = 0; i < 6; i++) p[i] = i + 1;
}
void bindc_67_dealloc(CFI_cdesc_t *res) { CFI_deallocate(res); }
void bindc_67_ptr_alloc(CFI_cdesc_t *res) {
    const CFI_index_t lb[1] = {5}, ub[1] = {6};
    CFI_allocate(res, lb, ub, 0);
    int *p = (int *)res->base_addr; p[0] = 7; p[1] = 8;
}
int bindc_67_alloc_fn(CFI_cdesc_t *res) {
    const CFI_index_t lb[1] = {1}, ub[1] = {4};
    return CFI_allocate(res, lb, ub, 0);
}
int bindc_67_count(CFI_cdesc_t *x) { return (int)x->dim[0].extent; }
