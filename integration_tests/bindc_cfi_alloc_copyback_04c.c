#include <string.h>
#include <ISO_Fortran_binding.h>

void c_alloc(CFI_cdesc_t *res) {
    const CFI_index_t lb[1] = {1};
    const CFI_index_t ub[1] = {3};
    if (CFI_allocate(res, lb, ub, 4) == CFI_SUCCESS) {
        memcpy(res->base_addr, "abcdefghijkl", 12);
    }
}

void c_dealloc(CFI_cdesc_t *res) {
    CFI_deallocate(res);
}
