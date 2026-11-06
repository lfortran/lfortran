#include <ISO_Fortran_binding.h>
#include <string.h>

int c_fill(CFI_cdesc_t *p) {
    int was_null = p->base_addr == NULL;
    const CFI_index_t lb[1] = {1};
    const CFI_index_t ub[1] = {3};
    if (CFI_allocate(p, lb, ub, 2) != CFI_SUCCESS) return -1;
    memcpy(p->base_addr, "abcdef", 6);
    return was_null;
}
