#include <ISO_Fortran_binding.h>
#include <stddef.h>

int bindc_optional_rank_forward_01_check(int expected, const CFI_cdesc_t *x)
{
    if (expected == 0) return x != NULL;
    if (expected == 4) {
        return x == NULL || x->rank != 1 || x->dim[0].extent != 0 ||
            x->attribute != CFI_attribute_other;
    }
    if (x == NULL || x->rank != expected - 1 ||
            x->base_addr == NULL || x->elem_len != sizeof(int) ||
            x->attribute != CFI_attribute_other) return 1;
    const char *base = (const char *)x->base_addr;
    if (*(const int *)base != 7) return 2;
    if (expected >= 2 &&
            (x->dim[0].extent != 2 ||
             *(const int *)(base + x->dim[0].sm) != 8)) return 3;
    if (expected == 3 &&
            (x->dim[1].extent != 2 ||
             *(const int *)(base + x->dim[1].sm) != 9 ||
             *(const int *)(base + x->dim[0].sm + x->dim[1].sm) != 10)) return 4;
    return 0;
}
