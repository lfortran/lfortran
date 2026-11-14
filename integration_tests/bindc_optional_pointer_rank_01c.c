#include <ISO_Fortran_binding.h>
#include <stddef.h>

int bindc_optional_pointer_rank_01_check(int expected, CFI_cdesc_t *x)
{
    if (expected == 0) return x != NULL;
    if (expected == 2) {
        return x == NULL || x->rank != 1 ||
            x->attribute != CFI_attribute_pointer || x->base_addr != NULL;
    }
    if (expected == 3) {
        return x == NULL || x->rank != 0 ||
            x->attribute != CFI_attribute_pointer || x->base_addr == NULL ||
            *(const int *)x->base_addr != 7;
    }
    if (expected == 4) {
        if (x == NULL || x->rank != 2 || x->elem_len != sizeof(int) ||
                x->attribute != CFI_attribute_pointer || x->base_addr == NULL ||
                x->dim[0].extent != 2 || x->dim[1].extent != 2) return 1;
        const char *base = (const char *)x->base_addr;
        return *(const int *)base != 7 ||
            *(const int *)(base + x->dim[0].sm) != 8 ||
            *(const int *)(base + x->dim[1].sm) != 9;
    }
    if (x == NULL || x->rank != 1 || x->elem_len != sizeof(int) ||
            x->attribute != CFI_attribute_pointer ||
            x->dim[0].extent != 2 || x->base_addr == NULL) return 1;
    const char *data = (const char *)x->base_addr;
    return *(const int *)data != 7 ||
        *(const int *)(data + x->dim[0].sm) != 8;
}
