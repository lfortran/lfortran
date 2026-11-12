#include <ISO_Fortran_binding.h>
#include <stddef.h>

int bindc_optional_type_star_rank_01_check(int expected, const CFI_cdesc_t *x)
{
    if (expected == 0) return x != NULL;
    if (x == NULL || x->rank != (expected >= 4 ? 1 : expected - 1) ||
            x->type != CFI_type_int32_t ||
            x->attribute != CFI_attribute_other ||
            x->elem_len != sizeof(int)) return 1;
    if (expected == 4) return x->dim[0].extent != 0;
    const char *data = (const char *)x->base_addr;
    if (data == NULL || *(const int *)data != 7) return 2;
    if (expected == 5 || expected == 6) {
        return x->dim[0].extent != 2 ||
            *(const int *)(data + x->dim[0].sm) != (expected == 5 ? 9 : 8);
    }
    if (expected >= 2 &&
            (x->dim[0].extent != 2 ||
             x->dim[0].sm != (CFI_index_t)sizeof(int) ||
             *(const int *)(data + x->dim[0].sm) != 8)) return 3;
    if (expected == 3 &&
            (x->dim[1].extent != 2 ||
             x->dim[1].sm != (CFI_index_t)(2 * sizeof(int)) ||
             *(const int *)(data + x->dim[1].sm) != 9 ||
             *(const int *)(data + x->dim[0].sm + x->dim[1].sm) != 10)) return 4;
    return 0;
}
