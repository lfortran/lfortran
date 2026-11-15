#include <ISO_Fortran_binding.h>
#include <stddef.h>

void select_rank_52_give(CFI_cdesc_t *slot);
void select_rank_52_drop(CFI_cdesc_t *slot);

int select_rank_52_c_give(void)
{
    CFI_CDESC_T(0) d;
    CFI_cdesc_t *a = (CFI_cdesc_t *)&d;
    if (CFI_establish(a, NULL, CFI_attribute_allocatable, CFI_type_int,
            0, 0, NULL) != CFI_SUCCESS) return -1;
    select_rank_52_give(a);
    if (a->base_addr == NULL) return -2;
    int v = *(int *)a->base_addr;
    select_rank_52_drop(a);
    if (a->base_addr != NULL) return -3;
    return v;
}
