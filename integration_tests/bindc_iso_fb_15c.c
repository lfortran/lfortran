#include <ISO_Fortran_binding.h>
#include <string.h>

int bindc_iso_fb_15_realloc(CFI_cdesc_t *p);

int bindc_iso_fb_15_drive(void) {
    CFI_CDESC_T(1) d;
    CFI_cdesc_t *p = (CFI_cdesc_t *) &d;
    CFI_index_t lb[1] = {1}, ub[1] = {3};
    if (CFI_establish(p, NULL, CFI_attribute_pointer, CFI_type_char, 2, 1, NULL)) return 10;
    if (CFI_allocate(p, lb, ub, 2)) return 11;
    memcpy(p->base_addr, "abcdef", 6);
    if (bindc_iso_fb_15_realloc(p) != 1) return 12;
    if (p->dim[0].extent != 2 || p->elem_len != 2) return 13;
    if (memcmp(p->base_addr, "xyzw", 4) != 0) return 14;
    if (CFI_deallocate(p)) return 15;
    return 1;
}
