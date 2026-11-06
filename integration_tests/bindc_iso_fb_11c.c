#include <ISO_Fortran_binding.h>
#include <string.h>

void f_alloc(CFI_cdesc_t *r);
int f_check(CFI_cdesc_t *s);

int c_alloc_test(void) {
    CFI_CDESC_T(1) d;
    CFI_establish((CFI_cdesc_t *)&d, NULL, CFI_attribute_allocatable,
                  CFI_type_char, 3, 1, NULL);
    f_alloc((CFI_cdesc_t *)&d);
    if (d.base_addr == NULL) return 1;
    if (d.elem_len != 3) return 2;
    if (d.dim[0].extent != 2) return 3;
    if (d.dim[0].sm != 3) return 4;
    if (memcmp(d.base_addr, "abcdef", 6) != 0) return 5;
    CFI_deallocate((CFI_cdesc_t *)&d);
    return 0;
}

int c_existing_test(void) {
    char buf[12];
    const CFI_index_t ext[1] = {4};
    CFI_CDESC_T(1) d;
    memcpy(buf, "abcdefghijkl", 12);
    CFI_establish((CFI_cdesc_t *)&d, buf, CFI_attribute_other,
                  CFI_type_char, 3, 1, ext);
    return f_check((CFI_cdesc_t *)&d);
}
