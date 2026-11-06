#include <ISO_Fortran_binding.h>

int c_elem_len(CFI_cdesc_t *s) {
    return (int)s->elem_len;
}

int c_byte_stride(CFI_cdesc_t *s) {
    return (int)s->dim[0].sm;
}

char c_first_char(CFI_cdesc_t *s, int i) {
    return *((char *)s->base_addr + i * s->dim[0].sm);
}
