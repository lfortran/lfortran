#include <ISO_Fortran_binding.h>

void c_set_42(CFI_cdesc_t *a) {
    *(int *)a->base_addr = 42;
}
