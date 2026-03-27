#include <ISO_Fortran_binding.h>
#include <stdint.h>

void check_cfi_type_int32(CFI_cdesc_t *desc, int32_t *ok) {
    *ok = (desc->type == CFI_type_int32_t) ? 1 : 0;
}

void check_cfi_type_real64(CFI_cdesc_t *desc, int32_t *ok) {
    *ok = (desc->type == CFI_type_double) ? 1 : 0;
}
