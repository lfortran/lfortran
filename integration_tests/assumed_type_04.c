#include <ISO_Fortran_binding.h>
#include <stdint.h>

void check_cfi_base_addr(CFI_cdesc_t *desc,
                         int32_t *expected_offset,
                         int32_t *expected_size,
                         int32_t *ok) {
    *ok = 0;
    if (desc->base_addr == NULL) return;

    const int32_t *data = (const int32_t *)desc->base_addr;
    if (data[0] == *expected_offset && data[1] == *expected_size) {
        *ok = 1;
    }
}
