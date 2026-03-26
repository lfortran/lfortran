#include <stdint.h>
#include <stdlib.h>

/* Minimal CFI_cdesc_t header matching ISO_Fortran_binding.h layout */
typedef struct {
    void    *base_addr;
    int64_t  elem_len;
    int32_t  version;
    uint8_t  rank;
    int8_t   type;       /* CFI type code */
    uint8_t  attribute;
    uint8_t  extra;
} CFI_header_t;

/* CFI_type_int32_t = 9 on LFortran / flang */
void check_cfi_type_int32(CFI_header_t *desc, int32_t *ok) {
    *ok = (desc->type == 9) ? 1 : 0;
}

void check_cfi_type_real64(CFI_header_t *desc, int32_t *ok) {
    *ok = (desc->type == 28) ? 1 : 0;
}
