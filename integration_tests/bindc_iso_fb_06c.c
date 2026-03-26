#include <ISO_Fortran_binding.h>
#include <stdio.h>
#include <stdlib.h>

int cfi_type_int32(void) { return CFI_type_int32_t; }
int cfi_type_float(void) { return CFI_type_float; }

void check_cfi_type(CFI_cdesc_t *desc, int expected_type) {
    if ((int)desc->type != expected_type) {
        printf("FAIL: CFI type = %d, expected %d\n",
               (int)desc->type, expected_type);
        exit(1);
    }
}
