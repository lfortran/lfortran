#include <ISO_Fortran_binding.h>
#include <stdio.h>
#include <stdlib.h>

void check_cfi_type(CFI_cdesc_t *desc, int expected_type) {
    if ((int)desc->type != expected_type) {
        printf("FAIL: CFI type = %d, expected %d\n",
               (int)desc->type, expected_type);
        exit(1);
    }
}
