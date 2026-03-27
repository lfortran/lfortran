#include <ISO_Fortran_binding.h>

int check_elem_len(CFI_cdesc_t* a, int expected) {
    if ((int)a->elem_len != expected) {
        return 1;
    }
    return 0;
}
