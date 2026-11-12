#include "ISO_Fortran_binding.h"

int is_contig(CFI_cdesc_t *a) {
    return CFI_is_contiguous(a);
}
