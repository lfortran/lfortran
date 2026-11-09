#include <ISO_Fortran_binding.h>
#include <stddef.h>
#include <stdint.h>

/* Check the CFI descriptor of character data forwarded through a type(*)
   wrapper, then overwrite the first character through base_addr. */
void check_char_cfi(CFI_cdesc_t *d, void *expected, size_t elem_len,
                    int32_t rank, int32_t *ok) {
    *ok = d->base_addr == expected && d->rank == rank &&
        (elem_len == 0 || d->elem_len == elem_len);
    if (d->base_addr != NULL) {
        ((char *)d->base_addr)[0] = 'Y';
    }
}
