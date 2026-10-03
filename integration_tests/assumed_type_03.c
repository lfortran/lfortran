#include <ISO_Fortran_binding.h>
#include <string.h>
#include <stdint.h>

void check_dt_char_inline(CFI_cdesc_t *desc, int32_t *ok) {
    /*
     * Verify that the 5-byte string "hello" appears physically inside the
     * raw struct bytes described by the CFI descriptor.  If the Fortran
     * compiler stores character(len=5) inline in the derived type (correct
     * per Fortran standard for non-pointer, non-allocatable components),
     * the bytes are part of the struct and we find them.  If the compiler
     * stores a heap pointer instead, the struct contains an address, not
     * the character data, and this search fails.
     */
    const char *base = (const char *)desc->base_addr;
    size_t len = desc->elem_len;
    *ok = 0;
    for (size_t i = 0; i + 5 <= len; i++) {
        if (memcmp(base + i, "hello", 5) == 0) {
            *ok = 1;
            return;
        }
    }
}

void copy_via_cfi(CFI_cdesc_t *src, CFI_cdesc_t *dst) {
    /*
     * Raw byte copy — exactly what GASNet's gex_Coll_BroadcastNB does
     * inside caffeine's caf_co_broadcast.
     */
    memcpy(dst->base_addr, src->base_addr, src->elem_len);
}
