/*
 * C helpers for bindc_27: character interop through descriptors
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>

/* generic recursive sum of char codes */
static int sum_chars(char *base, CFI_dim_t *d, int rank, int k,
                     int elem_len) {
    if (k == rank) {
        int total = 0;
        unsigned char *p = (unsigned char *)base;
        for (int j = 0; j < elem_len; j++)
            total += p[j];
        return total;
    }
    int total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_chars(base + i * d[k].sm, d, rank, k + 1, elem_len);
    return total;
}

/* ---- character(len=1) 1D sum ---- */
int c27_char_sum_1d(CFI_cdesc_t *a) {
    return sum_chars((char *)a->base_addr, a->dim, (int)a->rank, 0,
                     (int)a->elem_len);
}

/* ---- character(len=1) 2D sum ---- */
int c27_char_sum_2d(CFI_cdesc_t *a) {
    return sum_chars((char *)a->base_addr, a->dim, (int)a->rank, 0,
                     (int)a->elem_len);
}

/* ---- character inout: toupper ---- */
void c27_char_toupper(CFI_cdesc_t *a) {
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        unsigned char *ch = (unsigned char *)(base + i * a->dim[0].sm);
        if (*ch >= 'a' && *ch <= 'z') *ch -= 32;
    }
}
