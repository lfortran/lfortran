/*
 * C functions for bindc_44: TYPE(*), DIMENSION(..) tests
 *
 * Tests assumed-type + assumed-rank descriptor inspection and data access.
 * Receives CFI descriptors for TYPE(*), DIMENSION(..) arguments and
 * verifies rank, elem_len, shape, and data content.
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>

int32_t c44_get_rank(CFI_cdesc_t *a) {
    return (int32_t)a->rank;
}

int32_t c44_get_elem_len(CFI_cdesc_t *a) {
    return (int32_t)a->elem_len;
}

int32_t c44_total_size(CFI_cdesc_t *a) {
    if (a->rank == 0) return 1;
    int32_t total = 1;
    for (int i = 0; i < a->rank; i++) {
        total *= (int32_t)a->dim[i].extent;
    }
    return total;
}

static int32_t sum_int32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int32_t *)base;
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_int32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

static int32_t sum_float(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return (int32_t)(*(float *)base);
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_float(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

int32_t c44_sum_star(CFI_cdesc_t *a) {
    if (a->type == CFI_type_int32_t || a->type == CFI_type_int) {
        return sum_int32((char *)a->base_addr, a->dim, (int)a->rank, 0);
    }
    if (a->type == CFI_type_float) {
        return sum_float((char *)a->base_addr, a->dim, (int)a->rank, 0);
    }
    return -999;
}

int32_t c44_opt_rank(CFI_cdesc_t *a) {
    if (a == NULL) return -1;
    return (int32_t)a->rank;
}
