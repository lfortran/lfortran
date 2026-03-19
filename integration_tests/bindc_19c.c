/*
 * C helpers for bindc_19: complex, logical, character, CONTIGUOUS
 * via CFI descriptors (ISO_Fortran_binding.h).
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stdbool.h>

/* ---- complex float (2 floats per element) ---- */
void c19_sum_cfloat_1d(CFI_cdesc_t *a, float *re, float *im) {
    *re = 0; *im = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        float *c = (float *)(base + i * a->dim[0].sm);
        *re += c[0];
        *im += c[1];
    }
}

void c19_sum_cfloat_2d(CFI_cdesc_t *a, float *re, float *im) {
    *re = 0; *im = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t j = 0; j < a->dim[1].extent; j++)
        for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
            float *c = (float *)(base + i * a->dim[0].sm + j * a->dim[1].sm);
            *re += c[0];
            *im += c[1];
        }
}

void c19_scale_cfloat_1d(CFI_cdesc_t *a) {
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        float *c = (float *)(base + i * a->dim[0].sm);
        c[0] *= 2;
        c[1] *= 2;
    }
}

/* ---- complex double (2 doubles per element) ---- */
void c19_sum_cdouble_1d(CFI_cdesc_t *a, double *re, double *im) {
    *re = 0; *im = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        double *c = (double *)(base + i * a->dim[0].sm);
        *re += c[0];
        *im += c[1];
    }
}

void c19_sum_cdouble_2d(CFI_cdesc_t *a, double *re, double *im) {
    *re = 0; *im = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t j = 0; j < a->dim[1].extent; j++)
        for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
            double *c = (double *)(base + i * a->dim[0].sm + j * a->dim[1].sm);
            *re += c[0];
            *im += c[1];
        }
}

/* ---- logical (c_bool = 1 byte typically) ---- */
static int count_true(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) {
        /* c_bool: nonzero means true */
        return (*(unsigned char *)base) ? 1 : 0;
    }
    int total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += count_true(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

int c19_count_true_1d(CFI_cdesc_t *a) {
    return count_true((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

int c19_count_true_2d(CFI_cdesc_t *a) {
    return count_true((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

void c19_flip_bool_1d(CFI_cdesc_t *a) {
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        unsigned char *v = (unsigned char *)(base + i * a->dim[0].sm);
        *v = (*v) ? 0 : 1;
    }
}

/* ---- character ---- */
int c19_char_sum_1d(CFI_cdesc_t *a) {
    int total = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        unsigned char *ch = (unsigned char *)(base + i * a->dim[0].sm);
        total += *ch;
    }
    return total;
}

int c19_char_elem_len(CFI_cdesc_t *a) {
    return (int)a->elem_len;
}

/* ---- CONTIGUOUS ---- */
static int32_t sum_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int32_t *)base;
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

int c19_sum_contiguous(CFI_cdesc_t *a) {
    return (int)sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

int c19_is_contiguous_check(CFI_cdesc_t *a) {
    CFI_index_t expected = (CFI_index_t)a->elem_len;
    for (int i = 0; i < (int)a->rank; i++) {
        if (a->dim[i].sm != expected) return 0;
        expected *= a->dim[i].extent;
    }
    return 1;
}
