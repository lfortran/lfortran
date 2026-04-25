/*
 * Consolidated C helpers for bindc_iso_fb_01
 *
 * Merged from:
 *   - bindc_18c.c: CFI descriptor sums (int32/int64/float/double),
 *                  double-in-place, assumed-rank queries, allocatable,
 *                  pointer, contiguity, optional
 *   - bindc_19c.c: complex float/double sums, logical count/flip,
 *                  character helpers, CONTIGUOUS attribute
 *   - bindc_24c.c: scalar type operations (int8, int16, short, long,
 *                  long_long, size_t, intptr_t, ptrdiff_t), VALUE with
 *                  bool/complex/character, small-int array sums,
 *                  complex return values
 *
 * NOTE: bindc_18c.c and bindc_19c.c both defined an identical
 *       static int32_t sum_i32(...) helper. Only one copy is kept
 *       (from bindc_18c.c) and shared by both sets of functions.
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <math.h>
#include <complex.h>

/* ================================================================
 *  From bindc_18c.c
 * ================================================================ */

/* ----------------------------------------------------------------
 *  int32 — sum (any rank via recursive descent on descriptor dims)
 * ---------------------------------------------------------------- */
static int32_t sum_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int32_t *)base;
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
static int32_t sum_i32_entry(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
int32_t c_sum_int32_1d(CFI_cdesc_t *a) { return sum_i32_entry(a); }
int32_t c_sum_int32_2d(CFI_cdesc_t *a) { return sum_i32_entry(a); }
int32_t c_sum_int32_3d(CFI_cdesc_t *a) { return sum_i32_entry(a); }
int32_t c_sum_int32_ar(CFI_cdesc_t *a) { return sum_i32_entry(a); }

/* ----------------------------------------------------------------
 *  int64 — sum (any rank)
 * ---------------------------------------------------------------- */
static int64_t sum_i64(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int64_t *)base;
    int64_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i64(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
static int64_t sum_i64_entry(CFI_cdesc_t *a) {
    return sum_i64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
int64_t c_sum_int64_1d(CFI_cdesc_t *a) { return sum_i64_entry(a); }
int64_t c_sum_int64_2d(CFI_cdesc_t *a) { return sum_i64_entry(a); }
int64_t c_sum_int64_3d(CFI_cdesc_t *a) { return sum_i64_entry(a); }

/* ----------------------------------------------------------------
 *  float — sum (any rank)
 * ---------------------------------------------------------------- */
static float sum_f32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(float *)base;
    float total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_f32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
static float sum_f32_entry(CFI_cdesc_t *a) {
    return sum_f32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
float c_sum_float_1d(CFI_cdesc_t *a) { return sum_f32_entry(a); }
float c_sum_float_2d(CFI_cdesc_t *a) { return sum_f32_entry(a); }
float c_sum_float_3d(CFI_cdesc_t *a) { return sum_f32_entry(a); }

/* ----------------------------------------------------------------
 *  double — sum (any rank)
 * ---------------------------------------------------------------- */
static double sum_f64(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(double *)base;
    double total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_f64(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
static double sum_f64_entry(CFI_cdesc_t *a) {
    return sum_f64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
double c_sum_double_1d(CFI_cdesc_t *a) { return sum_f64_entry(a); }
double c_sum_double_2d(CFI_cdesc_t *a) { return sum_f64_entry(a); }
double c_sum_double_3d(CFI_cdesc_t *a) { return sum_f64_entry(a); }

/* ----------------------------------------------------------------
 *  int32 — double-in-place (any rank)
 * ---------------------------------------------------------------- */
static void dbl_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) { *(int32_t *)base *= 2; return; }
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        dbl_i32(base + i * d[k].sm, d, rank, k + 1);
}
static void dbl_i32_entry(CFI_cdesc_t *a) {
    dbl_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
void c_double_int32_1d(CFI_cdesc_t *a) { dbl_i32_entry(a); }
void c_double_int32_2d(CFI_cdesc_t *a) { dbl_i32_entry(a); }
void c_double_int32_3d(CFI_cdesc_t *a) { dbl_i32_entry(a); }

/* ----------------------------------------------------------------
 *  Assumed-rank queries (type(*), dimension(..))
 * ---------------------------------------------------------------- */
int c_get_rank(CFI_cdesc_t *a) {
    return (int)a->rank;
}

int c_get_elem_size(CFI_cdesc_t *a) {
    return (int)a->elem_len;
}

/* ----------------------------------------------------------------
 *  Allocatable arrays — same sum logic, separate entry points
 * ---------------------------------------------------------------- */
int32_t c_sum_alloc_1d(CFI_cdesc_t *a) { return sum_i32_entry(a); }
int c_attr_alloc(CFI_cdesc_t *a) { return a->attribute == CFI_attribute_allocatable; }
void c_double_alloc_1d(CFI_cdesc_t *a) { dbl_i32_entry(a); }

/* ----------------------------------------------------------------
 *  Pointer arrays — same sum logic, separate entry points
 * ---------------------------------------------------------------- */
int32_t c_sum_ptr_1d(CFI_cdesc_t *a) { return sum_i32_entry(a); }
int c_attr_ptr(CFI_cdesc_t *a) { return a->attribute == CFI_attribute_pointer; }

/* ----------------------------------------------------------------
 *  Attribute / contiguity queries for assumed-shape (other)
 * ---------------------------------------------------------------- */
int c_attr_other(CFI_cdesc_t *a) { return a->attribute == CFI_attribute_other; }

int c_is_contiguous(CFI_cdesc_t *a) {
    CFI_index_t expected = (CFI_index_t)a->elem_len;
    for (int i = 0; i < (int)a->rank; i++) {
        if (a->dim[i].sm != expected) return 0;
        expected *= a->dim[i].extent;
    }
    return 1;
}

/* ----------------------------------------------------------------
 *  Optional argument — NULL descriptor when absent
 * ---------------------------------------------------------------- */
int c_is_present(CFI_cdesc_t *a) {
    return (a != NULL) ? 1 : 0;
}

/* ================================================================
 *  From bindc_19c.c
 *
 *  NOTE: The duplicate static sum_i32() that was in bindc_19c.c has
 *  been removed. The c19_sum_contiguous() function below uses the
 *  single sum_i32() defined above (from bindc_18c.c).
 * ================================================================ */

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

/* ---- CONTIGUOUS (uses shared sum_i32 from bindc_18c.c section) ---- */
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

/* ================================================================
 *  From bindc_24c.c
 * ================================================================ */

/* ---- int8 ---- */
int8_t c24_add_int8(int8_t a, int8_t b) { return a + b; }
void c24_double_int8(int8_t *a) { *a *= 2; }

/* ---- int16 ---- */
int16_t c24_add_int16(int16_t a, int16_t b) { return a + b; }
void c24_double_int16(int16_t *a) { *a *= 2; }

/* ---- short ---- */
short c24_add_short(short a, short b) { return a + b; }

/* ---- long ---- */
long c24_add_long(long a, long b) { return a + b; }

/* ---- long long ---- */
long long c24_add_long_long(long long a, long long b) { return a + b; }

/* ---- size_t ---- */
size_t c24_add_size_t(size_t a, size_t b) { return a + b; }

/* ---- intptr_t ---- */
intptr_t c24_add_intptr(intptr_t a, intptr_t b) { return a + b; }

/* ---- ptrdiff_t ---- */
ptrdiff_t c24_add_ptrdiff(ptrdiff_t a, ptrdiff_t b) { return a + b; }

/* ---- VALUE with logical (c_bool = _Bool) ---- */
int c24_bool_to_int(_Bool x) { return x ? 1 : 0; }

/* ---- VALUE with complex ---- */
float c24_cabs_float(float _Complex z) {
    return (float)sqrt((double)(crealf(z)*crealf(z) + cimagf(z)*cimagf(z)));
}

double c24_cabs_double(double _Complex z) {
    return sqrt(creal(z)*creal(z) + cimag(z)*cimag(z));
}

/* ---- VALUE with character ---- */
int c24_char_to_int(char ch) { return (int)(unsigned char)ch; }

/* ---- int8/int16 arrays via descriptor ---- */
int c24_sum_int8_1d(CFI_cdesc_t *a) {
    int total = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        total += *(int8_t *)(base + i * a->dim[0].sm);
    }
    return total;
}

int c24_sum_int16_1d(CFI_cdesc_t *a) {
    int total = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        total += *(int16_t *)(base + i * a->dim[0].sm);
    }
    return total;
}

/* ---- function returning complex ---- */
float _Complex c24_make_complex(float re, float im) {
    return re + im * I;
}

double _Complex c24_make_dcomplex(double re, double im) {
    return re + im * I;
}
