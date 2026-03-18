/*
 * C helpers for bindc_24: additional scalar types and VALUE variations
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stddef.h>
#include <math.h>
#include <stdbool.h>
#include <complex.h>

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
