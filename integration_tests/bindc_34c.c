/*
 * C helpers for bindc_34: remaining F2018 bind(C) features
 *
 * Tests ENUM, derived type with array component, c_f_procpointer,
 * POINTER+CONTIGUOUS, module bind(C) variables, COMMON block.
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <complex.h>

/* ---- ENUM ---- */
enum { COLOR_RED = 0, COLOR_GREEN = 1, COLOR_BLUE = 2 };

int c34_get_color(int idx) {
    switch (idx) {
        case 0: return COLOR_RED;
        case 1: return COLOR_GREEN;
        case 2: return COLOR_BLUE;
        default: return -1;
    }
}

int c34_check_color(int val, int expected) {
    return val == expected ? 1 : 0;
}

/* ---- Derived type with fixed array component ---- */
typedef struct {
    int32_t data[5];
} vec5_t;

int32_t c34_sum_vec5(vec5_t *v) {
    int32_t s = 0;
    int i;
    for (i = 0; i < 5; i++) s += v->data[i];
    return s;
}

int32_t c34_get_vec5_elem(vec5_t *v, int idx) {
    return v->data[idx];
}

/* ---- c_f_procpointer: provide a C function ---- */
static int c34_double_impl(int x) { return x * 2; }

typedef void (*generic_func_t)(void);

generic_func_t c34_get_func_ptr(void) {
    return (generic_func_t)c34_double_impl;
}

/* ---- Module bind(C) variables ---- */
extern char g34_char;
extern float _Complex g34_cmplx;

void c34_set_globals(char c, float re, float im) {
    g34_char = c;
    /* Portable complex construction */
    float parts[2];
    parts[0] = re;
    parts[1] = im;
    __builtin_memcpy(&g34_cmplx, parts, sizeof(g34_cmplx));
}

char c34_get_char(void) {
    return g34_char;
}

float c34_get_cmplx_re(void) {
    return crealf(g34_cmplx);
}

float c34_get_cmplx_im(void) {
    return cimagf(g34_cmplx);
}

/* ---- COMMON block with BIND(C) ---- */
extern struct {
    int32_t x;
    int32_t y;
} c34_common;

void c34_set_common(int32_t x, int32_t y) {
    c34_common.x = x;
    c34_common.y = y;
}

int32_t c34_get_common_x(void) { return c34_common.x; }
int32_t c34_get_common_y(void) { return c34_common.y; }
