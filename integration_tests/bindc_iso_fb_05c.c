/*
 * Consolidated C helpers for bindc_iso_fb_05
 *
 * Merged from: bindc_21c, bindc_23c, bindc_25c, bindc_29c,
 *              bindc_34c, bindc_40c, bindc_42c, bindc_44c
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <complex.h>

/* ============================================================
 * Shared static helpers
 * (identical definition in bindc_21c, bindc_23c, bindc_29c — kept once)
 * ============================================================ */
static int32_t sum_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int32_t *)base;
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

/* ============================================================
 * From bindc_21c.c
 * ============================================================ */

static void dbl_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) { *(int32_t *)base *= 2; return; }
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        dbl_i32(base + i * d[k].sm, d, rank, k + 1);
}

/* ---- assumed-rank inout ---- */
void c21_double_ar(CFI_cdesc_t *a) {
    dbl_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- assumed-rank typed sums ---- */
static int64_t sum_i64(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int64_t *)base;
    int64_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i64(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
int64_t c21_sum_ar_i64(CFI_cdesc_t *a) {
    return sum_i64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

static double sum_f64(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(double *)base;
    double total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_f64(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
int c21_sum_ar_dbl(CFI_cdesc_t *a) {
    return (int)sum_f64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- optional assumed-rank ---- */
int c21_opt_ar_present(CFI_cdesc_t *a) {
    return (a != NULL) ? 1 : 0;
}

/* ---- optional scalar ---- */
int c21_opt_scalar_present(int32_t *x) {
    return (x != NULL) ? 1 : 0;
}
int32_t c21_opt_scalar_value(int32_t *x) {
    return (x != NULL) ? *x : 0;
}

/* ---- multiple descriptor arguments ---- */
int32_t c21_dot_product(CFI_cdesc_t *a, CFI_cdesc_t *b) {
    int32_t total = 0;
    char *ba = (char *)a->base_addr;
    char *bb = (char *)b->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        int32_t va = *(int32_t *)(ba + i * a->dim[0].sm);
        int32_t vb = *(int32_t *)(bb + i * b->dim[0].sm);
        total += va * vb;
    }
    return total;
}

void c21_add_arrays(CFI_cdesc_t *a, CFI_cdesc_t *b, CFI_cdesc_t *c) {
    char *ba = (char *)a->base_addr;
    char *bb = (char *)b->base_addr;
    char *bc = (char *)c->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        int32_t va = *(int32_t *)(ba + i * a->dim[0].sm);
        int32_t vb = *(int32_t *)(bb + i * b->dim[0].sm);
        *(int32_t *)(bc + i * c->dim[0].sm) = va + vb;
    }
}

/* ============================================================
 * From bindc_23c.c
 * (sum_i32 already defined above as shared helper)
 * ============================================================ */

/* ---- character assumed-shape ---- */
int c23_char_sum_1d(CFI_cdesc_t *a) {
    int total = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        unsigned char *ch = (unsigned char *)(base + i * a->dim[0].sm);
        total += *ch;
    }
    return total;
}

/* ---- assumed-rank rank query (scalar = rank 0) ---- */
int c23_get_rank(CFI_cdesc_t *a) {
    return (int)a->rank;
}

/* ---- optional allocatable ---- */
int c23_opt_alloc_present(CFI_cdesc_t *a) {
    return (a != NULL) ? 1 : 0;
}

int32_t c23_opt_alloc_sum(CFI_cdesc_t *a) {
    if (a == NULL) return 0;
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- optional pointer ---- */
int c23_opt_ptr_present(CFI_cdesc_t *a) {
    return (a != NULL) ? 1 : 0;
}

/* ---- C calling Fortran with descriptor ---- */
extern int32_t fortran_sum_1d(CFI_cdesc_t *a);

int32_t c23_call_fortran_sum(CFI_cdesc_t *a) {
    return fortran_sum_1d(a);
}

/* ============================================================
 * From bindc_25c.c
 * ============================================================ */

/* Fortran procedure declarations */
extern void f25_double_1d(CFI_cdesc_t *a);
extern int32_t f25_sum_1d(CFI_cdesc_t *a);
extern int32_t f25_sum_2d(CFI_cdesc_t *a);
extern int f25_get_rank(CFI_cdesc_t *a);
extern int f25_opt_present(CFI_cdesc_t *a);
extern int32_t f25_sum_contig(CFI_cdesc_t *a);
extern void f25_fill_array(CFI_cdesc_t *a);
extern int32_t f25_dot(CFI_cdesc_t *a, CFI_cdesc_t *b);
extern void f25_add(CFI_cdesc_t *a, CFI_cdesc_t *b, CFI_cdesc_t *c);
extern int32_t f25_sum_pairs(CFI_cdesc_t *pts);
extern void f25_sum_complex(CFI_cdesc_t *a, float *re, float *im);
extern int f25_count_true(CFI_cdesc_t *a);
extern int32_t f25_square(int32_t x);
extern float _Complex f25_conj(float _Complex z);

typedef struct { int32_t a; int32_t b; } pair_t;

/* Helper: set up a 1D int32 descriptor */
static void setup_1d_i32(CFI_cdesc_t *desc, int32_t *data, CFI_index_t n,
                          CFI_attribute_t attr) {
    CFI_index_t extents[1] = { n };
    CFI_establish(desc, data, attr, CFI_type_int32_t,
                  sizeof(int32_t), 1, extents);
}

/* Helper: set up a 2D int32 descriptor */
static void setup_2d_i32(CFI_cdesc_t *desc, int32_t *data,
                          CFI_index_t n1, CFI_index_t n2) {
    CFI_index_t extents[2] = { n1, n2 };
    CFI_establish(desc, data, CFI_attribute_other, CFI_type_int32_t,
                  sizeof(int32_t), 2, extents);
}

int c25_run_all_tests(void) {
    /* ---- Test 1: C calls Fortran subroutine with assumed-shape 1D ---- */
    {
        int32_t data[4] = {1, 2, 3, 4};
        CFI_CDESC_T(1) desc;
        setup_1d_i32((CFI_cdesc_t *)&desc, data, 4, CFI_attribute_other);

        f25_double_1d((CFI_cdesc_t *)&desc);
        if (data[0] != 2 || data[1] != 4 || data[2] != 6 || data[3] != 8)
            return 1;
    }

    /* ---- Test 2: C calls Fortran function with assumed-shape 1D ---- */
    {
        int32_t data[5] = {10, 20, 30, 40, 50};
        CFI_CDESC_T(1) desc;
        setup_1d_i32((CFI_cdesc_t *)&desc, data, 5, CFI_attribute_other);

        if (f25_sum_1d((CFI_cdesc_t *)&desc) != 150) return 2;
    }

    /* ---- Test 3: C calls Fortran with assumed-shape 2D ---- */
    {
        int32_t data[6] = {1, 2, 3, 4, 5, 6};
        CFI_CDESC_T(2) desc;
        setup_2d_i32((CFI_cdesc_t *)&desc, data, 2, 3);

        if (f25_sum_2d((CFI_cdesc_t *)&desc) != 21) return 3;
    }

    /* ---- Test 4: C calls Fortran with assumed-rank (1D and 2D) ---- */
    {
        int32_t data1[3] = {1, 2, 3};
        CFI_CDESC_T(1) desc1;
        setup_1d_i32((CFI_cdesc_t *)&desc1, data1, 3, CFI_attribute_other);

        if (f25_get_rank((CFI_cdesc_t *)&desc1) != 1) return 4;

        int32_t data2[6] = {1, 2, 3, 4, 5, 6};
        CFI_CDESC_T(2) desc2;
        setup_2d_i32((CFI_cdesc_t *)&desc2, data2, 2, 3);

        if (f25_get_rank((CFI_cdesc_t *)&desc2) != 2) return 5;
    }

    /* ---- Test 5: C calls Fortran with optional (present) ---- */
    {
        int32_t data[2] = {1, 2};
        CFI_CDESC_T(1) desc;
        setup_1d_i32((CFI_cdesc_t *)&desc, data, 2, CFI_attribute_other);

        if (f25_opt_present((CFI_cdesc_t *)&desc) != 1) return 5;
    }

    /* ---- Test 6: C calls Fortran with optional (absent = NULL) ---- */
    {
        if (f25_opt_present(NULL) != 0) return 6;
    }

    /* ---- Test 7: C calls Fortran with CONTIGUOUS ---- */
    {
        int32_t data[4] = {1, 2, 3, 4};
        CFI_CDESC_T(1) desc;
        setup_1d_i32((CFI_cdesc_t *)&desc, data, 4, CFI_attribute_other);

        if (f25_sum_contig((CFI_cdesc_t *)&desc) != 10) return 7;
    }

    /* ---- Test 8: C calls Fortran with intent(out) ---- */
    {
        int32_t data[3] = {0, 0, 0};
        CFI_CDESC_T(1) desc;
        setup_1d_i32((CFI_cdesc_t *)&desc, data, 3, CFI_attribute_other);

        f25_fill_array((CFI_cdesc_t *)&desc);
        if (data[0] != 10 || data[1] != 20 || data[2] != 30) return 8;
    }

    /* ---- Test 9: C calls Fortran with multiple descriptors (dot) ---- */
    {
        int32_t a[3] = {1, 2, 3};
        int32_t b[3] = {4, 5, 6};
        CFI_CDESC_T(1) da, db;
        setup_1d_i32((CFI_cdesc_t *)&da, a, 3, CFI_attribute_other);
        setup_1d_i32((CFI_cdesc_t *)&db, b, 3, CFI_attribute_other);

        /* dot = 1*4 + 2*5 + 3*6 = 32 */
        if (f25_dot((CFI_cdesc_t *)&da, (CFI_cdesc_t *)&db) != 32) return 9;
    }

    /* ---- Test 10: C calls Fortran with 3 descriptors (add) ---- */
    {
        int32_t a[3] = {10, 20, 30};
        int32_t b[3] = {1, 2, 3};
        int32_t c[3] = {0, 0, 0};
        CFI_CDESC_T(1) da, db, dc;
        setup_1d_i32((CFI_cdesc_t *)&da, a, 3, CFI_attribute_other);
        setup_1d_i32((CFI_cdesc_t *)&db, b, 3, CFI_attribute_other);
        setup_1d_i32((CFI_cdesc_t *)&dc, c, 3, CFI_attribute_other);

        f25_add((CFI_cdesc_t *)&da, (CFI_cdesc_t *)&db,
                (CFI_cdesc_t *)&dc);
        if (c[0] != 11 || c[1] != 22 || c[2] != 33) return 10;
    }

    /* ---- Test 11: C calls Fortran with derived type array ---- */
    {
        pair_t pts[3] = {{1, 10}, {2, 20}, {3, 30}};
        CFI_CDESC_T(1) desc;
        CFI_index_t ext[1] = {3};
        CFI_establish((CFI_cdesc_t *)&desc, pts, CFI_attribute_other,
                      CFI_type_struct, sizeof(pair_t), 1, ext);

        /* sum = (1+10) + (2+20) + (3+30) = 66 */
        if (f25_sum_pairs((CFI_cdesc_t *)&desc) != 66) return 11;
    }

    /* ---- Test 12: C calls Fortran with complex array ---- */
    {
        float _Complex cdata[3];
        cdata[0] = 1.0f + 2.0f * I;
        cdata[1] = 3.0f + 4.0f * I;
        cdata[2] = 5.0f + 6.0f * I;
        CFI_CDESC_T(1) desc;
        CFI_index_t ext[1] = {3};
        CFI_establish((CFI_cdesc_t *)&desc, cdata, CFI_attribute_other,
                      CFI_type_float_Complex, sizeof(float _Complex), 1, ext);

        float re = 0, im = 0;
        f25_sum_complex((CFI_cdesc_t *)&desc, &re, &im);
        if (re < 8.9f || re > 9.1f) return 12;
        if (im < 11.9f || im > 12.1f) return 13;
    }

    /* ---- Test 13: C calls Fortran with logical array ---- */
    {
        unsigned char bdata[5] = {1, 0, 1, 1, 0};
        CFI_CDESC_T(1) desc;
        CFI_index_t ext[1] = {5};
        CFI_establish((CFI_cdesc_t *)&desc, bdata, CFI_attribute_other,
                      CFI_type_Bool, sizeof(unsigned char), 1, ext);

        if (f25_count_true((CFI_cdesc_t *)&desc) != 3) return 14;
    }

    /* ---- Test 14: C calls Fortran function returning scalar ---- */
    {
        if (f25_square(7) != 49) return 15;
        if (f25_square(-5) != 25) return 16;
    }

    /* ---- Test 15: C calls Fortran function returning complex ---- */
    {
        float _Complex z = 3.0f + 4.0f * I;
        float _Complex result = f25_conj(z);
        float re = crealf(result);
        float im = cimagf(result);
        if (re < 2.9f || re > 3.1f) return 17;
        if (im < -4.1f || im > -3.9f) return 18;
    }

    return 0;
}

/* ============================================================
 * From bindc_29c.c
 * (sum_i32 already defined above as shared helper)
 * ============================================================ */

/* ---- C global variables bound to Fortran module variables ---- */
int32_t c29_global_arr[4] = {10, 20, 30, 40};

typedef struct { double x; double y; double z; } vec3_t;
vec3_t c29_global_vec = {1.0, 2.0, 3.0};

_Bool c29_global_flag = 1;
int32_t c29_global_counter = 42;

/* ---- check globals are initialized ---- */
int c29_check_globals(void) {
    if (c29_global_arr[0] != 10) return 1;
    if (c29_global_arr[1] != 20) return 2;
    if (c29_global_arr[2] != 30) return 3;
    if (c29_global_arr[3] != 40) return 4;
    if (c29_global_vec.x != 1.0) return 5;
    if (c29_global_vec.y != 2.0) return 6;
    if (c29_global_vec.z != 3.0) return 7;
    if (!c29_global_flag) return 8;
    if (c29_global_counter != 42) return 9;
    return 0;
}

/* ---- verify Fortran writes ---- */
int c29_verify_writes(void) {
    if (c29_global_arr[0] != 100) return 1;
    if (c29_global_arr[1] != 200) return 2;
    if (c29_global_arr[2] != 300) return 3;
    if (c29_global_arr[3] != 400) return 4;
    if (c29_global_counter != 99) return 5;
    return 0;
}

/* ---- descriptor type code queries ----
 * Return 1 if the descriptor's type matches the expected CFI_type_* macro,
 * 0 otherwise. The numeric values of type codes are implementation-defined
 * (GFortran uses a bitfield encoding, Flang uses sequential integers),
 * so we must compare against the macros on the C side. */
int c29_check_type_int32(CFI_cdesc_t *a) {
    return (a->type == CFI_type_int32_t) ? 1 : 0;
}

int c29_check_type_double(CFI_cdesc_t *a) {
    return (a->type == CFI_type_double) ? 1 : 0;
}

int c29_check_type_float(CFI_cdesc_t *a) {
    return (a->type == CFI_type_float) ? 1 : 0;
}

/* ---- same array to multiple descriptors ---- */
int32_t c29_self_dot(CFI_cdesc_t *a, CFI_cdesc_t *b) {
    int32_t total = 0;
    char *ba = (char *)a->base_addr;
    char *bb = (char *)b->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        int32_t va = *(int32_t *)(ba + i * a->dim[0].sm);
        int32_t vb = *(int32_t *)(bb + i * b->dim[0].sm);
        total += va * vb;
    }
    return total;
}

/* ============================================================
 * From bindc_34c.c
 * ============================================================ */

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

/* ============================================================
 * From bindc_40c.c
 * ============================================================ */

/* ---- CFI_type_cptr ---- */
int32_t c40_check_cptr_type(CFI_cdesc_t *arr) {
    return (arr->type == CFI_type_cptr) ? 1 : 0;
}

/* ---- CFI_type_cfunptr ---- */
int32_t c40_check_cfunptr_type(CFI_cdesc_t *arr) {
    return (arr->type == CFI_type_cfunptr) ? 1 : 0;
}

/* ---- Deferred-length allocatable character ---- */
int32_t c40_check_deferred_char(CFI_cdesc_t *s) {
    if (s->attribute != CFI_attribute_allocatable) return -1;
    return (int32_t)s->elem_len;
}

/* ---- Explicit-shape 2D: passed as raw pointer ---- */
int32_t c40_sum_explicit_2d(const int32_t *arr, int32_t r, int32_t c) {
    int32_t sum = 0;
    for (int i = 0; i < r * c; i++) sum += arr[i];
    return sum;
}

/* ---- Explicit-shape 3D: passed as raw pointer ---- */
int32_t c40_sum_explicit_3d(const int32_t *arr, int32_t d1, int32_t d2, int32_t d3) {
    int32_t sum = 0;
    for (int i = 0; i < d1 * d2 * d3; i++) sum += arr[i];
    return sum;
}

/* ---- Internal BIND(C) proc: C calls back via function pointer ---- */
int32_t c40_call_internal(int32_t (*fp)(int32_t), int32_t x) {
    return fp(x);
}

/* ============================================================
 * From bindc_42c.c
 * ============================================================ */

/* ---- Negative-stride CFI_section from C side ---- */
int32_t c42_test_negative_stride_section(void) {
    /* Create a 1D array [10, 20, 30, 40, 50] */
    int32_t data[5] = {10, 20, 30, 40, 50};
    CFI_CDESC_T(1) src_s;
    CFI_cdesc_t *src = (CFI_cdesc_t *)&src_s;
    CFI_index_t ext[1] = {5};
    int rc;

    rc = CFI_establish(src, data, CFI_attribute_other,
                       CFI_type_int32_t, sizeof(int32_t), 1, ext);
    if (rc != CFI_SUCCESS) return 0;

    /* Create section with negative stride: reverse the array */
    CFI_CDESC_T(1) res_s;
    CFI_cdesc_t *res = (CFI_cdesc_t *)&res_s;
    rc = CFI_establish(res, NULL, CFI_attribute_other,
                       CFI_type_int32_t, sizeof(int32_t), 1, NULL);
    if (rc != CFI_SUCCESS) return 0;

    CFI_index_t lb[1] = {4};   /* start from last element */
    CFI_index_t ub[1] = {0};   /* end at first */
    CFI_index_t st[1] = {-1};  /* negative stride */

    rc = CFI_section(res, src, lb, ub, st);
    if (rc != CFI_SUCCESS) return 0;

    /* Verify reversed: [50, 40, 30, 20, 10] */
    if (res->dim[0].extent != 5) return 0;

    for (CFI_index_t i = 0; i < 5; i++) {
        CFI_index_t sub[1] = { res->dim[0].lower_bound + i };
        int32_t *val = (int32_t *)CFI_address(res, sub);
        int32_t expected = (5 - (int)i) * 10;
        if (*val != expected) return 0;
    }

    return 1;
}

/* ---- SAVE+BIND(C) variable ---- */
extern int32_t g42_save_var;

void c42_set_save_var(int32_t val) {
    g42_save_var = val;
}

int32_t c42_get_save_var(void) {
    return g42_save_var;
}

/* ---- PROTECTED+BIND(C): C can still read (link-level access) ---- */
extern int32_t g42_protected;

int32_t c42_get_protected(void) {
    return g42_protected;
}

/* ---- Optional allocatable scalar ---- */
int32_t c42_opt_alloc_scalar(CFI_cdesc_t *x) {
    if (x == NULL) return -1;
    if (x->base_addr == NULL) return -1;
    return *(int32_t *)x->base_addr;
}

/* ---- Optional pointer scalar ---- */
int32_t c42_opt_ptr_scalar(CFI_cdesc_t *x) {
    if (x == NULL) return -1;
    if (x->base_addr == NULL) return -1;
    return *(int32_t *)x->base_addr;
}

/* ============================================================
 * From bindc_44c.c
 * ============================================================ */

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
