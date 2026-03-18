/*
 * C driver for bindc_25: C calling Fortran bind(C) procedures
 *
 * This C code creates CFI descriptors and calls Fortran procedures
 * that accept assumed-shape, assumed-rank, allocatable, pointer,
 * optional, contiguous, intent(out/inout), multiple descriptor args,
 * derived type, complex, and logical array arguments.
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <complex.h>
#include <stdbool.h>

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
