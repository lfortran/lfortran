/*
 * C driver for bindc_31: CFI_allocate / CFI_deallocate tests
 *
 * Tests C creating allocatable arrays and passing them to Fortran.
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>

/* Fortran procedures from bindc_31_mod */
extern int f31_is_allocated(CFI_cdesc_t *a);
extern int32_t f31_sum_1d(CFI_cdesc_t *a);
extern int f31_size_1d(CFI_cdesc_t *a);
extern int f31_lbound_1d(CFI_cdesc_t *a);
extern int f31_ubound_1d(CFI_cdesc_t *a);
extern int32_t f31_get_elem(CFI_cdesc_t *a, int idx);
extern int32_t f31_sum_2d(CFI_cdesc_t *a);
extern int f31_2d_shape_ok(CFI_cdesc_t *a, int n1, int n2);
extern double f31_sum_double(CFI_cdesc_t *a);
extern void f31_alloc_fill(CFI_cdesc_t *a);
extern void f31_realloc(CFI_cdesc_t *a);

int c31_run_tests(void) {
    int rc;

    /* ---- Test 1: Unallocated 1D descriptor ---- */
    {
        CFI_CDESC_T(1) raw;
        CFI_cdesc_t *desc = (CFI_cdesc_t *)&raw;
        rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                           CFI_type_int32_t, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 1;
        if (f31_is_allocated(desc) != 0) return 2;
    }

    /* ---- Test 2: Allocate [1:5], fill 10..50, verify ---- */
    {
        CFI_CDESC_T(1) raw;
        CFI_cdesc_t *desc = (CFI_cdesc_t *)&raw;
        CFI_index_t lb[] = {1}, ub[] = {5};
        int32_t *d;
        int i;

        rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                           CFI_type_int32_t, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 3;
        rc = CFI_allocate(desc, lb, ub, 0);
        if (rc != CFI_SUCCESS) return 4;

        d = (int32_t *)desc->base_addr;
        for (i = 0; i < 5; i++) d[i] = (i + 1) * 10;

        if (f31_is_allocated(desc) != 1) return 5;
        if (f31_sum_1d(desc) != 150)     return 6;
        if (f31_lbound_1d(desc) != 1)    return 7;
        if (f31_ubound_1d(desc) != 5)    return 8;

        rc = CFI_deallocate(desc);
        if (rc != CFI_SUCCESS) return 9;
    }

    /* ---- Test 3: Non-default bounds [-2:2] ---- */
    {
        CFI_CDESC_T(1) raw;
        CFI_cdesc_t *desc = (CFI_cdesc_t *)&raw;
        CFI_index_t lb[] = {-2}, ub[] = {2};
        int32_t *d;
        int i;

        rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                           CFI_type_int32_t, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 10;
        rc = CFI_allocate(desc, lb, ub, 0);
        if (rc != CFI_SUCCESS) return 11;

        d = (int32_t *)desc->base_addr;
        for (i = 0; i < 5; i++) d[i] = i + 1;  /* 1,2,3,4,5 */

        if (f31_sum_1d(desc) != 15)       return 12;
        if (f31_lbound_1d(desc) != -2)    return 13;
        if (f31_ubound_1d(desc) != 2)     return 14;
        if (f31_size_1d(desc) != 5)       return 15;
        /* a(-2)==1, a(0)==3, a(2)==5 */
        if (f31_get_elem(desc, -2) != 1)  return 16;
        if (f31_get_elem(desc,  0) != 3)  return 17;
        if (f31_get_elem(desc,  2) != 5)  return 18;

        rc = CFI_deallocate(desc);
        if (rc != CFI_SUCCESS) return 19;
    }

    /* ---- Test 4: 2D allocatable [1:3, 1:4] ---- */
    {
        CFI_CDESC_T(2) raw;
        CFI_cdesc_t *desc = (CFI_cdesc_t *)&raw;
        CFI_index_t lb[] = {1, 1}, ub[] = {3, 4};
        int32_t *d;
        int i;

        rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                           CFI_type_int32_t, 0, 2, NULL);
        if (rc != CFI_SUCCESS) return 20;
        rc = CFI_allocate(desc, lb, ub, 0);
        if (rc != CFI_SUCCESS) return 21;

        /* Column-major: 3 rows x 4 cols, sum(1..12) = 78 */
        d = (int32_t *)desc->base_addr;
        for (i = 0; i < 12; i++) d[i] = i + 1;

        if (f31_sum_2d(desc) != 78)            return 22;
        if (f31_2d_shape_ok(desc, 3, 4) != 1)  return 23;

        rc = CFI_deallocate(desc);
        if (rc != CFI_SUCCESS) return 24;
    }

    /* ---- Test 5: Real(c_double) allocatable ---- */
    {
        CFI_CDESC_T(1) raw;
        CFI_cdesc_t *desc = (CFI_cdesc_t *)&raw;
        CFI_index_t lb[] = {1}, ub[] = {3};
        double *d, s;

        rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                           CFI_type_double, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 25;
        rc = CFI_allocate(desc, lb, ub, 0);
        if (rc != CFI_SUCCESS) return 26;

        d = (double *)desc->base_addr;
        d[0] = 1.5; d[1] = 2.5; d[2] = 3.5;

        s = f31_sum_double(desc);
        if (s < 7.4 || s > 7.6) return 27;

        rc = CFI_deallocate(desc);
        if (rc != CFI_SUCCESS) return 28;
    }

    /* ---- Test 6: Intent(out) - Fortran allocates into C descriptor ---- */
    {
        CFI_CDESC_T(1) raw;
        CFI_cdesc_t *desc = (CFI_cdesc_t *)&raw;
        int32_t *d;

        rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                           CFI_type_int32_t, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 29;

        f31_alloc_fill(desc);  /* Fortran: allocate(a(3)); a=[10,20,30] */

        if (desc->base_addr == NULL) return 30;
        d = (int32_t *)desc->base_addr;
        if (d[0] != 10 || d[1] != 20 || d[2] != 30) return 31;
        if (f31_size_1d(desc) != 3) return 32;

        rc = CFI_deallocate(desc);
        if (rc != CFI_SUCCESS) return 33;
    }

    /* ---- Test 7: Intent(inout) - Fortran reallocates ---- */
    {
        CFI_CDESC_T(1) raw;
        CFI_cdesc_t *desc = (CFI_cdesc_t *)&raw;
        CFI_index_t lb[] = {1}, ub[] = {3};
        int32_t *d;

        rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                           CFI_type_int32_t, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 34;
        rc = CFI_allocate(desc, lb, ub, 0);
        if (rc != CFI_SUCCESS) return 35;

        d = (int32_t *)desc->base_addr;
        d[0] = 1; d[1] = 2; d[2] = 3;

        /* Fortran deallocates [1,2,3], then allocates [100..500] */
        f31_realloc(desc);

        if (f31_size_1d(desc) != 5)   return 36;
        if (f31_sum_1d(desc) != 1500) return 37;

        rc = CFI_deallocate(desc);
        if (rc != CFI_SUCCESS) return 38;
    }

    /* ---- Test 8: Zero-size allocation [1:0] ---- */
    {
        CFI_CDESC_T(1) raw;
        CFI_cdesc_t *desc = (CFI_cdesc_t *)&raw;
        CFI_index_t lb[] = {1}, ub[] = {0};

        rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                           CFI_type_int32_t, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 39;
        rc = CFI_allocate(desc, lb, ub, 0);
        if (rc != CFI_SUCCESS) return 40;

        if (f31_is_allocated(desc) != 1) return 41;
        if (f31_size_1d(desc) != 0)      return 42;

        rc = CFI_deallocate(desc);
        if (rc != CFI_SUCCESS) return 43;
    }

    return 0;
}
