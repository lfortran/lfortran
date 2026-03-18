/*
 * C driver for bindc_32: CFI_setpointer tests
 *
 * Tests C creating pointer associations and passing them to Fortran.
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>

/* Fortran procedures from bindc_32_mod */
extern int f32_is_associated(CFI_cdesc_t *a);
extern int32_t f32_sum_1d(CFI_cdesc_t *a);
extern int f32_lbound_1d(CFI_cdesc_t *a);
extern int f32_ubound_1d(CFI_cdesc_t *a);
extern int32_t f32_sum_2d(CFI_cdesc_t *a);
extern void f32_double_values(CFI_cdesc_t *a);

int c32_run_tests(void) {
    int rc;

    /* ---- Test 1: Disassociated pointer ---- */
    {
        CFI_CDESC_T(1) raw;
        CFI_cdesc_t *ptr = (CFI_cdesc_t *)&raw;

        rc = CFI_establish(ptr, NULL, CFI_attribute_pointer,
                           CFI_type_int32_t, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 1;
        if (f32_is_associated(ptr) != 0) return 2;
    }

    /* ---- Test 2: Associate pointer with data ---- */
    {
        int32_t data[5] = {10, 20, 30, 40, 50};
        CFI_CDESC_T(1) src_raw, ptr_raw;
        CFI_cdesc_t *src = (CFI_cdesc_t *)&src_raw;
        CFI_cdesc_t *ptr = (CFI_cdesc_t *)&ptr_raw;
        CFI_index_t ext[] = {5};

        rc = CFI_establish(src, data, CFI_attribute_other,
                           CFI_type_int32_t, 0, 1, ext);
        if (rc != CFI_SUCCESS) return 3;
        rc = CFI_establish(ptr, NULL, CFI_attribute_pointer,
                           CFI_type_int32_t, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 4;

        rc = CFI_setpointer(ptr, src, NULL);
        if (rc != CFI_SUCCESS) return 5;

        if (f32_is_associated(ptr) != 1) return 6;
        if (f32_sum_1d(ptr) != 150)      return 7;
    }

    /* ---- Test 3: Disassociate via CFI_setpointer(ptr, NULL, NULL) ---- */
    {
        int32_t data[3] = {1, 2, 3};
        CFI_CDESC_T(1) src_raw, ptr_raw;
        CFI_cdesc_t *src = (CFI_cdesc_t *)&src_raw;
        CFI_cdesc_t *ptr = (CFI_cdesc_t *)&ptr_raw;
        CFI_index_t ext[] = {3};

        rc = CFI_establish(src, data, CFI_attribute_other,
                           CFI_type_int32_t, 0, 1, ext);
        if (rc != CFI_SUCCESS) return 8;
        rc = CFI_establish(ptr, NULL, CFI_attribute_pointer,
                           CFI_type_int32_t, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 9;

        rc = CFI_setpointer(ptr, src, NULL);
        if (rc != CFI_SUCCESS) return 10;
        if (f32_is_associated(ptr) != 1) return 11;

        rc = CFI_setpointer(ptr, NULL, NULL);
        if (rc != CFI_SUCCESS) return 12;
        if (f32_is_associated(ptr) != 0) return 13;
    }

    /* ---- Test 4: Custom lower bounds [-1:2] ---- */
    {
        int32_t data[4] = {100, 200, 300, 400};
        CFI_CDESC_T(1) src_raw, ptr_raw;
        CFI_cdesc_t *src = (CFI_cdesc_t *)&src_raw;
        CFI_cdesc_t *ptr = (CFI_cdesc_t *)&ptr_raw;
        CFI_index_t ext[] = {4};
        CFI_index_t lbounds[] = {-1};

        rc = CFI_establish(src, data, CFI_attribute_other,
                           CFI_type_int32_t, 0, 1, ext);
        if (rc != CFI_SUCCESS) return 14;
        rc = CFI_establish(ptr, NULL, CFI_attribute_pointer,
                           CFI_type_int32_t, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 15;

        rc = CFI_setpointer(ptr, src, lbounds);
        if (rc != CFI_SUCCESS) return 16;

        if (f32_lbound_1d(ptr) != -1)   return 17;
        if (f32_ubound_1d(ptr) !=  2)   return 18;
        if (f32_sum_1d(ptr)    != 1000) return 19;
    }

    /* ---- Test 5: Re-point to different target ---- */
    {
        int32_t d1[3] = {1, 2, 3};
        int32_t d2[3] = {10, 20, 30};
        CFI_CDESC_T(1) s1_raw, s2_raw, ptr_raw;
        CFI_cdesc_t *s1  = (CFI_cdesc_t *)&s1_raw;
        CFI_cdesc_t *s2  = (CFI_cdesc_t *)&s2_raw;
        CFI_cdesc_t *ptr = (CFI_cdesc_t *)&ptr_raw;
        CFI_index_t ext[] = {3};

        rc = CFI_establish(s1, d1, CFI_attribute_other,
                           CFI_type_int32_t, 0, 1, ext);
        if (rc != CFI_SUCCESS) return 20;
        rc = CFI_establish(s2, d2, CFI_attribute_other,
                           CFI_type_int32_t, 0, 1, ext);
        if (rc != CFI_SUCCESS) return 21;
        rc = CFI_establish(ptr, NULL, CFI_attribute_pointer,
                           CFI_type_int32_t, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 22;

        rc = CFI_setpointer(ptr, s1, NULL);
        if (rc != CFI_SUCCESS) return 23;
        if (f32_sum_1d(ptr) != 6) return 24;

        rc = CFI_setpointer(ptr, s2, NULL);
        if (rc != CFI_SUCCESS) return 25;
        if (f32_sum_1d(ptr) != 60) return 26;
    }

    /* ---- Test 6: 2D pointer [2x3] ---- */
    {
        int32_t data[6] = {1, 2, 3, 4, 5, 6};
        CFI_CDESC_T(2) src_raw, ptr_raw;
        CFI_cdesc_t *src = (CFI_cdesc_t *)&src_raw;
        CFI_cdesc_t *ptr = (CFI_cdesc_t *)&ptr_raw;
        CFI_index_t ext[] = {2, 3};

        rc = CFI_establish(src, data, CFI_attribute_other,
                           CFI_type_int32_t, 0, 2, ext);
        if (rc != CFI_SUCCESS) return 27;
        rc = CFI_establish(ptr, NULL, CFI_attribute_pointer,
                           CFI_type_int32_t, 0, 2, NULL);
        if (rc != CFI_SUCCESS) return 28;

        rc = CFI_setpointer(ptr, src, NULL);
        if (rc != CFI_SUCCESS) return 29;

        if (f32_sum_2d(ptr) != 21) return 30;
    }

    /* ---- Test 7: Modify target through pointer ---- */
    {
        int32_t data[4] = {1, 2, 3, 4};
        CFI_CDESC_T(1) src_raw, ptr_raw;
        CFI_cdesc_t *src = (CFI_cdesc_t *)&src_raw;
        CFI_cdesc_t *ptr = (CFI_cdesc_t *)&ptr_raw;
        CFI_index_t ext[] = {4};

        rc = CFI_establish(src, data, CFI_attribute_other,
                           CFI_type_int32_t, 0, 1, ext);
        if (rc != CFI_SUCCESS) return 31;
        rc = CFI_establish(ptr, NULL, CFI_attribute_pointer,
                           CFI_type_int32_t, 0, 1, NULL);
        if (rc != CFI_SUCCESS) return 32;

        rc = CFI_setpointer(ptr, src, NULL);
        if (rc != CFI_SUCCESS) return 33;

        f32_double_values(ptr);  /* Fortran: a = a * 2 */

        /* Original data should be doubled */
        if (data[0] != 2 || data[1] != 4 ||
            data[2] != 6 || data[3] != 8) return 34;
    }

    return 0;
}
