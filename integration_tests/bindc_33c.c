/*
 * C helpers for bindc_33: CFI library function tests
 *
 * Tests CFI_address, CFI_is_contiguous, CFI_section, CFI_select_part.
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>

/* ---- CFI_address: 1D ---- */
int c33_test_address_1d(CFI_cdesc_t *a) {
    /* a = [10, 20, 30, 40, 50] (1-based) */
    CFI_index_t sub[1];
    int32_t *addr;

    /* a(1) = 10 */
    sub[0] = a->dim[0].lower_bound;
    addr = (int32_t *)CFI_address(a, sub);
    if (*addr != 10) return 1;

    /* a(3) = 30 */
    sub[0] = a->dim[0].lower_bound + 2;
    addr = (int32_t *)CFI_address(a, sub);
    if (*addr != 30) return 2;

    /* a(5) = 50 */
    sub[0] = a->dim[0].lower_bound + 4;
    addr = (int32_t *)CFI_address(a, sub);
    if (*addr != 50) return 3;

    return 0;
}

/* ---- CFI_address: 2D ---- */
int c33_test_address_2d(CFI_cdesc_t *a) {
    /* a(i,j) = i + (j-1)*3, shape [3,4], column-major */
    CFI_index_t sub[2];
    int32_t *addr;

    /* a(1,1) = 1 */
    sub[0] = a->dim[0].lower_bound;
    sub[1] = a->dim[1].lower_bound;
    addr = (int32_t *)CFI_address(a, sub);
    if (*addr != 1) return 1;

    /* a(2,3) = 2 + 2*3 = 8 */
    sub[0] = a->dim[0].lower_bound + 1;
    sub[1] = a->dim[1].lower_bound + 2;
    addr = (int32_t *)CFI_address(a, sub);
    if (*addr != 8) return 2;

    /* a(3,4) = 3 + 3*3 = 12 */
    sub[0] = a->dim[0].lower_bound + 2;
    sub[1] = a->dim[1].lower_bound + 3;
    addr = (int32_t *)CFI_address(a, sub);
    if (*addr != 12) return 3;

    return 0;
}

/* ---- CFI_is_contiguous ---- */
int c33_check_contiguous(CFI_cdesc_t *a) {
    return CFI_is_contiguous(a);
}

/* ---- CFI_section ---- */
int c33_test_section(CFI_cdesc_t *a) {
    /* a = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100] */
    /* Create section a(3:7:2) → [30, 50, 70] */
    CFI_CDESC_T(1) result_raw;
    CFI_cdesc_t *result = (CFI_cdesc_t *)&result_raw;
    CFI_index_t lb[1], ub[1], str[1];
    CFI_index_t sub[1];
    int32_t *addr;
    int rc;

    rc = CFI_establish(result, NULL, CFI_attribute_other,
                       CFI_type_int32_t, 0, 1, NULL);
    if (rc != CFI_SUCCESS) return 1;

    lb[0] = a->dim[0].lower_bound + 2;  /* index 3 */
    ub[0] = a->dim[0].lower_bound + 6;  /* index 7 */
    str[0] = 2;

    rc = CFI_section(result, a, lb, ub, str);
    if (rc != CFI_SUCCESS) return 2;

    if (result->dim[0].extent != 3) return 3;

    /* Check values via CFI_address */
    sub[0] = result->dim[0].lower_bound;
    addr = (int32_t *)CFI_address(result, sub);
    if (*addr != 30) return 4;

    sub[0] = result->dim[0].lower_bound + 1;
    addr = (int32_t *)CFI_address(result, sub);
    if (*addr != 50) return 5;

    sub[0] = result->dim[0].lower_bound + 2;
    addr = (int32_t *)CFI_address(result, sub);
    if (*addr != 70) return 6;

    return 0;
}

/* ---- CFI_select_part ---- */
int c33_test_select_part(CFI_cdesc_t *a) {
    /* a = [(10,100), (20,200), (30,300)] — array of pair_t */
    /* Select 'y' component (offset = sizeof(int32_t)) */
    CFI_CDESC_T(1) result_raw;
    CFI_cdesc_t *result = (CFI_cdesc_t *)&result_raw;
    CFI_index_t sub[1];
    int32_t *addr;
    int rc;

    rc = CFI_establish(result, NULL, CFI_attribute_other,
                       CFI_type_int32_t, sizeof(int32_t), 1, NULL);
    if (rc != CFI_SUCCESS) return 1;

    rc = CFI_select_part(result, a, sizeof(int32_t), sizeof(int32_t));
    if (rc != CFI_SUCCESS) return 2;

    /* y values: [100, 200, 300] */
    sub[0] = result->dim[0].lower_bound;
    addr = (int32_t *)CFI_address(result, sub);
    if (*addr != 100) return 3;

    sub[0] = result->dim[0].lower_bound + 1;
    addr = (int32_t *)CFI_address(result, sub);
    if (*addr != 200) return 4;

    sub[0] = result->dim[0].lower_bound + 2;
    addr = (int32_t *)CFI_address(result, sub);
    if (*addr != 300) return 5;

    return 0;
}
