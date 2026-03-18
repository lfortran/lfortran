#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* ================================================================
 * CFI_establish error conditions
 * ================================================================ */
int32_t c41_test_establish_errors(void) {
    CFI_CDESC_T(1) storage;
    CFI_cdesc_t *desc = (CFI_cdesc_t *)&storage;
    int rc;

    /* 1. Invalid type code (must fit in CFI_type_t range) */
    rc = CFI_establish(desc, NULL, CFI_attribute_other,
                       (CFI_type_t)100, sizeof(int32_t), 1, NULL);
    if (rc == CFI_SUCCESS) return 0;  /* should fail */

    /* 2. Invalid rank: > CFI_MAX_RANK */
    rc = CFI_establish(desc, NULL, CFI_attribute_other,
                       CFI_type_int32_t, 0, CFI_MAX_RANK + 1, NULL);
    if (rc != CFI_INVALID_RANK) return 0;

    /* 3. Invalid attribute code */
    rc = CFI_establish(desc, NULL, 77,
                       CFI_type_int32_t, 0, 1, NULL);
    if (rc == CFI_SUCCESS) return 0;  /* must fail for invalid attribute */

    /* 4. Valid call succeeds */
    CFI_index_t ext[1] = {5};
    rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                       CFI_type_int32_t, 0, 1, ext);
    if (rc != CFI_SUCCESS) return 0;

    /* 5. Negative rank */
    rc = CFI_establish(desc, NULL, CFI_attribute_other,
                       CFI_type_int32_t, 0, -1, NULL);
    if (rc == CFI_SUCCESS) return 0;  /* should fail */

    return 1;
}

/* ================================================================
 * CFI_allocate error conditions
 * ================================================================ */
int32_t c41_test_allocate_errors(void) {
    CFI_CDESC_T(1) storage;
    CFI_cdesc_t *desc = (CFI_cdesc_t *)&storage;
    int rc;

    /* Set up an allocatable descriptor */
    CFI_index_t ext[1] = {5};
    rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                       CFI_type_int32_t, 0, 1, ext);
    if (rc != CFI_SUCCESS) return 0;

    /* 1. Successful allocate */
    CFI_index_t lb[1] = {1};
    CFI_index_t ub[1] = {5};
    rc = CFI_allocate(desc, lb, ub, 0);
    if (rc != CFI_SUCCESS) return 0;

    /* 2. Double allocate: base_addr is non-NULL */
    rc = CFI_allocate(desc, lb, ub, 0);
    if (rc != CFI_ERROR_BASE_ADDR_NOT_NULL) return 0;

    /* Clean up */
    rc = CFI_deallocate(desc);
    if (rc != CFI_SUCCESS) return 0;

    /* 3. Set up a non-allocatable descriptor and try to allocate */
    int32_t data[5] = {1, 2, 3, 4, 5};
    rc = CFI_establish(desc, data, CFI_attribute_other,
                       CFI_type_int32_t, sizeof(int32_t), 1, ext);
    if (rc != CFI_SUCCESS) return 0;

    rc = CFI_allocate(desc, lb, ub, 0);
    if (rc == CFI_SUCCESS) return 0;  /* must fail */

    return 1;
}

/* ================================================================
 * CFI_deallocate error conditions
 * ================================================================ */
int32_t c41_test_deallocate_errors(void) {
    CFI_CDESC_T(1) storage;
    CFI_cdesc_t *desc = (CFI_cdesc_t *)&storage;
    int rc;

    /* 1. Deallocate unallocated (NULL base_addr) */
    CFI_index_t ext[1] = {5};
    rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                       CFI_type_int32_t, 0, 1, ext);
    if (rc != CFI_SUCCESS) return 0;

    rc = CFI_deallocate(desc);
    if (rc != CFI_ERROR_BASE_ADDR_NULL) return 0;

    /* 2. Deallocate non-allocatable descriptor */
    int32_t data[5] = {0};
    rc = CFI_establish(desc, data, CFI_attribute_other,
                       CFI_type_int32_t, sizeof(int32_t), 1, ext);
    if (rc != CFI_SUCCESS) return 0;

    rc = CFI_deallocate(desc);
    if (rc == CFI_SUCCESS) return 0;  /* must fail */

    return 1;
}

/* ================================================================
 * CFI_section error conditions
 * ================================================================ */
int32_t c41_test_section_errors(void) {
    /* Set up a valid 1D source array [1,2,3,4,5] */
    CFI_CDESC_T(1) src_s;
    CFI_cdesc_t *src = (CFI_cdesc_t *)&src_s;
    int32_t data[5] = {1, 2, 3, 4, 5};
    CFI_index_t ext[1] = {5};
    int rc;

    rc = CFI_establish(src, data, CFI_attribute_other,
                       CFI_type_int32_t, sizeof(int32_t), 1, ext);
    if (rc != CFI_SUCCESS) return 0;

    /* Valid section: elements 1..3 with stride 1 */
    CFI_CDESC_T(1) res_s;
    CFI_cdesc_t *res = (CFI_cdesc_t *)&res_s;
    rc = CFI_establish(res, NULL, CFI_attribute_other,
                       CFI_type_int32_t, sizeof(int32_t), 1, NULL);
    if (rc != CFI_SUCCESS) return 0;

    CFI_index_t lb[1] = {0};
    CFI_index_t ub[1] = {2};
    CFI_index_t st[1] = {1};
    rc = CFI_section(res, src, lb, ub, st);
    if (rc != CFI_SUCCESS) return 0;

    /* Verify the section contents */
    int32_t *rp = (int32_t *)res->base_addr;
    if (res->dim[0].extent != 3) return 0;

    return 1;
}

/* ================================================================
 * CFI_setpointer error conditions
 * ================================================================ */
int32_t c41_test_setpointer_errors(void) {
    int rc;

    /* Create a pointer descriptor (rank 1) */
    CFI_CDESC_T(1) ptr_s;
    CFI_cdesc_t *ptr = (CFI_cdesc_t *)&ptr_s;
    rc = CFI_establish(ptr, NULL, CFI_attribute_pointer,
                       CFI_type_int32_t, 0, 1, NULL);
    if (rc != CFI_SUCCESS) return 0;

    /* Create a source (rank 1) */
    int32_t data[5] = {10, 20, 30, 40, 50};
    CFI_CDESC_T(1) src_s;
    CFI_cdesc_t *src = (CFI_cdesc_t *)&src_s;
    CFI_index_t ext1[1] = {5};
    rc = CFI_establish(src, data, CFI_attribute_other,
                       CFI_type_int32_t, sizeof(int32_t), 1, ext1);
    if (rc != CFI_SUCCESS) return 0;

    /* Valid setpointer */
    rc = CFI_setpointer(ptr, src, NULL);
    if (rc != CFI_SUCCESS) return 0;

    /* Verify pointer now points to data */
    int32_t *p = (int32_t *)ptr->base_addr;
    if (p[0] != 10 || p[4] != 50) return 0;

    /* Disassociate */
    rc = CFI_setpointer(ptr, NULL, NULL);
    if (rc != CFI_SUCCESS) return 0;
    if (ptr->base_addr != NULL) return 0;

    return 1;
}

/* ================================================================
 * CFI_select_part error conditions
 * ================================================================ */
int32_t c41_test_select_part_errors(void) {
    /* Create a source array of a "struct" with two int32_t fields */
    typedef struct { int32_t x; int32_t y; } pair_t;
    pair_t data[3] = {{1, 10}, {2, 20}, {3, 30}};

    CFI_CDESC_T(1) src_s;
    CFI_cdesc_t *src = (CFI_cdesc_t *)&src_s;
    CFI_index_t ext[1] = {3};
    int rc;

    rc = CFI_establish(src, data, CFI_attribute_other,
                       CFI_type_struct, sizeof(pair_t), 1, ext);
    if (rc != CFI_SUCCESS) return 0;

    /* Valid: select the 'y' component (offset = sizeof(int32_t)) */
    CFI_CDESC_T(1) part_s;
    CFI_cdesc_t *part = (CFI_cdesc_t *)&part_s;
    rc = CFI_establish(part, NULL, CFI_attribute_other,
                       CFI_type_int32_t, sizeof(int32_t), 1, NULL);
    if (rc != CFI_SUCCESS) return 0;

    rc = CFI_select_part(part, src, sizeof(int32_t), sizeof(int32_t));
    if (rc != CFI_SUCCESS) return 0;

    /* Verify we got [10, 20, 30] */
    CFI_index_t sub[1];
    for (int i = 0; i < 3; i++) {
        sub[0] = part->dim[0].lower_bound + i;
        int32_t *val = (int32_t *)CFI_address(part, sub);
        if (*val != (i + 1) * 10) return 0;
    }

    return 1;
}

/* ================================================================
 * CFI_address error conditions / valid usage
 * ================================================================ */
int32_t c41_test_address_errors(void) {
    int32_t data[6] = {10, 20, 30, 40, 50, 60};
    CFI_CDESC_T(2) storage;
    CFI_cdesc_t *desc = (CFI_cdesc_t *)&storage;
    CFI_index_t ext[2] = {3, 2};
    int rc;

    rc = CFI_establish(desc, data, CFI_attribute_other,
                       CFI_type_int32_t, sizeof(int32_t), 2, ext);
    if (rc != CFI_SUCCESS) return 0;

    /* Valid addresses: 2D array [3 x 2], column-major */
    CFI_index_t sub[2];

    /* (0,0) => data[0] = 10 */
    sub[0] = 0; sub[1] = 0;
    int32_t *p = (int32_t *)CFI_address(desc, sub);
    if (*p != 10) return 0;

    /* (2,0) => data[2] = 30 */
    sub[0] = 2; sub[1] = 0;
    p = (int32_t *)CFI_address(desc, sub);
    if (*p != 30) return 0;

    /* (0,1) => data[3] = 40 */
    sub[0] = 0; sub[1] = 1;
    p = (int32_t *)CFI_address(desc, sub);
    if (*p != 40) return 0;

    /* (2,1) => data[5] = 60 */
    sub[0] = 2; sub[1] = 1;
    p = (int32_t *)CFI_address(desc, sub);
    if (*p != 60) return 0;

    return 1;
}
