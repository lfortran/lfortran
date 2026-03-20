/*
 * Consolidated C helpers for bindc_iso_fb_04
 * Merged from: bindc_33c.c, bindc_36c.c, bindc_37c.c, bindc_41c.c
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* ================================================================
 * From bindc_33c.c: CFI_address, CFI_is_contiguous, CFI_section,
 *                   CFI_select_part
 * ================================================================ */

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

/* ================================================================
 * From bindc_36c.c: CFI header validation — type macros, error codes,
 *                   CFI_MAX_RANK, CFI_VERSION, type matching, errors
 * ================================================================ */

/* ---- CFI_MAX_RANK ---- */
int c36_check_max_rank(void) {
    return CFI_MAX_RANK >= 15 ? 1 : 0;
}

/* ---- CFI_VERSION ---- */
int c36_check_version(void) {
    return CFI_VERSION > 0 ? 1 : 0;
}

/* ---- All CFI_type_* macros exist (compile-time) ---- */
int c36_check_type_macros(void) {
    volatile int t;
    t = CFI_type_signed_char;
    t = CFI_type_short;
    t = CFI_type_int;
    t = CFI_type_long;
    t = CFI_type_long_long;
    t = CFI_type_size_t;
    t = CFI_type_int8_t;
    t = CFI_type_int16_t;
    t = CFI_type_int32_t;
    t = CFI_type_int64_t;
    t = CFI_type_int_least8_t;
    t = CFI_type_int_least16_t;
    t = CFI_type_int_least32_t;
    t = CFI_type_int_least64_t;
    t = CFI_type_int_fast8_t;
    t = CFI_type_int_fast16_t;
    t = CFI_type_int_fast32_t;
    t = CFI_type_int_fast64_t;
    t = CFI_type_intmax_t;
    t = CFI_type_intptr_t;
    t = CFI_type_ptrdiff_t;
    t = CFI_type_float;
    t = CFI_type_double;
    t = CFI_type_long_double;
    t = CFI_type_float_Complex;
    t = CFI_type_double_Complex;
    t = CFI_type_long_double_Complex;
    t = CFI_type_Bool;
    t = CFI_type_char;
    t = CFI_type_struct;
    t = CFI_type_other;
    (void)t;
    return 1;
}

/* ---- All error code macros exist ---- */
int c36_check_error_codes(void) {
    volatile int e;
    e = CFI_SUCCESS;
    if (e != 0) return 0;
    e = CFI_ERROR_BASE_ADDR_NULL;
    e = CFI_ERROR_BASE_ADDR_NOT_NULL;
    e = CFI_INVALID_ELEM_LEN;
    e = CFI_INVALID_RANK;
    e = CFI_INVALID_TYPE;
    e = CFI_INVALID_ATTRIBUTE;
    e = CFI_INVALID_EXTENT;
    e = CFI_INVALID_DESCRIPTOR;
    e = CFI_ERROR_MEM_ALLOCATION;
    e = CFI_ERROR_OUT_OF_BOUNDS;
    (void)e;
    return 1;
}

/* ---- Return type field from a CFI descriptor ---- */
int c36_get_cfi_type(CFI_cdesc_t *a) {
    return (int)a->type;
}

/* ---- Return expected CFI_type_* values ---- */
int c36_expected_int32(void)          { return CFI_type_int32_t; }
int c36_expected_float(void)          { return CFI_type_float; }
int c36_expected_double(void)         { return CFI_type_double; }
int c36_expected_float_complex(void)  { return CFI_type_float_Complex; }
int c36_expected_double_complex(void) { return CFI_type_double_Complex; }
int c36_expected_bool(void)           { return CFI_type_Bool; }
int c36_expected_char(void)           { return CFI_type_char; }
int c36_expected_struct(void)         { return CFI_type_struct; }

/* ---- Error handling: CFI_establish ---- */
int c36_test_error_establish(void) {
    CFI_CDESC_T(1) desc_storage;
    CFI_cdesc_t *desc = (CFI_cdesc_t *)&desc_storage;
    int rc;

    /* Invalid rank (too large) */
    rc = CFI_establish(desc, NULL, CFI_attribute_other,
                       CFI_type_int, 0, CFI_MAX_RANK + 1, NULL);
    if (rc != CFI_INVALID_RANK) return 0;

    /* Invalid attribute */
    rc = CFI_establish(desc, NULL, 99,
                       CFI_type_int, 0, 1, NULL);
    if (rc != CFI_INVALID_ATTRIBUTE) return 0;

    /* Valid call should succeed */
    CFI_index_t ext[1] = {5};
    rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                       CFI_type_int32_t, 0, 1, ext);
    if (rc != CFI_SUCCESS) return 0;

    return 1;
}

/* ---- Error handling: CFI_allocate ---- */
int c36_test_error_allocate(void) {
    CFI_CDESC_T(1) desc_storage;
    CFI_cdesc_t *desc = (CFI_cdesc_t *)&desc_storage;
    int rc;

    CFI_index_t ext[1] = {5};
    rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                       CFI_type_int32_t, 0, 1, ext);
    if (rc != CFI_SUCCESS) return 0;

    /* Allocate successfully */
    CFI_index_t lb[1] = {1};
    CFI_index_t ub[1] = {5};
    rc = CFI_allocate(desc, lb, ub, 0);
    if (rc != CFI_SUCCESS) return 0;

    /* Allocate again with non-NULL base → error */
    rc = CFI_allocate(desc, lb, ub, 0);
    if (rc != CFI_ERROR_BASE_ADDR_NOT_NULL) return 0;

    /* Clean up */
    rc = CFI_deallocate(desc);
    if (rc != CFI_SUCCESS) return 0;

    return 1;
}

/* ---- Error handling: CFI_deallocate ---- */
int c36_test_error_deallocate(void) {
    CFI_CDESC_T(1) desc_storage;
    CFI_cdesc_t *desc = (CFI_cdesc_t *)&desc_storage;
    int rc;

    CFI_index_t ext[1] = {5};
    rc = CFI_establish(desc, NULL, CFI_attribute_allocatable,
                       CFI_type_int32_t, 0, 1, ext);
    if (rc != CFI_SUCCESS) return 0;

    /* Deallocate with NULL base → error */
    rc = CFI_deallocate(desc);
    if (rc != CFI_ERROR_BASE_ADDR_NULL) return 0;

    return 1;
}

/* ================================================================
 * From bindc_37c.c: char(len=1) 2D descriptor, CFI_section 2D,
 *                   high-rank arrays, scalar descriptor from C
 * ================================================================ */

/* ---- Character(len=1) 2D assumed-shape array ---- */
int c37_check_char_2d(CFI_cdesc_t *arr) {
    /* arr is 3x2, elem_len=1, rank=2 */
    if (arr->elem_len != 1) return 0;
    if (arr->rank != 2) return 0;
    if (arr->dim[0].extent != 3) return 0;
    if (arr->dim[1].extent != 2) return 0;

    /* Check elements using CFI_address */
    CFI_index_t sub[2];
    char *ch;

    /* arr(1,1) = 'A' */
    sub[0] = arr->dim[0].lower_bound;
    sub[1] = arr->dim[1].lower_bound;
    ch = (char *)CFI_address(arr, sub);
    if (*ch != 'A') return 0;

    /* arr(3,1) = 'C' */
    sub[0] = arr->dim[0].lower_bound + 2;
    sub[1] = arr->dim[1].lower_bound;
    ch = (char *)CFI_address(arr, sub);
    if (*ch != 'C') return 0;

    /* arr(2,2) = 'E' */
    sub[0] = arr->dim[0].lower_bound + 1;
    sub[1] = arr->dim[1].lower_bound + 1;
    ch = (char *)CFI_address(arr, sub);
    if (*ch != 'E') return 0;

    return 1;
}

/* ---- CFI_section on 2D array ---- */
int c37_section_2d(CFI_cdesc_t *a) {
    /*
     * Source: 4x4 matrix, a(i,j) = i*10 + j
     * Extract section a(2:3, 1:4:2) → rows 2-3, columns 1,3
     * Result shape: (2, 2)
     */
    CFI_CDESC_T(2) result_storage;
    CFI_cdesc_t *result = (CFI_cdesc_t *)&result_storage;
    int rc;

    /* Establish result descriptor */
    rc = CFI_establish(result, NULL, CFI_attribute_other,
                       a->type, a->elem_len, 2, NULL);
    if (rc != CFI_SUCCESS) return 0;

    /* Extract section: rows 2:3, columns 1:4:2 (1-based) */
    CFI_index_t lb[2], ub[2], st[2];
    lb[0] = a->dim[0].lower_bound + 1;  /* row 2 */
    ub[0] = a->dim[0].lower_bound + 2;  /* row 3 */
    st[0] = 1;
    lb[1] = a->dim[1].lower_bound;      /* col 1 */
    ub[1] = a->dim[1].lower_bound + 3;  /* col 4 */
    st[1] = 2;                           /* stride 2 → cols 1,3 */

    rc = CFI_section(result, a, lb, ub, st);
    if (rc != CFI_SUCCESS) return 0;

    /* Verify shape */
    if (result->dim[0].extent != 2) return 0;
    if (result->dim[1].extent != 2) return 0;

    /* Verify values using CFI_address */
    CFI_index_t sub[2];

    /* result(1,1) should be a(2,1) = 2*10+1 = 21 */
    sub[0] = result->dim[0].lower_bound;
    sub[1] = result->dim[1].lower_bound;
    int32_t *addr = (int32_t *)CFI_address(result, sub);
    if (*addr != 21) return 0;

    /* result(2,1) should be a(3,1) = 3*10+1 = 31 */
    sub[0] = result->dim[0].lower_bound + 1;
    sub[1] = result->dim[1].lower_bound;
    addr = (int32_t *)CFI_address(result, sub);
    if (*addr != 31) return 0;

    /* result(1,2) should be a(2,3) = 2*10+3 = 23 */
    sub[0] = result->dim[0].lower_bound;
    sub[1] = result->dim[1].lower_bound + 1;
    addr = (int32_t *)CFI_address(result, sub);
    if (*addr != 23) return 0;

    /* result(2,2) should be a(3,3) = 3*10+3 = 33 */
    sub[0] = result->dim[0].lower_bound + 1;
    sub[1] = result->dim[1].lower_bound + 1;
    addr = (int32_t *)CFI_address(result, sub);
    if (*addr != 33) return 0;

    return 1;
}

/* ---- Rank-15 array descriptor ---- */
int c37_check_rank15(CFI_cdesc_t *a) {
    if (a->rank != 15) return 0;

    /* Total elements: 2^15 = 32768 */
    CFI_index_t total = 1;
    for (int i = 0; i < 15; i++) {
        if (a->dim[i].extent != 2) return 0;
        total *= a->dim[i].extent;
    }
    if (total != 32768) return 0;

    return 1;
}

/* ---- Scalar allocatable from C side ---- */
void c37_scalar_from_c(CFI_cdesc_t *val) {
    /* val is allocatable, rank 0, intent(out) → already deallocated */
    CFI_index_t lb[1] = {0};
    CFI_index_t ub[1] = {0};
    int rc = CFI_allocate(val, lb, ub, 0);
    if (rc != CFI_SUCCESS) return;

    /* Set the value */
    *(int32_t *)val->base_addr = 999;
}

/* ================================================================
 * From bindc_41c.c: CFI function error conditions
 * ================================================================ */

/* ---- CFI_establish error conditions ---- */
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

/* ---- CFI_allocate error conditions ---- */
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

/* ---- CFI_deallocate error conditions ---- */
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

/* ---- CFI_section error conditions ---- */
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

/* ---- CFI_setpointer error conditions ---- */
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

/* ---- CFI_select_part error conditions ---- */
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

/* ---- CFI_address error conditions / valid usage ---- */
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
