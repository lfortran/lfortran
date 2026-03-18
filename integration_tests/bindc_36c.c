/*
 * C helpers for bindc_36: CFI header validation
 *
 * Tests CFI_MAX_RANK, CFI_VERSION, all CFI_type_* macros,
 * all CFI error codes, type code matching, error provocation.
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>

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
