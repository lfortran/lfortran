#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

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

/* ---- Optional allocatable scalar ----
 * Receives a CFI descriptor. If absent, the descriptor pointer itself is NULL.
 */
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
