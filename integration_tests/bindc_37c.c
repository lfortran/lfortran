/*
 * C helpers for bindc_37: char(len=1) 2D descriptor, CFI_section 2D,
 * high-rank arrays, scalar descriptor from C, C_F_POINTER with lbounds.
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <string.h>

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
