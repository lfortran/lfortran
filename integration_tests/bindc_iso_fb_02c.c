/*
 * Consolidated C helpers for bindc_iso_fb_02
 * Merged from: bindc_20c.c, bindc_28c.c, bindc_30c.c, bindc_31c.c, bindc_32c.c
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stdbool.h>
#include <complex.h>

/* ========================================================================
 * Shared static helper functions (deduplicated)
 *
 * sum_i32 appeared identically in bindc_20c.c and bindc_28c.c — kept once.
 * dbl_i32 from bindc_20c.c, sum_f32/sum_f64/sum_i64 from bindc_28c.c,
 * sum_chars from bindc_30c.c are each unique.
 * ======================================================================== */

/* generic recursive sum for int32 (used by bindc_20 and bindc_28 functions) */
static int32_t sum_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int32_t *)base;
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

/* generic recursive double-in-place for int32 (from bindc_20c.c) */
static void dbl_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) { *(int32_t *)base *= 2; return; }
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        dbl_i32(base + i * d[k].sm, d, rank, k + 1);
}

/* generic recursive sum for float (from bindc_28c.c) */
static float sum_f32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(float *)base;
    float total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_f32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

/* generic recursive sum for double (from bindc_28c.c) */
static double sum_f64(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(double *)base;
    double total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_f64(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

/* generic recursive sum for int64 (from bindc_28c.c) */
static int64_t sum_i64(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int64_t *)base;
    int64_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i64(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

/* generic recursive sum of char codes (from bindc_30c.c) */
static int sum_chars(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return (int)*(unsigned char *)base;
    int total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_chars(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

/* ========================================================================
 * Functions from bindc_20c.c
 * Multi-rank alloc/ptr, scalar alloc/ptr
 * ======================================================================== */

/* ---- allocatable 2D ---- */
int32_t c20_sum_alloc_2d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
int c20_attr_alloc_2d(CFI_cdesc_t *a) {
    return a->attribute == CFI_attribute_allocatable;
}
void c20_double_alloc_2d(CFI_cdesc_t *a) {
    dbl_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable 3D ---- */
int32_t c20_sum_alloc_3d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- pointer 2D ---- */
int32_t c20_sum_ptr_2d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
int c20_attr_ptr_2d(CFI_cdesc_t *a) {
    return a->attribute == CFI_attribute_pointer;
}

/* ---- pointer 3D ---- */
int32_t c20_sum_ptr_3d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- pointer inout ---- */
void c20_double_ptr_1d(CFI_cdesc_t *a) {
    dbl_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable scalar (rank 0) ---- */
int32_t c20_read_alloc_scalar(CFI_cdesc_t *a) {
    return *(int32_t *)a->base_addr;
}
int c20_attr_alloc_scalar(CFI_cdesc_t *a) {
    return a->attribute == CFI_attribute_allocatable;
}

/* ---- pointer scalar (rank 0) ---- */
int32_t c20_read_ptr_scalar(CFI_cdesc_t *a) {
    return *(int32_t *)a->base_addr;
}
int c20_attr_ptr_scalar(CFI_cdesc_t *a) {
    return a->attribute == CFI_attribute_pointer;
}

/* ========================================================================
 * Functions from bindc_28c.c
 * Attribute combinations and array features
 * ======================================================================== */

/* ---- allocatable float ---- */
float c28_sum_alloc_float(CFI_cdesc_t *a) {
    return sum_f32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable double ---- */
double c28_sum_alloc_double(CFI_cdesc_t *a) {
    return sum_f64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable int64 ---- */
int64_t c28_sum_alloc_int64(CFI_cdesc_t *a) {
    return sum_i64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable complex ---- */
void c28_sum_alloc_complex(CFI_cdesc_t *a, float *re, float *im) {
    *re = 0;
    *im = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        float *c = (float *)(base + i * a->dim[0].sm);
        *re += c[0];
        *im += c[1];
    }
}

/* ---- allocatable logical ---- */
int c28_count_alloc_bool(CFI_cdesc_t *a) {
    int count = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        if (*(unsigned char *)(base + i * a->dim[0].sm)) count++;
    }
    return count;
}

/* ---- pointer float ---- */
float c28_sum_ptr_float(CFI_cdesc_t *a) {
    return sum_f32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- pointer double ---- */
double c28_sum_ptr_double(CFI_cdesc_t *a) {
    return sum_f64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- pointer int64 ---- */
int64_t c28_sum_ptr_int64(CFI_cdesc_t *a) {
    return sum_i64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- zero-size array ---- */
int c28_zero_size_extent(CFI_cdesc_t *a) {
    return (int)a->dim[0].extent;
}

/* ---- unallocated allocatable ---- */
int c28_is_allocated(CFI_cdesc_t *a) {
    return (a->base_addr != NULL) ? 1 : 0;
}

/* ---- disassociated pointer ---- */
int c28_is_associated(CFI_cdesc_t *a) {
    return (a->base_addr != NULL) ? 1 : 0;
}

/* ---- rank 4 ---- */
int32_t c28_sum_4d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

int c28_get_rank_4d(CFI_cdesc_t *a) {
    return (int)a->rank;
}

/* ---- explicit shape: receives raw pointer + length ---- */
int32_t c28_sum_explicit(int32_t *a, int n) {
    int32_t total = 0;
    for (int i = 0; i < n; i++) total += a[i];
    return total;
}

/* ---- optional + contiguous ---- */
int32_t c28_opt_contig_sum(CFI_cdesc_t *a) {
    if (a == NULL) return 0;
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable + assumed-rank ---- */
int c28_alloc_ar_rank(CFI_cdesc_t *a) {
    return (int)a->rank;
}

/* ---- pointer + assumed-rank ---- */
int c28_ptr_ar_rank(CFI_cdesc_t *a) {
    return (int)a->rank;
}

/* ========================================================================
 * Functions from bindc_30c.c
 * Character alloc/ptr, allocatable+assumed-rank, pointer+assumed-rank
 * ======================================================================== */

/* ---- character allocatable ---- */
int c30_char_alloc_sum(CFI_cdesc_t *a) {
    return sum_chars((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

int c30_char_alloc_attr(CFI_cdesc_t *a) {
    return a->attribute == CFI_attribute_allocatable;
}

/* ---- character pointer ---- */
int c30_char_ptr_sum(CFI_cdesc_t *a) {
    return sum_chars((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

int c30_char_ptr_attr(CFI_cdesc_t *a) {
    return a->attribute == CFI_attribute_pointer;
}

/* ---- allocatable + assumed-rank ---- */
int c30_alloc_ar_rank(CFI_cdesc_t *a) {
    return (int)a->rank;
}

/* ---- pointer + assumed-rank ---- */
int c30_ptr_ar_rank(CFI_cdesc_t *a) {
    return (int)a->rank;
}

/* ========================================================================
 * Functions from bindc_31c.c
 * C driver: CFI_allocate / CFI_deallocate tests
 * ======================================================================== */

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

/* ========================================================================
 * Functions from bindc_32c.c
 * C driver: CFI_setpointer tests
 * ======================================================================== */

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
