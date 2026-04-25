/*
 * Consolidated C helpers for bindc_iso_fb_03
 * Merged from: bindc_22c.c, bindc_26c.c, bindc_27c.c
 *
 * bindc_22c.c: array sections, derived types, non-default bounds
 * bindc_26c.c: derived type extensions through descriptors
 * bindc_27c.c: character interop through descriptors
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stdbool.h>
#include <complex.h>

/* ============================================================
 * Type definitions (from bindc_26c.c)
 * ============================================================ */
typedef struct { int32_t x; int32_t y; } inner_t;
typedef struct { inner_t pos; int32_t id; } nested_t;
typedef struct { float _Complex z; int32_t tag; } complex_member_t;
typedef struct { _Bool flag; int32_t val; } bool_member_t;
typedef struct { char code; int32_t val; } char_member_t;

/* ============================================================
 * Static helpers from bindc_22c.c
 * ============================================================ */

/* generic recursive sum for int32 */
static int32_t sum_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int32_t *)base;
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

/* ============================================================
 * Static helpers from bindc_26c.c
 * ============================================================ */

static int32_t sum_nested(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) {
        nested_t *p = (nested_t *)base;
        return p->pos.x + p->pos.y + p->id;
    }
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_nested(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

static int32_t sum_inner(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) {
        inner_t *p = (inner_t *)base;
        return p->x + p->y;
    }
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_inner(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

/* ============================================================
 * Static helpers from bindc_27c.c
 * ============================================================ */

/* generic recursive sum of char codes */
static int sum_chars(char *base, CFI_dim_t *d, int rank, int k,
                     int elem_len) {
    if (k == rank) {
        int total = 0;
        unsigned char *p = (unsigned char *)base;
        for (int j = 0; j < elem_len; j++)
            total += p[j];
        return total;
    }
    int total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_chars(base + i * d[k].sm, d, rank, k + 1, elem_len);
    return total;
}

/* ============================================================
 * Public functions from bindc_22c.c
 * ============================================================ */

/* ---- 1D sum (used for negative stride, non-default bounds) ---- */
int32_t c22_sum_1d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- 2D sum (used for 2D sections) ---- */
int32_t c22_sum_2d(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- derived type array: point_t has { int32 x, int32 y } ---- */
int32_t c22_sum_points(CFI_cdesc_t *a) {
    int32_t total = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        int32_t *pt = (int32_t *)(base + i * a->dim[0].sm);
        total += pt[0] + pt[1]; /* x + y */
    }
    return total;
}

int c22_point_elem_size(CFI_cdesc_t *a) {
    return (int)a->elem_len;
}

/* ---- non-default bounds ---- */
int32_t c22_sum_nondefault(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

int c22_get_extent(CFI_cdesc_t *a) {
    return (int)a->dim[0].extent;
}

/* ============================================================
 * Public functions from bindc_26c.c
 * ============================================================ */

/* ---- nested bind(C) type ---- */
int32_t c26_sum_nested(CFI_cdesc_t *a) {
    return sum_nested((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- allocatable derived type array ---- */
int32_t c26_sum_alloc_nested(CFI_cdesc_t *a) {
    return sum_nested((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- pointer derived type array ---- */
int32_t c26_sum_ptr_nested(CFI_cdesc_t *a) {
    return sum_nested((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- 2D derived type array ---- */
int32_t c26_sum_2d_inner(CFI_cdesc_t *a) {
    return sum_inner((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ---- assumed-rank with derived type ---- */
int c26_rank_inner(CFI_cdesc_t *a) {
    return (int)a->rank;
}

/* ---- empty type ---- */
int c26_sizeof_empty(CFI_cdesc_t *a) {
    return (int)a->elem_len;
}

/* ---- complex component type ---- */
int32_t c26_sum_complex_member(CFI_cdesc_t *a) {
    int32_t total = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        complex_member_t *p = (complex_member_t *)(base + i * a->dim[0].sm);
        total += p->tag;
    }
    return total;
}

/* ---- logical component type ---- */
int c26_count_flagged(CFI_cdesc_t *a) {
    int count = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        bool_member_t *p = (bool_member_t *)(base + i * a->dim[0].sm);
        if (p->flag) count++;
    }
    return count;
}

/* ---- character component type ---- */
int c26_sum_char_codes(CFI_cdesc_t *a) {
    int total = 0;
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        char_member_t *p = (char_member_t *)(base + i * a->dim[0].sm);
        total += (int)(unsigned char)p->code;
    }
    return total;
}

/* ---- array section of derived type ---- */
int32_t c26_sum_inner_section(CFI_cdesc_t *a) {
    return sum_inner((char *)a->base_addr, a->dim, (int)a->rank, 0);
}

/* ============================================================
 * Public functions from bindc_27c.c
 * ============================================================ */

/* ---- character(len=1) 1D sum ---- */
int c27_char_sum_1d(CFI_cdesc_t *a) {
    return sum_chars((char *)a->base_addr, a->dim, (int)a->rank, 0,
                     (int)a->elem_len);
}

/* ---- character(len=1) 2D sum ---- */
int c27_char_sum_2d(CFI_cdesc_t *a) {
    return sum_chars((char *)a->base_addr, a->dim, (int)a->rank, 0,
                     (int)a->elem_len);
}

/* ---- character inout: toupper ---- */
void c27_char_toupper(CFI_cdesc_t *a) {
    char *base = (char *)a->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        unsigned char *ch = (unsigned char *)(base + i * a->dim[0].sm);
        if (*ch >= 'a' && *ch <= 'z') *ch -= 32;
    }
}
