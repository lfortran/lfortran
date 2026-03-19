/*
 * C functions that receive assumed-shape / assumed-rank arrays from Fortran
 * via CFI descriptors (ISO_Fortran_binding.h).
 *
 * Covers: int32, int64, float, double × ranks 1-3 + assumed rank.
 * Each Fortran interface gets its own C entry point to avoid
 * compiler warnings about incompatible external interfaces sharing
 * the same binding name.
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>

/* ----------------------------------------------------------------
 *  int32 — sum (any rank via recursive descent on descriptor dims)
 * ---------------------------------------------------------------- */
static int32_t sum_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int32_t *)base;
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
static int32_t sum_i32_entry(CFI_cdesc_t *a) {
    return sum_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
int32_t c_sum_int32_1d(CFI_cdesc_t *a) { return sum_i32_entry(a); }
int32_t c_sum_int32_2d(CFI_cdesc_t *a) { return sum_i32_entry(a); }
int32_t c_sum_int32_3d(CFI_cdesc_t *a) { return sum_i32_entry(a); }
int32_t c_sum_int32_ar(CFI_cdesc_t *a) { return sum_i32_entry(a); }

/* ----------------------------------------------------------------
 *  int64 — sum (any rank)
 * ---------------------------------------------------------------- */
static int64_t sum_i64(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int64_t *)base;
    int64_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i64(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
static int64_t sum_i64_entry(CFI_cdesc_t *a) {
    return sum_i64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
int64_t c_sum_int64_1d(CFI_cdesc_t *a) { return sum_i64_entry(a); }
int64_t c_sum_int64_2d(CFI_cdesc_t *a) { return sum_i64_entry(a); }
int64_t c_sum_int64_3d(CFI_cdesc_t *a) { return sum_i64_entry(a); }

/* ----------------------------------------------------------------
 *  float — sum (any rank)
 * ---------------------------------------------------------------- */
static float sum_f32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(float *)base;
    float total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_f32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
static float sum_f32_entry(CFI_cdesc_t *a) {
    return sum_f32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
float c_sum_float_1d(CFI_cdesc_t *a) { return sum_f32_entry(a); }
float c_sum_float_2d(CFI_cdesc_t *a) { return sum_f32_entry(a); }
float c_sum_float_3d(CFI_cdesc_t *a) { return sum_f32_entry(a); }

/* ----------------------------------------------------------------
 *  double — sum (any rank)
 * ---------------------------------------------------------------- */
static double sum_f64(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(double *)base;
    double total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_f64(base + i * d[k].sm, d, rank, k + 1);
    return total;
}
static double sum_f64_entry(CFI_cdesc_t *a) {
    return sum_f64((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
double c_sum_double_1d(CFI_cdesc_t *a) { return sum_f64_entry(a); }
double c_sum_double_2d(CFI_cdesc_t *a) { return sum_f64_entry(a); }
double c_sum_double_3d(CFI_cdesc_t *a) { return sum_f64_entry(a); }

/* ----------------------------------------------------------------
 *  int32 — double-in-place (any rank)
 * ---------------------------------------------------------------- */
static void dbl_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) { *(int32_t *)base *= 2; return; }
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        dbl_i32(base + i * d[k].sm, d, rank, k + 1);
}
static void dbl_i32_entry(CFI_cdesc_t *a) {
    dbl_i32((char *)a->base_addr, a->dim, (int)a->rank, 0);
}
void c_double_int32_1d(CFI_cdesc_t *a) { dbl_i32_entry(a); }
void c_double_int32_2d(CFI_cdesc_t *a) { dbl_i32_entry(a); }
void c_double_int32_3d(CFI_cdesc_t *a) { dbl_i32_entry(a); }

/* ----------------------------------------------------------------
 *  Assumed-rank queries (type(*), dimension(..))
 * ---------------------------------------------------------------- */
int c_get_rank(CFI_cdesc_t *a) {
    return (int)a->rank;
}

int c_get_elem_size(CFI_cdesc_t *a) {
    return (int)a->elem_len;
}

/* ----------------------------------------------------------------
 *  Allocatable arrays — same sum logic, separate entry points
 * ---------------------------------------------------------------- */
int32_t c_sum_alloc_1d(CFI_cdesc_t *a) { return sum_i32_entry(a); }
int c_attr_alloc(CFI_cdesc_t *a) { return a->attribute == CFI_attribute_allocatable; }
void c_double_alloc_1d(CFI_cdesc_t *a) { dbl_i32_entry(a); }

/* ----------------------------------------------------------------
 *  Pointer arrays — same sum logic, separate entry points
 * ---------------------------------------------------------------- */
int32_t c_sum_ptr_1d(CFI_cdesc_t *a) { return sum_i32_entry(a); }
int c_attr_ptr(CFI_cdesc_t *a) { return a->attribute == CFI_attribute_pointer; }

/* ----------------------------------------------------------------
 *  Attribute / contiguity queries for assumed-shape (other)
 * ---------------------------------------------------------------- */
int c_attr_other(CFI_cdesc_t *a) { return a->attribute == CFI_attribute_other; }

int c_is_contiguous(CFI_cdesc_t *a) {
    CFI_index_t expected = (CFI_index_t)a->elem_len;
    for (int i = 0; i < (int)a->rank; i++) {
        if (a->dim[i].sm != expected) return 0;
        expected *= a->dim[i].extent;
    }
    return 1;
}

/* ----------------------------------------------------------------
 *  Optional argument — NULL descriptor when absent
 * ---------------------------------------------------------------- */
int c_is_present(CFI_cdesc_t *a) {
    return (a != NULL) ? 1 : 0;
}
