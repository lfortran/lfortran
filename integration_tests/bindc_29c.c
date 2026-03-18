/*
 * C helpers for bindc_29: module variables, intrinsics, edge cases
 */
#include <ISO_Fortran_binding.h>
#include <stdint.h>
#include <stdbool.h>

/* ---- C global variables bound to Fortran module variables ---- */
int32_t c29_global_arr[4] = {10, 20, 30, 40};

typedef struct { double x; double y; double z; } vec3_t;
vec3_t c29_global_vec = {1.0, 2.0, 3.0};

_Bool c29_global_flag = 1;
int32_t c29_global_counter = 42;

/* ---- check globals are initialized ---- */
int c29_check_globals(void) {
    if (c29_global_arr[0] != 10) return 1;
    if (c29_global_arr[1] != 20) return 2;
    if (c29_global_arr[2] != 30) return 3;
    if (c29_global_arr[3] != 40) return 4;
    if (c29_global_vec.x != 1.0) return 5;
    if (c29_global_vec.y != 2.0) return 6;
    if (c29_global_vec.z != 3.0) return 7;
    if (!c29_global_flag) return 8;
    if (c29_global_counter != 42) return 9;
    return 0;
}

/* ---- verify Fortran writes ---- */
int c29_verify_writes(void) {
    if (c29_global_arr[0] != 100) return 1;
    if (c29_global_arr[1] != 200) return 2;
    if (c29_global_arr[2] != 300) return 3;
    if (c29_global_arr[3] != 400) return 4;
    if (c29_global_counter != 99) return 5;
    return 0;
}

/* ---- descriptor type code queries ---- */
int c29_get_type_code_int32(CFI_cdesc_t *a) {
    return (int)a->type;
}

int c29_get_type_code_double(CFI_cdesc_t *a) {
    return (int)a->type;
}

int c29_get_type_code_float(CFI_cdesc_t *a) {
    return (int)a->type;
}

/* ---- same array to multiple descriptors ---- */
static int32_t sum_i32(char *base, CFI_dim_t *d, int rank, int k) {
    if (k == rank) return *(int32_t *)base;
    int32_t total = 0;
    for (CFI_index_t i = 0; i < d[k].extent; i++)
        total += sum_i32(base + i * d[k].sm, d, rank, k + 1);
    return total;
}

int32_t c29_self_dot(CFI_cdesc_t *a, CFI_cdesc_t *b) {
    int32_t total = 0;
    char *ba = (char *)a->base_addr;
    char *bb = (char *)b->base_addr;
    for (CFI_index_t i = 0; i < a->dim[0].extent; i++) {
        int32_t va = *(int32_t *)(ba + i * a->dim[0].sm);
        int32_t vb = *(int32_t *)(bb + i * b->dim[0].sm);
        total += va * vb;
    }
    return total;
}
