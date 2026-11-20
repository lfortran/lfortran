#include "ISO_Fortran_binding.h"

/* 0: other, 1: pointer, 2: allocatable; -1 otherwise or when the
   descriptor does not have rank 1 and the expected extent */
static int attribute_of(CFI_cdesc_t *d, int extent) {
    if (d->rank != 1 || d->dim[0].extent != extent) return -1;
    if (d->attribute == CFI_attribute_other) return 0;
    if (d->attribute == CFI_attribute_pointer) return 1;
    if (d->attribute == CFI_attribute_allocatable) return 2;
    return -1;
}

int attr_int_shape(CFI_cdesc_t *x, int extent) { return attribute_of(x, extent); }
int attr_int_rank(CFI_cdesc_t *x, int extent) { return attribute_of(x, extent); }
int attr_char_shape(CFI_cdesc_t *x, int extent) { return attribute_of(x, extent); }
int attr_char_rank(CFI_cdesc_t *x, int extent) { return attribute_of(x, extent); }
int attr_struct_shape(CFI_cdesc_t *x, int extent) { return attribute_of(x, extent); }
int attr_struct_rank(CFI_cdesc_t *x, int extent) { return attribute_of(x, extent); }
int attr_int_pointer(CFI_cdesc_t *x, int extent) { return attribute_of(x, extent); }
int attr_int_allocatable(CFI_cdesc_t *x, int extent) { return attribute_of(x, extent); }

/* the sum of the elements of an integer array section */
int sum_int_shape(CFI_cdesc_t *x) {
    int s = 0;
    for (CFI_index_t i = 0; i < x->dim[0].extent; i++) {
        s += *(int *)((char *)x->base_addr + i * x->dim[0].sm);
    }
    return s;
}
