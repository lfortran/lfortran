#include "../src/libasr/runtime/ISO_Fortran_binding.h"
#include <assert.h>
#include <stddef.h>
#include <stdlib.h> 
#include <string.h>

CFI_cdesc_t* allocate_descriptor(int rank) {
    size_t size = sizeof(CFI_cdesc_t) + (rank * sizeof(CFI_dim_t));
    CFI_cdesc_t* desc = (CFI_cdesc_t*)malloc(size);
    memset(desc, 0, size);
    desc->rank = rank;
    return desc;
}

void test_cfi_section_crash() {
   
    CFI_cdesc_t* source = allocate_descriptor(2);
    CFI_cdesc_t* result = allocate_descriptor(1);
    
    CFI_index_t extents[] = {10, 10};
    int dummy_source_data[100];
    
    int stat = CFI_establish(source, &dummy_source_data, CFI_attribute_other, 
                             CFI_type_int, 0, 2, extents);
    assert(stat == CFI_SUCCESS);

    result->type = CFI_type_int;
    result->attribute = CFI_attribute_other;

    CFI_index_t lower[] = {0, 5};
    CFI_index_t upper[] = {9, 5};
    CFI_index_t strides[] = {1, 0}; 

    stat = CFI_section(result, source, lower, upper, strides);
    assert(stat == CFI_SUCCESS);

    free(source);
    free(result);
}

void test_cfi_allocate_corruption() {
    CFI_cdesc_t* desc = allocate_descriptor(1);
    CFI_index_t extents[] = {10};
    
    int stat = CFI_establish(desc, NULL, CFI_attribute_allocatable, 
                             CFI_type_int, 0, 1, extents);
    assert(stat == CFI_SUCCESS);

    size_t correct_elem_len = desc->elem_len;
    
    stat = CFI_allocate(desc, NULL, NULL, 9999);
    assert(stat == CFI_SUCCESS || stat == CFI_ERROR_MEM_ALLOCATION);
    assert(desc->elem_len == correct_elem_len);
    
    if (stat == CFI_SUCCESS) {
        CFI_deallocate(desc);
    }
    free(desc);
}

void test_cfi_contiguous_extent_1() {
    CFI_cdesc_t* desc = allocate_descriptor(2);
    CFI_index_t extents[] = {10, 1}; 
    int dummy_data[10]; 
    
    int stat = CFI_establish(desc, &dummy_data, CFI_attribute_other, 
                             CFI_type_int, 0, 2, extents);
    assert(stat == CFI_SUCCESS);
    
    desc->dim[1].sm = 9999; 

    int is_contig = CFI_is_contiguous(desc);
    assert(is_contig == 1);
    
    free(desc);
}

void run_all_cfi_tests() {
    test_cfi_section_crash();
    test_cfi_allocate_corruption();
    test_cfi_contiguous_extent_1();
}