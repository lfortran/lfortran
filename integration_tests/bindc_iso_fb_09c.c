#include "../src/libasr/runtime/ISO_Fortran_binding.h"
#include <assert.h>
#include <stddef.h>

void test_cfi_section_crash() {
    CFI_CDESC_T(2) source;
    CFI_index_t extents[] = {10, 10};
    int dummy_source_data[100];
    
    int stat = CFI_establish((CFI_cdesc_t *)&source, &dummy_source_data, CFI_attribute_other, 
                             CFI_type_int, 0, 2, extents);
    assert(stat == CFI_SUCCESS);

    CFI_CDESC_T(1) result;
    stat = CFI_establish((CFI_cdesc_t *)&result, NULL, CFI_attribute_pointer, 
                         CFI_type_int, 0, 1, NULL);
    assert(stat == CFI_SUCCESS);

    CFI_index_t lower[] = {0, 5};
    CFI_index_t upper[] = {9, 5};
    CFI_index_t strides[] = {1, 0}; 

    stat = CFI_section((CFI_cdesc_t *)&result, (CFI_cdesc_t *)&source, lower, upper, strides);
    assert(stat == CFI_SUCCESS);
}

void test_cfi_allocate_corruption() {
    CFI_CDESC_T(1) desc;
    CFI_index_t extents[] = {10};
    
    int stat = CFI_establish((CFI_cdesc_t *)&desc, NULL, CFI_attribute_allocatable, 
                             CFI_type_int, 0, 1, extents);
    assert(stat == CFI_SUCCESS);

    size_t correct_elem_len = ((CFI_cdesc_t *)&desc)->elem_len;
    CFI_index_t alloc_lower[] = {0};
    CFI_index_t alloc_upper[] = {9};
    
    stat = CFI_allocate((CFI_cdesc_t *)&desc, alloc_lower, alloc_upper, 9999);
    assert(stat == CFI_SUCCESS || stat == CFI_ERROR_MEM_ALLOCATION);
    assert(((CFI_cdesc_t *)&desc)->elem_len == correct_elem_len);
    
    if (stat == CFI_SUCCESS) {
        CFI_deallocate((CFI_cdesc_t *)&desc);
    }
}

void test_cfi_contiguous_extent_1() {
    CFI_CDESC_T(2) desc;
    CFI_index_t extents[] = {10, 1}; 
    int dummy_data[10]; 
    
    int stat = CFI_establish((CFI_cdesc_t *)&desc, &dummy_data, CFI_attribute_other, 
                             CFI_type_int, 0, 2, extents);
    assert(stat == CFI_SUCCESS);
    
    ((CFI_cdesc_t *)&desc)->dim[1].sm = 9999; 

    int is_contig = CFI_is_contiguous((CFI_cdesc_t *)&desc);
    assert(is_contig == 1);
}

void run_all_cfi_tests() {
    test_cfi_section_crash();
    test_cfi_allocate_corruption();
    test_cfi_contiguous_extent_1();
}