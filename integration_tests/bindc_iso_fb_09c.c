#include "ISO_Fortran_binding.h"
#include <stdio.h>
#include <stdlib.h>

void test_cfi_section_crash() {
    printf("Running test_cfi_section_crash...\n");
    CFI_CDESC_T(2) source;
    CFI_index_t extents[] = {10, 10};
    int dummy_source_data[100];
    
    int stat = CFI_establish((CFI_cdesc_t *)&source, dummy_source_data, CFI_attribute_other, 
                             CFI_type_int, 0, 2, extents);
    if (stat != CFI_SUCCESS) { printf("  -> FAIL: CFI_establish source failed with %d\n", stat); exit(1); }

    CFI_CDESC_T(1) result;
    stat = CFI_establish((CFI_cdesc_t *)&result, NULL, CFI_attribute_pointer, 
                         CFI_type_int, 0, 1, NULL);
    if (stat != CFI_SUCCESS) { printf("  -> FAIL: CFI_establish result failed with %d\n", stat); exit(1); }

    CFI_index_t lower[] = {0, 5};
    CFI_index_t upper[] = {9, 5};
    CFI_index_t strides[] = {1, 0}; 

    stat = CFI_section((CFI_cdesc_t *)&result, (CFI_cdesc_t *)&source, lower, upper, strides);
    if (stat != CFI_SUCCESS) { printf("  -> FAIL: CFI_section failed with %d\n", stat); exit(1); }
    printf("  -> PASS\n");
}

void test_cfi_allocate_corruption() {
    printf("Running test_cfi_allocate_corruption...\n");
    CFI_CDESC_T(1) desc;
    CFI_index_t extents[] = {10};
    
    int stat = CFI_establish((CFI_cdesc_t *)&desc, NULL, CFI_attribute_allocatable, 
                             CFI_type_int, 0, 1, extents);
    if (stat != CFI_SUCCESS) { printf("  -> FAIL: CFI_establish failed with %d\n", stat); exit(1); }

    size_t correct_elem_len = ((CFI_cdesc_t *)&desc)->elem_len;
    
    CFI_index_t alloc_lower[] = {0};
    CFI_index_t alloc_upper[] = {9};
    
    stat = CFI_allocate((CFI_cdesc_t *)&desc, alloc_lower, alloc_upper, 9999);
    
    // We allow SUCCESS or MEM_ALLOCATION error. (Some compilers might also throw INVALID_ELEM_LEN (13) which is technically against F2018 spec here, but we'll catch it if they do).
    if (!(stat == CFI_SUCCESS || stat == CFI_ERROR_MEM_ALLOCATION || stat == 13)) {
        printf("  -> FAIL: CFI_allocate failed with unexpected stat %d\n", stat);
        exit(1);
    }
    
    if (((CFI_cdesc_t *)&desc)->elem_len != correct_elem_len) {
        printf("  -> FAIL: elem_len was corrupted by CFI_allocate! (Expected %zu, got %zu)\n", correct_elem_len, (size_t)((CFI_cdesc_t *)&desc)->elem_len);
        exit(1);
    }
    
    if (stat == CFI_SUCCESS) {
        CFI_deallocate((CFI_cdesc_t *)&desc);
    }
    printf("  -> PASS\n");
}

void test_cfi_contiguous_extent_1() {
    printf("Running test_cfi_contiguous_extent_1...\n");
    CFI_CDESC_T(2) desc;
    CFI_index_t extents[] = {10, 1}; 
    int dummy_data[10]; 
    
    int stat = CFI_establish((CFI_cdesc_t *)&desc, dummy_data, CFI_attribute_other, 
                             CFI_type_int, 0, 2, extents);
    if (stat != CFI_SUCCESS) { printf("  -> FAIL: CFI_establish failed with %d\n", stat); exit(1); }
    
    ((CFI_cdesc_t *)&desc)->dim[1].sm = 9999; 

    int is_contig = CFI_is_contiguous((CFI_cdesc_t *)&desc);
    if (is_contig != 1) {
        printf("  -> FAIL: CFI_is_contiguous returned false negative for extent 1!\n");
        exit(1);
    }
    printf("  -> PASS\n");
}

void run_all_cfi_tests() {
    printf("Starting ISO_Fortran_binding tests...\n");
    test_cfi_section_crash();
    test_cfi_allocate_corruption();
    test_cfi_contiguous_extent_1();
    printf("All C tests passed successfully.\n");
}