! Test: CFI function error conditions
!
! Covers:
!   - CFI_establish error: invalid type, invalid elem_len for character
!   - CFI_allocate error: non-allocatable descriptor, already allocated
!   - CFI_deallocate error: non-allocatable, null base_addr
!   - CFI_section error: rank mismatch, out-of-bounds
!   - CFI_setpointer error: rank mismatch between result and source
!   - CFI_select_part error: invalid displacement, invalid elem_len
!   - CFI_address error: out-of-bounds subscripts
!   - CFI_INVALID_STRIDE provocation
!   - CFI_ERROR_OUT_OF_BOUNDS provocation
!   - CFI_INVALID_DESCRIPTOR provocation
!   - CFI_INVALID_EXTENT provocation
!   - CFI_INVALID_TYPE provocation
!   - CFI_INVALID_ELEM_LEN provocation

module bindc_41_ifaces
    use iso_c_binding
    implicit none
    interface
        integer(c_int) function c41_test_establish_errors() bind(C)
            import :: c_int
        end function

        integer(c_int) function c41_test_allocate_errors() bind(C)
            import :: c_int
        end function

        integer(c_int) function c41_test_deallocate_errors() bind(C)
            import :: c_int
        end function

        integer(c_int) function c41_test_section_errors() bind(C)
            import :: c_int
        end function

        integer(c_int) function c41_test_setpointer_errors() bind(C)
            import :: c_int
        end function

        integer(c_int) function c41_test_select_part_errors() bind(C)
            import :: c_int
        end function

        integer(c_int) function c41_test_address_errors() bind(C)
            import :: c_int
        end function
    end interface
end module

program bindc_41
    use iso_c_binding
    use bindc_41_ifaces
    implicit none

    if (c41_test_establish_errors() /= 1) error stop "FAIL: establish errors"
    if (c41_test_allocate_errors() /= 1) error stop "FAIL: allocate errors"
    if (c41_test_deallocate_errors() /= 1) error stop "FAIL: deallocate errors"
    if (c41_test_section_errors() /= 1) error stop "FAIL: section errors"
    if (c41_test_setpointer_errors() /= 1) error stop "FAIL: setpointer errors"
    if (c41_test_select_part_errors() /= 1) error stop "FAIL: select_part errors"
    if (c41_test_address_errors() /= 1) error stop "FAIL: address errors"

    print *, "All bindc_41 tests passed."

end program
