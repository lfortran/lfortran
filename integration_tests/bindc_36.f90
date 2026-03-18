! Test: CFI header validation — type macros, error codes, MAX_RANK, VERSION
!
! Covers:
!   - CFI_MAX_RANK >= 15
!   - CFI_VERSION > 0
!   - All CFI_type_* macros exist (compile-time check)
!   - All CFI error code macros exist
!   - Type code matching: integer, real, complex, logical, character, struct
!   - CFI error provocation (invalid rank, base-addr, extent)
module bindc_36_types
    use iso_c_binding
    implicit none
    type, bind(C) :: pair_t
        integer(c_int32_t) :: x, y
    end type
end module

program bindc_36
    use iso_c_binding
    use bindc_36_types
    implicit none

    interface
        integer(c_int) function c36_check_max_rank() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_check_version() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_check_type_macros() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_check_error_codes() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_get_cfi_type(a) bind(c)
            import :: c_int
            type(*), dimension(..) :: a
        end function
        integer(c_int) function c36_expected_int32() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_expected_float() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_expected_double() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_expected_float_complex() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_expected_double_complex() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_expected_bool() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_expected_char() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_expected_struct() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_test_error_establish() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_test_error_allocate() bind(c)
            import :: c_int
        end function
        integer(c_int) function c36_test_error_deallocate() bind(c)
            import :: c_int
        end function
    end interface

    ! CFI_MAX_RANK check
    if (c36_check_max_rank() /= 1) error stop "FAIL: CFI_MAX_RANK"

    ! CFI_VERSION check
    if (c36_check_version() /= 1) error stop "FAIL: CFI_VERSION"

    ! Type macros compile-time check
    if (c36_check_type_macros() /= 1) error stop "FAIL: CFI type macros"

    ! Error codes compile-time check
    if (c36_check_error_codes() /= 1) error stop "FAIL: CFI error codes"

    ! Type code matching
    call test_type_codes()

    ! Error handling tests
    if (c36_test_error_establish() /= 1) error stop "FAIL: CFI_establish errors"
    if (c36_test_error_allocate() /= 1) error stop "FAIL: CFI_allocate errors"
    if (c36_test_error_deallocate() /= 1) error stop "FAIL: CFI_deallocate errors"

    print *, "All bindc_36 tests passed."

contains

    subroutine test_type_codes()
        integer(c_int32_t) :: arr_int(1)
        real(c_float) :: arr_float(1)
        real(c_double) :: arr_double(1)
        complex(c_float_complex) :: arr_fc(1)
        complex(c_double_complex) :: arr_dc(1)
        logical(c_bool) :: arr_bool(1)
        character(kind=c_char, len=1) :: arr_char(1)
        type(pair_t) :: arr_struct(1)

        if (c36_get_cfi_type(arr_int) /= c36_expected_int32()) &
            error stop "FAIL: type int32"
        if (c36_get_cfi_type(arr_float) /= c36_expected_float()) &
            error stop "FAIL: type float"
        if (c36_get_cfi_type(arr_double) /= c36_expected_double()) &
            error stop "FAIL: type double"
        if (c36_get_cfi_type(arr_fc) /= c36_expected_float_complex()) &
            error stop "FAIL: type float_Complex"
        if (c36_get_cfi_type(arr_dc) /= c36_expected_double_complex()) &
            error stop "FAIL: type double_Complex"
        if (c36_get_cfi_type(arr_bool) /= c36_expected_bool()) &
            error stop "FAIL: type Bool"
        if (c36_get_cfi_type(arr_char) /= c36_expected_char()) &
            error stop "FAIL: type char"
        if (c36_get_cfi_type(arr_struct) /= c36_expected_struct()) &
            error stop "FAIL: type struct"
    end subroutine

end program
