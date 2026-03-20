! Consolidated ISO_Fortran_binding test: CFI Library Functions, Header Validation & Errors
! Merged from: bindc_33, bindc_36, bindc_37, bindc_41

! --- Module from bindc_33 ---
module bindc_33_mod
    use iso_c_binding
    implicit none

    type, bind(C) :: pair_t
        integer(c_int32_t) :: x, y
    end type

    interface
        integer(c_int) function c33_test_address_1d(a) bind(C)
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function

        integer(c_int) function c33_test_address_2d(a) bind(C)
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:,:)
        end function

        integer(c_int) function c33_check_contiguous(a) bind(C)
            import :: c_int, c_int32_t
            integer(c_int32_t), pointer, intent(in) :: a(:)
        end function

        integer(c_int) function c33_test_section(a) bind(C)
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function

        integer(c_int) function c33_test_select_part(a) bind(C)
            import :: c_int, pair_t
            type(pair_t), intent(in) :: a(:)
        end function
    end interface
end module

! --- Module from bindc_36 ---
module bindc_36_types
    use iso_c_binding
    implicit none
    type, bind(C) :: pair_t_36
        integer(c_int32_t) :: x, y
    end type
end module

! --- Module from bindc_37 ---
module bindc_37_types
    use iso_c_binding
    implicit none
    type, bind(C) :: pair_t_37
        integer(c_int32_t) :: x, y
    end type
end module

! --- Module from bindc_41 ---
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

program bindc_iso_fb_04
    use iso_c_binding
    use bindc_33_mod
    use bindc_36_types
    use bindc_37_types
    use bindc_41_ifaces
    implicit none

    ! --- Interfaces from bindc_36 (inline in original program) ---
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

    ! --- Interfaces from bindc_37 (inline in original program) ---
    interface
        integer(c_int) function c37_check_char_2d(arr) bind(c)
            import :: c_int, c_char
            character(kind=c_char, len=1), intent(in) :: arr(:,:)
        end function

        integer(c_int) function c37_section_2d(a) bind(c)
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:,:)
        end function

        integer(c_int) function c37_check_rank15(a) bind(c)
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(..)
        end function

        subroutine c37_scalar_from_c(val) bind(c)
            import :: c_int32_t
            integer(c_int32_t), allocatable, intent(out) :: val
        end subroutine
    end interface

    ! ===== Tests from bindc_33 =====
    call test_address()
    call test_contiguous()
    call test_section()
    call test_select_part()

    ! ===== Tests from bindc_36 =====
    if (c36_check_max_rank() /= 1) error stop "FAIL: CFI_MAX_RANK"
    if (c36_check_version() /= 1) error stop "FAIL: CFI_VERSION"
    if (c36_check_type_macros() /= 1) error stop "FAIL: CFI type macros"
    if (c36_check_error_codes() /= 1) error stop "FAIL: CFI error codes"
    call test_type_codes()
    if (c36_test_error_establish() /= 1) error stop "FAIL: CFI_establish errors"
    if (c36_test_error_allocate() /= 1) error stop "FAIL: CFI_allocate errors"
    if (c36_test_error_deallocate() /= 1) error stop "FAIL: CFI_deallocate errors"

    ! ===== Tests from bindc_37 =====
    call test_char_2d()
    call test_section_2d()
    call test_rank15()
    call test_scalar_from_c()
    call test_cf_pointer_lbounds()

    ! ===== Tests from bindc_41 =====
    if (c41_test_establish_errors() /= 1) error stop "FAIL: establish errors"
    if (c41_test_allocate_errors() /= 1) error stop "FAIL: allocate errors"
    if (c41_test_deallocate_errors() /= 1) error stop "FAIL: deallocate errors"
    if (c41_test_section_errors() /= 1) error stop "FAIL: section errors"
    if (c41_test_setpointer_errors() /= 1) error stop "FAIL: setpointer errors"
    if (c41_test_select_part_errors() /= 1) error stop "FAIL: select_part errors"
    if (c41_test_address_errors() /= 1) error stop "FAIL: address errors"

    print *, "All bindc_iso_fb_04 tests passed."

contains

    ! ===== Subroutines from bindc_33 =====

    subroutine test_address()
        integer(c_int32_t) :: a1(5)
        integer(c_int32_t) :: a2(3, 4)
        integer :: i, j
        do i = 1, 5
            a1(i) = i * 10
        end do
        do j = 1, 4
            do i = 1, 3
                a2(i, j) = i + (j - 1) * 3
            end do
        end do
        if (c33_test_address_1d(a1) /= 0) error stop "FAIL: CFI_address 1D"
        if (c33_test_address_2d(a2) /= 0) error stop "FAIL: CFI_address 2D"
    end subroutine

    subroutine test_contiguous()
        integer(c_int32_t), target :: a(10)
        integer(c_int32_t), pointer :: p(:)
        integer :: i
        do i = 1, 10
            a(i) = i
        end do
        p => a(1:10)
        if (c33_check_contiguous(p) /= 1) error stop "FAIL: contiguous"
        p => a(1:10:2)
        if (c33_check_contiguous(p) /= 0) error stop "FAIL: non-contiguous"
    end subroutine

    subroutine test_section()
        integer(c_int32_t) :: a(10)
        integer :: i
        do i = 1, 10
            a(i) = i * 10
        end do
        if (c33_test_section(a) /= 0) error stop "FAIL: CFI_section"
    end subroutine

    subroutine test_select_part()
        type(pair_t) :: arr(3)
        arr(1) = pair_t(10, 100)
        arr(2) = pair_t(20, 200)
        arr(3) = pair_t(30, 300)
        if (c33_test_select_part(arr) /= 0) error stop "FAIL: CFI_select_part"
    end subroutine

    ! ===== Subroutines from bindc_36 =====

    subroutine test_type_codes()
        integer(c_int32_t) :: arr_int(1)
        real(c_float) :: arr_float(1)
        real(c_double) :: arr_double(1)
        complex(c_float_complex) :: arr_fc(1)
        complex(c_double_complex) :: arr_dc(1)
        logical(c_bool) :: arr_bool(1)
        character(kind=c_char, len=1) :: arr_char(1)
        type(pair_t_36) :: arr_struct(1)

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

    ! ===== Subroutines from bindc_37 =====

    subroutine test_char_2d()
        character(kind=c_char, len=1) :: grid(3, 2)
        grid(1,1) = 'A'; grid(2,1) = 'B'; grid(3,1) = 'C'
        grid(1,2) = 'D'; grid(2,2) = 'E'; grid(3,2) = 'F'
        if (c37_check_char_2d(grid) /= 1) error stop "FAIL: char 2D"
    end subroutine

    subroutine test_section_2d()
        integer(c_int32_t) :: mat(4, 4)
        integer :: i, j
        do j = 1, 4
            do i = 1, 4
                mat(i, j) = i * 10 + j
            end do
        end do
        if (c37_section_2d(mat) /= 1) error stop "FAIL: section 2D"
    end subroutine

    subroutine test_rank15()
        integer(c_int32_t) :: arr(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
        arr = 1
        if (c37_check_rank15(arr) /= 1) error stop "FAIL: rank 15"
    end subroutine

    subroutine test_scalar_from_c()
        integer(c_int32_t), allocatable :: val
        call c37_scalar_from_c(val)
        if (.not. allocated(val)) error stop "FAIL: scalar alloc"
        if (val /= 999) error stop "FAIL: scalar value"
    end subroutine

    subroutine test_cf_pointer_lbounds()
        integer(c_int32_t), target :: arr(6)
        integer(c_int32_t), pointer :: p(:)
        type(c_ptr) :: cptr
        arr = [10, 20, 30, 40, 50, 60]
        cptr = c_loc(arr(1))
        call c_f_pointer(cptr, p, [6])
        if (p(1) /= 10) error stop "FAIL: cf_ptr default lb"
        if (p(6) /= 60) error stop "FAIL: cf_ptr default ub"
    end subroutine

end program
