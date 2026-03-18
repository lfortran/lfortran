! Test: CFI library functions
!
! Covers:
!   - CFI_address (1D and 2D element addressing)
!   - CFI_is_contiguous (contiguous vs non-contiguous pointer)
!   - CFI_section (1D section with stride)
!   - CFI_select_part (derived type component selection)
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

program bindc_33
    use bindc_33_mod
    implicit none

    call test_address()
    call test_contiguous()
    call test_section()
    call test_select_part()

    print *, "All bindc_33 tests passed."

contains

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

end program
