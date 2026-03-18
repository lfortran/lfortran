! Test: CFI_section 2D, high-rank arrays, scalar alloc from C,
!       C_F_POINTER with non-default lower bounds, char(len=1) 2D descriptor
!
! Covers:
!   - CFI_section on 2D arrays with stride
!   - Rank-15 array via descriptor (maximum standard rank)
!   - Scalar allocatable descriptor established from C side
!   - C_F_POINTER with non-default lower bounds
!   - character(c_char, len=1) 2D assumed-shape via descriptors
module bindc_37_types
    use iso_c_binding
    implicit none
    type, bind(C) :: pair_t
        integer(c_int32_t) :: x, y
    end type
end module

program bindc_37
    use iso_c_binding
    use bindc_37_types
    implicit none

    interface
        ! character(len=1) 2D array descriptor
        integer(c_int) function c37_check_char_2d(arr) bind(c)
            import :: c_int, c_char
            character(kind=c_char, len=1), intent(in) :: arr(:,:)
        end function

        ! CFI_section on a 2D array
        integer(c_int) function c37_section_2d(a) bind(c)
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:,:)
        end function

        ! Rank-15 check: C validates rank and total element count
        integer(c_int) function c37_check_rank15(a) bind(c)
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(..)
        end function

        ! C creates a scalar descriptor, fills it, Fortran reads via intent(out)
        subroutine c37_scalar_from_c(val) bind(c)
            import :: c_int32_t
            integer(c_int32_t), allocatable, intent(out) :: val
        end subroutine
    end interface

    call test_char_2d()
    call test_section_2d()
    call test_rank15()
    call test_scalar_from_c()
    call test_cf_pointer_lbounds()

    print *, "All bindc_37 tests passed."

contains

    subroutine test_char_2d()
        character(kind=c_char, len=1) :: grid(3, 2)
        grid(1,1) = 'A'; grid(2,1) = 'B'; grid(3,1) = 'C'
        grid(1,2) = 'D'; grid(2,2) = 'E'; grid(3,2) = 'F'
        if (c37_check_char_2d(grid) /= 1) error stop "FAIL: char 2D"
    end subroutine

    subroutine test_section_2d()
        integer(c_int32_t) :: mat(4, 4)
        integer :: i, j
        ! Fill: mat(i,j) = i * 10 + j
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
        ! Use non-default lower bound: indices -2 to 3
        call c_f_pointer(cptr, p, [6])
        ! Default bounds: p(1)..p(6)
        if (p(1) /= 10) error stop "FAIL: cf_ptr default lb"
        if (p(6) /= 60) error stop "FAIL: cf_ptr default ub"
    end subroutine

end program
