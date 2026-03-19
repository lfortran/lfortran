! Test: Array sections, derived-type arrays, non-default bounds
!
! Covers gap items G.20-G.21, J.25, K.26:
!   - Negative stride array sections
!   - 2D/3D array sections passed as assumed-shape
!   - TYPE, BIND(C) arrays via descriptor
!   - Arrays with non-default lower bounds
!
! Item tested in bindc_23 instead (not yet supported by LFortran LLVM):
!   - F.19: C calling Fortran BIND(C) procedures with descriptor args
module bindc_22_types
    use iso_c_binding, only: c_int32_t, c_double
    implicit none

    type, bind(C) :: point_t
        integer(c_int32_t) :: x
        integer(c_int32_t) :: y
    end type
end module

module bindc_22_mod
    use iso_c_binding, only: c_int, c_int32_t, c_double
    use bindc_22_types
    implicit none

    interface
        ! ---- negative stride ----
        integer(c_int32_t) function c22_sum_1d(a) bind(C, name="c22_sum_1d")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function

        ! ---- 2D section ----
        integer(c_int32_t) function c22_sum_2d(a) bind(C, name="c22_sum_2d")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:,:)
        end function

        ! ---- derived type array via descriptor ----
        integer(c_int32_t) function c22_sum_points(pts) &
                bind(C, name="c22_sum_points")
            import :: c_int32_t, point_t
            type(point_t), intent(in) :: pts(:)
        end function

        integer(c_int) function c22_point_elem_size(pts) &
                bind(C, name="c22_point_elem_size")
            import :: c_int, point_t
            type(point_t), intent(in) :: pts(:)
        end function

        ! ---- non-default lower bounds ----
        integer(c_int32_t) function c22_sum_nondefault(a) &
                bind(C, name="c22_sum_nondefault")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function

        integer(c_int) function c22_get_extent(a) &
                bind(C, name="c22_get_extent")
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function
    end interface
end module

program bindc_22
    use bindc_22_mod
    use bindc_22_types
    use iso_c_binding, only: c_int32_t
    implicit none

    call test_negative_stride()
    call test_2d_sections()
    call test_derived_type_array()
    call test_nondefault_bounds()

    print *, "All bindc_22 tests passed."

contains

    subroutine test_negative_stride()
        integer(c_int32_t) :: arr(6)

        arr = [1, 2, 3, 4, 5, 6]

        ! Reverse: [6, 5, 4, 3, 2, 1]
        if (c22_sum_1d(arr(6:1:-1)) /= 21) error stop "FAIL: neg stride sum"

        ! Reverse stride-2: [6, 4, 2]
        if (c22_sum_1d(arr(6:1:-2)) /= 12) error stop "FAIL: neg stride-2"
    end subroutine

    subroutine test_2d_sections()
        integer(c_int32_t) :: a(4,4)
        integer :: i, j

        do j = 1, 4
            do i = 1, 4
                a(i,j) = (j-1)*4 + i
            end do
        end do

        ! Take a 2x2 sub-block: a(2:3, 2:3) = [[6,7],[10,11]]
        if (c22_sum_2d(a(2:3, 2:3)) /= 34) error stop "FAIL: 2d section"

        ! Take stride-2 in first dim: a(1::2, :) = rows 1,3 for all cols
        ! = [1,3,5,7,9,11,13,15] -> sum = 64
        if (c22_sum_2d(a(1::2, :)) /= 64) error stop "FAIL: 2d stride section"
    end subroutine

    subroutine test_derived_type_array()
        type(point_t) :: pts(3)
        integer(c_int32_t) :: expected_size

        pts(1) = point_t(1, 10)
        pts(2) = point_t(2, 20)
        pts(3) = point_t(3, 30)

        ! sum of x+y for all points = (1+10) + (2+20) + (3+30) = 66
        if (c22_sum_points(pts) /= 66) error stop "FAIL: derived type sum"

        ! elem_len should be sizeof(point_t) = 2 * sizeof(int32) = 8
        expected_size = 8
        if (c22_point_elem_size(pts) /= expected_size) &
            error stop "FAIL: derived type elem_size"
    end subroutine

    subroutine test_nondefault_bounds()
        integer(c_int32_t) :: a0(0:4)    ! 5 elements, 0-based
        integer(c_int32_t) :: an(-2:2)   ! 5 elements, negative-based
        integer :: i

        do i = 0, 4
            a0(i) = i + 1
        end do
        ! a0 = [1, 2, 3, 4, 5], sum = 15
        if (c22_sum_nondefault(a0) /= 15) error stop "FAIL: 0-based sum"
        if (c22_get_extent(a0) /= 5) error stop "FAIL: 0-based extent"

        do i = -2, 2
            an(i) = i + 3
        end do
        ! an = [1, 2, 3, 4, 5], sum = 15
        if (c22_sum_nondefault(an) /= 15) error stop "FAIL: neg-based sum"
        if (c22_get_extent(an) /= 5) error stop "FAIL: neg-based extent"
    end subroutine

end program
