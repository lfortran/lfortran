! Test: C calling Fortran bind(C) procedures with descriptors
!
! Covers gap items C.1-C.15:
!   - C calling Fortran subroutine with assumed-shape arrays
!   - C calling Fortran with assumed-rank
!   - C calling Fortran with allocatable args
!   - C calling Fortran with pointer args
!   - C calling Fortran with optional args (present and absent)
!   - C calling Fortran with CONTIGUOUS args
!   - C calling Fortran with intent(out/inout) descriptor args
!   - C calling Fortran with multiple descriptor args
!   - C calling Fortran with derived type descriptor args
!   - C calling Fortran with complex/logical array descriptor args
!   - C calling Fortran function returning scalar
!   - C calling Fortran function returning complex
module bindc_25_types
    use iso_c_binding, only: c_int32_t, c_double
    implicit none

    type, bind(C) :: pair_t
        integer(c_int32_t) :: a
        integer(c_int32_t) :: b
    end type
end module

module bindc_25_fortran_procs
    use iso_c_binding
    use bindc_25_types
    implicit none
contains

    ! ---- assumed-shape subroutine (C calls this) ----
    subroutine f25_double_1d(a) bind(C, name="f25_double_1d")
        integer(c_int32_t), intent(inout) :: a(:)
        integer :: i
        do i = 1, size(a)
            a(i) = a(i) * 2
        end do
    end subroutine

    integer(c_int32_t) function f25_sum_1d(a) bind(C, name="f25_sum_1d")
        integer(c_int32_t), intent(in) :: a(:)
        integer :: i
        f25_sum_1d = 0
        do i = 1, size(a)
            f25_sum_1d = f25_sum_1d + a(i)
        end do
    end function

    integer(c_int32_t) function f25_sum_2d(a) bind(C, name="f25_sum_2d")
        integer(c_int32_t), intent(in) :: a(:,:)
        integer :: i, j
        f25_sum_2d = 0
        do j = 1, size(a, 2)
            do i = 1, size(a, 1)
                f25_sum_2d = f25_sum_2d + a(i, j)
            end do
        end do
    end function

    ! ---- assumed-rank (C calls this) ----
    integer(c_int) function f25_get_rank(a) bind(C, name="f25_get_rank")
        integer(c_int32_t), intent(in) :: a(..)
        f25_get_rank = rank(a)
    end function

    ! ---- allocatable arg (C calls this) ----
    integer(c_int32_t) function f25_sum_alloc(a) bind(C, name="f25_sum_alloc")
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        integer :: i
        f25_sum_alloc = 0
        if (allocated(a)) then
            do i = 1, size(a)
                f25_sum_alloc = f25_sum_alloc + a(i)
            end do
        end if
    end function

    ! ---- pointer arg (C calls this) ----
    integer(c_int32_t) function f25_sum_ptr(a) bind(C, name="f25_sum_ptr")
        integer(c_int32_t), pointer, intent(in) :: a(:)
        integer :: i
        f25_sum_ptr = 0
        if (associated(a)) then
            do i = 1, size(a)
                f25_sum_ptr = f25_sum_ptr + a(i)
            end do
        end if
    end function

    ! ---- optional arg (C calls this) ----
    integer(c_int) function f25_opt_present(a) bind(C, name="f25_opt_present")
        integer(c_int32_t), optional, intent(in) :: a(:)
        if (present(a)) then
            f25_opt_present = 1
        else
            f25_opt_present = 0
        end if
    end function

    ! ---- CONTIGUOUS arg (C calls this) ----
    integer(c_int32_t) function f25_sum_contig(a) &
            bind(C, name="f25_sum_contig")
        integer(c_int32_t), contiguous, intent(in) :: a(:)
        integer :: i
        f25_sum_contig = 0
        do i = 1, size(a)
            f25_sum_contig = f25_sum_contig + a(i)
        end do
    end function

    ! ---- intent(out) descriptor (C calls, Fortran fills) ----
    subroutine f25_fill_array(a) bind(C, name="f25_fill_array")
        integer(c_int32_t), intent(out) :: a(:)
        integer :: i
        do i = 1, size(a)
            a(i) = i * 10
        end do
    end subroutine

    ! ---- multiple descriptor args (C calls this) ----
    integer(c_int32_t) function f25_dot(a, b) bind(C, name="f25_dot")
        integer(c_int32_t), intent(in) :: a(:), b(:)
        integer :: i
        f25_dot = 0
        do i = 1, size(a)
            f25_dot = f25_dot + a(i) * b(i)
        end do
    end function

    subroutine f25_add(a, b, c) bind(C, name="f25_add")
        integer(c_int32_t), intent(in) :: a(:), b(:)
        integer(c_int32_t), intent(out) :: c(:)
        integer :: i
        do i = 1, size(a)
            c(i) = a(i) + b(i)
        end do
    end subroutine

    ! ---- derived type array (C calls this) ----
    integer(c_int32_t) function f25_sum_pairs(pts) &
            bind(C, name="f25_sum_pairs")
        type(pair_t), intent(in) :: pts(:)
        integer :: i
        f25_sum_pairs = 0
        do i = 1, size(pts)
            f25_sum_pairs = f25_sum_pairs + pts(i)%a + pts(i)%b
        end do
    end function

    ! ---- complex array (C calls this) ----
    subroutine f25_sum_complex(a, re, im) bind(C, name="f25_sum_complex")
        complex(c_float_complex), intent(in) :: a(:)
        real(c_float), intent(out) :: re, im
        integer :: i
        re = 0.0
        im = 0.0
        do i = 1, size(a)
            re = re + real(a(i))
            im = im + aimag(a(i))
        end do
    end subroutine

    ! ---- logical array (C calls this) ----
    integer(c_int) function f25_count_true(a) &
            bind(C, name="f25_count_true")
        logical(c_bool), intent(in) :: a(:)
        integer :: i
        f25_count_true = 0
        do i = 1, size(a)
            if (a(i)) f25_count_true = f25_count_true + 1
        end do
    end function

    ! ---- function returning scalar (C calls this) ----
    integer(c_int32_t) function f25_square(x) bind(C, name="f25_square")
        integer(c_int32_t), value :: x
        f25_square = x * x
    end function

    ! ---- function returning complex (C calls this) ----
    complex(c_float_complex) function f25_conj(z) bind(C, name="f25_conj")
        complex(c_float_complex), value :: z
        f25_conj = conjg(z)
    end function

end module

module bindc_25_c_iface
    use iso_c_binding, only: c_int, c_int32_t
    implicit none

    interface
        integer(c_int) function c25_run_all_tests() &
                bind(C, name="c25_run_all_tests")
            import :: c_int
        end function
    end interface
end module

program bindc_25
    use bindc_25_fortran_procs
    use bindc_25_c_iface
    use iso_c_binding, only: c_int
    implicit none

    integer(c_int) :: result
    result = c25_run_all_tests()
    if (result /= 0) then
        print *, "C test failed with code:", result
        error stop "FAIL: C calling Fortran tests failed"
    end if

    print *, "All bindc_25 tests passed."
end program
