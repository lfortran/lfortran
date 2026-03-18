! Test: C creates allocatable arrays via CFI_allocate and passes to Fortran
!
! Covers:
!   - Unallocated descriptor -> Fortran checks allocated() == .false.
!   - CFI_allocate with default bounds [1:N]
!   - CFI_allocate with non-default bounds [-2:2]
!   - Element access with non-default bounds
!   - 2D allocatable from C
!   - Real(c_double) allocatable from C
!   - Intent(out): Fortran allocates into C descriptor
!   - Intent(inout): Fortran reallocates C-allocated array
!   - Zero-size allocation
module bindc_31_mod
    use iso_c_binding
    implicit none
contains

    integer(c_int) function f31_is_allocated(a) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        f31_is_allocated = 0
        if (allocated(a)) f31_is_allocated = 1
    end function

    integer(c_int32_t) function f31_sum_1d(a) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        f31_sum_1d = sum(a)
    end function

    integer(c_int) function f31_size_1d(a) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        f31_size_1d = size(a)
    end function

    integer(c_int) function f31_lbound_1d(a) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        f31_lbound_1d = lbound(a, 1)
    end function

    integer(c_int) function f31_ubound_1d(a) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        f31_ubound_1d = ubound(a, 1)
    end function

    integer(c_int32_t) function f31_get_elem(a, idx) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        integer(c_int), value, intent(in) :: idx
        f31_get_elem = a(idx)
    end function

    integer(c_int32_t) function f31_sum_2d(a) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:,:)
        f31_sum_2d = sum(a)
    end function

    integer(c_int) function f31_2d_shape_ok(a, n1, n2) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:,:)
        integer(c_int), value, intent(in) :: n1, n2
        f31_2d_shape_ok = 0
        if (size(a, 1) == n1 .and. size(a, 2) == n2) f31_2d_shape_ok = 1
    end function

    real(c_double) function f31_sum_double(a) bind(C)
        real(c_double), allocatable, intent(in) :: a(:)
        f31_sum_double = sum(a)
    end function

    subroutine f31_alloc_fill(a) bind(C)
        integer(c_int32_t), allocatable, intent(out) :: a(:)
        allocate(a(3))
        a = [10, 20, 30]
    end subroutine

    subroutine f31_realloc(a) bind(C)
        integer(c_int32_t), allocatable, intent(inout) :: a(:)
        if (allocated(a)) deallocate(a)
        allocate(a(5))
        a = [100, 200, 300, 400, 500]
    end subroutine

end module

program bindc_31
    use iso_c_binding
    implicit none

    interface
        integer(c_int) function c31_run_tests() bind(C)
            import :: c_int
        end function
    end interface

    integer(c_int) :: res
    res = c31_run_tests()
    if (res /= 0) then
        print *, "C test failed with code:", res
        error stop "FAIL: C allocatable tests failed"
    end if
    print *, "All bindc_31 tests passed."
end program
