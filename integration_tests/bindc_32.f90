! Test: C creates pointer associations via CFI_setpointer and passes to Fortran
!
! Covers:
!   - Disassociated pointer -> Fortran checks associated() == .false.
!   - CFI_setpointer to associate with data
!   - Disassociate via CFI_setpointer(result, NULL, NULL)
!   - Custom lower bounds via CFI_setpointer
!   - Re-pointing to different target
!   - 2D pointer
!   - Modify target data through pointer
module bindc_32_mod
    use iso_c_binding
    implicit none
contains

    integer(c_int) function f32_is_associated(a) bind(C)
        integer(c_int32_t), pointer, intent(in) :: a(:)
        f32_is_associated = 0
        if (associated(a)) f32_is_associated = 1
    end function

    integer(c_int32_t) function f32_sum_1d(a) bind(C)
        integer(c_int32_t), pointer, intent(in) :: a(:)
        f32_sum_1d = sum(a)
    end function

    integer(c_int) function f32_lbound_1d(a) bind(C)
        integer(c_int32_t), pointer, intent(in) :: a(:)
        f32_lbound_1d = lbound(a, 1)
    end function

    integer(c_int) function f32_ubound_1d(a) bind(C)
        integer(c_int32_t), pointer, intent(in) :: a(:)
        f32_ubound_1d = ubound(a, 1)
    end function

    integer(c_int32_t) function f32_sum_2d(a) bind(C)
        integer(c_int32_t), pointer, intent(in) :: a(:,:)
        f32_sum_2d = sum(a)
    end function

    subroutine f32_double_values(a) bind(C)
        integer(c_int32_t), pointer, intent(inout) :: a(:)
        a = a * 2
    end subroutine

end module

program bindc_32
    use iso_c_binding
    implicit none

    interface
        integer(c_int) function c32_run_tests() bind(C)
            import :: c_int
        end function
    end interface

    integer(c_int) :: res
    res = c32_run_tests()
    if (res /= 0) then
        print *, "C test failed with code:", res
        error stop "FAIL: C pointer tests failed"
    end if
    print *, "All bindc_32 tests passed."
end program
