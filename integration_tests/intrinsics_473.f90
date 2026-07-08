program intrinsics_473
    ! Test maxloc/minloc compile-time evaluation on multi-dimensional
    ! parameter arrays with mask, and element-wise comparison of the result.
    implicit none

    call test_maxloc_2d_mask_all_false()
    call test_minloc_2d_mask_all_false()
    call test_maxloc_parameter_compare()

    print *, "All tests passed."

contains

    subroutine test_maxloc_2d_mask_all_false()
        ! When no mask element is true, maxloc must return all zeros.
        integer, parameter :: x(1, 1) = 1
        integer, parameter :: a(2) = maxloc(x, mask=x > 1)
        ! a must be [0, 0]
        if (any(a /= [0, 0])) error stop 1
    end subroutine

    subroutine test_minloc_2d_mask_all_false()
        ! When no mask element is true, minloc must return all zeros.
        integer, parameter :: z(1, 1) = 10
        integer, parameter :: c(2) = minloc(z, mask=z < 0)
        if (any(c /= [0, 0])) error stop 2
    end subroutine

    subroutine test_maxloc_parameter_compare()
        ! The original reproducer: comparison of parameter array from maxloc.
        integer, parameter :: x(1, 1) = 1
        integer, parameter :: a(2) = maxloc(x, mask=x > 1)
        logical :: res(2)
        res = a /= [0, 0]
        if (any(res)) error stop 3
        res = a == [0, 0]
        if (.not. all(res)) error stop 4
    end subroutine

end program