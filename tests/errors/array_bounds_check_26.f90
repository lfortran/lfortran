module array_bounds_check_26_mod
    use iso_c_binding, only: c_float
    implicit none
contains
    subroutine bindc_assumed_size_count(n, a) bind(c)
        integer, intent(in) :: n
        real(c_float), intent(in) :: a(n, *)
        if (size(a, 1) /= n) error stop "size(a, 1)"
    end subroutine

    subroutine neg_extent(n, m, a)
        integer, intent(in) :: n, m
        real, intent(inout) :: a(n, m)
        if (size(a) /= 0) error stop "size(a)"
    end subroutine
end module

program array_bounds_check_26
    ! Strict mode must accept the same valid zero-size calls as the default
    ! mode: a zero-element actual for an assumed-size dummy, and a negative
    ! runtime extent (a zero-size dimension) for a fixed-size actual.
    use array_bounds_check_26_mod
    use iso_c_binding, only: c_float
    implicit none
    real(c_float) :: x(3, 0), y(4, 0)
    real :: z(0, 0)
    call bindc_assumed_size_count(3, x)
    call bindc_assumed_size_count(4, y)
    call neg_extent(-2, -2, z)
    print *, "pass"
end program
