program intrinsics_424
    ! Test co_sum intrinsic subroutine (no-op in single-image mode)
    implicit none
    integer :: a(3), b
    real :: c(2)

    a = [1, 2, 3]
    call co_sum(a)
    if (a(1) /= 1) error stop
    if (a(2) /= 2) error stop
    if (a(3) /= 3) error stop

    b = 42
    call co_sum(b)
    if (b /= 42) error stop

    c = [1.5, 2.5]
    call co_sum(c)
    if (abs(c(1) - 1.5) > 1e-6) error stop
    if (abs(c(2) - 2.5) > 1e-6) error stop

    print *, "co_sum: all tests passed"
end program intrinsics_424
