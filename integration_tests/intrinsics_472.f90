program intrinsics_472
    ! Test co_min intrinsic subroutine (no-op in single-image mode)
    implicit none
    integer :: a(3), b
    real :: c(2)
    character(3) :: d, e(2)

    a = [1, 2, 3]
    call co_min(a)
    if (a(1) /= 1) error stop
    if (a(2) /= 2) error stop
    if (a(3) /= 3) error stop

    b = 42
    call co_min(b)
    if (b /= 42) error stop

    c = [1.5, 2.5]
    call co_min(c)
    if (abs(c(1) - 1.5) > 1e-6) error stop
    if (abs(c(2) - 2.5) > 1e-6) error stop

    d = "abc"
    call co_min(d)
    if (d /= "abc") error stop

    e = ["abc", "xyz"]
    call co_min(e)
    if (e(1) /= "abc") error stop
    if (e(2) /= "xyz") error stop

    print *, "co_min: all tests passed"
end program intrinsics_472
