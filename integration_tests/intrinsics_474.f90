program intrinsics_474
    ! Test co_broadcast intrinsic subroutine (no-op in single-image mode)
    implicit none
    integer :: a(3), b
    real :: c(2)
    character(3) :: d, e(2)
    logical :: f
    complex :: g

    a = [1, 2, 3]
    call co_broadcast(a, source_image=1)
    if (a(1) /= 1) error stop
    if (a(2) /= 2) error stop
    if (a(3) /= 3) error stop

    b = 42
    call co_broadcast(b, 1)
    if (b /= 42) error stop

    c = [1.5, 2.5]
    call co_broadcast(c, source_image=1)
    if (abs(c(1) - 1.5) > 1e-6) error stop
    if (abs(c(2) - 2.5) > 1e-6) error stop

    d = "abc"
    call co_broadcast(d, 1)
    if (d /= "abc") error stop

    e = ["abc", "xyz"]
    call co_broadcast(e, source_image=1)
    if (e(1) /= "abc") error stop
    if (e(2) /= "xyz") error stop

    f = .true.
    call co_broadcast(f, 1)
    if (.not. f) error stop

    g = (1.0, -2.0)
    call co_broadcast(g, source_image=1)
    if (abs(real(g) - 1.0) > 1e-6) error stop
    if (abs(aimag(g) + 2.0) > 1e-6) error stop

    print *, "co_broadcast: all tests passed"
end program intrinsics_474
