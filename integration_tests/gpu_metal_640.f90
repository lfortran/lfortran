program gpu_metal_640
    ! SUM of a user elemental function call combined with a scalar
    ! (an integer power, a scalar factor) inside an offloaded
    ! do concurrent, directly and in a pure function called from it,
    ! and of an array-valued intrinsic combined with a scalar.
    implicit none
    integer :: x(3), n, p, c(2)
    real :: t(2), u(2), v(2), w(2), z(2), b(4), d(2), e(2)
    x = [1, 2, 3]
    p = 3
    do concurrent (n = 1:2)
        t(n) = sum(f(x)**2)
        u(n) = sum(f(x)*2) + n
    end do
    if (any(abs(t - 14.0) > 1e-3)) error stop 1
    if (any(abs(u - [13.0, 14.0]) > 1e-3)) error stop 2
    do concurrent (n = 1:2)
        v(n) = squares(x)
        w(n) = powers(x, p)
        z(n) = sum(f(x)**2.0 - f(x)) * n
    end do
    if (any(abs(v - 14.0) > 1e-3)) error stop 3
    if (any(abs(w - 36.0) > 1e-2)) error stop 4
    if (any(abs(z - [8.0, 16.0]) > 1e-3)) error stop 5
    b = [1.0, -2.0, 3.0, -4.0]
    do concurrent (n = 1:2)
        c(n) = sum(cshift(x, n)**2)
        d(n) = sum(cshift(b, n)**2)
        e(n) = sum(cshift(b, n)*2) + n
    end do
    if (any(c /= 14)) error stop 6
    if (any(abs(d - 30.0) > 1e-3)) error stop 7
    if (any(abs(e - [-3.0, -2.0]) > 1e-3)) error stop 8
contains
    elemental real function f(i)
        integer, intent(in) :: i
        f = real(i)
    end function
    pure real function squares(values)
        integer, intent(in) :: values(:)
        squares = sum(f(values)**2)
    end function
    pure real function powers(values, k)
        integer, intent(in) :: values(:), k
        powers = sum(f(values)**k)
    end function
end program
