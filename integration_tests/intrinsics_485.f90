program intrinsics_485
    ! cshift of arrays whose lower bounds are named constants or differ from 1
    implicit none
    integer, parameter :: lo = 1, lo3 = 3
    integer :: x(lo:2)
    integer :: y(lo3:lo3+2), z(3)
    integer :: w(5:7)
    integer :: m(lo3:lo3+1, 0:2), r(2, 3)
    integer :: s(lo:3), t(lo3:lo3+1)

    x = [1, 2]
    x = cshift(x, 1)
    if (any(x /= [2, 1])) error stop

    y = [10, 20, 30]
    z = cshift(y, 1)
    if (any(z /= [20, 30, 10])) error stop
    y = cshift(y, -1)
    if (any(y /= [30, 10, 20])) error stop
    if (lbound(cshift(y, 1), 1) /= 1) error stop
    if (ubound(cshift(y, 1), 1) /= 3) error stop

    w = [1, 2, 3]
    w = cshift(w, 2)
    if (any(w /= [3, 1, 2])) error stop

    r = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    m = r
    r = cshift(m, 1, 2)
    if (any(r /= reshape([3, 4, 5, 6, 1, 2], [2, 3]))) error stop

    s = [1, 0, -1]
    r = cshift(m, s)
    if (any(r /= reshape([2, 1, 3, 4, 6, 5], [2, 3]))) error stop

    t = [1, 2]
    r = cshift(m, t, 2)
    if (any(r /= reshape([3, 6, 5, 2, 1, 4], [2, 3]))) error stop

    print *, x, y, w
end program
