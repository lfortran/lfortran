program intrinsics_487
    ! cshift with an array shift whose lower bounds are named constants or
    ! differ from 1
    implicit none
    integer, parameter :: lo = 1, lo3 = 3
    integer :: m(lo3:lo3+1, 0:2), r(2, 3)
    integer :: s(lo:3), t(lo3:lo3+1), u(0:1)

    r = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    m = r

    s = [1, 0, -1]
    r = cshift(m, s)
    if (any(r /= reshape([2, 1, 3, 4, 6, 5], [2, 3]))) error stop

    t = [1, 2]
    r = cshift(m, t, 2)
    if (any(r /= reshape([3, 6, 5, 2, 1, 4], [2, 3]))) error stop

    u = [2, -1]
    r = cshift(m, u, 2)
    if (any(r /= reshape([5, 6, 1, 2, 3, 4], [2, 3]))) error stop

    print *, r
end program
