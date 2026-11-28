program findloc_dim_01
    ! findloc with dim: mask is used, and arrays of rank 1 to 3 work
    implicit none
    integer :: g(2, 3), a(2, 3, 2), v(5), i
    integer :: r2(2), r22(2, 2), r23(2, 3)
    logical :: m(2, 3), ma(2, 3, 2), mv(5)

    g = reshape([4, 9, 7, 1, 7, 8], [2, 3])
    m = .true.
    m(1, 2) = .false.
    r2 = findloc(g, 7, dim=2, mask=m)
    if (any(r2 /= [3, 0])) error stop 1
    r2 = findloc(g, 7, dim=2, mask=m, back=.true.)
    if (any(r2 /= [3, 0])) error stop 2
    r2 = findloc(g, 7, dim=2)
    if (any(r2 /= [2, 0])) error stop 3

    v = [1, 7, 3, 7, 5]
    mv = [.true., .false., .true., .true., .true.]
    if (findloc(v, 7, dim=1, mask=mv) /= 4) error stop 4
    if (findloc(v, 7, dim=1, mask=.false.) /= 0) error stop 5
    if (findloc(v, 7, dim=1, back=.true.) /= 4) error stop 6

    a = reshape([(mod(i * 7, 11), i = 1, 12)], [2, 3, 2])
    r22 = findloc(a, 3, dim=2)
    if (any(r22 /= reshape([0, 1, 0, 0], [2, 2]))) error stop 7
    r23 = findloc(a, 3, dim=3, back=.true.)
    if (any(r23 /= reshape([0, 1, 0, 0, 0, 0], [2, 3]))) error stop 8
    ma = a /= 3
    r22 = findloc(a, 3, dim=2, mask=ma)
    if (any(r22 /= 0)) error stop 9
    r23 = findloc(a, 7, dim=3)
    if (any(r23 /= reshape([1, 0, 0, 0, 0, 2], [2, 3]))) error stop 10
    print *, "ok"
end program
