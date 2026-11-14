program real128_array_const_03
    ! Operations on real(16) and complex(16) parameter arrays that are not
    ! folded at compile time are evaluated at run time
    implicit none
    real(16), parameter :: m(4) = [1.5_16, 2.0_16, 0.25_16, 4.0_16]
    real(16), parameter :: n(4) = [2.5_16, 1.0_16, 0.5_16, 3.0_16]
    complex(16), parameter :: a(2) = [(1.5_16, 2.25_16), (2.0_16, 1.0_16)]
    complex(16), parameter :: z = (0.5_16, -0.25_16)
    real(16), parameter :: tol = 1.0e-30_16
    real(16) :: r(4), mv(4), nv(4)
    complex(16) :: c(2), av(2), zv
    integer :: i(1)

    mv = m
    nv = n

    r = max(m, n)
    if (any(abs(r - [2.5_16, 2.0_16, 0.5_16, 4.0_16]) > tol)) error stop 1
    r = min(m, n)
    if (any(abs(r - [1.5_16, 1.0_16, 0.25_16, 3.0_16]) > tol)) error stop 2
    r = max(m, m)
    if (any(abs(r - mv) > tol)) error stop 3
    r = min(m, 3.0_16)
    if (any(abs(r - min(mv, 3.0_16)) > tol)) error stop 4
    r = -m
    if (any(abs(r + mv) > tol)) error stop 5
    r = m / n
    if (any(abs(r - mv / nv) > tol)) error stop 6
    r = m ** 2
    if (any(abs(r - mv * mv) > tol)) error stop 7
    r = atan2(m, n)
    if (any(abs(r - atan2(mv, nv)) > tol)) error stop 8
    r = hypot(m, n)
    if (any(abs(r - hypot(mv, nv)) > tol)) error stop 9

    i = maxloc(m)
    if (i(1) /= 4) error stop 10
    i = minloc(m)
    if (i(1) /= 3) error stop 11
    i = findloc(m, 2.0_16)
    if (i(1) /= 2) error stop 12
    if (abs(norm2(m) - sqrt(sum(mv * mv))) > tol) error stop 13

    av = a
    zv = z
    c = a / a
    if (any(abs(c - (1.0_16, 0.0_16)) > tol)) error stop 14
    c = a ** 2
    if (any(abs(c - av * av) > tol)) error stop 15
    c = a / z
    if (any(abs(c - av / zv) > tol)) error stop 16
    c = -a
    if (any(abs(c + av) > tol)) error stop 17
    c = a ** a
    if (any(abs(c - av ** av) > 1.0e-28_16)) error stop 18
    print *, "ok"
end program
