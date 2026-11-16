program complex10_parameter_array_01
    implicit none
    integer, parameter :: xp = selected_real_kind(18)
    integer, parameter :: n = 3
    complex(xp), parameter :: z = (0.5_xp, -0.25_xp)
    complex(xp), parameter :: w(n) = [cmplx(1, -1, kind=xp), cmplx(2, -2, kind=xp), cmplx(3, -3, kind=xp)]
    complex(xp), parameter :: u(2) = [cmplx(1.5_xp, 2.5_xp, kind=xp), z]
    complex(xp), parameter :: zz(2) = [z, 2 * z]
    complex(xp) :: r(n)

    if (w(1) /= (1, -1)) error stop "w(1)"
    if (w(n) /= (3, -3)) error stop "w(n)"
    if (u(1) /= (1.5_xp, 2.5_xp) .or. u(2) /= z) error stop "u"
    if (zz(2) /= (1.0_xp, -0.5_xp)) error stop "zz"
    if (size(w) /= n) error stop "size"
    if (sum(w) /= (6, -6)) error stop "sum"
    r = w * 2
    if (r(3) /= (6, -6)) error stop "runtime use"
    print *, "ok"
end program
