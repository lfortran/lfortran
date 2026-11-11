program complex128_parameter_array_01
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    integer, parameter :: n = 3
    complex(qp), parameter :: z = (0.5_qp, -0.25_qp)
    complex(qp), parameter :: w(n) = [cmplx(1, -1, kind=qp), cmplx(2, -2, kind=qp), cmplx(3, -3, kind=qp)]
    complex(qp), parameter :: u(2) = [cmplx(1.5_qp, 2.5_qp, kind=qp), z]
    complex(qp), parameter :: zz(2) = [z, 2 * z]
    complex(qp) :: r(n)

    if (w(1) /= (1, -1)) error stop "w(1)"
    if (w(n) /= (3, -3)) error stop "w(n)"
    if (u(1) /= (1.5_qp, 2.5_qp) .or. u(2) /= z) error stop "u"
    if (zz(2) /= (1.0_qp, -0.5_qp)) error stop "zz"
    if (size(w) /= n) error stop "size"
    if (sum(w) /= (6, -6)) error stop "sum"
    r = w * 2
    if (r(3) /= (6, -6)) error stop "runtime use"
    print *, "ok"
end program
