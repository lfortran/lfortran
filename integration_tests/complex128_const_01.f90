program complex128_const_01
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    real(qp), parameter :: tol = 1.0e-30_qp
    real(qp), parameter :: eps = 1.0e-20_qp
    ! Parts that a double cannot represent
    complex(qp), parameter :: c1 = cmplx(1.0_qp + eps, 2.0_qp - eps, qp)
    complex(qp), parameter :: c2 = (0.5_qp, -0.25_qp)
    complex(qp), parameter :: c3 = c1 + c2
    complex(qp), parameter :: c4 = c1 - c2
    complex(qp), parameter :: c5 = c1 * c2
    complex(qp), parameter :: c6 = -c1
    complex(qp), parameter :: c7 = cmplx(1.0_qp + eps, 3.0_qp, kind=qp)
    complex(qp), parameter :: c9 = (3, 4)
    complex(4), parameter :: s1 = cmplx(c2, kind=4)
    complex(8), parameter :: d1 = cmplx(c2, kind=8)
    real(qp), parameter :: r1 = real(c1)
    real(qp), parameter :: r2 = c1%im
    real(qp), parameter :: r3 = c1%re
    real(qp), parameter :: r4 = c1%im
    real(qp) :: r5
    complex(qp), parameter :: arr(2) = [c1, c2]
    complex(qp), save :: sv = cmplx(1.0_qp + eps, 2.0_qp, qp)
    complex(qp) :: z, w, c8

    if (kind(c1) /= 16) error stop "kind"
    if (abs((r1 - 1.0_qp) - eps) > tol) error stop "real(c1)"
    if (abs((r2 - 2.0_qp) + eps) > tol) error stop "aimag(c1)"
    if (r3 /= r1 .or. r4 /= r2) error stop "c1%re, c1%im"
    if (aimag(c1) /= r2) error stop "aimag"

    if (abs(real(c3) - (1.5_qp + eps)) > tol .or. abs(aimag(c3) - (1.75_qp - eps)) > tol) error stop "add"
    if (abs(real(c4) - (0.5_qp + eps)) > tol .or. abs(aimag(c4) - (2.25_qp - eps)) > tol) error stop "sub"
    if (abs(real(c5) - (1.0_qp + 0.25_qp * eps)) > tol) error stop "mul re"
    if (abs(aimag(c5) - (0.75_qp - 0.75_qp * eps)) > tol) error stop "mul im"
    if (real(c6) /= -1.0_qp - eps .or. aimag(c6) /= -2.0_qp + eps) error stop "neg"
    if (real(c7) /= 1.0_qp + eps .or. aimag(c7) /= 3.0_qp) error stop "cmplx"
    c8 = cmplx(1.0_8, 2.0_8, kind=qp)
    if (real(c8) /= 1.0_qp .or. aimag(c8) /= 2.0_qp) error stop "cmplx r8"
    if (real(c9) /= 3.0_qp .or. aimag(c9) /= 4.0_qp) error stop "integer parts"
    r5 = abs(c9)
    if (abs(r5 - 5.0_qp) > tol) error stop "abs"
    if (s1 /= (0.5, -0.25)) error stop "to c4"
    if (d1 /= (0.5_8, -0.25_8)) error stop "to c8"
    if (real(arr(1)) /= 1.0_qp + eps .or. aimag(arr(2)) /= -0.25_qp) error stop "array"
    if (real(sv) /= 1.0_qp + eps .or. aimag(sv) /= 2.0_qp) error stop "save"

    if (.not. (c1 == cmplx(1.0_qp + eps, 2.0_qp - eps, qp))) error stop "eq"
    if (c1 == (1.0_qp, 2.0_qp)) error stop "ne"
    if (conjg(c1) /= cmplx(1.0_qp + eps, -2.0_qp + eps, qp)) error stop "conjg"

    ! The same constants through variables
    z = c1
    w = z * c2
    if (real(z) /= 1.0_qp + eps .or. aimag(z) /= 2.0_qp - eps) error stop "assign"
    if (abs(real(w) - real(c5)) > tol .or. abs(aimag(w) - aimag(c5)) > tol) error stop "mul var"

    print *, c1
    print *, c9
end program
