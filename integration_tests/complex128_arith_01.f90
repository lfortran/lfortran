program complex128_arith_01
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    real(qp), parameter :: tol = 1.0e-30_qp
    complex(qp) :: z, w, r
    real(qp) :: a

    if (qp /= 16) error stop "qp"
    z = (1.5_qp, 2.5_qp)
    w = cmplx(3.0_qp, -1.0_qp, kind=qp)
    if (kind(z) /= 16) error stop "kind"

    r = z + w
    if (real(r) /= 4.5_qp .or. aimag(r) /= 1.5_qp) error stop "add"
    r = z - w
    if (real(r) /= -1.5_qp .or. aimag(r) /= 3.5_qp) error stop "sub"
    r = z * w
    if (real(r) /= 7.0_qp .or. aimag(r) /= 6.0_qp) error stop "mul"
    r = z / w
    if (abs(real(r) - 0.2_qp) > tol .or. abs(aimag(r) - 0.9_qp) > tol) error stop "div"

    r = -z
    if (real(r) /= -1.5_qp .or. aimag(r) /= -2.5_qp) error stop "neg"
    r = conjg(z)
    if (real(r) /= 1.5_qp .or. aimag(r) /= -2.5_qp) error stop "conjg"

    a = abs(w)
    if (abs(a - sqrt(10.0_qp)) > tol) error stop "abs"
    if (abs(abs(z)**2 - 8.5_qp) > tol) error stop "abs2"

    if (.not. (z == (1.5_qp, 2.5_qp))) error stop "eq"
    if (z /= z) error stop "ne"
    if (z == w) error stop "ne2"

    ! Mixed kinds and integers
    r = z * 2
    if (real(r) /= 3.0_qp .or. aimag(r) /= 5.0_qp) error stop "mul int"
    r = 2 * z + 1.0
    if (real(r) /= 4.0_qp .or. aimag(r) /= 5.0_qp) error stop "mixed"
    r = z * 0.5_8
    if (real(r) /= 0.75_qp .or. aimag(r) /= 1.25_qp) error stop "mixed r8"
    r = cmplx((1.0, 2.0), kind=qp)
    if (real(r) /= 1.0_qp .or. aimag(r) /= 2.0_qp) error stop "cmplx c4"
    r = cmplx(1.0_8, 2.0_8, qp)
    if (real(r) /= 1.0_qp .or. aimag(r) /= 2.0_qp) error stop "cmplx r8"
    if (cmplx(z, kind=4) /= (1.5, 2.5)) error stop "to c4"
    if (cmplx(z, kind=8) /= (1.5_8, 2.5_8)) error stop "to c8"
    if (real(z, 8) /= 1.5_8 .or. dble(z) /= 1.5_8) error stop "real(z, 8)"
    if (real(z) /= 1.5_qp .or. kind(real(z)) /= 16) error stop "real kind"
    if (aimag(z) /= 2.5_qp .or. kind(aimag(z)) /= 16) error stop "aimag kind"

    ! Powers
    r = z**2
    if (abs(real(r) + 4.0_qp) > tol .or. abs(aimag(r) - 7.5_qp) > tol) error stop "pow 2"
    r = z**(-2)
    if (abs(r * z**2 - 1) > tol) error stop "pow -2"
    r = z**0
    if (r /= (1, 0)) error stop "pow 0"
    r = z**w
    if (abs(r - exp(w * log(z))) > tol * 100) error stop "pow complex"
    r = z**0.5_qp
    if (abs(r * r - z) > tol * 100) error stop "pow real"

    ! Inquiry
    if (precision(z) /= 33) error stop "precision"
    if (range(z) /= 4931) error stop "range"
    if (epsilon(a) > 2.0e-34_qp) error stop "epsilon"
    print *, "ok"
end program
