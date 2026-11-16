program complex10_arith_01
    implicit none
    integer, parameter :: xp = selected_real_kind(18)
    real(xp), parameter :: tol = 1.0e-17_xp
    complex(xp) :: z, w, r
    real(xp) :: a

    if (xp /= 10) error stop "xp"
    z = (1.5_xp, 2.5_xp)
    w = cmplx(3.0_xp, -1.0_xp, kind=xp)
    if (kind(z) /= 10) error stop "kind"

    r = z + w
    if (real(r) /= 4.5_xp .or. aimag(r) /= 1.5_xp) error stop "add"
    r = z - w
    if (real(r) /= -1.5_xp .or. aimag(r) /= 3.5_xp) error stop "sub"
    r = z * w
    if (real(r) /= 7.0_xp .or. aimag(r) /= 6.0_xp) error stop "mul"
    r = z / w
    if (abs(real(r) - 0.2_xp) > tol .or. abs(aimag(r) - 0.9_xp) > tol) error stop "div"

    r = -z
    if (real(r) /= -1.5_xp .or. aimag(r) /= -2.5_xp) error stop "neg"
    r = conjg(z)
    if (real(r) /= 1.5_xp .or. aimag(r) /= -2.5_xp) error stop "conjg"

    a = abs(w)
    if (abs(a - sqrt(10.0_xp)) > tol) error stop "abs"
    if (abs(abs(z)**2 - 8.5_xp) > tol) error stop "abs2"

    if (.not. (z == (1.5_xp, 2.5_xp))) error stop "eq"
    if (z /= z) error stop "ne"
    if (z == w) error stop "ne2"

    ! Mixed kinds and integers
    r = z * 2
    if (real(r) /= 3.0_xp .or. aimag(r) /= 5.0_xp) error stop "mul int"
    r = 2 * z + 1.0
    if (real(r) /= 4.0_xp .or. aimag(r) /= 5.0_xp) error stop "mixed"
    r = z * 0.5_8
    if (real(r) /= 0.75_xp .or. aimag(r) /= 1.25_xp) error stop "mixed r8"
    r = cmplx((1.0, 2.0), kind=xp)
    if (real(r) /= 1.0_xp .or. aimag(r) /= 2.0_xp) error stop "cmplx c4"
    r = cmplx(1.0_8, 2.0_8, xp)
    if (real(r) /= 1.0_xp .or. aimag(r) /= 2.0_xp) error stop "cmplx r8"
    if (cmplx(z, kind=4) /= (1.5, 2.5)) error stop "to c4"
    if (cmplx(z, kind=8) /= (1.5_8, 2.5_8)) error stop "to c8"
    if (real(z, 8) /= 1.5_8 .or. dble(z) /= 1.5_8) error stop "real(z, 8)"
    if (real(z) /= 1.5_xp .or. kind(real(z)) /= 10) error stop "real kind"
    if (aimag(z) /= 2.5_xp .or. kind(aimag(z)) /= 10) error stop "aimag kind"

    ! Powers
    r = z**2
    if (abs(real(r) + 4.0_xp) > tol .or. abs(aimag(r) - 7.5_xp) > tol) error stop "pow 2"
    r = z**(-2)
    if (abs(r * z**2 - 1) > tol) error stop "pow -2"
    r = z**0
    if (r /= (1, 0)) error stop "pow 0"
    r = z**w
    if (abs(r - exp(w * log(z))) > tol * 100) error stop "pow complex"
    r = z**0.5_xp
    if (abs(r * r - z) > tol * 100) error stop "pow real"

    ! Inquiry
    if (precision(z) /= 18) error stop "precision"
    if (range(z) /= 4931) error stop "range"
    if (epsilon(a) > 2.0e-19_xp) error stop "epsilon"
    print *, "ok"
end program
