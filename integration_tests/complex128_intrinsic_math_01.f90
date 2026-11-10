program complex128_intrinsic_math_01
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    real(qp), parameter :: tol = 1.0e-30_qp
    complex(qp) :: z, w, one
    complex(qp), parameter :: i_ = cmplx(0, 1, kind=qp)

    z = (0.7_qp, 0.3_qp)
    one = (1, 0)

    ! Inverse pairs
    if (abs(sqrt(z)**2 - z) > tol) error stop "sqrt"
    if (abs(exp(log(z)) - z) > tol) error stop "exp log"
    if (abs(log(exp(z)) - z) > tol) error stop "log exp"
    if (abs(sin(asin(z)) - z) > tol) error stop "asin"
    if (abs(cos(acos(z)) - z) > tol) error stop "acos"
    if (abs(tan(atan(z)) - z) > tol) error stop "atan"
    if (abs(sinh(asinh(z)) - z) > tol) error stop "asinh"
    if (abs(cosh(acosh(z)) - z) > tol) error stop "acosh"
    if (abs(tanh(atanh(z)) - z) > tol) error stop "atanh"

    ! Identities
    if (abs(sin(z)**2 + cos(z)**2 - one) > tol) error stop "sin2+cos2"
    if (abs(cosh(z)**2 - sinh(z)**2 - one) > tol) error stop "cosh2-sinh2"
    if (abs(tan(z) - sin(z) / cos(z)) > tol) error stop "tan"
    if (abs(tanh(z) - sinh(z) / cosh(z)) > tol) error stop "tanh"
    if (abs(exp(i_ * z) - (cos(z) + i_ * sin(z))) > tol) error stop "euler"
    if (abs(sin(i_ * z) - i_ * sinh(z)) > tol) error stop "sin i"

    ! Known values
    w = exp(i_ * 3.14159265358979323846264338327950288_qp)
    if (abs(w + 1) > tol) error stop "exp(i pi)"
    w = sqrt(cmplx(-4, 0, kind=qp))
    if (abs(w - 2 * i_) > tol) error stop "sqrt(-4)"
    w = log(cmplx(-1, 0, kind=qp))
    if (abs(w - 3.14159265358979323846264338327950288_qp * i_) > tol) error stop "log(-1)"
    w = exp(cmplx(1, 0, kind=qp))
    if (abs(real(w) - 2.71828182845904523536028747135266250_qp) > tol) error stop "e"
    print *, "ok"
end program
