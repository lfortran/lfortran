program real10_elemental_fold_01
    ! exp, log and the trigonometric and hyperbolic functions of real(10)
    ! constants are folded with full precision, also elementwise
    implicit none
    real(10), parameter :: tol = 4 * epsilon(1.0_10)
    real(10), parameter :: s_exp = exp(0.5_10)
    real(10), parameter :: s_log = log(1.5_10)
    real(10), parameter :: s_cos = cos(0.5_10)
    real(10), parameter :: s_tan = tan(0.5_10)
    real(10), parameter :: s_asin = asin(0.5_10)
    real(10), parameter :: s_sinh = sinh(0.5_10)
    real(10), parameter :: s_cosh = cosh(0.5_10)
    real(10), parameter :: s_tanh = tanh(0.5_10)
    real(10), parameter :: s_asinh = asinh(0.5_10)
    real(10), parameter :: s_acosh = acosh(1.5_10)
    real(10), parameter :: s_atanh = atanh(0.5_10)
    real(10), parameter :: a_exp(2) = exp([0.0_10, 1.0_10])
    real(10), parameter :: a_log(2) = log([1.0_10, 1.5_10])
    real(10), parameter :: a_cos(2) = cos([0.0_10, 0.5_10])
    real(10), parameter :: a_tanh(2) = tanh([0.0_10, 0.5_10])
    real(10), parameter :: a_acosh(2) = acosh([1.0_10, 1.5_10])

    if (abs(s_exp - 1.648721270700128146893_10) > tol) error stop "exp"
    if (abs(s_log - 0.4054651081081643819860_10) > tol) error stop "log"
    if (abs(s_cos - 0.8775825618903727161303_10) > tol) error stop "cos"
    if (abs(s_tan - 0.5463024898437905132694_10) > tol) error stop "tan"
    if (abs(s_asin - 0.5235987755982988730674_10) > tol) error stop "asin"
    if (abs(s_sinh - 0.5210953054937473616403_10) > tol) error stop "sinh"
    if (abs(s_cosh - 1.127625965206380785253_10) > 2 * tol) error stop "cosh"
    if (abs(s_tanh - 0.4621171572600097584595_10) > tol) error stop "tanh"
    if (abs(s_asinh - 0.4812118250596034474994_10) > tol) error stop "asinh"
    if (abs(s_acosh - 0.9624236501192068950531_10) > tol) error stop "acosh"
    if (abs(s_atanh - 0.5493061443340548457345_10) > tol) error stop "atanh"

    if (a_exp(1) /= 1.0_10) error stop "exp 0"
    if (abs(a_exp(2) - 2.718281828459045235428_10) > 2 * tol) error stop "exp 1"
    if (a_log(1) /= 0.0_10 .or. a_log(2) /= s_log) error stop "log array"
    if (a_cos(1) /= 1.0_10 .or. a_cos(2) /= s_cos) error stop "cos array"
    if (a_tanh(1) /= 0.0_10 .or. a_tanh(2) /= s_tanh) error stop "tanh array"
    if (a_acosh(1) /= 0.0_10 .or. a_acosh(2) /= s_acosh) error stop "acosh array"
    print *, s_exp, a_exp
end program
