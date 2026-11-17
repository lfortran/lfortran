program real10_elemental_scalar_fold_01
    ! exp, log and the trigonometric and hyperbolic functions of real(10)
    ! constants are folded with full precision
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
    real(10), parameter :: s_exp0 = exp(0.0_10), s_log1 = log(1.0_10)
    real(10) :: v_exp = exp(1.0_10), v_tanh = tanh(0.5_10)
    real(10) :: x

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
    if (s_exp0 /= 1.0_10 .or. s_log1 /= 0.0_10) error stop "exp 0, log 1"
    if (abs(v_exp - 2.718281828459045235428_10) > 2 * tol) error stop "exp 1"
    if (v_tanh /= s_tanh) error stop "tanh variable"

    ! The run time computes the same values
    x = 0.5_10
    if (abs(exp(x) - s_exp) > tol .or. abs(cosh(x) - s_cosh) > 2 * tol) error stop "rt"
    print *, s_exp, s_acosh
end program
