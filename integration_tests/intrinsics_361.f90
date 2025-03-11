program intrinsics_1arg
    implicit none

    real :: x1
    complex :: z1
    integer :: i1
    character(len=10) :: str
    real, parameter :: eps = 1e-8

    x1 = abs(a = -4.0)
    print *, x1
    if (abs(x1 - 4.0) > eps) error stop

    x1 = acos(x = 0.5)
    print *, x1
    if (abs(x1 - 1.04719755) > eps) error stop

    x1 = acosd(x = 0.5)
    print *, x1
    if (abs(x1 - 60.0) > eps) error stop

    x1 = acosh(x = 1.5)
    print *, x1
    if (abs(x1 - 0.962423623) > eps) error stop

    str = adjustl(string = "text")
    print *, str

    str = adjustr(string = "text")
    print *, str 

    x1 = aimag(z = (1.0, 2.0))
    print *, x1
    if (abs(x1 - 2.0) > eps) error stop

    x1 = asin(x = -1.0)
    print *, x1
    if (abs(x1 - (-1.57079637e+00)) > eps) error stop

    x1 = asind(x = 0.5)
    print *, x1
    if (abs(x1 - 30.0) > eps) error stop

    x1 = asinh(x = 0.5)
    print *, x1
    if (abs(x1 - 0.481211811) > eps) error stop

    x1 = atan(x = 1.0)
    print *, x1
    if (abs(x1 - 0.78539816) > eps) error stop

    x1 = atanh(x = 0.5)
    print *, x1
    if (abs(x1 - 0.549306154) > eps) error stop

    i1 = bit_size(i = 8)
    print *, i1
    if (i1 /= 32) error stop

    z1 = conjg(z = (3.0, -4.0))
    print *, z1

    x1 = cos(x = 0.0)
    print *, x1
    if (abs(x1 - 1.0) > eps) error stop

    x1 = cosd(x = 60.0)
    print *, x1
    if (abs(x1 - 0.5) > eps) error stop

    x1 = cosh(x = 1.0)
    print *, x1
    if (abs(x1 - 1.54308069) > eps) error stop

    x1 = erf(x = 1.0)
    print *, x1

    x1 = erfc(x = 1.0)
    print *, x1

    x1 = exp(x = 1.0)
    print *, x1
    if (abs(x1 - 2.71828175) > eps) error stop

    x1 = gamma(x = 0.5)
    print *, x1
    if (abs(x1 - 1.77245390) > eps) error stop

    x1 = log(x = 2.0)
    print *, x1
    if (abs(x1 - 0.693147182) > eps) error stop

    x1 = log10(x = 10.0)
    print *, x1
    if (abs(x1 - 1.0) > eps) error stop

    x1 = log_gamma(x = 2.0)
    print *, x1

    x1 = sin(x = 1.0)
    print *, x1
    if (abs(x1 - 0.841470957) > eps) error stop

    x1 = sinh(x = 1.0)
    print *, x1
    if (abs(x1 - 1.17520118) > eps) error stop

    x1 = sqrt(x = 4.0)
    print *, x1
    if (abs(x1 - 2) > eps) error stop

    x1 = tan(x = 1.0)
    print *, x1
    if (abs(x1 - 1.55740774) > eps) error stop

    x1 = tanh(x = 1.0)
    print *, x1
    if (abs(x1 - 0.761594176) > eps) error stop

    x1 = tand(x = 45.0)
    print *, x1
    if (abs(x1 - 1) > eps) error stop

    x1 = not(i = 0)
    print *, x1

    i1 = precision(x = 1.0)
    print *, i1

    i1 = range(x = 1.0)
    print *, i1

    x1 = tiny(x = 1.0)
    print *, x1
    if (abs(x1 - 1.17549435e-38) > eps) error stop

    x1 = huge(x = 1.0)
    print *, x1
    if (abs(x1 - 3.40282347e+38) > eps) error stop

    x1 = epsilon(x = 1.0)
    print *, x1
    if (abs(x1 - 1.19209290e-07) > eps) error stop

    i1 = digits(x = 1.0)
    print *, i1
    if (i1 /= 24) error stop

    i1 = leadz(i = 8)
    print *, i1
    if (i1 /= 28) error stop

    i1 = trailz(i = 8)
    print *, i1
    if (i1 /= 3) error stop

end program intrinsics_1arg
