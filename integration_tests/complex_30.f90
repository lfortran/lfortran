program complex_ops_01
    ! Test that complex arithmetic works correctly
    ! This also serves to verify inline complex ops when implemented
    implicit none
    complex :: a, b, c, d
    real :: eps

    eps = 1.0e-6

    ! Test complex multiplication: (1+2i) * (3+4i) = (1*3-2*4) + (1*4+2*3)i = -5 + 10i
    a = (1.0, 2.0)
    b = (3.0, 4.0)
    c = a * b
    if (abs(real(c) - (-5.0)) > eps) error stop "mul real failed"
    if (abs(aimag(c) - 10.0) > eps) error stop "mul imag failed"

    ! Test complex addition: (1+2i) + (3+4i) = 4 + 6i
    c = a + b
    if (abs(real(c) - 4.0) > eps) error stop "add real failed"
    if (abs(aimag(c) - 6.0) > eps) error stop "add imag failed"

    ! Test complex subtraction: (1+2i) - (3+4i) = -2 - 2i
    c = a - b
    if (abs(real(c) - (-2.0)) > eps) error stop "sub real failed"
    if (abs(aimag(c) - (-2.0)) > eps) error stop "sub imag failed"

    ! Test complex division: (1+2i) / (3+4i) = (1*3+2*4)/(3^2+4^2) + (2*3-1*4)/(3^2+4^2)i
    ! = 11/25 + 2/25 i = 0.44 + 0.08i
    c = a / b
    if (abs(real(c) - 0.44) > eps) error stop "div real failed"
    if (abs(aimag(c) - 0.08) > eps) error stop "div imag failed"

    ! Test compound expression
    d = (a * b + a) / b - a
    ! = ((-5+10i) + (1+2i)) / (3+4i) - (1+2i)
    ! = (-4+12i) / (3+4i) - (1+2i)
    ! = ((-4*3+12*4)/25 + (12*3-(-4)*4)/25 i) - (1+2i)
    ! = (36/25 + 52/25 i) - (1+2i)
    ! = (1.44 - 1) + (2.08 - 2)i = 0.44 + 0.08i
    if (abs(real(d) - 0.44) > eps) error stop "compound real failed"
    if (abs(aimag(d) - 0.08) > eps) error stop "compound imag failed"

    print *, "PASS"
end program
