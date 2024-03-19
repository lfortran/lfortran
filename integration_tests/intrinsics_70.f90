program intrinsics_70
    real :: x
    real, parameter :: epsilon = 1e-10
    real :: res_real(4)

    x = 4.23
    print *, aint(x)
    if (abs(aint(x) - 4.0) > epsilon) error stop

    x = -4.23
    print *, aint(x)
    if (abs(aint(x)  - (-4.0)) > epsilon) error stop

    print *, aint(0.0)
    if (abs(aint(0.0) - 0.0) > epsilon) error stop

    print *, aint(4.23)
    if (abs(aint(4.23) - 4.0) > epsilon) error stop

    print *, aint(-4.23, 4)
    if (abs(aint(-4.23, 4) - (-4.0)) > epsilon) error stop

    ! Compile time broadcasting
    res_real = aint([real :: 1.2, 3.5, 3.4, 2.1])
    print *, res_real
    if (abs(res_real(1) - 1.0) > epsilon) error stop
    print *, res_real(2)
    if (abs(res_real(2) - 3.0) > epsilon) error stop
    print *, res_real(3)
    if (abs(res_real(3) - 3.0) > epsilon) error stop
    print *, res_real(4)
    if (abs(res_real(4) - 2.0) > epsilon) error stop
end program
