program intrinsics_70
    implicit none
    
    real :: x
    real, parameter :: epsilon = 1e-10
    real :: res_real(4)
    real, parameter :: r1 = aint(11.21)
    real(4), parameter :: r2 = aint(11.21_8, 4)
    real(8), parameter :: r3 = aint(231.3, 8)
    real, parameter :: ar1(4) = aint([1.2, 3.5, 3.4, 2.1])
    real(8), parameter :: ar3(4) = aint([1.2_4, 3.5_4, 3.4_4, 2.1_4], 8)
    real :: arr1(4) = [1.2, 3.5, 3.4, 2.1]
    real(4) :: arr2(4) = [1.2_4, 3.5_4, 3.4_4, 2.1_4]
    real(8) :: arr3(4) = [1.2_8, 3.5_8, 3.4_8, 2.1_8]

    print *, aint(arr1)
    if (any(abs(aint(arr1) - [1.0, 3.0, 3.0, 2.0]) > epsilon)) error stop
    print *, aint(arr2, 4)
    if (any(abs(aint(arr2, 4) - [1.0, 3.0, 3.0, 2.0]) > epsilon)) error stop
    print *, aint(arr3, 8)
    if (any(abs(aint(arr3, 8) - [1.0, 3.0, 3.0, 2.0]) > epsilon)) error stop

    print *, r1
    if (abs(r1 - 11.0) > epsilon) error stop
    print *, r2
    if (abs(r2 - 11.0) > epsilon) error stop
    print *, r3
    if (abs(r3 - 231.0) > epsilon) error stop
    print *, ar1
    if (any(abs(ar1 - [1.0, 3.0, 3.0, 2.0]) > epsilon)) error stop
    print *, ar3
    if (any(abs(ar3 - [1.0, 3.0, 3.0, 2.0]) > epsilon)) error stop

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
