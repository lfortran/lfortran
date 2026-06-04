program empty_array_03
    implicit none

    integer, parameter :: z1(0) = 0
    integer, parameter :: z3(0,0,0) = 0
    real, parameter :: x1(0) = 0.0
    real, parameter :: x3(0,0,0) = 0.0
    complex, parameter :: a1(0) = (0.0, 0.0)
    complex, parameter :: a3(0,0,0) = (0.0, 0.0)

    if (iall(z1) /= not(int(0, kind(z1)))) error stop
    if (iall(z3) /= not(int(0, kind(z3)))) error stop

    if (iparity(z1) /= 0) error stop
    if (iparity(z3) /= 0) error stop

    if (maxval(z1) /= -huge(0) - 1) error stop
    if (maxval(z3) /= -huge(0) - 1) error stop
    if (maxval(x1) /= -huge(x1)) error stop
    if (maxval(x3) /= -huge(x3)) error stop

    if (minval(z1) /= huge(0)) error stop
    if (minval(z3) /= huge(0)) error stop
    if (minval(x1) /= huge(x1)) error stop
    if (minval(x3) /= huge(x3)) error stop

    if (product(z1) /= 1) error stop
    if (product(z3) /= 1) error stop
    if (product(x1) /= 1.0) error stop
    if (product(x3) /= 1.0) error stop
    if (product(a1) /= (1.0, 0.0)) error stop
    if (product(a3) /= (1.0, 0.0)) error stop

    if (sum(z1) /= 0) error stop
    if (sum(z3) /= 0) error stop
    if (sum(x1) /= 0.0) error stop
    if (sum(x3) /= 0.0) error stop
    if (sum(a1) /= (0.0, 0.0)) error stop
    if (sum(a3) /= (0.0, 0.0)) error stop
    print *, "Pass"
end program
