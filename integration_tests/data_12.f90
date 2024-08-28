! testing data statements in a module
module data_12_module
    implicit none
    integer :: x, td
    real :: y, z, c1(4), bf1, xx90, xx95, b
    data x, td, b /1, 4, 3/
    data y, z /2.0, 3.0/
    data c1 /0.0, 0.22, -0.14, -0.21/
    data bf1 /0.8/, xx90, xx95 /0.55, 0.62/
end module data_12_module

program data_12_program
    use data_12_module
    implicit none
    print *, x
    if (x /= 1) error stop

    print *, y
    if (abs(y-2.0) > 1e-5) error stop

    print *, z
    if (abs(z-3.0) > 1e-5) error stop

    print *, c1(1)
    if (abs(c1(1)-0.0) > 1e-5) error stop

    print *, c1(2)
    if (abs(c1(2)-0.22) > 1e-5) error stop

    print *, c1(4)
    if (abs(c1(4)+0.21) > 1e-5) error stop

    print *, bf1
    if (abs(bf1-0.8) > 1e-5) error stop

    print *, xx90
    if (abs(xx90-0.55) > 1e-5) error stop

    print *, xx95
    if (abs(xx95-0.62) > 1e-5) error stop
end program data_12_program
