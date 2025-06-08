program declaration_01
    implicit none
    integer, parameter :: xi = 2
    real, parameter :: xr = 2.0
    ! real array initialized with integer constant value
    real, parameter :: x_real_3(3) = xi

    ! complex array initialized with integer constant value
    complex, parameter :: x_cmplx_3(3) = xi

    ! integer array initialized with real constant value
    integer, parameter :: x_int_3(3) = xr
    real :: y(2) = real([2, 3])

    print *, x_real_3
    if (any(x_real_3 /= xi)) error stop

    print *, x_cmplx_3
    if (any(x_cmplx_3 /= (xi, 0))) error stop

    print *, x_int_3
    if (any(x_int_3 /= xr)) error stop

    print *, y
    if (abs(y(1) - 2.0) > 1e-7) error stop
    if (abs(y(2) - 3.0) > 1e-7) error stop
end program declaration_01
