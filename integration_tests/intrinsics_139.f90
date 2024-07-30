program intrinsic_139
    integer :: i = 17, j = 3
    real :: f = 17.5_4, f2 = 5.5_4
    real :: r = 17.5_8, r2 = 5.5_8
    integer, parameter :: x1 = modulo(11, 2)
    integer(8), parameter :: x2 = modulo(11, -2)
    real, parameter :: x3 = modulo(11.0_4, 2.0_4)
    real, parameter :: x4 = modulo(11.0_8, -2.0_4)
    real(8), parameter :: x5 = modulo(11.0_8, 2.0_8)
    integer, parameter :: ar1(3) = modulo([1, 2, 3], 2)
    integer(8), parameter :: ar2(3) = modulo([11_8, -2_8, 31_8], 2)
    real, parameter :: ar3(3) = modulo([1.0, 2.0, 3.0], 2.0)
    real(8), parameter :: ar4(3) = modulo([1.0_8, -2.0_8, 3.0_8], 2.0_8)

    print *, x1
    if ( x1 /= 1 ) error stop
    print *, x2
    if ( x2 /= -1 ) error stop
    print *, x3
    if ( abs(x3 - 1.00000000) > 1e-5 ) error stop
    print *, x4
    if ( abs(x4 - (-1.00000000)) > 1e-5 ) error stop
    print *, x5
    if ( abs(x5 - 1.00000000) > 1e-5 ) error stop
    print *, ar1
    if ( any(ar1 /= [1, 0, 1]) ) error stop
    print *, ar2
    if ( any(ar2 /= [1, 0, 1]) ) error stop
    print *, ar3
    if ( any(abs(ar3 - [1.00000000, 0.00000000, 1.00000000]) > 1e-5) ) error stop
    print *, ar4
    if ( any(abs(ar4 - [1.00000000, 0.00000000, 1.00000000]) > 1e-5) ) error stop

    print *, modulo(i,j)
    if ( modulo(i,j) /= 2 ) error stop
    print *, modulo(f,f2)
    if ( abs(modulo(f,f2) - 1.00000000) > 1e-5 ) error stop
    print *, modulo(r,f2)
    if ( abs(modulo(r,f2) - 1.00000000) > 1e-5 ) error stop
    print *, modulo(f,r2)
    if ( abs(modulo(f,r2) - 1.00000000) > 1e-5 ) error stop

    print *, modulo(-i,j)
    if ( modulo(-i,j) /= 1 ) error stop
    print *, modulo(-f,f2)
    if ( abs(modulo(-f,f2) - 4.50000000) > 1e-5 ) error stop
    print *, modulo(-r,f2)
    if ( abs(modulo(-r,f2) - 4.50000000) > 1e-5 ) error stop
    print *, modulo(-f,r2)
    if ( abs(modulo(-f,r2) - 4.50000000) > 1e-5 ) error stop

    print *, modulo(i,-j)
    if ( modulo(i,-j) /= -1 ) error stop
    print *, modulo(f,-f2)
    if ( abs(modulo(f,-f2) + 4.50000000 ) > 1e-5 ) error stop
    print *, modulo(r,-f2)
    if ( abs(modulo(r,-f2) + 4.50000000) > 1e-5 ) error stop
    print *, modulo(f,-r2)
    if ( abs(modulo(f,-r2) + 4.50000000) > 1e-5 ) error stop

    print *, modulo(-i,-j)
    if ( modulo(-i,-j) /= -2 ) error stop
    print *, modulo(-f,-f2)
    if ( abs(modulo(-f,-f2) + 1.00000000) > 1e-5 ) error stop
    print *, modulo(-r,-f2)
    if ( abs(modulo(-r,-f2) + 1.00000000) > 1e-5 ) error stop
    print *, modulo(-f,-r2)
    if ( abs(modulo(-f,-r2) + 1.00000000) > 1e-5 ) error stop

    print *, modulo(17,-3)
    if ( modulo(17,-3) /= -1 ) error stop
    print*, modulo(-17, 3)
    if ( modulo(-17,3) /= 1 ) error stop
end program
