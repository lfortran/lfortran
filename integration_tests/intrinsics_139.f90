program intrinsic_139
    integer :: i = 17, j = 3
    real :: f = 17.5_4, f2 = 5.5_4
    real :: r = 17.5_8, r2 = 5.5_8

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

end program