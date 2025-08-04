program intrinsics_224
    implicit none
    real(4) :: x1 = 1.0_4, x2 = 42.0_4
    real(8) :: y1 = 1.0_8, y2 = 42.0_8
    real(4), parameter :: x3 = 1.0_4, x4 = 42.0_4
    real(8), parameter :: y3 = 1.0_8, y4 = 42.0_8

    print*, spacing(x1) 
    if (abs(spacing(x1) - 1.19209290E-07) > 1e-6) error stop
    print*, spacing(y1)
    if (abs(spacing(y1) - 2.2204460492503131E-016) > 1e-6) error stop     
    print*, spacing(x2)
    if (abs(spacing(x2) - 3.81469727E-06) > 1e-6) error stop
    print*, spacing(y2)
    if (abs(spacing(y2) - 7.1054273576010019E-015) > 1e-6) error stop

    print*, spacing(x3)  
    if ( abs(spacing(x3) - 1.19209290e-07) > 1e-6 ) error stop
    print*, spacing(y3)   
    if ( abs(spacing(y3) - 2.2204460492503131e-16) > 1e-6 ) error stop 
    print*, spacing(x4)
    if ( abs(spacing(x4) - 3.81469727e-06) > 1e-6 ) error stop
    print*, spacing(y4)
    if ( abs(spacing(y4) - 2.2204460492503131e-16) > 1e-6 ) error stop

end program