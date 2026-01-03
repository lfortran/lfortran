program data_07
    implicit none

    double precision bands
    dimension bands(4, 5)
    common /jac/ bands
    
    data bands / 0.00D0, -1.0D0, 0.25D0, 0.10D0, 0.25D0, -5.0D0, 0.25D0, 0.10D0, &
    0.25D0, -25.0D0, 0.25D0, 0.10D0, 0.25D0,-125.0D0, 0.25D0, 0.00D0, &
    0.25D0, -625.0D0, 0.00D0, 0.00D0 /


end program
