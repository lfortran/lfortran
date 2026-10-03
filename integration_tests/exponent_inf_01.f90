program exponent_inf_01
    implicit none
    real :: inf, ninf, nan_val
    character(len=3) :: cinf = 'INF'
    character(len=4) :: cninf = '-INF'
    character(len=3) :: cnan = 'NaN'

    read(cinf, *) inf
    read(cninf, *) ninf
    read(cnan, *) nan_val

    if (exponent(inf) /= huge(0)) error stop
    if (exponent(ninf) /= huge(0)) error stop    
    if (exponent(nan_val) /= huge(0)) error stop
end program exponent_inf_01