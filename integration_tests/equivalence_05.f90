program equivalence_05
    implicit none
    INTEGER IMACH(16), OUTPUT, SMALL(2)
    REAL RMACH
    EQUIVALENCE (IMACH(4),OUTPUT), (RMACH,SMALL(1))
    imach(4) = 6
    if(output /= 6) error stop
    small = 99
    rmach = 5.6_8
    if(small(1) /= 1085485875) error stop
    if(small(2) /= 99) error stop
end program
