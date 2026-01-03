program equivalence_06
    use iso_c_binding, only: c_loc, c_f_pointer
    implicit none
    INTEGER, target :: IMACH(16), SMALL(2)
    INTEGER, pointer :: OUTPUT(:)
    REAL, pointer :: RMACH(:)
    call c_f_pointer(c_loc(imach(4)), output, [1])
    call c_f_pointer(c_loc(small(1)), rmach, [1])
    imach(4) = 6
    if(output(1) /= 6) error stop
    small = 99
    rmach = 5.6_8
    if(small(1) /= 1085485875) error stop
    if(small(2) /= 99) error stop
end program
