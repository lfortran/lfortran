program intrinsics_128
    double precision :: x
    x = 4.23

    print *, idint(x)
    if( .not. idint(x) == 4 ) error stop

    print *, idint(4.23D0)
    if( .not. idint(4.23D0) == 4 ) error stop

end program
