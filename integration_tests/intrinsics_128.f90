program intrinsics_128
    double precision :: x
    x = 4.23

    print *, idint(x)
    if( .not. idint(x) == 4 ) error stop
end
