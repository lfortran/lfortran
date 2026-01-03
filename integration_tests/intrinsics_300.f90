program intrinsics_300
    integer, parameter :: ar1 = kind([1.0, 1.0])
    print *, ar1
    if ( ar1 /= 4 ) error stop
end program
