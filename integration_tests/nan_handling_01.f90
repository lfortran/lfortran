program nan_handling_01
    double precision :: b, c
    c = 0.0D0
    b = c / 0.0D0
    if (b >= 2.0D0) error stop
end program
