program main 
    print*, dnint(1.0_8, 8)
    if (abs(dnint(1.0_8, 8) - 1.0_8) > 10e-5 ) error stop
end program