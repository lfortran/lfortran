program main 
    print*, dint(1.0_8, 8)
    if (abs(dint(1.0_8, 8) - 1.0_8) > 10e-5 ) error stop
end program