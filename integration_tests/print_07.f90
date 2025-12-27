program print_07
    implicit none
    character(20) :: output
    
    ! Test RU - Round Up
    write(output, "(RU,F6.2)") 1.235
    print *, output
    if (output /= "  1.24") error stop "RU rounding failed"
    
    ! Test RD - Round Down
    write(output, "(RD,F6.2)") 1.235
    print *, output
    if (output /= "  1.23") error stop "RD rounding failed"
    
    ! Test RN - Round to Nearest (round half away from zero)
    write(output, "(RN,F6.2)") 1.235
    print *, output
    if (output /= "  1.24") error stop "RN rounding (1.235) failed"
    
    write(output, "(RN,F6.2)") 1.225
    print *, output
    if (output /= "  1.23") error stop "RN rounding (1.225) failed"
    
    ! Test RZ - Round toward Zero
    write(output, "(RZ,F6.2)") 1.225
    print *, output
    if (output /= "  1.22") error stop "RZ rounding (1.225) failed"
    
    write(output, "(RZ,F6.2)") 1.235
    print *, output
    if (output /= "  1.23") error stop "RZ rounding (1.235) failed"

end program
