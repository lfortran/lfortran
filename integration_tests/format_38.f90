program format_38
    implicit none
    real :: value
    character(len=20) :: output

    value = 42.43

    write(output, '(1p, f10.3)') value
    if (output /= "   424.300         ") error stop

    write(output, '(-1p, f10.3)') value
    if (output /= "     4.243         ") error stop

    write(output, '(+2p, f10.3)') value
    if (output /= "  4243.000         ") error stop

    write(output, '(+0p, f10.3)') value
    if (output /= "    42.430         ") error stop

    write(output, '(+1p, f10.3)') value
    if (output /= "   424.300         ") error stop

    print *, "All tests passed"
end program
