program format_69
    implicit none
    real :: x = -0.0
    character(len=20) :: s

    write(s,"(ES10.2E0)") 123.45
    if (trim(adjustl(s)) /= "1.23E+2") error stop

    write(s,"(ES10.2E1)") x
    if (trim(adjustl(s)) /= "-0.00E+0") error stop

    write(s,"(ES0.0E0)") x
    if (trim(adjustl(s)) /= "-0.E+0") error stop

    write(s,"(ES10.2E2)") 1.0e-2
    if (trim(adjustl(s)) /= "1.00E-02") error stop

    write(s,"(ES10.2E2)") 9.999e+1
    if (trim(adjustl(s)) /= "1.00E+02") error stop

    write(s,"(ES10.2E2)") 9.999e-1
    if (trim(adjustl(s)) /= "1.00E+00") error stop

    write(s,"(ES6.1E1)") -0.0
    if (trim(adjustl(s)) /= "******") error stop

end program format_69
