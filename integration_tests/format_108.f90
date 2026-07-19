program format_108
    implicit none
    real :: x = -0.0
    real(8) :: y
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

    y = 9.999d9
    write(s,"(ES9.2E1)") y
    if (trim(s) /= "*********") error stop

    write(s,"(ES0.2E0)") y
    if (trim(adjustl(s)) /= "1.00E+10") error stop

    y = 9.999d99
    write(s,"(ES15.2)") y
    if (trim(adjustl(s)) /= "1.00+100") error stop
end program format_108