program write_fortran_01
    implicit none
    character(len=100) :: output
    
    write(output, '(A)') "Hello"
    if (trim(output) /= "Hello") error stop "A format failed"
    
    write(output, '(I0)') 42
    if (trim(output) /= "42") error stop "I0 format failed"
    
    write(output, '(I5)') 42
    if (output(1:5) /= "   42") error stop "I5 format failed"
    
    write(output, '(F8.2)') 3.14159
    if (trim(adjustl(output)) /= "3.14") error stop "F8.2 format failed"
    
    write(output, '(E12.4)') 1.23e-5
    if (index(output, "123") == 0) error stop "E12.4 format failed"
    
    write(output, *) 123
    if (trim(adjustl(output)) /= "123") error stop "Default format failed"
end program write_fortran_01
