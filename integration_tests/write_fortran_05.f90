program write_fortran_05
    implicit none
    character(len=100) :: output
    
    write(output, '(I5)') 42
    if (output(1:5) /= "   42") error stop "I5 format failed"
    
    write(output, '(I0)') 42
    if (trim(output) /= "42") error stop "I0 format failed"
    
    write(output, '(F8.2)') 3.14159
    if (trim(adjustl(output)) /= "3.14") error stop "F8.2 format failed"
    
    write(output, '(E12.4)') 3.14159
    if (index(output, "314") == 0 .and. index(output, "3.14") == 0) error stop "E12.4 format failed"
    
    write(output, '(A)') "Hello"
    if (trim(output) /= "Hello") error stop "A format failed"
    
    write(output, '(A10)') "World"
    if (output(6:10) /= "World") error stop "A10 format failed"
    
    write(output, '(A, I0, A, F6.2)') "x=", 10, " y=", 20.5
    if (trim(output) /= "x=10 y= 20.50") error stop "Mixed format failed"
    
    write(output, '(I0, 1X, I0, 1X, I0)') 1, 2, 3
    if (trim(output) /= "1 2 3") error stop "Multiple separator format failed"
end program write_fortran_05
