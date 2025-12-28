program write_fortran_01
    implicit none
    character(len=100) :: output
    
    write(output, '(A)') "Formatted output"
    if (trim(output) /= "Formatted output") error stop "Format write failed"
    
    write(output, '(I0)') 42
    if (trim(output) /= "42") error stop "I0 integer write failed"
    
    write(output, '(I0, 1X, A)') 100, "meters"
    if (trim(output) /= "100 meters") error stop "Multi-value write failed"
    
    write(output, '(I5)') 42
    if (output(1:5) /= "   42") error stop "I5 format failed"
    
    write(output, *) 123
    if (trim(adjustl(output)) /= "123") error stop "Default format write failed"
end program write_fortran_01
