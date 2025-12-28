program write_fortran_04
    implicit none
    character(len=100) :: output
    integer :: val
    real :: fval
    
    val = 12345
    fval = 3.14159
    
    write(output, '(I0)') val
    if (trim(output) /= "12345") error stop "Integer variable write failed"
    
    write(output, '(A, I0, A)') "Value: ", val, " units"
    if (trim(output) /= "Value: 12345 units") error stop "Composite format write failed"
    
    write(output, '(F8.2)') fval
    if (trim(adjustl(output)) /= "3.14") error stop "Real variable write failed"
    
    write(output, '(I0, 1X, I0, 1X, I0)') 10, 20, 30
    if (trim(output) /= "10 20 30") error stop "Multiple integers write failed"
    
    write(output, '(I0)') -99
    if (trim(output) /= "-99") error stop "Negative integer write failed"
end program write_fortran_04
