program write_fortran_02
    implicit none
    character(len=100) :: output
    integer :: val, ierr
    real :: fval
    
    val = 12345
    fval = 3.14159
    
    write(output, '(I0)') val
    if (trim(output) /= "12345") error stop "Integer variable failed"
    
    write(output, '(A, I0, A)') "Value: ", val, " units"
    if (trim(output) /= "Value: 12345 units") error stop "Mixed format failed"
    
    write(output, '(F8.2)') fval
    if (trim(adjustl(output)) /= "3.14") error stop "Real variable failed"
    
    write(output, '(I0, 1X, I0, 1X, I0)') 10, 20, 30
    if (trim(output) /= "10 20 30") error stop "Separator format failed"
    
    write(output, '(I0)') -99
    if (trim(output) /= "-99") error stop "Negative integer failed"
    
    write(output, '(A, I0, A, F6.2)') "x=", 10, " y=", 20.5
    if (trim(output) /= "x=10 y= 20.50") error stop "Complex mixed format failed"
    
    write(output, '(A10)') "World"
    if (output(6:10) /= "World") error stop "A10 format failed"
    
    write(output, '(A)', iostat=ierr) "Hello World"
    if (ierr /= 0) error stop "iostat check failed"
end program write_fortran_02
