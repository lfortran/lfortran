program write_fortran_02
    implicit none
    character(len=100) :: output
    integer :: ierr
    
    write(output, '(A)', iostat=ierr) "Hello World"
    if (ierr /= 0) error stop "String write failed"
    if (trim(output) /= "Hello World") error stop "String write verification failed"
    
    write(output, '(I0)', iostat=ierr) 42
    if (ierr /= 0) error stop "Integer write failed"
    if (trim(output) /= "42") error stop "Integer write verification failed"
    
    write(output, '(F8.2)', iostat=ierr) 3.14159
    if (ierr /= 0) error stop "Real write failed"
    if (trim(adjustl(output)) /= "3.14") error stop "Real write verification failed"
    
    write(output, '(A, I0, A)', iostat=ierr) "Value: ", 99, " units"
    if (ierr /= 0) error stop "Mixed format write failed"
    if (trim(output) /= "Value: 99 units") error stop "Mixed format verification failed"
    
    write(output, *, iostat=ierr) 1, 2, 3
    if (ierr /= 0) error stop "Default format write failed"
end program write_fortran_02
