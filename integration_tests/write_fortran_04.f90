program write_fortran_04
    implicit none
    character(len=50) :: buffer
    integer :: val
    
    val = 12345

    write(buffer, '(I0)') val
    if (trim(buffer) /= "12345") error stop "Integer write failed"
    
    write(buffer, '(A, I0, A)') "Value: ", val, " units"
    if (trim(buffer) /= "Value: 12345 units") error stop "Formatted write failed"

    write(buffer, '(F8.2)') 3.14159
    if (trim(adjustl(buffer)) /= "3.14") error stop "Real write failed"
    
end program write_fortran_04
