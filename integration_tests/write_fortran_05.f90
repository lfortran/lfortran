program write_fortran_05
    implicit none
    integer :: iunit
    character(len=100) :: line
    
    open(newunit=iunit, file='test_write_05.txt', status='replace')
    
    write(iunit, '(I5)') 42         ! Right-aligned in 5 characters
    write(iunit, '(I0)') 42         ! No leading spaces

    write(iunit, '(F8.2)') 3.14159  ! Fixed-point
    write(iunit, '(E12.4)') 3.14159 ! Scientific notation

    write(iunit, '(A)') "Hello"
    write(iunit, '(A10)') "World"   ! Right-aligned in 10 characters

    write(iunit, '(A, I0, A, F6.2)') "x=", 10, " y=", 20.5
    
    close(iunit)

    open(newunit=iunit, file='test_write_05.txt', status='old')
    
    read(iunit, '(A)') line
    if (line(1:5) /= "   42") error stop "I5 format verification failed"
    
    read(iunit, '(A)') line
    if (trim(line) /= "42") error stop "I0 format verification failed"
    
    read(iunit, '(A)') line
    if (trim(adjustl(line)) /= "3.14") error stop "F8.2 format verification failed"
    
    read(iunit, '(A)') line
    
    read(iunit, '(A)') line
    if (trim(line) /= "Hello") error stop "Character format verification failed"
    
    read(iunit, '(A)') line
    if (trim(adjustl(line)) /= "World") error stop "A10 format verification failed"
    
    read(iunit, '(A)') line
    if (trim(line) /= "x=10 y= 20.50") error stop "Mixed format verification failed"
    
    close(iunit)
end program write_fortran_05
