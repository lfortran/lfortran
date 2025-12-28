program write_fortran_02
    implicit none
    integer :: iunit, ierr
    character(len=100) :: line
    
    open(newunit=iunit, file='test_write_02.txt', status='replace', iostat=ierr)
    if (ierr /= 0) error stop "Failed to open file"
    
    write(iunit, '(A)', iostat=ierr) "Line 1"
    if (ierr /= 0) error stop "Write 1 failed"
    
    write(iunit, '(I0)', iostat=ierr) 42
    if (ierr /= 0) error stop "Write 2 failed"
    
    write(iunit, '(F8.2)', iostat=ierr) 3.14159
    if (ierr /= 0) error stop "Write 3 failed"
    
    close(iunit)

    open(newunit=iunit, file='test_write_02.txt', status='old')
    
    read(iunit, '(A)') line
    if (trim(line) /= "Line 1") error stop "Line 1 verification failed"
    
    read(iunit, '(A)') line
    if (trim(line) /= "42") error stop "Line 2 verification failed"
    
    read(iunit, '(A)') line
    if (trim(adjustl(line)) /= "3.14") error stop "Line 3 verification failed"
    
    close(iunit)
end program write_fortran_02
