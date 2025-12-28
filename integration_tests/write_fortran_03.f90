program write_fortran_03
    implicit none
    integer :: iunit, ierr
    
    open(newunit=iunit, file='test_write_03.txt', status='replace')
    
    ! Write without newline using advance='no'
    write(iunit, '(A)', advance='no') "Part1"
    write(iunit, '(A)', advance='no') " Part2"
    write(iunit, '(A)') " Part3"
    
    ! Write numbers on same line
    write(iunit, '(I0)', advance='no') 10
    write(iunit, '(A)', advance='no') " + "
    write(iunit, '(I0)', advance='no') 20
    write(iunit, '(A)', advance='no') " = "
    write(iunit, '(I0)') 30
    
    close(iunit)
    
    open(newunit=iunit, file='test_write_03.txt', status='old')
    
    character(len=100) :: line
    read(iunit, '(A)') line
    if (trim(line) /= "Part1 Part2 Part3") error stop "Line 1 mismatch"
    
    read(iunit, '(A)') line
    if (trim(line) /= "10 + 20 = 30") error stop "Line 2 mismatch"
    
    close(iunit)
    
end program write_fortran_03
