program read_26
    implicit none
    character(8) :: string = 'abcd'
    integer :: unit

    open(newunit=unit, file='foobar_test', status='replace')
    write(unit, "(A)") string(1:4)
    rewind(unit)
    
    read(unit, "(A)") string(5:8)
    close(unit, status='delete')
    
    if (string /= 'abcdabcd') error stop 'Expected: abcdabcd, got: ' // string
    print "(A)", string
end program read_26