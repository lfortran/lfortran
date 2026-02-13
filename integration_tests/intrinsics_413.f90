program ichar_test
    implicit none
    integer :: i
    
    ! This should work - char(0) returns a character of length 1
    i = ichar(char(0))
    print *, i
    if (i /= 0) error stop
    
end program ichar_test
