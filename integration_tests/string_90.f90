program string_test
    implicit none
    character(len=2) :: c(6) = 'a'
    
    !  c(1) should be 'a ' not 'a'
    if (len(c(1)) /= 2) error stop
    if ((c(1)(1:1) /= "a")) error stop
    if ((c(1)(2:2) /= " ")) error stop
    
    print *, c
    
end program string_test
