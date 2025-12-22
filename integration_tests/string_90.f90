program string_test
    implicit none
    character(len=2) :: c(6) = 'a'
    
    !  c(1) should be 'a ' not 'a'
    if (len(c(1)) /= 2) then
        print *, "Error: length mismatch"
        stop 1
    end if
    
    print *, c
    
end program string_test
