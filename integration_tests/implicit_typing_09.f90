program implicit_do_9
    integer :: n(3) = [(42, i = 1, 3)] !declaration test (initialization) 
    
    if (n(1) /= 42) error stop 
    if (n(2) /= 42) error stop
    if (n(3) /= 42) error stop
    
    print "(3I3)", n
    
end program 
