program stop_03
    implicit none

    integer :: x
    
    x = 5
    print *, x

    x = x + 2
    print *, x

    error stop

    x = x + 2
    print *, x
    
end program
