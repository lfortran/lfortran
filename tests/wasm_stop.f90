program wasm_stop
    implicit none

    integer :: x
    
    x = 5
    print *, x

    x = x + 2
    print *, x

    stop

    x = x + 2
    print *, x
    
end program
