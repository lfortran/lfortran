program arith_if_twice
    implicit none
    integer :: n = 0

    if (f()) 10, 20, 30
    10 continue
    20 continue
    30 continue
    
    print *, n
    if (n /= 1) error stop

contains
    integer function f()
        n = n + 1
        f = 1
    end function f
end program
