program implicit_do_1
    integer :: n(3)
    n = [(42, i = 1, size(n))]
    if (n(1) /= 42) error stop 
    if (n(2) /= 42) error stop
    if (n(3) /= 42) error stop
    print "(3I3)", n
end program
