program implicit_do_01
    integer :: n
    n = 0
    do 10 i = 1, 2
        n = n + i
10  continue
    if (n /= 3) error stop
end program
