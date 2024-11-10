program arithmetic_if1_cc
    integer :: x, c
    x = -3
    c = 0
    if ("yy") 1, 2, 3
    1 c = c + 1
    2 c = c + 2
    3 c = c + 4
    print *, c
    print *, "continued compilation"
    if (c /= 7) error stop
end program
    