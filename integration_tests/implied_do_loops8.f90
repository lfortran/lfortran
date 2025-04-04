
program implied_do_loops8
    integer :: i, input(2,2)
    input = reshape([(i, i = 1, 4)], [2, 2])
    print * , input
    if (input(1,1) /= 1) error stop
    if (input(2,1) /= 2) error stop
    if (input(1,2) /= 3) error stop
    if (input(2,2) /= 4) error stop
end program