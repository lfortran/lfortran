program doloop_23
    ! A comment after the terminal statement of labelled DO loops
    implicit none
    integer :: i, j, n
    n = 0
    do 10 i = 1, 3
    do 10 j = 1, 3
        n = n + 1
10  continue ! both loops end here
    n = n + 100
    if (n /= 109) error stop
    n = 0
    do 20 i = 1, 4
20  n = n + i ! the loop ends here
    n = n + 100
    if (n /= 110) error stop
    print *, n
end program doloop_23
