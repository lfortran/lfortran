program doloop_21
    ! Nested DO loops that share their terminal statement
    implicit none
    integer :: i, j, k, n
    n = 0
    do 10 i = 1, 3
    do 10 j = 1, 3
    do 10 k = 1, 2
        if (k == 2) go to 10
        if (j == 2) go to 10
        n = n + 1
10  continue
    if (n /= 6) error stop
    k = 0
    do 20 i = 1, 2
    do 20 j = 1, 2
20  k = k + i*j
    if (k /= 9) error stop
end program doloop_21
