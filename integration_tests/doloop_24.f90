program doloop_24
    ! DO loops with a non-constant step in the main program body
    implicit none
    integer :: i, k, m, n, s
    k = 2
    n = 0
    s = 0
    do i = 1, 10, k
        n = n + 1
        s = s + i
    end do
    print *, n, s
    if (n /= 5) error stop
    if (s /= 25) error stop

    m = -3
    n = 0
    s = 0
    do i = 10, 1, m
        n = n + 1
        s = s + i
    end do
    print *, n, s
    if (n /= 4) error stop
    if (s /= 22) error stop
end program
