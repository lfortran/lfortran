program doloop_10
    integer(4) :: i, j = 0
    integer(8) :: n = 10

    do i = 1, n
        j = j + i
    end do

    print *, j
    if (j /= 55) error stop
end program
