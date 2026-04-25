program doloop_19
    integer :: i, s
    s = 0
    do i = 1, 5
        if (i == 3) go to 3
        s = s + i
3   end do
    if (s /= 12) error stop
end program
