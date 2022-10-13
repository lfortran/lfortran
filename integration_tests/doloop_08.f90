program doloop_08
    implicit none
    integer :: i
    i = 1
    do
        i = i + 1
        if (i == 10) exit
    end do
    if (i /= 10) error stop
end program doloop_08
