program doloop11
    implicit none

    integer :: i
    integer :: s
    s = 0
    do i = 1, 2
        s = i
    end do

    print *, s, i

    if (i /= 3) error stop
    if (s /= 2) error stop
end program doloop11
