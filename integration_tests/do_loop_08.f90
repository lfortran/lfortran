program do_loop_08
    implicit none
    integer :: i, j, s

    s = 0
    loop1: do i = 1, 5
        s = s + i
    end do loop1
    if (s /= 15) error stop

    s = 0
    loop2: do 10 i = 1, 5
        s = s + i
10  end do loop2
    if (s /= 15) error stop

    s = 0
    outer: do 20 i = 1, 3
        inner: do 30 j = 1, 2
            s = s + 1
30      end do inner
20  end do outer
    if (s /= 6) error stop
end program
