program do_concurrent_21
    integer :: i, a(8)
    i = 100
    a = -1
    do concurrent (i = 1:1)
        i__dc = 7
        a(i) = i__dc
    end do
    if (a(1) /= 7 .or. a(7) /= -1) error stop
    if (i /= 100) error stop
    i__dc1 = 6
    do concurrent (i = 1:1)
        a(i) = i__dc + i__dc1
    end do
    i__dc2 = 5
    if (a(1) /= 13 .or. i /= 100) error stop
    if (i__dc /= 7 .or. i__dc1 /= 6 .or. i__dc2 /= 5) error stop
end program
