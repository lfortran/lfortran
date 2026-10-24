! A scalar the host reads after a `do concurrent` that every iteration
! assigns and then reads: each iteration has to read its own value.
program gpu_liveout_01
    implicit none
    integer :: k, i, last
    integer :: b(100)
    k = 5
    last = -1
    do concurrent (i = 1:100)
        k = i * 10
        b(i) = k + 1
        if (i == 100) last = k + i
    end do
    do i = 1, 100
        if (b(i) /= 10 * i + 1) error stop 1
    end do
    if (last /= 1100) error stop 2
    k = 0
    print *, b(1), b(100), last, k
end program
