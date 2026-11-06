! A scalar a `do concurrent` may read before assigning it in the same
! iteration -- because no iteration assigns it, or one reads it before
! another assigns it in a loop of one iteration -- needs the value it had
! before the loop, even when a later statement assigns it before reading
! it.
program gpu_liveout_18
    implicit none
    integer, parameter :: n = 100
    real(8) :: t, x(n), y(n)
    integer :: i, m
    t = 5
    x = -1
    do concurrent (i = 1:n)
        if (x(i) > 0) t = x(i)
        y(i) = t
    end do
    t = 0
    if (any(y /= 5)) error stop 1

    m = 1
    t = 7
    do concurrent (i = 1:m)
        y(i) = t
        t = i
    end do
    t = 0
    if (y(1) /= 7) error stop 2
    print *, y(1), t
end program
