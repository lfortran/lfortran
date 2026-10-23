! Variables that a `do concurrent` defines inside BLOCK constructs of an
! iteration, nested and in an inner loop: the kernel puts a thread's copy
! back after each definition there.
program gpu_liveout_09
    implicit none
    integer :: k, m, s, i, j, a(100)
    k = 0
    m = 0
    s = 0
    do concurrent (i = 1:100)
        a(i) = i
        block
            integer :: t
            t = i
            if (t == 7) k = 70
            block
                if (t == 9) m = 90
            end block
            do j = 1, 3
                if (t == 11) s = s + j
            end do
        end block
    end do
    print *, k, m, s
    if (k /= 70) error stop 1
    if (m /= 90) error stop 2
    if (s /= 6) error stop 3
    if (a(100) /= 100) error stop 4
end program
