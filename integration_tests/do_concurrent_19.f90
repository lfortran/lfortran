program do_concurrent_19
    implicit none
    integer :: i, j, r(3), a(2,3)

    i = 77
    j = 100
    r = 0
    a = 0

    ! Test that the outer index remains unchanged
    do concurrent (j = 1:3)
        r(j) = j
    end do

    if (j /= 100) error stop
    if (any(r /= [1, 2, 3])) error stop

    ! Test multiple concurrent indices
    do concurrent (i = 1:2, j = 1:3)
        a(i,j) = i * 10 + j
    end do

    if (i /= 77) error stop
    if (j /= 100) error stop
    if (sum(a) /= 102) error stop

    print *, "PASS"
end program
