program do_concurrent_20
    ! An index-name declared by a type-spec in the concurrent-header has
    ! construct scope and does not change a same-named outer variable.
    implicit none
    integer :: j, r(3), s(2, 3)
    j = 100
    do concurrent (integer :: j = 1:3)
        r(j) = 3*j
    end do
    if (j /= 100) error stop
    if (any(r /= [3, 6, 9])) error stop

    do concurrent (integer :: p = 1:2, q = 1:3)
        s(p, q) = 10*p + q
    end do
    if (s(1, 1) /= 11 .or. s(2, 3) /= 23) error stop

    do concurrent (integer :: q = 1:3)
        r(q) = -q
    end do
    if (any(r /= [-1, -2, -3])) error stop
    print *, "PASSED"
end program
