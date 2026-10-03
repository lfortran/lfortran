program do_concurrent_15
    implicit none
    integer :: i, j
    integer :: a(3), b(4)

    i = 99
    j = 99

    ! Index variables declared inline with a type-spec that are ALSO declared
    ! in the enclosing scope must not cause a duplicate-symbol error.
    do concurrent (integer :: j = 1:3)
        a(j) = j
    end do

    do concurrent (integer :: i = 1:4)
        b(i) = 2*i
    end do

    if (a(1) /= 1) error stop
    if (a(2) /= 2) error stop
    if (a(3) /= 3) error stop
    if (b(1) /= 2) error stop
    if (b(2) /= 4) error stop
    if (b(3) /= 6) error stop
    if (b(4) /= 8) error stop

    print *, a, b
end program do_concurrent_15
