program string_98
    implicit none
    character(3) :: c1(6) = 'ab'
    character :: c2(6)*3 = 'ab'
    integer :: i

    do i = 1, 6
        if (c1(i) /= 'ab ') error stop
        if (len(c1(i)) /= 3) error stop
        if (c2(i) /= 'ab ') error stop
        if (len(c2(i)) /= 3) error stop
    end do

    print *, c1(1), len(c1(1))
    print *, c2(1), len(c2(1))
end program
